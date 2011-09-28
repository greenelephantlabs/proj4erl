// proj4erl_nif.c
#include "projects.h"
#include "erl_nif.h"
#include <string.h>
#include <assert.h>

#define MAXBUFLEN 1024
/* #define DEBUG */

#ifdef DEBUG
#define DFLOG(fmt, ...) fprintf(stderr, fmt, __VA_ARGS__);
#define DLOG(fmt) fprintf(stderr, fmt);
#else
#define DFLOG(fmt, ...)
#define DLOG(fmt)
#endif

static ErlNifResourceType *pj_cd_type = NULL;

typedef struct { projPJ pj; } pj_cd;

static void
gc_pj_cd(ErlNifEnv *env, void *cd)
{
  pj_free(((pj_cd *) cd)->pj);
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  ErlNifResourceType *rt = enif_open_resource_type(env, "proj4erl",
                                                   "pj_type", gc_pj_cd, ERL_NIF_RT_CREATE, NULL);
  if (rt == NULL)
    return -1;

  pj_cd_type = rt;

  return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv,
        ERL_NIF_TERM load_info)
{
  return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
  pj_deallocate_grids();
  return;
}


static ERL_NIF_TERM
erl_proj4_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM result;
  ERL_NIF_TERM head, tail;
  pj_cd* cd;
  unsigned paramc;
  char** paramv;
  char tmp[MAXBUFLEN];
  int tmpc;
  int i = 0;

  if (argc != 1) {
    DFLOG("Wrong arguments number %d\n", argc);
    return enif_make_badarg(env);
  }

  ERL_NIF_TERM list = argv[0];

  if (!enif_get_list_length(env, list, &paramc)) {
    DLOG("Not a list\n");
    return enif_make_badarg(env);
  }

  paramv = (char **)enif_alloc(paramc);

  while (enif_get_list_cell(env, list, &head, &tail)) {
    tmpc = enif_get_string(env, head, tmp, sizeof(tmp), ERL_NIF_LATIN1);
    if (tmpc <= 0) {
      DLOG("Unable to read one of args\n");
      return enif_make_badarg(env);
    }
    DFLOG("Paramv %d len %d \"%s\"\n", i, tmpc, tmp);
    paramv[i] = (char *)enif_alloc(strlen(tmp) + 1);
    strcpy(paramv[i], tmp);
    list = tail;
    i++;
  }
  if (i != paramc) {
    DLOG("Inconsistency\n");
    return enif_make_badarg(env);
  }

  cd = enif_alloc_resource(pj_cd_type, sizeof(pj_cd));

  DFLOG("Paramc: %d\n", paramc);
  for (i = 0; i < paramc; i++) {
    DFLOG("Paramv[%d] = %s\n", i, paramv[i]);
  }

  if (!(cd->pj = pj_init(paramc, paramv))) {
    enif_release_resource(cd);
    DFLOG("Unable to init PJ: %s\n", pj_strerrno(pj_errno));
    return enif_make_badarg(env);
  }

  result = enif_make_resource(env, cd);
  enif_release_resource(cd);

  return result;
}

static ERL_NIF_TERM
erl_proj4_get_def(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM res;
  pj_cd *p;
  char* def;

  if (!enif_get_resource(env, argv[0], pj_cd_type, (void **) &p)) {
    DLOG("Unable to get arg\n");
    return enif_make_badarg(env);
  }
  def = pj_get_def(p->pj, 0);
  int defl = strlen(def);
  memcpy(enif_make_new_binary(env, defl, &res), def, defl);
  return res;
}

static ERL_NIF_TERM
erl_proj4_transform(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  const ERL_NIF_TERM *terms;
  int vint;
  double x, y, z;
  pj_cd *from, *to;

  if (!enif_get_resource(env, argv[0], pj_cd_type, (void **) &from)) {
    DLOG("Unable to get from\n");
    return enif_make_badarg(env);
  }
  if (!enif_get_resource(env, argv[1], pj_cd_type, (void **) &to)) {
    DLOG("Unable to get to\n");
    return enif_make_badarg(env);
  }


  if (!enif_get_tuple(env, argv[2], &vint, &terms)) {
    DLOG("Not a tuple\n");
    return enif_make_badarg(env);
  }

  if (!((vint == 2) || (vint == 3))) {
    DLOG("Wrong tuple length\n");
    return enif_make_badarg(env);
  }

  if (!enif_get_double(env, terms[0], &x)) {
    DLOG("Unable to get X\n");
    return enif_make_badarg(env);
  }
  if (!enif_get_double(env, terms[1], &y)) {
    DLOG("Unable to get Y\n");
    return enif_make_badarg(env);
  }

  if (vint == 3) {
    if (!enif_get_double(env, terms[2], &z)) {
      DLOG("Unable to get Z\n");
      return enif_make_badarg(env);
    }
  }
  DFLOG("From: %x %s\n", &(from->pj), pj_get_def(from->pj, 0));
  DFLOG("To: %x %s\n", &(to->pj), pj_get_def(to->pj, 0));
  DFLOG("Old: %f %f\n", x, y);
  if(pj_is_latlong(from->pj)) {
    x *= DEG_TO_RAD;
    y *= DEG_TO_RAD;
    z *= DEG_TO_RAD;
  }
  if(pj_transform( from->pj, to->pj, 1, 1, &x, &y, &z) != 0) {
    DFLOG("Unable to transform: %s\n", pj_strerrno(pj_errno));
    return enif_make_badarg(env);
  }
  if(pj_is_latlong(to->pj)) {
    x *= RAD_TO_DEG;
    y *= RAD_TO_DEG;
    z *= RAD_TO_DEG;
  }
  DFLOG("New: %f %f\n", x, y);

  if (vint == 3) {
    return enif_make_tuple(env, 3, enif_make_double(env, x), enif_make_double(env, y), enif_make_double(env, z));
  } else {
    return enif_make_tuple(env, 2, enif_make_double(env, x), enif_make_double(env, y));
  }
}

/* static ERL_NIF_TERM */
/* get_binary(ErlNifEnv* env, ERL_NIF_TERM a1) */
/* { */
/*   unsigned long mem_loc; */
/*   if (!enif_get_ulong(env, a1, &mem_loc)) */
/*     { */
/*       return enif_make_badarg(env); */
/*     } */
/*   else */
/*     { */
/*       Binary* bin_ptr = (Binary*) mem_loc; */
/*       ErlNifBinary nif_bin; */

/*       enif_alloc_binary(env, bin_ptr->orig_size, &nif_bin); */
/*       memcpy(nif_bin.data, bin_ptr->orig_bytes, bin_ptr->orig_size); */
/*       return enif_make_binary(env, &nif_bin); */
/*     } */
/* } */

static ErlNifFunc proj4erl_funcs[] =
  {
    {"init", 1, erl_proj4_init},
    {"transform", 3, erl_proj4_transform},
    {"get_def", 1, erl_proj4_get_def}
  };

ERL_NIF_INIT(proj4, proj4erl_funcs, load, reload, upgrade, unload)
