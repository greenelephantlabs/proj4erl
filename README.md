proj4erl
--------

proj4erl is a NIF-based binding for [PROJ.4] library.

It supports three API calls

  - init/1 - equivalent of pj_init_plus, returns opaque value which should be used as a reference to initialized projection (equivalent to projPJ type)
  - get_def/1 - equivalent of pj_get_def, returns string
  - transform/3 - equivalent of pj_transform

Here is an example of usage:

```erlang
    {ok, WGS84} = proj4:init("+init=epsg:4326"),
    {ok, CRS2180} = proj4:init("+init=epsg:2180"),
    P = {21.049804687501, 52.22900390625},
    {ok, P2} = proj4:transform(WGS84, CRS2180, P),
    {639951.5695094677, 486751.7840663176} = P2.
```

Initialized projPJ references are being automatically garbage collected. 

Feel free to fork and send pull requests!

TODO
----
 * ad-hoc transform calls, with implicit initialization of projPJ and
   it's caching
 * support for linestrings, rings, polygons for speed

  [proj.4]: http://trac.osgeo.org/proj/
