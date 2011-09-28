-module(proj4).
-export([init/1, transform/3, get_def/1]).

-include_lib("eunit/include/eunit.hrl").

-on_load(load_nif/0).

load_nif() ->
    Dir = filename:dirname(code:where_is_file("proj4erl.app")),
    Path = filename:join([Dir, "..", "priv", "proj4erl_nif"]),
    erlang:load_nif(Path, 0).

init(_Args) ->
    nif_error(?LINE).

transform(_From, _To, _Point) ->
    nif_error(?LINE).

get_def(_Proj) ->
    nif_error(?LINE).

nif_error(Line) ->
    exit({nif_not_loaded,module,?MODULE,line,Line}).


init_test() ->
    proj4:init(["proj=merc", "ellps=clrk66", "lat_ts=33"]).

wgs84_2180_test() ->
    % WGS84 = proj4:init(string:tokens("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", " ")),
    % CRS2180 = proj4:init(string:tokens("+proj=tmerc +lat_0=0 +lon_0=19 +k=0.9993 +x_0=500000 +y_0=-5300000 +ellps=GRS80 +units=m +no_defs", " ")),
    WGS84 = proj4:init(string:tokens("init=epsg:4326", " ")),
    CRS2180 = proj4:init(string:tokens("init=epsg:2180", " ")),
    P = {21.049804687501, 52.22900390625},
    P2 = proj4:transform(WGS84, CRS2180, P),
    io:format("Transformed from '~s' to '~s': ~p~n", [proj4:get_def(WGS84), proj4:get_def(CRS2180), P2]).
