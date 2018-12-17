%%%-------------------------------------------------------------------
%% @doc cowapp public API
%% @end
%%%-------------------------------------------------------------------

-module(cowapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', 
	     [{"/", simple_handler, [list]},
	     {"/get/:id", simple_handler, [get]},
	     {"/add/:title", simple_handler, [add]},
	     {"/update/:id/:title", simple_handler, [update]},
	     {"/delete/:id", simple_handler, []}
	]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),	
    cowapp_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
