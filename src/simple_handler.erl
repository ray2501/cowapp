-module(simple_handler).

%% Handler API
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([db_to_json/2]).
-export([text_to_db/2]).
-export([get_record_list/2]).
-export([get_one_record/2]).
-export([add_one_recotd/2]).
-export([update_one_recotd/2]).
-export([delete_resource/2]).
-record(state, {op}).

init(Req, Opts) ->
    [Op | _] = Opts,
    State = #state{op=Op},
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>],	
    {Methods, Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, db_to_json}
	], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/x-www-form-urlencoded">>, text_to_db}
     ], Req, State}.

db_to_json(Req, #state{op=Op} = State) ->
    {Body, Req1, State1} = case Op of
        list ->
            get_record_list(Req, State);
        get ->
            get_one_record(Req, State)
    end,
    {Body, Req1, State1}.

text_to_db(Req, #state{op=Op} = State) ->
    {Body, Req1, State1} = case Op of
        add ->
            add_one_recotd(Req, State);
        update ->		   
            update_one_recotd(Req, State)
    end,
    {Body, Req1, State1}.

get_record_list(Req, State) ->
      case epgsql:connect("localhost", "danilo", "danilo", #{
          database => "danilo",
          timeout => 1000
        }) of
      {ok, C} ->
          Data = epgsql:squery(C, "SELECT * from Notes"),
          {ok, Cs, Rs} = Data,
          Columns = [X || {_, X, _, _, _, _,_} <- Cs],
          Result = [lists:zipwith(fun(N, V) -> {N, V} end,
                 Columns, tuple_to_list(Row)) || Row <- Rs],
          ok = epgsql:close(C),
          Body = jsx:encode(Result),
          {Body, Req, State};
      {error, _} ->
	  Req1 = cowboy_req:reply(500, Req),
	  {false, Req1, State}
      end.

get_one_record(Req, State) ->
    Id = cowboy_req:binding(id, Req),
    case epgsql:connect("localhost", "danilo", "danilo", #{
        database => "danilo",
        timeout => 1000
      }) of
    {ok, C} ->
        Data = epgsql:equery(C, "select * from Notes where Id = $1", [Id]),		      
        {ok, Cs, Rs} = Data,
        Columns = [X || {_, X, _, _, _, _,_} <- Cs],
        Result = [lists:zipwith(fun(N, V) -> {N, V} end,
               Columns, tuple_to_list(Row)) || Row <- Rs],
        ok = epgsql:close(C),
        Body = jsx:encode(Result),
        {Body, Req, State};
    {error, _} ->
        Req1 = cowboy_req:reply(500, Req),
        {false, Req1, State}
    end.

add_one_recotd(Req, State) ->
    {ok, Data, Req1} = cowboy_req:read_body(Req),
    Body = cow_qs:urldecode(Data),
    Title = cow_qs:urldecode(cowboy_req:binding(title, Req1)),
    case epgsql:connect("localhost", "danilo", "danilo", #{
         database => "danilo",
         timeout => 1000
      }) of
    {ok, C} ->
        {ok, Count} = epgsql:equery(C, 
            "insert into Notes (Title, Body, Created) values ($1, $2, now())", [Title, Body]),
        ok = epgsql:close(C),
        if
            Count > 0 ->
                Response = true,
                {Response, Req, State};
            true ->
                Response = false,
               {Response, Req, State}
       end;
    {error, _} ->
         Req2 = cowboy_req:reply(500, Req1),
         {false, Req2, State}
    end.
    
update_one_recotd(Req, State) ->
    {ok, Data, Req1} = cowboy_req:read_body(Req),	
    Body = cow_qs:urldecode(Data),
    Id = cowboy_req:binding(id, Req1),
    Title = cow_qs:urldecode(cowboy_req:binding(title, Req1)),
    case epgsql:connect("localhost", "danilo", "danilo", #{
         database => "danilo",
         timeout => 1000
      }) of
    {ok, C} ->
        {ok, Count} = epgsql:equery(C, 
            "update Notes Set Title = $1, Body = $2 where Id = $3", [Title, Body, Id]),
        ok = epgsql:close(C),
        if
           Count > 0 ->
               Response = true,
               {Response, Req, State};
           true ->
               Response = false,
               {Response, Req, State}
       end;
    {error, _} ->
         Req2 = cowboy_req:reply(500, Req1),
         {false, Req2, State}
    end.

delete_resource(Req, State) ->
    Id = cowboy_req:binding(id, Req),
    case epgsql:connect("localhost", "danilo", "danilo", #{
        database => "danilo",
        timeout => 1000
      }) of
    {ok, C} ->
        {ok, Count} = epgsql:equery(C, "delete from Notes where Id = $1", [Id]),
        ok = epgsql:close(C),
        if
            Count > 0 ->
                Response = true,
                {Response, Req, State};
            true ->
                Response = false,
                {Response, Req, State}
        end;
    {error, _} ->
        Req1 = cowboy_req:reply(500, Req),
        {false, Req1, State}
    end.

