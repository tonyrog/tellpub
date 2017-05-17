%%%-------------------------------------------------------------------
%%% @author  <tony@rogvall.se>
%%% @copyright (C) 2017, 
%%% @doc
%%%    Publish tellstick/(net/duo) events on erlbus
%%% @end
%%% Created :  2 Apr 2017 by  <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(tellpub_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
	{
	  subscription,
	  action_list,
	  inhibit = []   %% [{Topic,Message}]
	}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok,Ref} = tellstick_server:subscribe(),
    {ok,ActionList} = application:get_env(tellpub, action_list),
    {ok, #state{ subscription=Ref,
		 action_list = ActionList }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tellstick_event,Ref,Data},  State) 
  when Ref =:= State#state.subscription ->
    case match(State#state.action_list, Data) of
	false ->
	    lager:info("data=~p", [Data]),
	    {noreply, State};
	{_Pattern,Inhibit,Topic,Message} ->
	    IKey = {Topic,Message},
	    case lists:member(IKey,State#state.inhibit) of
		false ->
		    xbus:pub(Topic,Message),
		    erlang:start_timer(Inhibit,self(),{inhibit,IKey}),
		    IList = [IKey|State#state.inhibit],
		    %% lager:info("topic=~p, message=~p, data=~p", 
		    %% [Topic,Message,Data]),
		    {noreply, State#state { inhibit = IList }};
		true ->
		    {noreply, State}
	    end
    end;
handle_info({timeout,_Ref,{inhibit,IKey}}, State) ->
    IList = lists:delete(IKey, State#state.inhibit),
    {noreply, State#state { inhibit = IList }};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

match([P={Pattern,_Inhibit,_Topic,_Value}|ActionList],Data) ->
    case match_pattern(Pattern, Data) of
	true -> P;
	false -> match(ActionList, Data)
    end;
match([], _Data) ->
    false.

match_pattern([{Key,Value}|Pattern], Data) ->
    case lists:keyfind(Key, 1, Data) of
	false -> false;
	{Key,Value} -> match_pattern(Pattern,Data);
	_ -> false
    end;
match_pattern([], _Data) ->
    true.




	    
