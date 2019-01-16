%%%-------------------------------------------------------------------
%% @doc taak top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(taak_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    observer:start(),
    %test_eunit:test(),
    %proper:module(test_proper),

    %simpleSystem:start(),
    %test_simpleSystem:test(),

    %system:start(5),
    %test_system:test(),
    %proper:module(test_system),

    %digitalTwin:start(7,3,2),
    %testEunit_digitalTwin:test(),
    proper:module(testProper_digitalTwin),
    {ok, {{one_for_all, 0, 1}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
