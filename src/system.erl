-module(system).
-export([startSur/0, start/1, stop/0, makePipes/3]).

-spec startSur() -> {'ok','started'}.
-spec start(_) -> {'ok',{_,[any(),...],maybe_improper_list(),_,_,_,_,_,_,_,_,_}}.
-spec stop() -> {'ok','stopped'}.
-spec makePipes(_,_,_) -> [any(),...] | {'error',[32 | 49 | 60 | 78,...]}.
-spec makeConnections([any(),...]) -> any().
-spec makeConnections(_,_,[any()]) -> any().
-spec getConnectors([any(),...]) -> any().
-spec getConnectors([any()],_) -> any().
-spec getLocations([any(),...]) -> any().
-spec getLocations([any()],_) -> any().

startSur() ->
	survivor:start(),
	{ok, started}.

start(N) when N < 3 ->
	{error, "Not enough pipes"};

start(N) ->
	%survivor:start(),
	
	%% PIPE
	{ok, PipeTyp_Pid} = pipeTyp:create(),
	PipeList = makePipes(N, PipeTyp_Pid, []),	
	makeConnections(PipeList),
	ConnectorList = getConnectors(PipeList),
	LocationList = getLocations(PipeList),

	%% FLUIDUM
	[Pc11|_ConnectorList1] = ConnectorList,
	FluidumTyp = fluidumTyp:create(), 
	{ok, FluidumInst} = fluidumInst:create(Pc11, FluidumTyp), %(Root_ConnectorPid, ResTyp_Pid)
	fillPipes(FluidumInst, LocationList),

	%% PUMP
	RealWorldCmdFnPump = fun(on) -> 
					{ok,on};
				(off) ->
					{ok,off}
				end,	
	
	
	[P1|PipeList1] = PipeList,
	{ok, PumpTyp} = pumpTyp:create(),
	{ok, PumpInst} = pumpInst:create(self(), PumpTyp, P1, RealWorldCmdFnPump), %(Host, PumpTyp_Pid, PipeInst_Pid, RealWorldCmdFn)

	%% FLOWMETER
	RealWorldCmdFnFlowMeter = fun() ->	
					{ok, real_flow} 
				  end,

	[P2|PipeList2] = PipeList1,
	{ok, FlowMeterTyp} = flowMeterTyp:create(),
	{ok, FlowMeterInst} = flowMeterInst:create(self(), FlowMeterTyp, P2, RealWorldCmdFnFlowMeter), %(Host, FlowMeterTyp_Pid, ResInst_Pid, RealWorldCmdFn)
	
	%% HEAT
	Difference = 1,
	HE_link_spec = #{delta => Difference},

	[P3|_PipeList3] = PipeList2,
	{ok, HeatExTyp} = heatExchangerTyp:create(),
	{ok, HeatExInst} = heatExchangerInst:create(self(), HeatExTyp, P3, HE_link_spec), %(Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec)
	
	% return
	{ok, {PipeTyp_Pid, PipeList, ConnectorList, LocationList, FluidumTyp, FluidumInst, PumpTyp, PumpInst, FlowMeterTyp, FlowMeterInst, HeatExTyp, HeatExInst}}.


% ----------------------------------------------------------------------- STOP -----------------------------------------------------------------------

stop() ->
	survivor ! stop,
	{ok, stopped}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ----------------------------------------------------------------------- MAKEPIPES -----------------------------------------------------------------------

makePipes(N, PipeTyp_Pid, List) when N > 1 ->
	{ok, PipeInst_Pid} = pipeInst:create(self(), PipeTyp_Pid),
	NewList = List ++[PipeInst_Pid],
	M = N-1,
	makePipes(M,PipeTyp_Pid,NewList);

makePipes(N, PipeTyp_Pid, List) when N == 1 ->
	{ok, PipeInst_Pid} = pipeInst:create(self(), PipeTyp_Pid),
	NewList = List ++[PipeInst_Pid],
	NewList;

makePipes(N, _PipeTyp_Pid, _List) when N < 1 ->
	{error, "N < 1"}.

% ----------------------------------------------------------------------- MAKECONNECTIONS -----------------------------------------------------------------------

makeConnections([Pipe1|PipeRest]) ->
	makeConnections(Pipe1, Pipe1, PipeRest).

% gaat niet want kan niet laatste pijp aan eerste connecteren
%makeConnections(Pipe1, [Pipe2|PipeRest]) ->
%	{ok, [Pc11, Pc12]} = resource_instance:list_connectors(Pipe1),
%	{ok, [Pc21, Pc22]} = resource_instance:list_connectors(Pipe2),
%	connector:connect(Pc12,Pc21),
%	makeConnections(Pipe2, PipeRest).

makeConnections(PipeFirst, Pipe1, [Pipe2|PipeRest]) ->
	{ok, [_Pc11, Pc12]} = msg:get(Pipe1, get_connectors),
	{ok, [Pc21, _Pc22]} = msg:get(Pipe2, get_connectors),
	connector:connect(Pc12,Pc21),
	makeConnections(PipeFirst, Pipe2, PipeRest);

makeConnections(Pipe1, Pipe2, []) ->
	{ok, [Pc11, _Pc12]} = msg:get(Pipe1, get_connectors),
	{ok, [_Pc21, Pc22]} = msg:get(Pipe2, get_connectors),
	connector:connect(Pc22,Pc11).

% ----------------------------------------------------------------------- GETCONNECTORS -----------------------------------------------------------------------

getConnectors(Pipes) ->
	getConnectors(Pipes, []).

getConnectors([Pipe1|PipeRest], Connectors) ->
	{ok, Pcs} = msg:get(Pipe1, get_connectors),
	NewConnectors = lists:append(Connectors, Pcs),
	getConnectors(PipeRest, NewConnectors);

getConnectors([], Connectors) ->
	Connectors.


% ----------------------------------------------------------------------- GETLOCATIONS -----------------------------------------------------------------------

getLocations(Pipes) ->
	getLocations(Pipes, []).

getLocations([Pipe1|PipeRest], Locations) ->
	{ok, Pl} = msg:get(Pipe1, get_locations),
	getLocations(PipeRest, Locations++Pl);

getLocations([], Locations) ->
	Locations.
% ----------------------------------------------------------------------- FILLPIPES -----------------------------------------------------------------------

fillPipes(FluidumInst, [L|LocationRest]) ->
	location:arrival(L, FluidumInst),
	fillPipes(FluidumInst, LocationRest);

fillPipes(_FluidumInst, []) ->
	ok.

