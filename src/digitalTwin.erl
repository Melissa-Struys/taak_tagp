-module(digitalTwin).
-export([startSurvivor/0, start/3, stop/0]).

-spec startSurvivor() -> {'ok','started'}.
-spec start(_,_,_) -> {'error',[32 | 78 | 101 | 103 | 104 | 105 | 110 | 111 | 112 | 115 | 116 | 117,...]} | {'ok',{_,[any(),...],nonempty_maybe_improper_list(),[any()],_,_,_,'error' | [any(),...],_,_,_,'error' | [any(),...]}}.
-spec stop() -> {'ok','stopped'}.
-spec makePipes(_,_,[any()]) -> [any(),...] | {'error',[32 | 49 | 60 | 78,...]}.
-spec makeConnections([any(),...]) -> any().
-spec makeConnections(_,_,[any()]) -> any().
-spec getConnectors([any(),...]) -> any().
-spec getConnectors([any()],_) -> any().
-spec getLocations([any(),...]) -> any().
-spec getLocations([any()],_) -> any().
-spec fillPipes(_,[any()]) -> 'ok'.
-spec makePumps(_,_,[any()],[any()]) -> {'error' | [any(),...],[any()]}.
-spec makeHeatEx(_,_,[any()],[any()],#{'delta':=1}) -> {'error' | [any(),...],[any()]}.


startSurvivor() ->
	survior:start(),
	{ok, started}.

start(N,P,H) when N < P+H+1 ->
	{error, "Not enough pipes"};

start(N,P,H) ->
	survivor:start(),
	
	%% PIPE
	{ok, PipeTyp_Pid} = pipeTyp:create(),
	PipeList = makePipes(N, PipeTyp_Pid, []),	
	makeConnections(PipeList),
	ConnectorList = getConnectors(PipeList),
	LocationList = getLocations(PipeList),

	%% FLUIDUM
	[Pc11|_ConnectorList1] = ConnectorList,
	FluidumTyp = fluidumTyp:create(), 
	{ok, FluidumInst} = fluidumInst:create(Pc11, FluidumTyp),
	fillPipes(FluidumInst, LocationList),

	%% PUMP
	{ok, PumpTyp} = pumpTyp:create(),
	{PumpList, PipeListPump} = makePumps(P, PumpTyp, PipeList, []),


	%% FLOWMETER
	RealWorldCmdFnFlowMeter = fun() ->	
					{ok, real_flow} 
				  end,

	[Pfm|PipeListFlowMeter] = PipeListPump,
	{ok, FlowMeterTyp} = flowMeterTyp:create(),
	{ok, FlowMeterInst} = flowMeterInst:create(self(), FlowMeterTyp, Pfm, RealWorldCmdFnFlowMeter),
	
	%% HEAT
	Difference = 1,
	HE_link_spec = #{delta => Difference},
	{ok, HeatExTyp} = heatExchangerTyp:create(),
	{HeatExList, _PipeListHeatEx} = makeHeatEx(H, HeatExTyp, PipeListFlowMeter, [], HE_link_spec),
	
	% return
	{ok, {PipeTyp_Pid, PipeList, ConnectorList, LocationList, FluidumTyp, FluidumInst, PumpTyp, PumpList, FlowMeterTyp, FlowMeterInst, HeatExTyp, HeatExList}}.


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

% ----------------------------------------------------------------------- MAKEPUMPS -----------------------------------------------------------------------

makePumps(P, PumpTyp, Pipes, PumpList) when P > 1 ->
	RealWorldCmdFnPump = fun(on) -> 
					{ok,on};
				(off) ->
					{ok,off}
				end,	
	[Pipe|PipeRest] = Pipes,
	{ok, PumpInst} = pumpInst:create(self(), PumpTyp, Pipe, RealWorldCmdFnPump),
	PumpListNew = PumpList ++[PumpInst], 
	Q = P-1,
	makePumps(Q, PumpTyp, PipeRest, PumpListNew);

makePumps(P, PumpTyp, Pipes, PumpList) when P == 1 ->
	RealWorldCmdFnPump = fun(on) -> 
					{ok,on};
				(off) ->
					{ok,off}
				end,	
	[Pipe|PipeRest] = Pipes,
	{ok, PumpInst} = pumpInst:create(self(), PumpTyp, Pipe, RealWorldCmdFnPump),
	PumpListNew = PumpList ++[PumpInst], 
	{PumpListNew, PipeRest};


makePumps(P, _PumpTyp, _Pipes, _PumpList) when P < 1 ->
	{error, "P<1"}.

% ----------------------------------------------------------------------- MAKEHEATEX -----------------------------------------------------------------------

makeHeatEx(H, HeatExTyp, Pipes, HeatExList, HE_link_spec) when H > 1 ->
	[Pipe|PipeRest] = Pipes,
	{ok, HeatExInst} = heatExchangerInst:create(self(), HeatExTyp, Pipe, HE_link_spec),
	HeatExListNew = HeatExList ++[HeatExInst], 
	Q = H-1,
	makeHeatEx(Q, HeatExTyp, PipeRest, HeatExListNew, HE_link_spec);

makeHeatEx(H, HeatExTyp, Pipes, HeatExList, HE_link_spec) when H == 1 ->
	[Pipe|PipeRest] = Pipes,
	{ok, HeatExInst} = heatExchangerInst:create(self(), HeatExTyp, Pipe, HE_link_spec),
	HeatExListNew = HeatExList ++[HeatExInst], 
	{HeatExListNew, PipeRest};


makeHeatEx(H, _HeatExTyp, _Pipes, _HeatExList, _HE_link_spec) when H < 1 ->
	{error, "H<1"}.











	

