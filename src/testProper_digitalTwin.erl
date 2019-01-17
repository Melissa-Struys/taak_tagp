-module(testProper_digitalTwin).
-include_lib("proper/include/proper.hrl").

%% ------------------------------------------------------------------------- TEST FLOW -------------------------------------------------------------------------

prop_flow() ->
	survivor:start(),
	Result = ?FORALL(F, integer(1,100), test_flow(F)),
	timer:send_after(1000, survivor, stop),
	Result.

test_flow(F) ->
	{ok, {_PipeTyp_Pid, Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, _HeatExs}} = digitalTwin:start(3,1,1),
	[P|_RestPipes] = Pipes,

	{ok, FlowInf} = pipeInst:get_flow_influence(P),
	CalcPipe = -0.01 * F,
	TestPipe = (FlowInf(F) == CalcPipe),
	

	[PumpInst|_RestPumps] = Pumps,
	pumpInst:switch_on(PumpInst),
	CalcPump = (250 - 5 * F - 2 * F * F),
	{ok, FlowOn} = pumpInst:flow_influence(PumpInst),
	TestPump = (FlowOn(F) == CalcPump),

	Result = [TestPipe, TestPump],
	case lists:member(false, Result) of
	    true ->
		false
		;
	    false ->
		true
	end.

%% ------------------------------------------------------------------------- TEST HEATEXCHANGER -------------------------------------------------------------------------

prop_heatExFlow() ->
	survivor:start(),
	Result = ?FORALL(F, integer(1,100), test_heatExFlow(F)),
	timer:send_after(1000, survivor, stop),
	Result.


test_heatExFlow(F) ->
	{ok, {_PipeTyp_Pid, _Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, HeatExs}} = digitalTwin:start(3,1,1),
	[HeatExInst|_RestHeatEx] = HeatExs,
	
	Diff = 1,
	Temp = 25,

	{ok, {ok, TempInf}} = heatExchangerInst:temp_influence(HeatExInst),
	{ok, Influence} = TempInf(F, Temp),
	Calc = Temp + (Diff/F),	
	Influence == Calc.



prop_heatExTemp() ->
	survivor:start(),
	Result = ?FORALL(T, integer(1,100), test_heatExTemp(T)),
	timer:send_after(1000, survivor, stop),
	Result.


test_heatExTemp(T) ->
	{ok, {_PipeTyp_Pid, _Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, HeatExs}} = digitalTwin:start(3,1,1),
	[HeatExInst|_RestHeatEx] = HeatExs,
	
	Flow = 3,
	Diff = 1,

	{ok, {ok, TempInf}} = heatExchangerInst:temp_influence(HeatExInst),
	{ok, Influence} = TempInf(Flow, T),
	Calc = T + (Diff/Flow),	
	Influence == Calc.


%% ------------------------------------------------------------------------- TEST LENGTH -------------------------------------------------------------------------
	
%prop_length() ->
%	survivor:start(),
%	Result = ?FORALL(N, integer(3,100), test_length(N)),
%	timer:send_after(1000, survivor, stop),
%	Result.

test_length(N) ->
	{ok, {_PipeTyp_Pid, Pipes, Connectors, Locations, _FluidumTyp, _FluidumInst, _PumpTyp, Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, HeatExs}} = digitalTwin:start(N*3,N,N),
	ResultPipes = (length(Pipes) == N*3),
	ResultConns = (length(Connectors) == N*6), %N*2*3
	ResultLocs = (length(Locations) == N*3),
	ResultPumps = (length(Pumps) == N),
	ResultHeatExs = (length(HeatExs) == N),
	Result = [ResultPipes, ResultConns, ResultLocs, ResultPumps, ResultHeatExs],
	case lists:member(false, Result) of
	    true ->
		false;
	    false ->
		true
	end.

