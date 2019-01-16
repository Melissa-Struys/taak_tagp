-module(testEunit_digitalTwin).
-include_lib("eunit/include/eunit.hrl").

digitalTwin_test_() ->
	{foreach,
	 	fun startTest/0,
	 	fun stopTest/1,
	 	[fun testDigitalTwin/1,
		 fun testPipes/1,
		 fun testFlowMeter/1,
		 fun testPumps/1,
		 fun testHeatEx/1
		]
	}.

startTest() ->
	N = 10,
	P = 2,
	H = 4,
	{ok, {PipeTyp_Pid, Pipes, Connectors, Locations, FluidumTyp, FluidumInst, PumpTyp, Pumps, FlowMeterTyp, FlowMeterInst, HeatExTyp, HeatExs}} = digitalTwin:start(N,P,H),
	{PipeTyp_Pid, Pipes, Connectors, Locations, FluidumTyp, FluidumInst, PumpTyp, Pumps, FlowMeterTyp, FlowMeterInst, HeatExTyp, HeatExs, N, P, H}.

stopTest(_) ->
	digitalTwin:stop().

%% ------------------------------------------------------------------------- TEST DIGITAL TWIN -------------------------------------------------------------------------

testDigitalTwin({PipeTyp_Pid, Pipes, Connectors, Locations, FluidumTyp, FluidumInst, PumpTyp, Pumps, FlowMeterTyp, FlowMeterInst, HeatExTyp, HeatExs, N, P, H}) -> 
	[?_assert(erlang:is_process_alive(PipeTyp_Pid)),
	 ?_assert(erlang:is_process_alive(FluidumTyp)),
	 ?_assert(erlang:is_process_alive(FluidumInst)),
	 ?_assert(erlang:is_process_alive(PumpTyp)),
	 ?_assert(erlang:is_process_alive(FlowMeterTyp)),
	 ?_assert(erlang:is_process_alive(FlowMeterInst)),
	 ?_assert(erlang:is_process_alive(HeatExTyp)),
	 ?_assertEqual(N, length(Pipes)),
	 ?_assertEqual(N*2, length(Connectors)),
	 ?_assertEqual(N, length(Locations)),
	 ?_assertEqual(P, length(Pumps)),
	 ?_assertEqual(H, length(HeatExs))
	].

%% ------------------------------------------------------------------------- TEST PIPE -------------------------------------------------------------------------

testPipes({_PipeTyp_Pid, Pipes, Connectors, Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, _HeatExs, N, _P, _H}) ->
	testPipes(N, Pipes, Connectors, Locations, []).

testPipes(N, Pipes, Connectors, Locations, Tests) when N > 1->
	[P|RestPipes] = Pipes,
	[C1,C2|RestConnectors] = Connectors,
	[L|RestLocations] = Locations,

	Test = [?_assert(erlang:is_process_alive(P)),
	 	?_assert(erlang:is_process_alive(C1)),
	 	?_assert(erlang:is_process_alive(C2)),
	 	?_assert(erlang:is_process_alive(L))
	       ],
	TestList = Tests ++[Test],
	M = N-1,
	testPipes(M, RestPipes, RestConnectors, RestLocations, TestList);

testPipes(N, Pipes, Connectors, Locations, Tests) when N == 1->
	[P|_RestPipes] = Pipes,
	[C1,C2|_RestConnectors] = Connectors,
	[L|_RestLocations] = Locations,

	Test = [?_assert(erlang:is_process_alive(P)),
	 	?_assert(erlang:is_process_alive(C1)),
	 	?_assert(erlang:is_process_alive(C2)),
	 	?_assert(erlang:is_process_alive(L))
	       ],
	TestList = Tests ++[Test],
	TestList.

%% ------------------------------------------------------------------------- TEST FLOWMETER -------------------------------------------------------------------------

testFlowMeter({_PipeTyp_Pid, _Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _Pumps, FlowMeterTyp, FlowMeterInst, _HeatExTyp, _HeatExs, _N, _P, _H}) ->
	
	AliveTests = [	?_assert(erlang:is_process_alive(FlowMeterTyp)),
	 		?_assert(erlang:is_process_alive(FlowMeterInst))
		     ],

	{ok, Answer} = flowMeterInst:measure_flow(FlowMeterInst),
	%%%%%%%%%%%% estimate_flow
	EqualTests = [	?_assertEqual(Answer, {ok, real_flow})
		     ],

	[AliveTests, EqualTests].

%% ------------------------------------------------------------------------- TEST PUMP -------------------------------------------------------------------------

testPumps({_PipeTyp_Pid, _Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, _HeatExs, _N, P, _H}) ->
	testPumps(P, Pumps, []).

testPumps(P, Pumps, Tests) when P > 1->
	[PumpInst|RestPumps] = Pumps,
	AliveTests = ?_assert(erlang:is_process_alive(PumpInst)),
	
	{ok, InitState} = pumpInst:is_on(PumpInst),
	TestInit = ?_assertEqual(InitState, off),
	
	pumpInst:switch_off(PumpInst),
	{ok, OffAgain} = pumpInst:is_on(PumpInst),
	TestOffAgain = ?_assertEqual(OffAgain, off),

	pumpInst:switch_on(PumpInst),
	{ok, SetOn} = pumpInst:is_on(PumpInst),
	TestOn = ?_assertEqual(SetOn, on),

	pumpInst:switch_on(PumpInst),
	{ok, OnAgain} = pumpInst:is_on(PumpInst),
	TestOnAgain = ?_assertEqual(OnAgain, on),
	
	pumpInst:switch_off(PumpInst),
	{ok, SetOff} = pumpInst:is_on(PumpInst),
	TestOff = ?_assertEqual(SetOff, off),

	{ok, FlowOff} = pumpInst:flow_influence(PumpInst),
	TestFlowOff = [	?_assertEqual(FlowOff(0), 0),
			?_assertEqual(FlowOff(5), 0)
		      ],

	pumpInst:switch_on(PumpInst),
	{ok, OnAgain2} = pumpInst:is_on(PumpInst),
	TestOnAgain2 = ?_assertEqual(OnAgain2, on),
	{ok, FlowOn} = pumpInst:flow_influence(PumpInst),
	TestFlowOn = [	?_assertEqual(FlowOn(0), 250),
			?_assertEqual(FlowOn(5), 175)
		     ],

	NewTests = [AliveTests, TestInit, TestOffAgain, TestOn, TestOnAgain, TestOff, TestFlowOff, TestOnAgain2, TestFlowOn], 
	
	TestList = Tests ++[NewTests],
	M = P-1,
	testPumps(M, RestPumps, TestList);

testPumps(P, Pumps, Tests) when P == 1->
	[PumpInst|RestPumps] = Pumps,
	AliveTests = ?_assert(erlang:is_process_alive(PumpInst)),
	
	{ok, InitState} = pumpInst:is_on(PumpInst),
	TestInit = ?_assertEqual(InitState, off),
	
	pumpInst:switch_off(PumpInst),
	{ok, OffAgain} = pumpInst:is_on(PumpInst),
	TestOffAgain = ?_assertEqual(OffAgain, off),

	pumpInst:switch_on(PumpInst),
	{ok, SetOn} = pumpInst:is_on(PumpInst),
	TestOn = ?_assertEqual(SetOn, on),

	pumpInst:switch_on(PumpInst),
	{ok, OnAgain} = pumpInst:is_on(PumpInst),
	TestOnAgain = ?_assertEqual(OnAgain, on),
	
	pumpInst:switch_off(PumpInst),
	{ok, SetOff} = pumpInst:is_on(PumpInst),
	TestOff = ?_assertEqual(SetOff, off),

	{ok, FlowOff} = pumpInst:flow_influence(PumpInst),
	TestFlowOff = [	?_assertEqual(FlowOff(0), 0),
			?_assertEqual(FlowOff(5), 0)
		      ],

	pumpInst:switch_on(PumpInst),
	{ok, OnAgain2} = pumpInst:is_on(PumpInst),
	TestOnAgain2 = ?_assertEqual(OnAgain2, on),
	{ok, FlowOn} = pumpInst:flow_influence(PumpInst),
	TestFlowOn = [	?_assertEqual(FlowOn(0), 250),
			?_assertEqual(FlowOn(5), 175)
		     ],

	NewTests = [AliveTests, TestInit, TestOffAgain, TestOn, TestOnAgain, TestOff, TestFlowOff, TestOnAgain2, TestFlowOn], 
	
	TestList = Tests ++[NewTests],
	TestList.

%% ------------------------------------------------------------------------- TEST HEATEXCHANGER -------------------------------------------------------------------------

testHeatEx({_PipeTyp_Pid, _Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, HeatExs, _N, _P, H}) ->
	testHeatEx(H, HeatExs, []).

testHeatEx(H, HeatExs, Tests) when H > 1->
	[HeatExInst|RestHeatEx] = HeatExs,
	
	Flow = 3,
	Diff = 1,
	Temp = 25,

	{ok, {ok, TempInf}} = heatExchangerInst:temp_influence(HeatExInst),
	{ok, Influence} = TempInf(Flow, Temp),
	Calc = Temp + (Diff/Flow),	

	Test = [ ?_assertEqual(Influence, Calc),
		 ?_assert(erlang:is_process_alive(HeatExInst))
	       ],

	TestList = Tests ++[Test],
	M = H-1,
	testHeatEx(M, RestHeatEx, TestList);

testHeatEx(H, HeatExs, Tests) when H == 1->
	[HeatExInst|_RestHeatEx] = HeatExs,
	
	Flow = 3,
	Diff = 1,
	Temp = 25,

	{ok, {ok, TempInf}} = heatExchangerInst:temp_influence(HeatExInst),
	{ok, Influence} = TempInf(Flow, Temp),
	Calc = Temp + (Diff/Flow),	

	Test = [ ?_assertEqual(Influence, Calc),
		 ?_assert(erlang:is_process_alive(HeatExInst))
	       ],

	TestList = Tests ++[Test],
	TestList.






