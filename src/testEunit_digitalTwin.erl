-module(testEunit_digitalTwin).
-include_lib("eunit/include/eunit.hrl").

digitalTwin_test_() ->
	{foreach,
	 	fun startTest/0,
	 	fun stopTest/1,
	 	[fun testDigitalTwin/1,
		 fun testPipes/1,
		 fun testConnectors/1,
		 fun testFlowMeter/1,
		 fun testPumps/1,
		 fun testHeatEx/1
		]
	}.

startTest() ->
	N = 15,
	P = 8,
	H = 5,
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

testPipes(N, Pipes, Connectors, Locations, Tests) ->
	[P|RestPipes] = Pipes,
	[C1,C2|RestConnectors] = Connectors,
	[L|RestLocations] = Locations,

	Flow = 3,
	{ok, FlowInf1} = pipeInst:get_flow_influence(P),
	Calc = -0.01 * Flow,

	{ok, InstC1} = connector:get_ResInst(C1),
	{ok, InstC2} = connector:get_ResInst(C2),
	{ok, TypC1} = connector:get_type(C1),
	{ok, TypC2} = connector:get_type(C2),

	Test = [?_assert(erlang:is_process_alive(P)),
	 	?_assert(erlang:is_process_alive(C1)),
	 	?_assert(erlang:is_process_alive(C2)),
	 	?_assert(erlang:is_process_alive(L)),
		?_assertEqual(FlowInf1(Flow), Calc),
		?_assertEqual(InstC1, P),
		?_assertEqual(InstC2, P),
		?_assertEqual(TypC1, simplePipe),
		?_assertEqual(TypC2, simplePipe)
	       ],
	
	TestList = Tests ++[Test],
	if N>1 ->
		M = N-1,
		testPipes(M, RestPipes, RestConnectors, RestLocations, TestList);
	true -> TestList
	end.
%% ------------------------------------------------------------------------- TEST CONNECTORS -------------------------------------------------------------------------

testConnectors({_PipeTyp_Pid, _Pipes, Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, _HeatExs, N, _P, _H}) ->
	[C11|RestConnectors] = Connectors,
	testConnectors(C11, RestConnectors, [], N).

testConnectors(C11, [C12,C21|RestConnectors], Tests, N) ->
	{ok, ConnectedC12} = connector:get_connected(C12),

	connector:disconnect(C12),
	{ok, DisconnectedC12} = connector:get_connected(C12),

	connector:connect(C12, C21),
	{ok, ReconnectedC12} = connector:get_connected(C12),

	Test =  [?_assertEqual(ConnectedC12, C21),
		 ?_assertEqual(DisconnectedC12, disconnected),
		 ?_assertEqual(ReconnectedC12, C21)
		],

	TestList = Tests ++[Test],
	M = N-1,
	if M > 1 ->
		testConnectors(C11, RestConnectors, TestList, M);
	true ->
		testConnectors(C11, RestConnectors, TestList)
	end.

testConnectors(C11, [C32|_RestConnectors], Tests) ->
	{ok, ConnectedC32} = connector:get_connected(C32),

	connector:disconnect(C32),
	{ok, DisconnectedC32} = connector:get_connected(C32),

	connector:connect(C32, C11),
	{ok, ReconnectedC32} = connector:get_connected(C32),

	Test =  [?_assertEqual(ConnectedC32, C11),
		 ?_assertEqual(DisconnectedC32, disconnected),
		 ?_assertEqual(ReconnectedC32, C11)
		],

	TestList = Tests ++[Test],
	TestList.


%% ------------------------------------------------------------------------- TEST FLOWMETER -------------------------------------------------------------------------

testFlowMeter({_PipeTyp_Pid, _Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _Pumps, FlowMeterTyp, FlowMeterInst, _HeatExTyp, _HeatExs, _N, _P, _H}) ->
	
	AliveTests = [	?_assert(erlang:is_process_alive(FlowMeterTyp)),
	 		?_assert(erlang:is_process_alive(FlowMeterInst))
		     ],

	{ok, Answer} = flowMeterInst:measure_flow(FlowMeterInst),
	EqualTests = [	?_assertEqual(Answer, {ok, real_flow})
		     ],

	[AliveTests, EqualTests].

%% ------------------------------------------------------------------------- TEST PUMP -------------------------------------------------------------------------

testPumps({_PipeTyp_Pid, _Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, _HeatExs, _N, P, _H}) ->
	testPumps(P, Pumps, []).

testPumps(P, Pumps, Tests) ->
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
	if P>1 ->
		M = P-1,
		testPumps(M, RestPumps, TestList);
	true -> TestList
	end.


%% ------------------------------------------------------------------------- TEST HEATEXCHANGER -------------------------------------------------------------------------

testHeatEx({_PipeTyp_Pid, _Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, HeatExs, _N, _P, H}) ->
	testHeatEx(H, HeatExs, []).

testHeatEx(H, HeatExs, Tests) ->
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
		
	if H>1 ->
		M = H-1,
		testHeatEx(M, RestHeatEx, TestList);
	true -> TestList
	end.
