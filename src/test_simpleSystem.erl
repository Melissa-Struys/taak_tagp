-module(test_simpleSystem).
-include_lib("eunit/include/eunit.hrl").



simpleSystem_test_() ->
	{foreach,
	 	fun startTest/0,
	 	fun stopTest/1,
	 	[fun testSimpleSystem/1,
		 fun testPipe/1,
		 fun testFlowMeter/1,
		 fun testPump/1,
		 fun testHeatExchanger/1
		]
	}.

%% TEST SIMPLE SYSTEM -------------------------------------------------------------------------

startTest() ->
	{ok, {PipeTyp_Pid, Pipes, Connectors, Locations, FluidumTyp, FluidumInst, PumpTyp, PumpInst, FlowMeterTyp, FlowMeterInst, HeatExTyp, HeatExInst}} = simpleSystem:start(),
	{PipeTyp_Pid, Pipes, Connectors, Locations, FluidumTyp, FluidumInst, PumpTyp, PumpInst, FlowMeterTyp, FlowMeterInst, HeatExTyp, HeatExInst}.

% stopTest/0 geeft *** bad test descriptor *** => niet gebruikte parameter meegeven
stopTest(_) ->
	simpleSystem:stop().

testSimpleSystem({PipeTyp_Pid, Pipes, Connectors, Locations, FluidumTyp, FluidumInst, PumpTyp, PumpInst, FlowMeterTyp, FlowMeterInst, HeatExTyp, HeatExInst}) -> 
	[P1,P2,P3] = Pipes,
	[[C11,C12],[C21,C22],[C31,C32]] = Connectors,
	[L1,L2,L3] = Locations,
	[?_assert(erlang:is_process_alive(PipeTyp_Pid)),
	 ?_assert(erlang:is_process_alive(P1)),
	 ?_assert(erlang:is_process_alive(P2)),
	 ?_assert(erlang:is_process_alive(P3)),
	 ?_assert(erlang:is_process_alive(C11)),
	 ?_assert(erlang:is_process_alive(C12)),
	 ?_assert(erlang:is_process_alive(C21)),
	 ?_assert(erlang:is_process_alive(C22)),
	 ?_assert(erlang:is_process_alive(C31)),
	 ?_assert(erlang:is_process_alive(C32)),
	 ?_assert(erlang:is_process_alive(L1)),
	 ?_assert(erlang:is_process_alive(L2)),
	 ?_assert(erlang:is_process_alive(L3)),
	 ?_assert(erlang:is_process_alive(FluidumTyp)),
	 ?_assert(erlang:is_process_alive(FluidumInst)),
	 ?_assert(erlang:is_process_alive(PumpTyp)),
	 ?_assert(erlang:is_process_alive(PumpInst)),
	 ?_assert(erlang:is_process_alive(FlowMeterTyp)),
	 ?_assert(erlang:is_process_alive(FlowMeterInst)),
	 ?_assert(erlang:is_process_alive(HeatExTyp)),
	 ?_assert(erlang:is_process_alive(HeatExInst))
	].

%% TEST PIPE -------------------------------------------------------------------------

testPipe({PipeTyp_Pid, Pipes, Connectors, Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _PumpInst, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, _HeatExInst}) ->
	[P1,P2,P3] = Pipes,
	[[C11,C12],[C21,C22],[C31,C32]] = Connectors,
	[L1,L2,L3] = Locations,

	AliveTests = [	?_assert(erlang:is_process_alive(PipeTyp_Pid)), 
			?_assert(erlang:is_process_alive(P1)),
			?_assert(erlang:is_process_alive(P2)),
			?_assert(erlang:is_process_alive(P3)),
			?_assert(erlang:is_process_alive(C11)),
			?_assert(erlang:is_process_alive(C12)),
			?_assert(erlang:is_process_alive(C21)),
			?_assert(erlang:is_process_alive(C22)),
			?_assert(erlang:is_process_alive(C31)),
			?_assert(erlang:is_process_alive(C32)),
			?_assert(erlang:is_process_alive(L1)),
			?_assert(erlang:is_process_alive(L2)),
			?_assert(erlang:is_process_alive(L3))
		     ],
	
	Flow = 3,
	%fun(Flow) -> flow(Flow) end, % placeholder only. 
	%flow(N) -> - 0.01 * N,
	{ok, FlowInf1} = pipeInst:get_flow_influence(P1),
	{ok, FlowInf2} = pipeInst:get_flow_influence(P2),
	{ok, FlowInf3} = pipeInst:get_flow_influence(P3),
	Calc = -0.01 * Flow,

	{ok, ConnectedC12} = connector:get_connected(C12),
	{ok, ConnectedC22} = connector:get_connected(C22),
	{ok, ConnectedC32} = connector:get_connected(C32),
	{ok, InstC12} = connector:get_ResInst(C12),
	{ok, InstC22} = connector:get_ResInst(C22),
	{ok, InstC32} = connector:get_ResInst(C32),
	{ok, TypC12} = connector:get_type(C12),
	{ok, TypC22} = connector:get_type(C22),
	{ok, TypC32} = connector:get_type(C32),
	
	InitTests = [	?_assertEqual(FlowInf1(Flow), Calc),
			?_assertEqual(FlowInf2(Flow), Calc),
			?_assertEqual(FlowInf3(Flow), Calc),
			?_assertEqual(ConnectedC12, C21),
			?_assertEqual(ConnectedC22, C31),
			?_assertEqual(ConnectedC32, C11),
			?_assertEqual(InstC12, P1),
			?_assertEqual(InstC22, P2),
			?_assertEqual(InstC32, P3),
			?_assertEqual(TypC12, simplePipe),
			?_assertEqual(TypC22, simplePipe),
			?_assertEqual(TypC32, simplePipe)
		     ],

	connector:disconnect(C12),
	connector:disconnect(C22),
	connector:disconnect(C32),

	{ok, DisconnectedC12} = connector:get_connected(C12),
	{ok, DisconnectedC22} = connector:get_connected(C22),
	{ok, DisconnectedC32} = connector:get_connected(C32),
	
	DisconTests = [	?_assertEqual(DisconnectedC12, disconnected),
			?_assertEqual(DisconnectedC22, disconnected),
			?_assertEqual(DisconnectedC32, disconnected)
		     ],

	connector:connect(C12, C21),
	connector:connect(C22, C31),
	connector:connect(C32, C11),

	{ok, ReconnectedC12} = connector:get_connected(C12),
	{ok, ReconnectedC22} = connector:get_connected(C22),
	{ok, ReconnectedC32} = connector:get_connected(C32),
	
	ReconTests = [	?_assertEqual(ReconnectedC12, C21),
			?_assertEqual(ReconnectedC22, C31),
			?_assertEqual(ReconnectedC32, C11)
		     ],

%	DiscardC12 = connector:discard(C12),
%	DiscardC22 = connector:discard(C22),
%	DiscardC32 = connector:discard(C32),
	
%	DiscarTests = [	?_assertEqual(DiscardC12, discard),
%			?_assertEqual(DiscardC22, discard),
%			?_assertEqual(DiscardC32, discard)
%		     ],

	[AliveTests, InitTests, DisconTests, ReconTests]. %, DiscarTests].


%% TEST FLOWMETER -------------------------------------------------------------------------

%flowMeter_test_() ->
%	{setup,
%	 	fun startTest/0,
%	 	fun stopTest/1,
%	 	fun testFlowMeter/1
%	}.

testFlowMeter({_PipeTyp_Pid, _Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _PumpInst, FlowMeterTyp, FlowMeterInst, _HeatExTyp, _HeatExInst}) ->
	
	AliveTests = [	?_assert(erlang:is_process_alive(FlowMeterTyp)),
	 		?_assert(erlang:is_process_alive(FlowMeterInst))
		     ],

	%{ok, InfluenceFn} = flowMeterInst:estimate_flow(FlowMeterInst),
	{ok, Answer} = flowMeterInst:measure_flow(FlowMeterInst),
	
	EqualTests = [	?_assertEqual(Answer, {ok, real_flow})
		     ],

	[AliveTests, EqualTests].

%% TEST PUMP -------------------------------------------------------------------------

%pump_test_() ->
%	{setup,
%	 	fun startTest/0,
%	 	fun stopTest/1,
%	 	fun testPump/1
%	}.

testPump({_PipeTyp_Pid, _Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, PumpTyp, PumpInst, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, _HeatExInst}) ->
	
	AliveTests = [	?_assert(erlang:is_process_alive(PumpTyp)),
	 		?_assert(erlang:is_process_alive(PumpInst))
		     ],

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
	%fun(Flow) -> flow(Flow, OnOrOff) end, % placeholder only. 
	%flow(Flow, on)  -> (250 - 5 * Flow - 2 * Flow * Flow);
	TestFlowOn = [	?_assertEqual(FlowOn(0), 250),
			?_assertEqual(FlowOn(5), 175)
		     ],

	[AliveTests, TestInit, TestOffAgain, TestOn, TestOnAgain, TestOff, TestFlowOff, TestOnAgain2, TestFlowOn]. 



%% TEST HEATEXCHANGER -------------------------------------------------------------------------

testHeatExchanger({_PipeTyp_Pid, _Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _PumpInst, _FlowMeterTyp, _FlowMeterInst, HeatExTyp, HeatExInst}) ->
	
	AliveTests = [	?_assert(erlang:is_process_alive(HeatExTyp)),
	 		?_assert(erlang:is_process_alive(HeatExInst))
		     ],
	
	Flow = 3,
	Diff = 1,
	Temp = 25,

	%fun(Flow, InTemp) -> #{delta := Difference} = HE_link_spec, {ok, InTemp + (Difference/Flow)} end}.
	{ok, {ok, TempInf}} = heatExchangerInst:temp_influence(HeatExInst),
	{ok, Influence} = TempInf(Flow, Temp),
	Calc = Temp + (Diff/Flow),	
	EqualTests = [	?_assertEqual(Influence, Calc)
		     ],

	[AliveTests, EqualTests].


%% TEST FLUIDUM -------------------------------------------------------------------------

%testFluidum({_PipeTyp_Pid, _Pipes, _Connectors, _Locations, FluidumTyp, FluidumInst, _PumpTyp, _PumpInst, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, _HeatExInst}) ->
	
%	AliveTests = [	?_assert(erlang:is_process_alive(FluidumTyp)),
%	 		?_assert(erlang:is_process_alive(FluidumInst))
%		     ],

%	{ok, C} = fluidumInst:get_recource_circuit(FluidumInst),
%	EqualTests = [	?_assertEqual(Influence, Calc)
%		     ],

%	[AliveTests, EqualTests].



















