-module(simpleSystem).
-export([start/0, stop/0]).


-spec(start() -> tuple()).
start() ->
	%start survivor
	survivor:start(),
	
	%% PIPES
	% create pipe type
	{ok, PipeTyp_Pid} = pipeTyp:create(),
	% create 3 pipes
	{ok, PipeInst1_Pid} = pipeInst:create(self(), PipeTyp_Pid), %create(Host, ResTyp_Pid)
	{ok, PipeInst2_Pid} = pipeInst:create(self(), PipeTyp_Pid),
	{ok, PipeInst3_Pid} = pipeInst:create(self(), PipeTyp_Pid),
	%{ok, PipeInst1_Pid} = resource_instance:create(pipeInst, [self(), PipeTyp_Pid]),
	%{ok, PipeInst2_Pid} = resource_instance:create(pipeInst, [self(), PipeTyp_Pid]),
	%{ok, PipeInst3_Pid} = resource_instance:create(pipeInst, [self(), PipeTyp_Pid]),	
	Pipes = [PipeInst1_Pid, PipeInst2_Pid, PipeInst3_Pid],
	% get connectors
	{ok, [Pc11, Pc12]} = msg:get(PipeInst1_Pid, get_connectors),
	{ok, [Pc21, Pc22]} = msg:get(PipeInst2_Pid, get_connectors),
	{ok, [Pc31, Pc32]} = msg:get(PipeInst3_Pid, get_connectors),
	%{ok, [Pc11, Pc12]} = resource_instance:list_connectors(PipeInst1_Pid),
	%{ok, [Pc21, Pc22]} = resource_instance:list_connectors(PipeInst2_Pid),
	%{ok, [Pc31, Pc32]} = resource_instance:list_connectors(PipeInst3_Pid),
	Connectors = [[Pc11, Pc12], [Pc21, Pc22], [Pc31, Pc32]],
	% get locations
	{ok, [Pl1]} = msg:get(PipeInst1_Pid, get_locations),
	{ok, [Pl2]} = msg:get(PipeInst2_Pid, get_locations),
	{ok, [Pl3]} = msg:get(PipeInst3_Pid, get_locations),
	%{ok, [Pl1]} = resource_instance:list_locations(PipeInst1_Pid),
	%{ok, [Pl2]} = resource_instance:list_locations(PipeInst2_Pid),
	%{ok, [Pl3]} = resource_instance:list_locations(PipeInst3_Pid),
	Locations = [Pl1, Pl2, Pl3],
	% connect pipes
	connector:connect(Pc12,Pc21),
	connector:connect(Pc22,Pc31),
	connector:connect(Pc32,Pc11),
	
	%% FLUIDUM
	FluidumTyp = fluidumTyp:create(), 
	{ok, FluidumInst} = fluidumInst:create(Pc11, FluidumTyp), %(Root_ConnectorPid, ResTyp_Pid)

	%% PUMP
	RealWorldCmdFnPump = fun(on) -> 
					{ok,on};
				(off) ->
					{ok,off}
				end,	
	
	
	{ok, PumpTyp} = pumpTyp:create(),
	{ok, PumpInst} = pumpInst:create(self(), PumpTyp, PipeInst1_Pid, RealWorldCmdFnPump), %(Host, PumpTyp_Pid, PipeInst_Pid, RealWorldCmdFn)

	%% FLOWMETER
	RealWorldCmdFnFlowMeter = fun() ->	
					{ok, real_flow} 
				  end,

	{ok, FlowMeterTyp} = flowMeterTyp:create(),
	{ok, FlowMeterInst} = flowMeterInst:create(self(), FlowMeterTyp, PipeInst2_Pid, RealWorldCmdFnFlowMeter), %(Host, FlowMeterTyp_Pid, ResInst_Pid, RealWorldCmdFn)
	
	%% HEAT
	Difference = 1,
	HE_link_spec = #{delta => Difference},

	{ok, HeatExTyp} = heatExchangerTyp:create(),
	{ok, HeatExInst} = heatExchangerInst:create(self(), HeatExTyp, PipeInst3_Pid, HE_link_spec), %(Host, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec)
	
	% return
	{ok, {PipeTyp_Pid, Pipes, Connectors, Locations, FluidumTyp, FluidumInst, PumpTyp, PumpInst, FlowMeterTyp, FlowMeterInst, HeatExTyp, HeatExInst}}.

-spec stop() -> {'ok','stopped'}.
stop() ->
	survivor ! stop,
	{ok, stopped}.

