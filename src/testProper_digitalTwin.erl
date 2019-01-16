-module(testProper_digitalTwin).
-include_lib("proper/include/proper.hrl").

prop_length() ->
	survivor:start(),
	Result = ?FORALL(N, integer(3,100), test_length(N)),
	timer:send_after(1000, survivor, stop),
	Result.

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
		false
		;
	    false ->
		true
	end.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%prop_pipeLength() ->
%	survivor:start(),
%	Result = ?FORALL(N, integer(3,100), test_pipeLength(N)),
%	timer:send_after(1000, survivor, stop),
%	Result.

%prop_connectorLength() ->
%	survivor:start(),
%	Result = ?FORALL(N, integer(3,100), test_connectorLength(N)),
%	timer:send_after(1000, survivor, stop),
%	Result.

%prop_locationLength() ->
%	survivor:start(),
%	Result = ?FORALL(N, integer(3,100), test_locationLength(N)),
%	timer:send_after(1000, survivor, stop),
%	Result.	

%prop_pumpLength() ->
%	survivor:start(),
%	Result = ?FORALL(N, integer(1,100), test_pumpLength(N)),
%	timer:send_after(1000, survivor, stop),
%	Result.

%prop_heatExLength() ->
%	survivor:start(),
%	Result = ?FORALL(N, integer(1,100), test_heatExLength(N)),
%	timer:send_after(1000, survivor, stop),
%	Result.






test_pipeLength(N) ->
	{ok, {_PipeTyp_Pid, Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, _HeatExs}} = digitalTwin:start(N,1,1),
	Result = (length(Pipes) == N),
	Result.

test_connectorLength(N) ->
	{ok, {_PipeTyp_Pid, _Pipes, Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, _HeatExs}} = digitalTwin:start(N,1,1),
	length(Connectors) == 2*N.
	
test_locationLength(N) ->
	{ok, {_PipeTyp_Pid, _Pipes, _Connectors, Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, _HeatExs}} = digitalTwin:start(N,1,1),
	length(Locations) == N.

test_pumpLength(N) ->
	{ok, {_PipeTyp_Pid, _Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, _HeatExs}} = digitalTwin:start(N*2,N,1),
	length(Pumps) == N.
	
test_heatExLength(N) ->
	{ok, {_PipeTyp_Pid, _Pipes, _Connectors, _Locations, _FluidumTyp, _FluidumInst, _PumpTyp, _Pumps, _FlowMeterTyp, _FlowMeterInst, _HeatExTyp, HeatExs}} = digitalTwin:start(N*2,1,N),
	length(HeatExs) == N.
