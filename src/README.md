## Dialyzer
_https://jmilet.github.io/dialyzer/elixir/erlang/2016/03/13/dialyzer-basics.html_
_http://gertm.blogspot.com/2012/06/getting-started-with-dialyzer-for.html_

$ dialyzer msg.erl
 done in 0m0.49s
done (passed successfully)

$ dialyzer simpleSystem.erl
Unknown functions:
  connector:connect/2
  flowMeterInst:create/4
  flowMeterTyp:create/0
  fluidumInst:create/2
  fluidumTyp:create/0
  heatExchangerInst:create/4
  heatExchangerTyp:create/0
  msg:get/2
  pipeInst:create/2
  pipeTyp:create/0
  pumpInst:create/4
  pumpTyp:create/0
  survivor:start/0
 done in 0m0.42s
done (passed successfully)



# specs
simleSystem.erl : -spec(start() -> number()).
	simpleSystem.erl:4: Invalid type specification for function simpleSystem:start/0. The success typing is () -> {'ok',{_,[any(),...],[any(),...],[any(),...],_,_,_,_,_,_,_,_}}

simleSystem.erl : -spec(start() -> tuple()).
	done (passed successfully)

## TypEr
$ typer simpleSystem.erl
%% File: "simpleSystem.erl"
%% ------------------------
-spec start() -> tuple().
-spec stop() -> {'ok','stopped'}.

$ typer --annotate simpleSystem.erl 
-> nieuwe map : typer_ann met file : simpleSystem.ann.erl


