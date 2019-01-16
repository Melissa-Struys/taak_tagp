-module(fluidumTyp).

-export([create/0, init/0, discover_circuit/1]).
-export([get_resource_circuit/2]).

-spec create() -> pid().
-spec init() -> no_return().
-spec get_resource_circuit(_,_) -> any().
-spec loop() -> no_return().
-spec extract(map()) -> #{_=>'processed'}.
-spec extract('none' | {_,_,maps:iterator()},#{_=>'processed'}) -> #{_=>'processed'}.
-spec discover_circuit(_) -> {'ok',{_,#{_=>'processed'}}}.
-spec discover_circuit([any()],#{_=>'processed'}) -> {'ok',#{_=>'processed'}}.
-spec process_connection(_,'error' | {'ok',_},[any()],#{_=>'processed'}) -> {'ok',[any()],#{_=>'processed'}}.


create() -> spawn(?MODULE, init, []).

init() -> 
	survivor:entry(fluidTyp_created), 
	loop().

get_resource_circuit(TypePid, State) ->
	msg:get(TypePid, resource_circuit, State). 

loop() -> 
	receive
		{initial_state, [ResInst_Pid, [Root_ConnectorPid, TypeOptions]], ReplyFn} -> 
			{ok, C} = discover_circuit(Root_ConnectorPid), 
			ReplyFn(#{resInst => ResInst_Pid, circuit => C, typeOptions => TypeOptions}), 
			loop();
		{connections_list, _State , ReplyFn} -> 
			ReplyFn([]), 
			loop();
		{locations_list, _State, ReplyFn} -> 
			ReplyFn([]),
			loop();
		{resource_circuit, State, ReplyFn} -> 
			#{circuit := C} = State, 
			{_RootC, CircuitMap} = C, 
			ReplyFn(extract(CircuitMap)),
			loop()
	end. 

extract(C) -> extract(maps:next(maps:iterator(C)), #{}).

extract({C, _ , Iter }, ResLoop) ->
		{ok, ResPid} = connector:get_ResInst(C),
		extract(maps:next(Iter), ResLoop#{ResPid => processed});

extract( none , ResLoop) -> ResLoop. 

discover_circuit(Root_Pid) -> 
	{ok,  Circuit} = discover_circuit([Root_Pid], #{  }),
	{ok, {Root_Pid, Circuit}}.

discover_circuit([ disconnected | Todo_List], Circuit) -> 
	discover_circuit(Todo_List, Circuit);

discover_circuit([C | Todo_List], Circuit) -> 
	{ok, Updated_Todo_list, Updated_Circuit} = 
		process_connection(C, maps:find(C, Circuit ), Todo_List, Circuit),
	discover_circuit(Updated_Todo_list, Updated_Circuit);

discover_circuit([], Circuit) ->
	{ ok, Circuit }.

process_connection(C, error, Todo_List, Circuit) -> 
	Updated_Circuit = Circuit#{ C => processed },
    {ok, CC} = connector:get_connected(C),
	Updated_Todo_list = [ CC | Todo_List],
	{ok, ResPid} = connector:get_ResInst(C),
	{ok, C_list} = resource_instance:list_connectors(ResPid),
	{ok, C_list ++  Updated_Todo_list, Updated_Circuit};

process_connection( _, _ , Todo_List, Circuit) -> 
	{ok, Todo_List, Circuit}.





