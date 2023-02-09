-module(utPid).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([
   whereis_name/1,
   register/3,
   unregister/2,
   is_process_alive/1

]).


%% @doc 寻找PID
whereis_name({local, Atom}) ->
   erlang:whereis(Atom);

whereis_name({global, Atom}) ->
   global:whereis_name(Atom).

register(local, Name, Pid) ->
   erlang:register(Name, Pid);

register(global, Name, Pid) ->
   global:re_register_name(Name, Pid).

unregister(local, Name) ->
   erlang:unregister(Name);
unregister(global, Name) ->
   global:unregister_name(Name).

is_process_alive(Pid) ->
   try
      case Pid of
         _ when is_pid(Pid) ->
            Node = node(),
            Result = case node(Pid) of
                        Node -> erlang:is_process_alive(Pid);
                        Node1 -> erpc:call(Node1, erlang, is_process_alive, [Pid])
                     end,
            case Result of
               {badrpc, _Reason} -> false;
               Res -> Res
            end;
         _ -> false
      end
   catch
      _:_ -> false
   end.

get_child_count(Atom) ->
   case whereis_name({local, Atom}) of
      undefined ->
         0;
      _ ->
         [_, {active, ChildCount}, _, _] = supervisor:count_children(Atom),
         ChildCount
   end.

get_child_message_queue_length(Atom) ->
   case whereis_name({local, Atom}) of
      undefined ->
         [];
      _ ->
         Child_list = supervisor:which_children(Atom),
         lists:map(
            fun({Name, Pid, _Type, [Class]}) when is_pid(Pid) ->
               {message_queue_len, Qlen} = erlang:process_info(Pid, message_queue_len),
               {links, Links} = erlang:process_info(Pid, links),
               {Name, Pid, Qlen, Class, length(Links)}
            end,
            Child_list)
   end.

%% --------------------------------------------------------------------
%% Func: get pid info/7
%% Param Process: atom Pid or Pid RegName
%% 		 Top: 0=all result, N=0-N record in the result
%% 		 NeedModule fiter Pid module,[]=all
%% 		 Layer node child layer, 0=all,1=self
%%       MinMsgLen message queue length >= MinMsgLen
%%       MinMemSize pid memory size >= MinMemSize
%%       OrderKey, type atom and the value is: msglen,memory
%% Purpose: get pid info
%% Returns: {ok,Result,Count} Result=[{Pid,RegName,MemSize,MessageLength,Module},...]
%% 			{error,Reason}
%% --------------------------------------------------------------------
get_process_info(Process, Top, NeedModule, Layer, MinMsgLen, MinMemSize, OrderKey) ->
   RootPid =
      if erlang:is_pid(Process) ->
         Process;
         true ->
            case whereis_name({local, Process}) of
               undefined ->
                  error;
               ProcessPid ->
                  ProcessPid
            end
      end,
   case RootPid of
      error ->
         {error, lists:concat([Process, " is not process reg name in the ", node()])};
      _ ->
         AllPidList = get_process_all_pid(RootPid, Layer),
         RsList = get_process_info_detail(NeedModule, AllPidList, []),
         Len = erlang:length(RsList),
         FilterRsList =
            case OrderKey of
               msglen ->
                  lists:filter(fun({_, _, _, Qlen, _}) -> Qlen >= MinMsgLen end, RsList);
               memory ->
                  lists:filter(fun({_, _, Qmem, _, _}) -> Qmem >= MinMemSize end, RsList);
               _ ->
                  lists:filter(fun({_, _, _, Qlen, _}) -> Qlen >= MinMsgLen end, RsList)
            end,
         RsList2 =
            case OrderKey of
               msglen ->
                  lists:sort(fun({_, _, _, MsgLen1, _}, {_, _, _, MsgLen2, _}) -> MsgLen1 > MsgLen2 end, FilterRsList);
               memory ->
                  lists:sort(fun({_, _, MemSize1, _, _}, {_, _, MemSize2, _, _}) ->
                     MemSize1 > MemSize2 end, FilterRsList);
               _ ->
                  lists:sort(fun({_, _, _, MsgLen1, _}, {_, _, _, MsgLen2, _}) -> MsgLen1 > MsgLen2 end, FilterRsList)
            end,
         NewRsList =
            if Top =:= 0 ->
               RsList2;
               true ->
                  if erlang:length(RsList2) > Top ->
                     lists:sublist(RsList2, Top);
                     true ->
                        RsList2
                  end
            end,
         {ok, NewRsList, Len}
   end.

%% --------------------------------------------------------------------
%% Func: get_process_info_detail/3
%% Purpose: get pid detail info
%% Returns: [{Pid,RegName,MemSize,MessageLength,Module},...]
%% --------------------------------------------------------------------
get_process_info_detail(_NeedModule, [], Result) -> Result;
get_process_info_detail(NeedModule, [H | T], Result) ->
   Mql = get_process_data({message_queue_len, H}),
   MemSize = get_process_data({memory, H}),
   RegName = get_process_data({registered_name, H}),
   case NeedModule of
      [] ->
         Module = get_process_info_initial_call(H),
%% 			io:format("~p process RegName:~p,Mql:~p,MemSize:~p,Module:~p\n",[H, RegName, Mql, MemSize, Module]),
         get_process_info_detail(NeedModule, T, lists:append(Result, [{H, RegName, MemSize, Mql, Module}]));
      _ ->
         case get_process_info_check_initial_call(NeedModule, H) of
            "" ->
               get_process_info_detail(NeedModule, T, Result);
            Module ->
%% 					io:format("~p process RegName:~p,Mql:~p,MemSize:~p\n",[H, RegName, Mql, MemSize]),
               get_process_info_detail(NeedModule, T, lists:append(Result, [{H, RegName, MemSize, Mql, Module}]))

         end
   end.

%% --------------------------------------------------------------------
%% Func: get_process_info_check_initial_call/2
%% Purpose: check inital call
%% Returns: true or false
%% --------------------------------------------------------------------
get_process_info_check_initial_call(NeedModule, Pid) ->
   DictionaryList = get_process_data({dictionary, Pid}),
%% 	io:format("Dictionary List:~p\n",[DictionaryList]),
   case proplists:lookup('$initial_call', DictionaryList) of
      {'$initial_call', {Module, _, _}} ->
%% 			io:format("~p found initial_call Module=~p\n",[Pid,Module]),
         case lists:member(Module, NeedModule) of
            true ->
               Module;
            _ ->
               ""
         end;
      _ ->
         ""
   end.
%% --------------------------------------------------------------------
%% Func: get_process_info_initial_call/1
%% Purpose: get initial call
%% Returns: true or false
%% --------------------------------------------------------------------
get_process_info_initial_call(Pid) ->
   DictionaryList = get_process_data({dictionary, Pid}),
%% 	io:format("Dictionary List:~p\n",[DictionaryList]),
   case proplists:lookup('$initial_call', DictionaryList) of
      {'$initial_call', {Module, _, _}} ->
         Module;
      _ ->
         ""
   end.
%% --------------------------------------------------------------------
%% Func: get_process_all_pid/1
%% Purpose: get pid and child pid, Layer 0 all 1 fisrt
%% Returns: [Pid,...]
%% --------------------------------------------------------------------
get_process_all_pid(RootPid, Layer) ->
   ParentPid = get_process_parent_pid(RootPid),
   RootLinkPidList = get_process_data({links, RootPid}),
%% 	io:format("~p links process links~p,and parent pid is~p\n",[RootPid, RootLinkPidList, ParentPid]),
   case RootLinkPidList of
      [] ->
         [RootPid];
      _ ->
         if erlang:length(RootLinkPidList) =:= 1 ->
            [RootPid];
            true ->
               NewLinkPidList =
                  if erlang:is_pid(ParentPid) ->
                     lists:delete(ParentPid, RootLinkPidList);
                     true ->
                        RootLinkPidList
                  end,
               LinkPidList = lists:delete(RootPid, NewLinkPidList),

%% 				io:format("~p do handle links process links~p\n",[RootPid,LinkPidList]),
               if Layer =:= 1 ->
                  [RootPid];
                  true ->
                     get_process_all_pid(LinkPidList, Layer, [RootPid], 2)
               end
         end
   end.

get_process_all_pid([], _Layer, ResultList, _Index) -> ResultList;
get_process_all_pid([H | T], Layer, ResultList, Index) ->
%% 	io:format("get process all pid Index=~p", [Index]),
   if erlang:is_pid(H) ->
      ParentPid = get_process_parent_pid(H),
      RootLinkPidList = get_process_data({links, H}),
%% 			io:format("~p links process links~p,and parent pid is~p\n",[H, RootLinkPidList, ParentPid]),
      case RootLinkPidList of
         [] ->
            get_process_all_pid(T, Layer, lists:append(ResultList, [H]), Index);
         _ ->
            if erlang:length(RootLinkPidList) =:= 1 ->
               get_process_all_pid(T, Layer, lists:append(ResultList, [H]), Index);
               true ->
                  NewLinkPidList =
                     if erlang:is_pid(ParentPid) ->
                        lists:delete(ParentPid, RootLinkPidList);
                        true ->
                           RootLinkPidList
                     end,
                  LinkPidList = lists:delete(H, NewLinkPidList),
                  NewIndex = Index + 1,
                  SubResultList =
                     if NewIndex > Layer, Layer =/= 0 ->
                        [H];
                        true ->
                           get_process_all_pid(LinkPidList, Layer, [H], NewIndex)
                     end,
                  get_process_all_pid(T, Layer, lists:append(ResultList, SubResultList), Index)
            end
      end;
      true ->
         get_process_all_pid(T, Layer, ResultList, Index)
   end.

%% --------------------------------------------------------------------
%% Func: get_process_parent_pid/1
%% Purpose: get the pid parent pid
%% Returns: Pid or ignore
%% --------------------------------------------------------------------
get_process_parent_pid(Pid) ->
   DictionaryList = get_process_data({dictionary, Pid}),
%% 	io:format("Dictionary List:~p\n",[DictionaryList]),
   case proplists:lookup('$ancestors', DictionaryList) of
      {'$ancestors', [ParentPid | _]} ->
%% 			io:format("~p found parent pid is ~p\n",[Pid,ParentPid]),
         if erlang:is_pid(ParentPid) ->
            ParentPid;
            true ->
               whereis_name({local, ParentPid})
         end;
      _ ->
         ignore
   end.
%% --------------------------------------------------------------------
%% Func: get_process_data/1
%% Purpose: get the dictionary info of the process
%% Returns: [] or DictionaryList
%% --------------------------------------------------------------------
get_process_data({dictionary, Pid}) ->
   try erlang:process_info(Pid, dictionary) of
      {_, DList} -> DList;
      _ -> []
   catch
      _:_ -> []
   end;
%% --------------------------------------------------------------------
%% Func: get_process_data/1
%% Purpose: get the links info of the process
%% Returns: [] or LinksList
%% --------------------------------------------------------------------
get_process_data({links, Pid}) ->
   try erlang:process_info(Pid, links) of
      {_, Links} -> lists:filter(fun(I) -> erlang:is_pid(I) end, Links);
      _ -> []
   catch
      _:_ -> []
   end;
%% --------------------------------------------------------------------
%% Func: get_process_data/1
%% Purpose: get the message queue length info of the process
%% Returns: 0 or Length
%% --------------------------------------------------------------------
get_process_data({message_queue_len, Pid}) ->
   try erlang:process_info(Pid, message_queue_len) of
      {message_queue_len, Length} -> Length;
      _ -> 0
   catch
      _:_ -> 0
   end;
%% --------------------------------------------------------------------
%% Func: get_process_data/1
%% Purpose: get the memory size info of the process
%% Returns: 0 or MemorySize
%% --------------------------------------------------------------------
get_process_data({memory, Pid}) ->
   try erlang:process_info(Pid, memory) of
      {memory, Size} -> Size;
      _ -> 0
   catch
      _:_ -> 0
   end;
%% --------------------------------------------------------------------
%% Func: get_process_data/1
%% Purpose: get the registered name info of the process
%% Returns: "" or RegisteredName
%% --------------------------------------------------------------------
get_process_data({registered_name, Pid}) ->
   try erlang:process_info(Pid, registered_name) of
      {registered_name, RegName} -> RegName;
      _ -> ""
   catch
      _:_ -> ""
   end.