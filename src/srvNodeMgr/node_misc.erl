-module(node_misc).

-export([names/1, make/1, parts/1, cookie_hash/0,
   is_running/2, is_process_running/2]).

-define(EPMD_TIMEOUT, 30000).

names(Hostname) ->
   Self = self(),
   Ref = make_ref(),
   {Pid, MRef} = spawn_monitor(
      fun() ->
         Self ! {Ref, net_adm:names(Hostname)}
      end),
   timer:exit_after(?EPMD_TIMEOUT, Pid, timeout),
   receive
      {Ref, Names} ->
         erlang:demonitor(MRef, [flush]),
         Names;
      {'DOWN', MRef, process, Pid, Reason} ->
         {error, Reason}
   end.

make({Prefix, Suffix}) ->
   list_to_atom(lists:append([Prefix, "@", Suffix]));
make(NodeStr) ->
   make(parts(NodeStr)).

parts(Node) when is_atom(Node) ->
   parts(atom_to_list(Node));
parts(NodeStr) ->
   case lists:splitwith(fun(E) -> E =/= $@ end, NodeStr) of
      {Prefix, []} ->
         {_, Suffix} = parts(node()),
         {Prefix, Suffix};
      {Prefix, Suffix} ->
         {Prefix, tl(Suffix)}
   end.

cookie_hash() ->
   base64:encode_to_string(erlang:md5(atom_to_list(erlang:get_cookie()))).

is_running(Node, Application) ->
   case rpc:call(Node, app_utils, which_applications, []) of
      {badrpc, _} ->
         false;
      Apps ->
         proplists:is_defined(Application, Apps)
   end.

is_process_running(Node, Process) ->
   case rpc:call(Node, erlang, whereis, [Process]) of
      {badrpc, _} ->
         false;
      undefined ->
         false;
      P when is_pid(P) ->
         true
   end.
