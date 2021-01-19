-module(utGenTerm).

-export([
   any/0,
   any/1,
   genAtom/1,
   genInteger/1,
   genFloat/1,
   genReference/1,
   genPort/1,
   genPid/1,
   genTuple/1,
   genList/1,
   genShortString/1,
   genString/1,
   genBinary/1,
   genBitstring/1,
   genBigNum/1,
   genFunction/1
]).

any() ->
   any(16).

any(MaxSize) when MaxSize =< 0 ->
   Fun = choice(valueTypes()),
   ?MODULE:Fun(MaxSize);
any(MaxSize) ->
   Fun = choice(allTypes()),
   ?MODULE:Fun(MaxSize).

genAtom(MaxSize) ->
   list_to_atom(genShortString(MaxSize)).

genInteger(_) ->
   Value = case rand:uniform() < 0.5 of
              true -> rand:uniform(127);
              false -> rand:uniform(16#FFFFFFFF)
           end,
   case rand:uniform() < 0.5 of
      true -> -1 * Value;
      false -> Value
   end.

genFloat(_) ->
   rand:uniform() * float(16#FFFFFFFF).

genReference(_) ->
   erlang:make_ref().

genPort(_) ->
   Ports = erlang:ports(),
   lists:nth(rand:uniform(length(Ports)), Ports).

genPid(_) ->
   Pids = erlang:processes(),
   lists:nth(rand:uniform(length(Pids)), Pids).

genTuple(MaxSize) ->
   list_to_tuple(genList(MaxSize)).

genList(MaxSize) ->
   Width = rand:uniform(MaxSize),
   [any(MaxSize - Width) || _ <- lists:seq(1, Width)].

genShortString(_) ->
   Size = rand:uniform(255),
   [rand:uniform(127) || _ <- lists:seq(1, Size)].

genString() ->
   Size = rand:uniform(4096),
   [rand:uniform(127) || _ <- lists:seq(1, Size)].

genString(MaxSize) ->
   [rand:uniform(255) || _ <- lists:seq(1, MaxSize)].

genBinary(MaxSize) ->
   list_to_binary(genString(MaxSize)).

genBitstring(MaxSize) ->
   B = genBinary(MaxSize),
   <<2:4/integer, B/binary>>.

genBigNum(_) ->
   16#FFFFFFFFFFFFFFFF + rand:uniform(16#FFFFFFFF).

genFunction(_) ->
   choice(allTypes()).

choice(Options) ->
   lists:nth(rand:uniform(length(Options)), Options).

valueTypes() ->
   [
      genAtom,
      genInteger,
      genFloat,
      genReference,
      genPort,
      genPid,
      genShortString,
      genString,
      genBinary,
      genBitstring,
      genBigNum,
      genFunction
   ].

allTypes() ->
   valueTypes() ++ [genTuple, genList].