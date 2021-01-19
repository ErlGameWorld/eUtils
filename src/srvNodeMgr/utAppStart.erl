-module(utAppStart).

-export([
   startApp/2
]).

-export([
   ensure_started/1,
   ensure_loaded/1,
   stop_apps/1,
   reload/1,
   reload_modules/1,
   get_var/3,
   get_var/2,
   set_var/3,
   load_config/1,
   load_config/2,
   load_config/3
]).


startApp(App, Type) ->
   startRely(application:start(App, Type), App, Type).

startRely(ok, _App, _Type) ->
   ok;
startRely({error, {already_started, _App}}, _App, _Type) ->
   ok;
startRely({error, {not_started, Rely}}, App, Type) ->
   ok = start(Rely, Type),
   start(App, Type);
startRely({error, Reason}, App, Type) ->
   erlang:error({app_start_failed, App, Type, Reason}).

start(_Rely, _Type) ->
   ok.

%% @doc
%% Tries to start applications or check whether they have started.
%% @end
-spec ensure_started(list(atom()) | atom()) -> ok | error.
ensure_started([]) -> ok;

ensure_started(App) when is_atom(App) -> ensure_started([App]);

ensure_started([App | Apps]) ->
   case application:start(App) of
      ok -> ensure_started(Apps);
      {error, {already_started, App}} -> ensure_started(Apps);
      {error, {not_started, Dep}} -> ensure_started(Dep), ensure_started([App | Apps])
   end.


%% @doc
%% Tries to load modules.
%% @end
-spec ensure_loaded(list(atom()) | atom()) -> ok | error.
ensure_loaded([]) -> ok;

ensure_loaded(Mod) when is_atom(Mod) -> ensure_started([Mod]);

ensure_loaded([Mod | Mods]) ->
   {module, _} = code:ensure_loaded(Mod),
   ensure_loaded(Mods).

%% @doc
%% Stop applications.
%% @end
-spec stop_apps(list(atom())|atom()) -> ok.
stop_apps([]) -> ok;
stop_apps(App) when is_atom(App) -> stop_apps([App]);
stop_apps([App | Apps]) ->
   application:stop(App),
   stop_apps(Apps).


%% @doc Compiles and reloads application modules
%% @end
-spec reload(atom()) -> ok.
reload(App) ->
   application:load(App),
   case application:get_key(App, modules) of
      undefined ->
         ok;
      {ok, Modules} ->
         reload_modules(lists:usort(Modules))
   end.


%% @doc Reload specified modules
%% @end
-spec reload_modules(list(atom()) | atom()) -> ok.
reload_modules([]) -> ok;

reload_modules([?MODULE | T]) -> reload_modules(T);

reload_modules(M) when is_atom(M) -> reload_modules([M]);

reload_modules([H | T]) ->
   reload_mod(H),
   reload_modules(T).

%% @private
-spec reload_mod(atom()) -> true.
reload_mod(Module) when is_atom(Module) ->
   error_logger:info_msg("[ulitos] Reload module: ~p", [Module]),
   code:purge(Module),
   code:load_file(Module),
   true.


%% @doc
%% Return application environment variable called <code>Var</code> if exists; otherwise return <code>Def</code>.
%% @end
-spec get_var(atom(), atom(), any()) -> any().
get_var(App, Var, Def) ->
   case application:get_env(App, Var) of
      {ok, Val} -> Val;
      _ -> Def
   end.


-spec get_var(atom(), atom()) -> any() | undefined.
get_var(App, Var) ->
   get_var(App, Var, undefined).


%% @doc
%% Set application environment variable.
%% @end
-spec set_var(atom(), atom(), any()) -> ok.
set_var(App, Var, Val) ->
   application:set_env(App, Var, Val).

%% @doc
%% Set application <code>App</code> environment from default config ("priv/APP_NAME.config").
%% @end
-spec load_config(atom()) -> ok.
load_config(App) ->
   File = atom_to_list(App) ++ ".config",
   load_config(App, File).

%% @doc
%% Set application <code>App</code> environment vars from file with name <code>File</code> located in app priv_dir.
%% @end
-spec load_config(atom(), string()) -> ok.
load_config(App, File) ->
   load_config(App, File, []).

%% @doc
%% Set application <code>App</code> environment vars from file with name <code>File</code> located in some of Dirs or in priv_dir.
%% @end
-spec load_config(atom(), string(), list()) -> ok.
load_config(App, File, Dirs) ->
   Path = code:priv_dir(App),
   Env = load_file_config(Dirs ++ [Path], File),
   [application:set_env(App, Key, Value) || {Key, Value} <- Env],
   ok.

%% @private
-spec load_file_config(Paths :: list(string()), File :: string()) -> any().
load_file_config(Paths, File) ->
   case file:path_consult(Paths, File) of
      {ok, Env, _Path} -> Env;
      _ -> []
   end.

%% Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(PATH(File), filename:join([code:lib_dir(ulitos), File])).

load_config_test_() ->
   {"List dir tests",
      {foreach,
         fun config_setup/0,
         fun config_cleanup/1,
         [
            fun load_config_by_app_t_/1,
            fun load_config_by_name_t_/1,
            fun load_config_by_name_and_paths_t_/1,
            fun load_config_by_name_and_paths_2_t_/1
         ]
      }
   }.

unconsult(File, L) ->
   {ok, S} = file:open(File, write),
   lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
   file:close(S).

config_setup() ->
   unconsult(?PATH("priv/ulitos.config"), [{test, 1}]),
   ok = file:make_dir(?PATH("priv/a")),
   unconsult(?PATH("priv/a/ulitos.config"), [{test, 2}]),
   unconsult(?PATH("priv/ul.config"), [{test, 3}]),
   unconsult(?PATH("priv/a/ul.config"), [{test, 4}]),
   ok.

config_cleanup(_) ->
   ulitos_file:recursively_del_dir(?PATH("priv/a")),
   file:delete(?PATH("priv/ulitos.config")),
   file:delete(?PATH("priv/ul.config")),
   application:unset_env(ulitos, test),
   ok.

load_config_by_app_t_(_) ->
   load_config(ulitos),
   [
      ?_assertEqual(1, get_var(ulitos, test))
   ].

load_config_by_name_t_(_) ->
   load_config(ulitos, "ul.config"),
   [
      ?_assertEqual(3, get_var(ulitos, test))
   ].

load_config_by_name_and_paths_t_(_) ->
   load_config(ulitos, "ul.config", [?PATH("priv/b"), ?PATH("priv/a")]),
   [
      ?_assertEqual(4, get_var(ulitos, test))
   ].

load_config_by_name_and_paths_2_t_(_) ->
   load_config(ulitos, "ul.config", [?PATH("priv/b"), ?PATH("c")]),
   [
      ?_assertEqual(3, get_var(ulitos, test))
   ].

reload_modules_test_() ->
   {"Reload module code",
      {foreach,
         fun reload_setup/0,
         fun reload_cleanup/1,
         [
            fun reload_modules_t_/1
         ]
      }
   }.

reload_setup() ->
   code:add_patha(?PATH("")),
   ok = file:write_file(?PATH("my_module.erl"), <<"-module(my_module).\n-export([say/0]).\nsay() -> hello.">>),
   {ok, _} = compile:file(?PATH("my_module.erl"), {outdir, ?PATH("")}),
   {module, my_module} = code:load_file(my_module),
   ok.

reload_cleanup(_) ->
   ok.

reload_modules_t_(_) ->
   Before = my_module:say(),
   ok = file:write_file(?PATH("my_module.erl"), <<"-module(my_module).\n-export([say/0]).\nsay() -> hi.">>),
   {ok, _} = compile:file(?PATH("my_module.erl"), {outdir, ?PATH("")}),
   reload_modules(my_module),
   After = my_module:say(),
   [
      ?_assertEqual(hello, Before),
      ?_assertEqual(hi, After)
   ].

-endif.
