-module(ut_sup).

%% sup行为模块

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% 主管的孩子被定义为孩子规格列表 。当主管启动时，将根据此列表从左到右依次启动子进程。主管终止时，它首先以相反的启动顺序从右到左终止其子进程。
% sup_flags() =
%     #{
%        strategy => strategy(),          % optional one_for_one | one_for_all | rest_for_one | simple_one_for_one
%        intensity => non_neg_integer(),  % optional MaxR 周期时间内最大重启次数
%        period => pos_integer()          % optional MaxT 重启时间周期  MaxT里重启次数大于MaxR
%     }
% child_spec() =
%     #{
%        id => child_id(),                % mandatory Id用来内部标识子规范
%        start => mfargs(),               % mandatory {M，F，A}
%        restart => restart(),            % optional  permanent(总是重启) | transient(异常关闭会重启即关闭原因非 normal,shutdown,{shutdown,Term}) | temporary(不会重启)
%        shutdown => shutdown(),          % optional  brutal_kill | infinity | integer
%        type => worker(),                % optional  supervisor | worker
%        modules => modules()             % optional  [Module] 假如子进程是supervisor、gen_server 或 gen_fsm，那么Module 是回调模块的名称；假如子进程是gen_event，那么Modules 应该是dynamic
%     }
init([]) ->
   SupFlags =
      #{
         strategy => one_for_all,
         intensity => 0,
         period => 1
      },
   ChildSpecs =
      [],
   {ok, {SupFlags, ChildSpecs}}.
