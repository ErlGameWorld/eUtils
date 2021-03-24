# 服务器用法
    arangod [<option>]
    
    可以将数据库目录指定为位置 第一个参数
    arangod /path/to/datadir
    或者显示作为命名参数
    arangod --database.directory /path/to/datadir
    
    所有其他参数都需要作为命名参数传递
    两个连字符(--)后跟选项名称 然后 等号或者空格 最后是参数值 如果参数值包含了空格 还需要用引号引起来
    = 两边 有空格
    
# 各种配置选项
    
## Global
    Name	            Typ
    check-configuration boolean
    	检查配置并退出 这是一个命令，不需要指定任何值。执行命令后，进程终止。
    config	            string
        The configuration file or ‘none’
        Default: ""
    configuration       string
        The configuration file or ‘none’
        Default: ""
    console 	        boolean
        Start a JavaScript emergency console
        This option can be specified without value to enable it.
        Default: false
    daemon	            boolean
        Background the server, running it as daemon
        This option can be specified without value to enable it.
        Default: false
    default-language    string
        ISO-639 language code
        Default: ""
    define              string…
        Define key=value for a @key@ entry in config file
        Default: []
    dump-dependencies   boolean
        Dump dependency graph
        This is a command, no value needs to be specified. The process terminates after executing the command.
    dump-options        boolean
        Dump configuration options in JSON format
        This is a command, no value needs to be specified. The process terminates after executing the command.
    fortune	            boolean
        Show fortune cookie on startup
        This option can be specified without value to enable it.
        Default: false
    gid	                string
        Switch to group-id after reading config files
        Default: ""
    hund                boolean
        Make ArangoDB bark on startup
        This option can be specified without value to enable it.
        Default: false
    log	                string…
        The global or topic-specific log level
        Default: ["info","info"]
        Deprecated in: v3.5.0
    pid-file            string
        Pid-file in daemon mode
        Default: ""
    supervisor	        boolean
        Background the server, starts a supervisor
        This option can be specified without value to enable it.
        Default: false
    uid	                string
        Switch to user-id after reading config files
        Default: ""
    version             boolean
        Reports the version and exits
        This is a command, no value needs to be specified. The process terminates after executing the command.
    working-directory	string
        Working directory in daemon mode
        Default: "/var/tmp"   

## Agency
    Name	Type	Description
    agency.activate	boolean	Activate agency
    This option can be specified without value to enable it.
    Default: false
    Effective on Agents only.
    agency.compaction-keep-size	uint64	Keep as many indices before compaction point
    Default: 50000
    Effective on Agents only.
    agency.compaction-step-size	uint64	Step size between state machine compactions
    Default: 1000
    Effective on Agents only.
    agency.disaster-recovery-id	string	Allows for specification of the id for this agent; dangerous option for disaster recover only!
    Default: ""
    Effective on Agents only.
    agency.election-timeout-max	double	Maximum timeout before an agent calls for new election (in seconds)
    Default: 5
    Effective on Agents only.
    agency.election-timeout-min	double	Minimum timeout before an agent calls for new election (in seconds)
    Default: 1
    Effective on Agents only.
    agency.endpoint	string…	Agency endpoints
    Default: []
    Effective on Agents only.
    agency.max-append-size	uint64	Maximum size of appendEntries document (# log entries)
    Default: 250
    Effective on Agents only.
    agency.my-address	string	Which address to advertise to the outside
    Default: ""
    Effective on Agents only.
    agency.pool-size	uint64	Number of agent pool
    Default: 1
    Effective on Agents only.
    agency.size	uint64	Number of agents
    Default: 1
    Effective on Agents only.
    agency.supervision	boolean	Perform arangodb cluster supervision
    This option can be specified without value to enable it.
    Default: false
    Effective on Agents only.
    agency.supervision-frequency	double	Arangodb cluster supervision frequency (in seconds)
    Default: 1
    Effective on Agents only.
    agency.supervision-grace-period	double	Supervision time, after which a server is considered to have failed (in seconds)
    Default: 10
    Effective on Agents only.
    agency.supervision-ok-threshold	double	Supervision time, after which a server is considered to be bad [s]
    Default: 5
    Effective on Agents only.
    agency.wait-for-sync	boolean	Wait for hard disk syncs on every persistence call (required in production)
    This option can be specified without value to enable it.
    Default: true
    Effective on Agents only.  
    
##  ArangoSearch
    Name	Type	Description
    arangosearch.threads	uint64	The exact number of threads to use for asynchronous tasks (0 == autodetect)
    Default: 0
    arangosearch.threads-limit	uint64	Upper limit to the autodetected number of threads to use for asynchronous   
    
## Audit
    Name	Type	Description
    audit.hostname	string	Enterprise Edition only
    Hostname to use
    Default: ""
    audit.output	string…	Enterprise Edition only
    Audit destinatio    
    
## Backup    
    Name	Type	Description
    backup.api-enabled	string	Whether the backup api is enabled (true) or not (false), or only enabled for superuser JWT (jwt)
    Default: "true"
    backup.local-path-prefix	This option restricts any backup target to a given path prefix  Default: "/"
    
## Cache
    cache.rebalancing-interval	uint64	Microseconds between rebalancing attempts
    Default: 2000000
    cache.size	uint64	Size of cache in bytes
    Default: dynamic (e.g. 33252951040)    
        
## Cluster
    Name	Type	Description
    cluster.agency-endpoint	string…	Agency endpoint to connect to
    Default: []
    Effective on Coordinators and DB-Servers only.
    cluster.create-waits-for-sync-replication	boolean	Active coordinator will wait for all replicas to create collection
    This option can be specified without value to enable it.
    Default: true
    Effective on Coordinators and DB-Servers only.
    cluster.default-replication-factor	uint32	Default replication factor for non-system collections
    Default: 0
    Effective on Coordinators only.
    Introduced in: v3.6.0
    cluster.force-one-shard	boolean	Force one-shard mode for all new collections
    This option can be specified without value to enable it.
    Default: false
    Effective on Coordinators only.
    Introduced in: v3.6.0
    cluster.index-create-timeout	double	Amount of time (in seconds) the coordinator will wait for an index to be created before giving up
    Default: 3600
    Effective on Coordinators only.
    cluster.max-number-of-shards	uint32	Maximum number of shards when creating new collections (0 = unrestricted)
    Default: 1000
    Effective on Coordinators only.
    Introduced in: v3.5.1
    cluster.max-replication-factor	uint32	Maximum replication factor for new collections (0 = unrestricted)
    Default: 10
    Effective on Coordinators only.
    Introduced in: v3.6.0
    cluster.min-replication-factor	uint32	Minimum replication factor for new collections
    Default: 1
    Effective on Coordinators only.
    Introduced in: v3.6.0
    cluster.my-address	string	This server’s endpoint (cluster internal)
    Default: ""
    Effective on Coordinators and DB-Servers only.
    cluster.my-advertised-endpoint	string	This server’s advertised endpoint (e.g. external IP address or load balancer, optional)
    Default: ""
    Effective on Coordinators and DB-Servers only.
    cluster.my-role	string	This server’s role
    Default: ""
    cluster.require-persisted-id	boolean	If set to true, then the instance will only start if a UUID file is found in the database on startup. Setting this option will make sure the instance is started using an already existing database directory and not a new one. For the first start, the UUID file must either be created manually or the option must be set to false for the initial startup
    This option can be specified without value to enable it.
    Default: false
    cluster.resign-leadership-on-shutdown	boolean	Create resign leader ship job for this dbsever on shutdown
    This option can be specified without value to enable it.
    Default: false
    Effective on DB-Servers only.
    cluster.synchronous-replication-timeout-factor	double	All synchronous replication timeouts are multiplied by this factor
    Default: 1
    cluster.synchronous-replication-timeout-minimum	double	All synchronous replication timeouts will be at least this value (in seconds)
    Default: 30
    cluster.synchronous-replication-timeout-per-4k	double	All synchronous replication timeouts are increased by this amount per 4096 bytes (in seconds)
    Default: 0.1
    cluster.system-replication-factor	uint32	Default replication factor for system collections
    Default: 2
    Effective on Coordinators only.
    cluster.upgrade	string	Perform a cluster upgrade if necessary (auto = perform upgrade and shut down only if --database.auto-upgrade true is set, disable = never perform upgrade, force = always perform an upgrade and shut down, online = always perform an upgrade but don’t shut down)
    Default: "auto"
    Possible values: “auto”, “disable”, “force”, “online”
    cluster.write-concern	uint32	Write concern used for writes to new collections
    Default: 1
    Effective on Coordinators only.
    Introduced in: v3.6.0    
    
## Database
    Name	Type	Description
    database.auto-upgrade	boolean	Perform a database upgrade if necessary
    This option can be specified without value to enable it.
    Default: false
    database.check-version	boolean	Checks the versions of the database and exit
    This is a command, no value needs to be specified. The process terminates after executing the command.
    database.directory	string	Path to the database directory
    Default: ""
    database.force-sync-properties	boolean	Force syncing of collection properties to disk, will use waitForSync value of collection when turned off
    This option can be specified without value to enable it.
    Default: true
    database.ignore-datafile-errors	boolean	Load collections even if datafiles may contain errors
    This option can be specified without value to enable it.
    Default: false
    database.init-database	boolean	Initializes an empty database
    This is a command, no value needs to be specified. The process terminates after executing the command.
    database.password	string	Initial password of root user
    Default: ""
    database.required-directory-state	string	Required state of database directory at startup (non-existing: database directory must not exist, existing: database directory must exist, empty: database directory must exist but be empty, populated: database directory must exist and contain specific files already, any: any state allowed)
    Default: "any"
    Possible values: “any”, “empty”, “existing”, “non-existing”, “populated”
    database.restore-admin	boolean	Resets the admin users and sets a new password
    This is a command, no value needs to be specified. The process terminates after executing the command.
    database.throw-collection-not-loaded-error	boolean	Throw an error when accessing a collection that is still loading
    This option can be specified without value to enable it.
    Default: false
    Deprecated in: v3.7.0
    database.upgrade-check	boolean	Skip a database upgrade
    This option can be specified without value to enable it.
    Default: true
    database.wait-for-sync	boolean	Default wait-for-sync behavior, can be overwritten when creating a collection
    This option can be     
  
##  Foxx
    Name	Type	Description
    foxx.api	boolean	Enables Foxx management REST APIs
    This option can be specified without value to enable it.
    Default: true
    Effective on Coordinators and Single Servers only.
    Introduced in: v3.5.0
    foxx.queues	boolean	Enable Foxx queues
    This option can be specified without value to enable it.
    Default: true
    Effective on Coordinators and Single Servers only.
    foxx.queues-poll-interval	double	Poll interval (in seconds) for Foxx queue manager
    Default: 1
    Effective on Coordinators and Single Servers only.
    foxx.store	boolean	Enables Foxx store in web interface
    This option can be specified without value to enable it.
    Default: true
    Effective on Coordinators and Single Servers only.
    Introduced in: v3.5.0   
    
# Frontend  
    Name	Type	Description
    frontend.proxy-request-check	boolean	Enable proxy request checking
    This option can be specified without value to enable it.
    Default: true
    Effective on Coordinators and Single Servers only.
    frontend.trusted-proxy	string…	List of proxies to trust (may be IP or network). Make sure --frontend.proxy-request-check is enabled
    Default: []
    Effective on Coordinators and Single Servers only.
    frontend.version-check	boolean	Alert the user if new versions are available
    This option can be specified without value to enable it.
    Default: true
    Effective on Coordinators and Single Servers only.  
    
## HTTP
    Name	Type	Description
    http.allow-method-override	boolean	Allow HTTP method override using special headers
    This option can be specified without value to enable it.
    Default: false
    http.hide-product-header	boolean	Do not expose “Server: ArangoDB” header in HTTP responses
    This option can be specified without value to enable it.
    Default: false
    http.keep-alive-timeout	double	Keep-alive timeout in seconds
    Default: 300
    http.trusted-origin	string…	Trusted origin URLs for CORS requests with credentials
    Default: []   
    
# JavaScript
    Name	Type	Description
    javascript.allow-admin-execute	boolean	For testing purposes allow ‘_admin/execute’, NEVER enable on production
    This option can be specified without value to enable it.
    Default: false
    Effective on Coordinators and Single Servers only.
    javascript.allow-external-process-control	boolean	Allow execution and control of external processes from within JavaScript actions
    This option can be specified without value to enable it.
    Default: false
    Effective on Coordinators and Single Servers only.
    Introduced in: v3.5.0
    javascript.allow-port-testing	boolean	Allow testing of ports from within JavaScript actions
    This option can be specified without value to enable it.
    Default: false
    Effective on Coordinators and Single Servers only.
    Introduced in: v3.5.0
    javascript.app-path	string	Directory for Foxx applications
    Default: "./js/apps"
    Effective on Coordinators and Single Servers only.
    javascript.copy-installation	boolean	Copy contents of ‘javascript.startup-directory’ on first start
    This option can be specified without value to enable it.
    Default: false
    Effective on Coordinators and Single Servers only.
    javascript.enabled	boolean	Enable the V8 JavaScript engine
    This option can be specified without value to enable it.
    Default: true
    Effective on Coordinators and Single Servers only.
    javascript.endpoints-blacklist	string…	Endpoints that cannot be connected to via @arangodb/request module in JavaScript actions if not whitelisted
    Default: []
    Effective on Coordinators and Single Servers only.
    Introduced in: v3.5.0
    javascript.endpoints-whitelist	string…	Endpoints that can be connected to via @arangodb/request module in JavaScript actions
    Default: []
    Effective on Coordinators and Single Servers only.
    Introduced in: v3.5.0
    javascript.environment-variables-blacklist	string…	Environment variables that will be inaccessible in JavaScript if not whitelisted
    Default: []
    Effective on Coordinators and Single Servers only.
    Introduced in: v3.5.0
    javascript.environment-variables-whitelist	string…	Environment variables that will be accessible in JavaScript
    Default: []
    Effective on Coordinators and Single Servers only.
    Introduced in: v3.5.0
    javascript.files-whitelist	string…	Filesystem paths that will be accessible from within JavaScript actions
    Default: []
    Effective on Coordinators and Single Servers only.
    Introduced in: v3.5.0
    javascript.gc-frequency	double	JavaScript time-based garbage collection frequency (each x seconds)
    Default: 60
    Effective on Coordinators and Single Servers only.
    javascript.gc-interval	uint64	JavaScript request-based garbage collection interval (each x requests)
    Default: 2000
    Effective on Coordinators and Single Servers only.
    javascript.harden	boolean	Disables access to JavaScript functions in the internal module: getPid() and logLevel()
    This option can be specified without value to enable it.
    Default: false
    Effective on Coordinators and Single Servers only.
    Introduced in: v3.5.0
    javascript.module-directory	string…	Additional paths containing JavaScript modules
    Default: ["./enterprise/js"]
    Effective on Coordinators and Single Servers only.
    javascript.script	string…	Run scripts and exit
    Default: []
    javascript.script-parameter	string…	Script parameter
    Default: []
    javascript.startup-directory	string	Path to the directory containing JavaScript startup scripts
    Default: "./js"
    Effective on Coordinators and Single Servers only.
    javascript.startup-options-blacklist	string…	Startup options whose names match this regular expression will not be exposed (if not whitelisted) to JavaScript actions
    Default: []
    Effective on Coordinators and Single Servers only.
    Introduced in: v3.5.0
    javascript.startup-options-whitelist	string…	Startup options whose names match this regular expression will be whitelisted and exposed to JavaScript
    Default: []
    Effective on Coordinators and Single Servers only.
    Introduced in: v3.5.0
    javascript.v8-contexts	uint64	Maximum number of V8 contexts that are created for executing JavaScript actions
    Default: 0
    Effective on Coordinators and Single Servers only.
    javascript.v8-contexts-max-age	double	Maximum age for each V8 context (in seconds) before it is disposed
    Default: 60
    Effective on Coordinators and Single Servers only.
    javascript.v8-contexts-max-invocations	uint64	Maximum number of invocations for each V8 context before it is disposed
    Default: 0
    Effective on Coordinators and Single Servers only.
    javascript.v8-contexts-minimum	uint64	Minimum number of V8 contexts that keep available for executing JavaScript actions
    Default: 0
    Effective on Coordinators and Single Servers only.
    javascript.v8-max-heap	uint64	Maximal heap size (in MB)
    Default: 3072
    javascript.v8-options	string…	Options to pass to v8
    Default: []
    
## LDAP  
    Name	Type	Description
    ldap.allow-offline	boolean	Enterprise Edition only
    If a refresh attempt fails to connect to the LDAP server, continue with the cached authentication data
    This option can be specified without value to enable it.
    Default: false
    ldap.async-connect	boolean	Enterprise Edition only
    Whether or not the connection to the LDAP library will be done asynchronously
    This option can be specified without value to enable it.
    Default: false
    ldap.basedn	string	Enterprise Edition only
    Ldap basedn, eg. dc=example,dc=com
    Default: ""
    ldap.binddn	string	Enterprise Edition only
    Ldap binddn, eg. cn=admin,dc=example,dc=com
    Default: ""
    ldap.bindpasswd	string	Enterprise Edition only
    Ldap bindpassword, eg. admin
    Default: ""
    ldap.debug	boolean	Enterprise Edition only
    Turn on internal OpenLDAP library output (warning: will print to stdout)
    This option can be specified without value to enable it.
    Default: false
    ldap.enabled	boolean	Enterprise Edition only
    Enable LDAP
    This option can be specified without value to enable it.
    Default: false
    ldap.network-timeout	double	Enterprise Edition only
    Timeout value (in seconds) after which network operations following the initial connection return in case of no activity (a value of 0 means default timeout)
    Default: 0
    ldap.port	uint16	Enterprise Edition only
    Port to use
    Default: 389
    ldap.prefix	string	Enterprise Edition only
    Ldap prefix, eg. uid= xor dn= xor cn=
    Default: ""
    ldap.referrals	boolean	Enterprise Edition only
    Whether or not the LDAP library should implicitly chase referrals
    This option can be specified without value to enable it.
    Default: false
    ldap.refresh-rate	double	Enterprise Edition only
    Refresh user settings after this time (in seconds)
    Default: 300
    ldap.restart	boolean	Enterprise Edition only
    Whether or not the LDAP library should implicitly restart connections
    This option can be specified without value to enable it.
    Default: false
    ldap.retries	uint32	Enterprise Edition only
    Number of tries to attempt connecting to the LDAP server. Setting it to values greater than one will retry connecting in case the LDAP server is unavailable or denies the connection
    Default: 1
    ldap.roles-attribute-name	string	Enterprise Edition only
    Ldap attributename where the role are located.
    Default: ""
    ldap.roles-exclude	string	Enterprise Edition only
    Regexp to exclude groups. Leave empty to exclude none.
    Default: ""
    ldap.roles-include	string	Enterprise Edition only
    Regexp to include groups. Leave empty to include all.
    Default: ""
    ldap.roles-search	string	Enterprise Edition only
    Ldap search for roles; ‘{USER}’ is replaced by the ‘dn’ of the user.
    Default: ""
    ldap.roles-transformation	string…	Enterprise Edition only
    Regexp to normalizer role name, e.g. ‘/^ (.[^ ]])*/$2/’
    Default: []
    ldap.search-attribute	string	Enterprise Edition only
    Ldap search attribute, eg. uid
    Default: "uid"
    ldap.search-filter	string	Enterprise Edition only
    Ldap search filter, eg. (objectClass=simpleSecurityObject)
    Default: "objectClass=*"
    ldap.search-scope	string	Enterprise Edition only
    Ldap search scope, one of base, one, sub
    Default: "sub"
    ldap.serialize-timeout	double	Enterprise Edition only
    Maximum amount of time (in seconds) that will be waited for the serialization mutex
    Default: 5
    ldap.serialized	boolean	Enterprise Edition only
    Whether or not calls into the LDAP library should be serialized. This option can be used to work around thread-unsafe LDAP library functionality
    This option can be specified without value to enable it.
    Default: false
    ldap.server	string	Enterprise Edition only
    Server to use
    Default: ""
    ldap.suffix	string	Enterprise Edition only
    Ldap suffix, eg. ,dc=example,dc=com
    Default: ""
    ldap.superuser-role	string	Enterprise Edition only
    Role mapping to the super-users
    Default: ""
    ldap.timeout	double	Enterprise Edition only
    Timeout value (in seconds) for synchronous LDAP API calls (a value of 0 means default timeout)
    Default: 0
    ldap.tls	boolean	Enterprise Edition only
    Enable TLS
    This option can be specified without value to enable it.
    Default: false
    ldap.tls-cacert-dir	string	Enterprise Edition only
    Ldap tls cacert dir
    Default: ""
    ldap.tls-cacert-file	string	Enterprise Edition only
    Ldap tls cacert file
    Default: ""
    ldap.tls-cert-check-strategy	string	Enterprise Edition only
    Ldap tls cert check strategy, one of never, hard, demand, allow, try
    Default: "hard"
    ldap.tls-version	string	Enterprise Edition only
    Ldap tls version, one of 1.0, 1.1, 1.2
    Default: "1.2"
    ldap.url	string	Enterprise Edition only
    Ldap url, eg. ldap://example.com:389/dc=example,dc=com?uid?sub
    Default: ""    