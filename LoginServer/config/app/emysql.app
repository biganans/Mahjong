{application,						emysql,
	[{description,					"emysql,依赖第三方的数据库app，固定写法"},
	{id,							"emysql"},
	{vsn,							"0.1.0"},
	{modules,						[emysql,emysql_app,emysql_auth,emysql_conn,emysql_conn_mgr,emysql_statements,emysql_sup,emysql_tcp,emysql_tracer,emysql_util,emysql_worker]},
	{maxP,							infinity},
	{maxT,							infinity},
	{registered,					[]},
	{included_applications,			[]},
	{applications,					[kernel,stdlib,crypto]},
	{env,							[{default_timeout,30000},{conn_test_period,28000},{lock_timeout,15000}]},
	{mod,							{emysql_app,["2013年12月 4日 星期三 20时10分07秒 CST"]}},
	{start_phases,					undefined}]}.

