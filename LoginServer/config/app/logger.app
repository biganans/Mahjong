{application,						logger,
	[{description,					"logger,日志记录app"},
	{id,							"logger"},
	{vsn,							"0.1"},
	{modules,						[]},
	{maxP,							infinity},
	{maxT,							infinity},
	{registered,					[]},
	{included_applications,			[]},
	{applications,					[kernel,stdlib,sasl]},
	{env,							[{log_level,2},{log_dir,"./log"}]},
	{mod,							{logger_app,[]}},
	{start_phases,					undefined}]}.

