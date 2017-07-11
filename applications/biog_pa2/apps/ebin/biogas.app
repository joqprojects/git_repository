%% This is the application resource file (.app file) for the 'base'
%% application.
{application, biogas,
[{description, "biogas " },
{vsn, "0.0.1" },
{modules, 
	  [biogas_app,biogas_sup,biogas,web_biogas]},
{registered,[biogas]},
{applications, [kernel,stdlib]},
{mod, {biogas_app,[]}},
{start_phases, []}
]}.
