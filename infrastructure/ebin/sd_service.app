{application, sd_service,
 [{description, "sd service for master node"},
  {vsn, "1.0.0"},
  {modules, [sd_service_sup,sd_service_app,sd_server,sd_service]},
  {registered, [sd_service]},
  {applications, [kernel, stdlib]},
  {mod, {sd_service_app, []}}
 ]}.