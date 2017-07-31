{application, oam_service,
 [{description, "oam service for master node"},
  {vsn, "1.0.0"},
  {modules, [oam_service_sup,oam_service_app,oam_server,oam_service]},
  {registered, [oam_service]},
  {applications, [kernel, stdlib]},
  {mod, {oam_service_app, []}}
 ]}.