{application, bm_service,
 [{description, "prototype board mgr"},
  {vsn, "1.0.0"},
  {modules, [ssl_server,ssl_server_sup,bm_service_app,bm_service]},
  {registered, [bm_service]},
  {applications, [kernel, stdlib]},
  {mod, {bm_service_app, []}}
 ]}.
