{application, math_service,
 [{description, "template for jle microservice with ssl"},
  {vsn, "1.0.0"},
  {modules, [math_service_sup,math_service_app,math_server,math_service]},
  {registered, [math_service]},
  {applications, [kernel, stdlib]},
  {mod, {math_service_app, []}}
 ]}.