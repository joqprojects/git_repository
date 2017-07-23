{application, template_ssl,
 [{description, "template for jle microservice with ssl"},
  {vsn, "0.1.0"},
  {modules, [template_ssl_app,template_ssl_sup,template_ssl_server]},
  {registered, [template_ssl]},
  {applications, [kernel, stdlib]},
  {mod, {template_ssl_app, []}}
 ]}.
