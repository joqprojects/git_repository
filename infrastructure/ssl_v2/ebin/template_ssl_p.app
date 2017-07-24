{application, template_ssl_p,
 [{description, "template for jle microservice with ssl"},
  {vsn, "0.1.0"},
  {modules, [template_ssl_p_app,template_ssl_p_sup,template_ssl_p_server]},
  {registered, [template_ssl_p]},
  {applications, [kernel, stdlib]},
  {mod, {template_ssl_p_app, []}}
 ]}.
