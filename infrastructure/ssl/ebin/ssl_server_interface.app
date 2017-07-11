{application, ssl_server_interface,
 [{description, "template for jle microservice with ssl"},
  {vsn, "0.1.0"},
  {modules, [ssl_server_app,
             ssl_server_sup,
	     ssl_server_server,
	    ssl_server_interface]},
  {registered, [ssl_server_sup]},
  {applications, [kernel, stdlib]},
  {mod, {ssl_server_app, []}}
 ]}.
