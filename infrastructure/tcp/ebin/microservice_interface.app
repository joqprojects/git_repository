{application, microservice_interface,
 [{description, "template for jle microservice"},
  {vsn, "0.1.0"},
  {modules, [microservice_app,
             microservice_sup,
	     microservice_server]},
  {registered, [microservice_sup]},
  {applications, [kernel, stdlib]},
  {mod, {microservice_app, []}}
 ]}.
