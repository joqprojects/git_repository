-define(SERVICE,bm_service).  % Defines the application in app file
-define(SERVICE_SERVER,bm_server).%
-define(SERVICE_SUP,bm_service_sup).


-define(CALLBACKMODULE,bm_service). % Call back module used by the gen_server
-define(PACKET_FORMAT,[binary,{packet,4},{reuseaddr, true}, {active, true}]).
-define(CERTFILE,"ebin/certificate.pem").
-define(KEYFILE,"ebin/key.pem").
-define(CLIENT_PACKET_FORMAT,[binary,{packet,4}]).
-define(CONNECT_TIMEOUT,1000).
-define(SYSTEM_TIMEOUT,1000).
