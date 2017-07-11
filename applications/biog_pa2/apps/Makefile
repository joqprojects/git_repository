## set the paths for a default setup

all: beam
	erl -pa ../deps/cowboy/ebin\
            -pa ../deps/ranch/ebin\
            -pa ../deps/cowlib/ebin\
            -pa ../../ebin\
            -pa ../ebin\
	    -pa ./ebin\
	    -pa /home/pi/erlang/lib/ebin\
	    -pa /home/pi/erlang/remote_services/ebin\
            -s boot_host start -sname remote_control

beam: 
	erlc -o ebin *.erl */*.erl


biogas:
	erlc -o ebin *.erl */*.erl;
	erl -pa ../deps/cowboy/ebin\
            -pa ../deps/ranch/ebin\
            -pa ../deps/cowlib/ebin\
            -pa ../../ebin\
            -pa ../ebin\
	    -pa ./ebin\
	    -pa /home/pi/erlang/lib/ebin\
	    -pa /home/pi/erlang/remote_services/ebin\
            -s boot_host start -sname remote_control

clean:
	rm -rf *~ ebin/*.beam system/*.beam app_tellstick/*.beam *.tmp

compile:
	erlc -o ebin *.erl */*.erl
