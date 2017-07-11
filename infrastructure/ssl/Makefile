## set the paths for a default setup

all:


beam: 
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	erl -pa ebin sname simple

clean:
	rm -rf */*~ *.beam */*.beam *~ *.dump
ssl_server_test:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	erl -pa ebin -s ssl_server_test test  -sname ssl_test
