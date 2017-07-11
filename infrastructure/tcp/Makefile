## set the paths for a default setup

all:


beam: 
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	erl -pa ebin sname simple

clean:
	rm -rf */*~ *.beam */*.beam *~ *.dump
tcp_test:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	erl -pa ebin -s ms_test_eunit test  -sname tcp_test
