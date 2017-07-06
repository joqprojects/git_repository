## set the paths for a default setup

all:


beam: 
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	erl -pa ebin sname simple

create:
	erlc -o ebin src/*.erl

clean:
	rm -rf */*~ *.beam */*.beam *~ *.dump
test1:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	erl -pa ebin -s ms_test_eunit test  -sname tcp_test

test2:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	erl -pa ebin -s tcp_test start  -sname tcp_test
test3:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	erl -pa ebin -s test start  -sname test


s:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;	
	erl -pa ebin -s tcp_test start -sname server_test
c:
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	erl -pa ebin -s tcp_test start_client  -sname client_test
