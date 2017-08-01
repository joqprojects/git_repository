## set the paths for a default setup
clean:
	rm -rf */*~ *.beam */*.beam *~ *.dump

xxmaster_test:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin master.erl;
	erl -pa ebin -pa ../boardmgr/ebin -pa ../oam/ebin -s master start  -sname master

master_test:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	cp -R ../include .;
	cp -R ../oam .;
	cp -R ../boardmgr .;
	erlc -o ebin oam/src/*.erl;
	erlc -o ebin oam/test/*.erl;
	erlc -o ebin boardmgr/src/*.erl;
	erlc -o ebin boardmgr/test/*.erl;
	cp oam/src/*.app ebin;
	cp boardmgr/src/*.app ebin;
	erlc -o ebin master.erl;
	erl -pa ebin -s master start  -sname master

bm_test:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin boardmgr/*/*.erl;
	cp boardmgr/src/*.app ebin;
	erl -pa ebin -s bm_test test  -sname bm_test

oam_test:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin oam/*/*.erl;
	cp oam/src/*.app ebin;
	erl -pa ebin -s oam_test test  -sname oam_test
