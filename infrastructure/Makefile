## set the paths for a default setup
clean:
	rm -rf */*~ *.beam */*.beam *~ *.dump

infra_test:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin */*/*.erl;
	cp */src/*.app ebin;
	erlc -o ebin infra_test.erl;
	erl -pa ebin -s infra_test start  -sname infra_test

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
