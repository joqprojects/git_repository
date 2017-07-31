## set the paths for a default setup
clean:
	rm -rf */*~ *.beam */*.beam *~ *.dump

bm_test:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	cp src/bm_service.app ebin;
	erl -pa ebin -s bm_test test  -sname bm_test
math_test:
	rm -rf */*~ *.beam */*.beam *~ *.dump ebin/*.app;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	cp src/math_service.app ebin;
	erl -pa ebin -s math_test test  -sname math_test
