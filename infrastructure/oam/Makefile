## set the paths for a default setup
clean:
	rm -rf */*~ *.beam */*.beam *~ *.dump

oam_test:
	rm -rf */*~ *.beam */*.beam *~ *.dump ebin/oam_service.app;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	cp src/oam_service.app ebin;
	erl -pa ebin -s oam_test test  -sname oam_test
