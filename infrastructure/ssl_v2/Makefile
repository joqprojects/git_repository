## set the paths for a default setup

all:


beam: 
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	erl -pa ebin sname simple

clean:
	rm -rf */*~ *.beam */*.beam *~ *.dump

template_ssl_test:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	erl -pa ebin -s template_ssl_test test  -sname template_ssl_test
p_template_ssl_test:
	rm -rf */*~ *.beam */*.beam *~ *.dump;
	erlc -o ebin src/*.erl;
	erlc -o ebin test/*.erl;
	erl -pa ebin -s template_ssl_p_test test  -sname template_ssl_p_test
