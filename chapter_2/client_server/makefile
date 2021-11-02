APP_NAME := afile_client_server

compile:
	erlc ./afile_server.erl ./afile_client.erl

run_start:
	# 	erl -s afile_server start "." # this works but I'm going to instead just run init
	# 	erl -s afile_server init
	erl
	# from within erlang shell
	# 	FileServer = afile_server:start(".").
	# 	afile_client:get_file(FileServer, "missing").
	# 		- returns {error, enoent}
	# 	afile_client:get_file(FileServer, "sometestfile.txt").

run_before:
	erl -noshell -s afile_server before "./" -s init stop # displays current dir

