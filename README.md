EWG - Erlang word list generator
======================================

= Author

Matteo Redaelli
http://www.redaelli.org/matteo/

= Quickstart

Install Erlang, with Linux debian run

	apt-get install erlang

Clone my repository

	erl -pa $PWD/ebin -s ewg start -ewg max_length 4


Change any parameters in the file

	src/ewg.app.src

Compile it

	./rebar compile

Run the tool with (it is still possible to overwrite any parameters) with

	erl -pa $PWD/ebin -s ewg start -ewg min_length 2 max_length 

See the Online Wiki for more information

	https://github.com/matteoredaelli/ewg/wiki
