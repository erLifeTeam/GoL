compile: rebar3
	./rebar3 compile
shell: compile
	erl -pa _build/default/lib/*/ebin \
	-config config/sys \
	-eval "application:ensure_all_started(gol)"


rebar3:
	wget -c https://s3.amazonaws.com/rebar3/rebar3
	chmod +x rebar3
