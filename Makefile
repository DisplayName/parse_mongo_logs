all: generate

get-deps:
	@./rebar get-deps

clean:
	@./rebar clean

compile: get-deps
	@./rebar compile

generate: compile
	@rm -rf ./rel/parse_mongo_logs
	@./rebar generate

console:
	@chmod +x ./rel/parse_mongo_logs/bin/parse_mongo_logs
	@./rel/parse_mongo_logs/bin/parse_mongo_logs console