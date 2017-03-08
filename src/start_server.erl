-module(start_server).
-author("nekrasov").

-export([main/0]).

loop() -> loop().

main() ->
  erlang:display(compile:file('src/bank')),
  erlang:display(compile:file('src/bank_server')),
  erlang:display(code:load_file(bank)),
  erlang:display(code:load_file(bank_server)),
  erlang:display(bank_server:start_link()),
  loop().