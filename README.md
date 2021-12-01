## Compile

```
$ dune build bin/ocabot.exe
```

## Run

```
$ dune exec -- bin/ocabot.exe https://ya.ru https://goo.gl --check-timeout 1 --telegram-chat <chat-id> --telegram-url https://api.telegram.org/<bot-token> --parallel-ping
```