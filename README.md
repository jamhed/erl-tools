Erlang AST Tools
================

tags.erl
--------

Make CTags file.

fmt
---

Tick Erlang atoms (atom -> 'atom').

Usage
-----
```sh
make script
./fmt tick|untick path_to_erl_file
```

It can't parse atoms in macros definitions at the moment.
