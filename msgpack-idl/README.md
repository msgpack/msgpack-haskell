IDL compiler for MessagePack RPC
================================

# Install

~~~ {.bash}
$ cabal update
$ cabal install msgpack-idl
~~~

If you use ghc <= 7.0.x, you may need to specify template-haskell's version.

~~~ {.bash}
$ cabal install msgpack-idl --constraint='template-haskell == 2.5.*'
~~~

# Usage

~~~
msgpack-rpc 0.1

config [OPTIONS] IDLFILE LANG
MessagePack RPC IDL Compiler
  
Common flags:
  -o --output=DIR  Output directory
  -? --help        Display help message
  -V --version     Print version information
~~~

# Tutorial

* Prepare/Write msgspec file

~~~
message UserInfo {
  1: int uid
  2: string name
  3: optional int flags = 1
}

enum Sites {
  0: SiteA
  1: SiteB
  2: SiteC
}

message LogInLog {
  1: UserInfo user
  2: Sites site
}

service Foo {
  bool login(1: Sites site, 2: UserInfo)
}
~~~

* execute msgspec command for generating client/server code

~~~ {.bash}
$ mprpc foo.msgspec cpp -o cpp
$ ls cpp
client.hpp
server.hpp
types.hpp
~~~
