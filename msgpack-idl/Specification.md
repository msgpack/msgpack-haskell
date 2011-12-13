MessagePack IDL Specification
=============================

# Syntax of Specification File

~~~
<spec> <- <message>
        / <exception>
        / <type-alias>
        / <enum>
        / <service>
~~~

## message

~~~
message <name> ['<' <type-param>, ... '>'] {
  <field>*
}
~~~

## exception

* Similar to message definition.
* It can throw as an exception.

~~~
exception <name> ['<' <type-param>, ... '>'] [< <exception-name>] {
  <field>*
}
~~~

## type alias

* no type-parameter

~~~
type <name> = <type>
~~~

## enum

~~~
enum <name> {
  <enum-id>: <enum-name>
  ...
}
~~~

## service

* multiple services can be defind
* One server contains several services

~~~
service <name> [: <version>] {
  <method>
  ...
}
~~~

## field

~~~
<field> = <field-id> : <type> <field-name> [ = <literal>]
~~~

## method

~~~
inherit *                            # inherit all
inherit <name>                       # inherit specified method
inherit <type> <name> (<field>, ...) # inherit specified method and check type
<type> <name> (<field>, ...)         # define new-method
~~~

# Types

* Primitive types
    - `void`
    - `object`
    - `bool`
    - integral types
        - `byte` / `short` / `int` / `long`
        - `ubyte` / `ushort` / `uint` / `ulong`
    - fractional types
        - `float`
        - `double`
    - `raw`
    - `string`

* Compound types
    - `list<type>`
    - `map<type, type>`
    - `tuple<type, ...>`
    - `<type>?`
        - nullable type

* User-defined types
    - `<class-name><type, ...>`

# Literals

* bool
    - `true`
    - `false`

* integral
    - `0`, `1`, `-1`, ...
 
* fractional
    - `3.14`, `.9`, `-1.23`, `1e9`, `2.23e-2`

* string
    - `"Hello, World!"`, `"\n\r\t\u1234"` # unicode string

* nullable
    - `null`

# include other files

~~~
include "foo.idl"
~~~

# Protocol extensions

## Request

* `(type, msgid, method-name, param)`
    - same as normal msgpack-rpc
    - calls <method-name> method in newest version of default service

* `(type, msgid, (method-name, service?, versoin?), param)`
    - extension of msgpack-idl
    - can specify service name and version
    - service name and version can be omitted
    - this make one server can serve multiple services

## Response

* `(type, msgid, error, result)`
    - same as normal msgpack-rpc

# Semantics

* Field
    - `field-id` specifies an index of serialized array
    - default value specified by `literal` is used when it is omitted
    - field type is nullable
        - it's value is omitted, it becomes to null.
        - otherwise, type error will be occured

* Version
    - server invokes only method matches exact same as specified version.
    - `inherit` inherits
        - same service
        - less version
        - has specified method
        - largest version's method
