ale
=====

ale, a [lager](https://github.com/basho/lager) extension, makes it possible for several processes to trace the same modules.


### Dependencies

To build ale you will need a working installation of Erlang R15B (or
later).<br/>
Information on building and installing [Erlang/OTP](http://www.erlang.org)
can be found [here](https://github.com/erlang/otp/wiki/Installation)
([more info](https://github.com/erlang/otp/blob/master/INSTALL.md)).

ale is built using rebar that can be found [here](https://github.com/basho/rebar), with building instructions [here](https://github.com/basho/rebar/wiki/Building-rebar).

ale also requires [lager](https://github.com/basho/lager) to be installed.

### Downloading

Clone the repository in a suitable location:

```sh
$ git clone git://github.com/tonyrog/ale.git
```
### Configurating
#### Concepts

ale extends lager but currently only for traces to console.
Available api is:
<ul>
<li> trace(on | off, ModuleOrPid::atom() | pid(), Loglevel::atom()) - prints trace output on console as long as calling process hasn't terminated.</li>
<li> trace_gl(on | off, ModuleOrPid::atom() | pid(), Loglevel::atom()) - prints trace output on console as long as calling process' group leader hasn't terminated. Suitable for calls from a shell.</li>
</ul>
See lager documentations for more details. 


#### Files

Arguments to all applicable erlang applications are specified in an erlang configuration file.<br/>
An example can be found in ["sys.config"](https://github.com/tonyrog/ale/raw/master/sys.config).<br/>

### Building

Rebar will compile all needed dependencies.<br/>
Compile:

```sh
$ cd ale
$ rebar compile
...
==> ale (compile)
```

### Running

There is a quick way to run the application for testing:

```sh
$ erl -sname ale -config sys -pa <path>/ale/ebin
>ale:start().
```
(Instead of specifing the path to the ebin directory you can set the environment ERL_LIBS.)

Stop:

```sh
>halt().
```

### Release

To generate a proper release follow the instructions in 
 [Release Handling](https://github.com/basho/rebar/wiki/Release-handling) or look in the [Rebar tutorial](http://www.metabrew.com/article/erlang-rebar-tutorial-generating-releases-upgrades).

<b>Before</b> the last step you have to update the file "ale/rel/files/sys.config" with your own settings.
You probably also have to update "ale/rel/reltool.config" with the correct path to your application (normally "{lib_dirs, ["../.."]}") and all apps you need.
```
       {app, sasl,   [{incl_cond, include}]},
       {app, stdlib, [{incl_cond, include}]},
       {app, kernel, [{incl_cond, include}]},
       {app, ale, [{incl_cond, include}]}
```


And then you run: 
```
$ rebar generate
```
.

When generating a new release the old has to be (re)moved.

Start node:

```sh
$ cd rel
$ ale/bin/ale start
```

(If you want to have access to the erlang node use 
``` 
console 
```
instead of 
``` 
start
```
.)

### Documentation

ale is documented using edoc. To generate the documentation do:

```sh
$ cd ale
$ rebar doc
```
