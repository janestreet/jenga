Jenga is an executable and library for building build systems.

Some general things about jenga
-------------------------------

What jenga tries to do:
- be expressive enough to properly build ocaml. Many build system cannot handle or not
  conveniently handle the fact that .cmi and .cmx need to be built in dependency
  order. Having to know when to call "make depends" or "make" also doesn't qualify as
  convenient. Having to list which files are part of a library is also not convenient
  and is error-prone.
- be efficient at scale, in particular properly make use of the available parallelism
- be correct, ie an incremental build give the same result as a build from scratch, and
  one should never need to do a build from scratch. For instance, most if not all build
  systems do not make a distinction between a stale generated file and a source file,
  meaning the build is allowed to read stale generated file, which is definitely wrong.
  jenga will delete the stale generated file instead.

A single jenga instance can build a whole tree of source files, no recursive invocation
like make. By default the root of the tree is determined by looking for the first
surrounding directory containing a file called jenga.conf (this can be changed).
jenga can be invoked from anywhere beneath that root directory.

Building is done by asking for targets: `jenga foo.exe`, or `jenga` to build the default
way, as defined by the jenga rules.

All the files that jenga creates for its internal use are stored below the .jenga
directory at the root of the tree. So you want to make your version control system ignore
that directory, and if you want to reset the state of jenga for some reason, this is what
you want to delete.

The most common command line interface flags
-------------------------------------------

- `-P`: run in polling mode, meaning the build system will not stop when it's
        done building but will wait for filesystem changes and restart building
        when anything changes. The result is the same as restarting the build
        system manually, but faster and more conveniently.

- `--progress`: to show how far along the build is

- `act`, or `-verbose`: to see more information about what jenga is running, especially
        if you're playing around with jenga for the first time

What the jenga is and isn't
---------------------------

Jenga doesn't come with rules for building anything out of the box. Everything needs to be
provided by the user.  If you are looking for pre-made rules to build ocaml and other
things, https://github.com/janestreet/jenga-rules is more likely to be what you want.

You need to care about this executable and library if either:
- you plan on writing rules yourself
- you want to connect to a jenga server (to get information out of it in a typed way,
  rather than parsing output)

Writing rules yourself
----------------------

If you want to write rules, you should look at the documentation in `api.mli`, because
this is how you will do so. You may want to take a look at the examples subdirectory.

The rules can be either statically linked inside the executable (which is simple but the
rules can't be changed without rebuilding jenga) or dynamically linked using ocaml-plugin
(in which case the rules can be changed easily, but requires that every repository
contains a copy of the rules and is not composable right now).

If you want dynamically linked rules, you can use the existing jenga executable and
look at the documentation in `build.mli` for where concretely you write the rules.

If you want statically linked rules, you will need to start function yourself (so
you can give it the rules). The way to start jenga is `Run.main'`. You can reuse
the jenga command line interface `Cmd_build.config_param` to create the `Config.t`,
or you can provide your own settings if you prefer.

Talking to an existing jenga server
-----------------------------------

You can see all the rpcs the jenga server implements in `rpc_intf.ml`.
You can connect to a server using the code in `jenga_client.mli`. You can look
at the bottom of `cmd_errors.ml` for an example.
