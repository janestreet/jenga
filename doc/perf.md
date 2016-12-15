This document contains our ideas about performance problems and
speeding up jenga.

Short term todos
----------------

(A section to list some proposals of things to focus on now.)

Excessive memory usage
----------------------

The problem: jenga takes a lot of memory, which makes it hard for
people to do full tree builds of jane. If it gets out of hand (jane is
not likely to shrink), it's a big problem. Finally, high memory usage
here probably comes with high memory allocation and repeated gc
cycles, which slows down the build.

Several difficulties with this problem are:

- there is no single big memory user, it seems to be spread out. So
  there is no good metric that we can focus on and optimize.
- profiling information is hard to interpret
- no good intuition

Several potential ways of trying to go forward are:

- give a shot at suspected problems
  - lack of sharing in the jengaroot
  - too much tenacious in fs.ml
- play with gc
  - reduce the gc overhead setting
  - force compactions/full majors
  - make jenga dynamically adjust gc setting when it's starting to use too
    much memory
- try to get a clue about what's going on
  - isolate high level systems, see what their impact is
    - fs: dummy implementation with no inotify, no tenacious
    - what about an implementation of tenacious that doesn't support
      retriggering, for comparison?
    - jengaroot: maybe we can precompute all the deps, marshal them to
      disk and split the result back out?
  - see how memory usage scales up with tree size, perhaps by deleting
    chunks of the tree. If it scales up in a superlinear way, maybe we
    should focus on complexity rather that constants. 
  - if something in particular seems to make performance hard to
    understand, perhaps we can get rid of it, and then think about the
    simpler code.

Insert more details about what we currently do know.

Blaming for build time
----------------------

Related to jenga benchmarks. We don't have a clear idea of:

- what part of the build time is due to jenga (my estimate: a null
  build is ~30% of a build from scratch, so that's an upper bound).

- some heuristics to assign blame to the rest of the actions (for
  instance, a small jane-script that looks at .jenga/debug). Splitting
  off preprocessors from the compiler would be good.

- among these commands, assuming most of that is ocaml, we want to
  know where in the ocaml compiler.

Typing-only builds
------------------

People making interface changes to core libraries usually want to
compile the whole tree, but just type checking would be enough the
vast majority of the time, and would be faster, possibly take less
disk space depending on how it's done.  One possible implementation is
using bytecode. An other one is splitting the compiler after typing
but before the native/bytecode, so we can continue the native code
compilation from there (side benefit: would get rid the weirdness with
ml without mli, since they would be built in a backend-agnostic way
with this).

Sharing across repositores
--------------------------

What the name says: we could share build artifacts across repositories
(simple version is only from the current machine and user, the
ambitious version is across machines and developers). What we get is
that people wouldn't need to do most compilation (which in the best
(and expected) case brings the build time to about the time of a null
build). We could perhaps save disk space too, with the current
workspace workflow.

The simplest possible version would have a mapping from digest of
(names of targets, (name and digest of dependencies, action, directory
of the action)) to the actual (read-only) target somewhere on
disk. The jenga needs to consult (and hardlink targets on hits), and
fill the cache. And finally the jengaroot needs to avoid being tripped
by these read only files.  And then of course, there's the question of
when to remove entries from the cache.

Another version of this might be to save snapshots that are done at
particular Jane versions, and picking all of the artifacts from the
most recent version for which there's a cached set of build artifacts.

Speeding up the compiler itself
-------------------------------

Depending on what we learn about benchmarking the compiler on our
tree, we can imagine:

- having compiler servers, to avoid reading/refreshing cmis
  repeatedly. I think this can be done in a way that's almost a
  drop-in replacement for the ocaml compiler.
- perhaps even going further, making the compiler itself be
  incremental (rather than just file loading). Probably a last resort
  solution.
