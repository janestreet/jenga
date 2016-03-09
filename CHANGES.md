## 113.33.00

- Jenga's `Api` is extended with `printf` and `printf_verbose`, and `verbose` is removed.
  These changes are in preparation for jenga/server.


  In jenga/server, builds will run on a daemonized server; the build trace is displayed on
  any clients which are `watch`ing. `Api.printf` and `Api.printf_verbose` allow messages
  from the `jenga/root.ml`.

  The command line flag `-verbose` will be a client flag, not a server flag, allowing the
  choice of verbosity to be selected per-client. It therefore won't make sense to allow
  `jenga/root.ml` to ask if `verbose` is true, so we remove `verbose` from the jenga `Api`.

  Instead `jenga/root/ml` can use `printf_verbose` for messages which are to be displayed
  only by a client that is watching with `-verbose`.


- Make benchmark analysis script compute heap stats in addition to time.

- Give more information about why we build `Dep.action` and `Dep.action_stdout`, because right
  now, the output of jenga sometimes doesn't contain the information (the summary line
  contains it, but it's not always printed, at least when using `-stop-on-error`). For
  instance:

  Old jenga:

      *** jenga: ERROR: (summary) a/.DEFAULT: External command failed
            - build a stdout
            + bash -e -u -o pipefail -c false
            - exit a stdout, 3ms, exited with code 1
      *** jenga: ERROR: (summary) a/foo.ml.d: External command failed
            - build a stdout
            + .../ocamldep.opt -modules -impl foo.ml
            foo.ml:
            File "foo.ml", line 2, characters 0-0:
            Error: Syntax error
            - exit a stdout, 4ms, exited with code 2


  New jenga:

      *** jenga: ERROR: (summary) a/.DEFAULT: External command failed
            - build a .DEFAULT
            + bash -e -u -o pipefail -c false
            - exit a .DEFAULT, 5ms, exited with code 1
      *** jenga: ERROR: (summary) a/foo.ml.d: External command failed
            - build a rule of foo.ml.d
            + .../ocamldep.opt -modules -impl foo.ml
            foo.ml:
            File "foo.ml", line 2, characters 0-0:
            Error: Syntax error
            - exit a rule of foo.ml.d, 6ms, exited with code 2

- When

  - a command run by jenga forks
  - and that fork inherits the stdout/stderr pipes to jenga
  - and the fork doesn't terminate
  - but the initial process does terminate

  we are in a situation where, jenga before this feature would consider the command is still
  running. This makes for some annoying debug, because which command started the stray
  process is hard to tell, the stdout and stderr of the initial process are not visible
  anywhere (they are in jenga's memory), etc.
  Also jenga appears stuck.

  So instead, we expect the stdout/stderr to close shortly after the initial process dies,
  and if not, we close stdout/stderr ourselves and consider that the command failed.


  To test, I put this in a jbuild:
      (alias
       ((name DEFAULT)
        (deps ())
        (action "(echo false >&2; sleep 4) & ")))

  and `JENGA_OPTIONS='((fd_close_timeout 2s))' jenga -P` complained:

      *** jenga: ERROR: (summary) lib/sexplib/src/.DEFAULT: External command failed
            - build lib/sexplib/src stdout
            + bash -e -u -o pipefail -c '(echo false >&2; sleep 4) & '
            false
            - exit lib/sexplib/src stdout, 2.004s, stdout or stderr wasn't closed 2s after process exited (due to a stray process perhaps?)

  Also I had hydra do a full tree build (both in jane-continuous-release and jane-release)
  and they both succeeded.

- Remove jenga command line flag which was `-delay N`

  This controlled `Config.delay_for_dev` which inserted a delay of N seconds before running
  any job in `Job.run`. I have heard this described as the most pointless command line flag
  of all time!

  Reason to fix now: Another feature `jane/jenga/api.printf` removes `Api.verbose` which
  exposes `Config.verbose` to jenga/root.ml. Following both features, we can remove the
  `Run.For_user` hackery.

- Avoid `nan` from divide-by-0 in jenga monitor's `finish_time_estimator`.

  Without this fix, we may get `nan` as the value of a `Time.t` which breaks an invariant of
  the `Time` module. This may result in a crash in `Time.to_ofday`.

      ("unhandled exception"
       ((monitor.ml.Error_
         ((exn "Option.value_exn None")
          (backtrace
           ("Raised by primitive operation at file \"option.ml\", line 59, characters 4-21"
            "Called from file \"zone.ml\", line 751, characters 8-104"
            "Called from file \"zone.ml\", line 761, characters 10-48"
            "Called from file \"time0.ml\", line 275, characters 31-54"
            "Called from file \"finish_time_estimator.ml\", line 38, characters 14-54"
            "Called from file \"message.ml\", line 384, characters 30-66"
            "Called from file \"list.ml\", line 73, characters 12-15"
            "Called from file \"list.ml\", line 73, characters 12-15"
            "Called from file \"message.ml\", line 158, characters 2-42"
            "Called from file \"deferred0.ml\", line 65, characters 64-69"
            "Called from file \"job_queue.ml\", line 160, characters 6-47" ""))
          (monitor
           (((name try_with) (here ()) (id 4) (has_seen_error true)
             (is_detached true))))))
        ((pid 17688) (thread_id 4

- Code refactoring to avoid circular module dependencies in `jenga/server` feature.

  There changes have no semantic effects; it's just code movement.

  1. Extract `Message.Job_summary` as a top level module.
  2. Extract `Build.Run_reason` as a top level module.
  3. Create all `Effort.Counter` instances in `Progress`.

- The current cycle detection in build.ml is very hard to use correctly.
  Implement a robust cycle detection in Tenacious.

  Additionally, this feature lets the user see what jenga is currently working on by doing `jenga diagnostics dump-tenacious-graph`.

  Along the way, fixed `jenga monitor` to exit non-zero when it can't discover jengaroot.

- Change `jenga -time` to prefix lines with absolute times. More standard & helpful.

- Extend jenga Api with support for registering environment variables.
  These can be accessed and manipulated via RPC to the running jenga.

  This feature is necessary to support env-var discovery in build-manager via RPC, rather
  than the current hack of parsing the output from jenga.

  Environment variable manipulation is exposed on the command line with:

      jenga env get NAME
      jenga env set NAME VALUE
      jenga env unset NAME
      jenga env print

  The Api supports access to either the current value of a variable, or to a dependency
  which responds dynamically to changes made by `setenv`.

      Var.peek   : 'a Var.t -> 'a
      Dep.getenv : 'a Var.t -> 'a Dep.t

- Removed `update-stream` RPC. Was never used by anything.
  It wouldn't have worked anyway because it contained interned paths.

  Removed `app/jenga/lib/rpc_intf.mli` - just unhelpful code duplication of the
  descriptions in `rpc_intf.ml`.

- Error message when running on nfs was super-confusing.

- Avoid double-reporting build errors originating from the action of a multi-target rule.
  (And in future avoid double-reporting the error to the `error-pipe' of jenga/server.)

  1. Attribute an error to the head target ONLY.

  2. Additional targets are treated as dependants of the `head_target`, and so (if
  demanded) are regarded as having `failure' not `error' status.

  3. Always use `head-target` in the "-build" line message, instead of whatever target
  happens (non deterministically) to get `demanded` first.

  4. Allow user control over the which target is regarded as the `head_target`, by taking
  the first target listed as the `head_target`.


- Couple of extra messages bracketing when GC is performed & finished.

- Stop taking nfs locks, just rely on local locks, so we're finally
  free from the bug of Nfs_lock where it can't remove a half created
  lock.
  Also clean things up while I'm here, no change intended in
  special_paths.ml.

- Refactor code to remove distinction between types `Progess.Need.t` and `Goal.t`; removing
  the `Progess.Need.t` type by supporting the one additional case of `Need.jengaroot` as a
  `Goal.t`.

  Allow the user to demand a `build' of (just) the jengaroot.

- Add support for unsetting environment variables prior to running actions

## 113.24.02

- Fix the build of jenga and updated the examples

## 113.24.00

- Restructure the code in a way that allows to build binaries that statically link jenga
  with the rules.
  This is useful because some debugging/profiling tools don't work in the presence of
  dynamically loaded code very well.

- Switch to PPX.

- Change the gc info output by jenga so it shows heap size and top heap size, instead of
  live and heap size. The live part is not super useful given how random it is. I have seen
  cases where jenga was using 20GB during building and jenga reported a heap size of 13GB at
  the end so the top heap size avoids being tricked.

- First half of the fixes no packing: sharing the structures of dependencies, so they take
  less space on disk (and in memory as well, when they are loaded from disk, but not really
  when building from scratch given the way we will use them).

  The sexp format also has sharing, because it would also blow up in size otherwise (this is
  different from the interning of paths, where the interning saves a constant factor).  And
  of course, it makes it possible to see the actual on-disk representation which is nice.

  Also fix unhelpful error (contains no information) when the db can't be loaded.

  Break the thing that avoids rerunning rules when the set of dependencies decreases. I
  think it was never useful anyway.

- Better error on duplicate targets in the same rule.

- To prevent running more than one jenga in a repository, use a local
  lock rather than an nfs one. We need a transition period though, so
  for now we use both kinds of locks.
  Building on nfs is slow, so I don't think there's any downside is not
  supporting nfs this way. And maybe inotify doesn't work. The upside is
  that we don't step into Lock.Nfs bugs where if a process is
  interrupted at the wrong time (when the two lock files are empty) the
  locks won't be cleaned up automatically, forcing someone to get rid of
  the lock files manually.

  Also rename `.jenga/.jenga.*` to `.jenga/*`, because all these prefixes
  are annoying.

- Added a couple of options to turn off some part of jenga, which I used
  to check how they impacted performance, and could still be handy
  later.

- Optionally display additional information about much allocation was done, at the end of
  builds. Used it to try to improve memory usage of full tree builds without actually
  doing full tree builds.

- Make stat'ing faster.
  Hash cons some tenacious that build mtimes map to avoid a huge
  increase of memory use.

- Got rid of noise when stopping jenga.

- Some changes to the implementation of Tenacious to improve memory efficiency.

  Includes the following changes:

  - Remove `strong_refs` field of `Heart.fragile` type and instead insert links from
    the `clients` `Ring.t` to its parent using `Ring.keep_alive`.

  - Replace the `Ring.t` in the `triggers` field of `Hearts.fragile` with an
    `Ivar.t` since all uses of `triggers` were producing their own equivalent
    `IVar.t`s. Remove the functions broken by this because they were unused.

  - Make the `Tenacious.t` type a concrete datatype, and optimize pure
    computations by partially evaluating this datatype directly in the pure case.

  - Rather than building separate `Heart.t`s for cancellation and the result,
    split the cancellation heart into two hearts `cancel` and `dep` and then use
    `dep` as the result heart. This means that a tenacious is cancelled if either
    `cancel` or `dep` is broken, and it must return a heart representing the
    validity of its result combined with `dep`.

  Cursory benchmarking indicates a 23% improvement in maximum resident set size and a 10% improvment in (user) execution
  time when building the lib directory from scratch.

- Adding some `sexp_of` functions, since they're always missing and
  it's a pain when debugging.

- Adding direct support for `Dep.map`.
  Even now that `Tenacious` is smarter, this still creates less `bind`s. Doesn't seem to make
  much of a difference (perhaps 3-5% less allocation, on a null build of lib), but if
  nothing else, it's much less surprising to think that `Dep.Map` becomes `Tenacious.Map`.

- Added Dep.List.concat.

- Make it possible to turn off the behavior where jenga rejects commands that output on
  stderr.

  It increases slightly the footprint of the in memory db, but the difference is tiny
  compared to the rest of the memory usage.

- Added a few tests about `Jenga_lib.Api.Reflect`

## 113.00.00

- Treat output to stderr by an action as a failure.

## 112.35.00

- Make `.jenga.db` be format version aware, and store `.jenga` files in
  a subdirectory.
- Switch md5 computation to use a C binding, fixes #10.
- Move `jem.exe` to `jenga.exe monitor`, `jenga_offline.exe` to
  `jenga.exe offline`, `jenga.exe -cat-api` to `jenga.exe cat-api`.
- Remove `Path.dotdot`.
- Adjust behaviour of `Path.Repo.is_descendant` to be more consistent.
- Repo-root invariance: remove `Path.Abs.the_root`,
  `Path.Rel.to_absolute_string`, `Path.Rel.create_from_absolute`, and
  make `Path.relative Path.the_root ".."` fail.
- Add a flag `-sandbox-action` to `jenga build` to run the action in an
  environment that (attempts to) detect missing dependencies or overly
  large sets of targets.
- Don't interpret paths of shape `./foo.ext` as aliases.
- Always interpret `Dep.path path` as the file at `path` rather than the
  default alias for the directory at `path`
- Make jenga look for the variable JENGA_OPTIONS, and use it to add
  a debug setting.
- Fix handling of directories created during the build.
- Basic support for symlink resolution.
- Add an option to the monitor command to only display a single snapshot
  of the progress, rather than a continually updated pipe.
- Fix a deadlock where File_access throttle and directory lock are
  obtained in the wrong order.

## 112.24.00

- Interns strings, significantly reducing memory use.

## 112.17.00

- Fixed byte-compile targets to avoid stale artifact deletion of
  `.for-byte-compile.cmt` file when compilation fails.

  This avoids polling jenga trigger loop.
- Show `Removed stale build artifact` messages only when `-act` flag is given.
- Extended Jenga API with `val file_existence : Path.t -> unit t`.

  `file-existence` has same relationship to `file-exists` as
  `glob_change` has to `glob_listing`.
- Fixed memory leak in tenacious hearts by using weak references and
  finalizers.

  * Strip code for obsolete versions of hearts.
  * Strip code for `OLD_TENACIOUS=true`.
  * Ensure `Ring` support preemptive calls to `detach`.  Add ring tests.
  * Keep message showing `Live(Kb-delta)`.
- Fixed curly braces in globs.
- Throttled calls to `Writer.save`, to fix the `too many open files`
  bug.

  Throttled calls to `Writer.save` from `save_description.run` sharing
  the same throttle used for all FD access, with
  `~max_concurrent_jobs:500`
- Added to `jenga -progress` `~save=XXX`, the number of calls to
  `Writer.save` in `Progress.saves_run`.

## 112.06.00

- Support for user control of stale-artifact deletion, by allowing
  specification of an artifact-determination policy.
- Expose jenga's internal (and better - only quotes when necessary)
  definition of `Shell.escape` in `Api`
- Removed `Action.shell` from the API, superseded by `Action.process`.
- Changed RPC interface as needed for build manager to switch from
  scraping error messages to RPCs.
- Fixed jenga's per-rule memo table, which mistakenly kept stale values.
- Show what target is being demanded, useful for debugging rules.
- Run user action when persistent format changes.
- When filtering buildable targets by globs, pay attention to the kinds
  allowed by the glob.

    Specifically, if the kinds don't include `` `File `` (i.e. only
    include `` `Directory ``) then we should not see any
    `buildable_targets` in the filtered list.

## 112.01.00

- Don't show noisy `glob..changed` messages except with `-show-glob-changed` flag.
- Support shared build rules via `${jenga}/share`.
- Detect cycle in dep scheme instead of hanging.
- Made standalone actions atomic, just like actions associated with
  target files.

  Running actions and recording the result in the persistent
  `.jenga.db` should be performed atomically for standalone actions,
  as it is for actions which are associated with target files

## 111.31.00

- Switched API to composable generator schemes.
- Support `-api` flag to show the embedded API.
- New examples.

## 111.28.00

- Fixed problem that caused `rule failed to generate targets`.

## 111.25.00

- Switched to un-version-numbered API.
- Renamed `Tenacious_sample_lib.Tenacious` to
  `Tenacious_sample_lib.Tenacious_sample` to avoid conflicts in the
  public release.
- Write `buildable_targets.list` (on alias `.info`).

## 111.21.00

- Introduced jenga API v3, a small cleanup of v2 which has been planned
  for a while.

## 111.08.00

- Fix a hang.

    Jenga could reach a state with a non-zero todo-count, but have no
    jobs actually running, and then hang in this state forever. The hang
    would be evident from a progress line with not all targets built and
    with `j=0+0` such as:

        todo: 17 (100406 / 100423) j=0+0 con=149956 act=3303, finish at: 16:20

## 111.06.00

- Improved the error message when the same library is defined multiple
  times.
- Fixed an issue where jenga sometimes would sometimes complain about
  a self cycle when `foo.ml` uses a module `Foo`.
- With `-no-notifiers`, jenga doesn't use `inotify` to watch for file
  changes.  This is useful for linting `jengaroot.ml`.
- Allowed writing jenga rules which restrict dependencies from an
  initial conservative approximation to a more accurate set discovered
  after an action is run

