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

