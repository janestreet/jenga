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

