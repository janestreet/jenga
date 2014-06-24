include Inotify_lib.Inotify
type watch = wd
let int_of_watch = int_of_wd
let string_of_event_kind = string_of_event
let create = init
