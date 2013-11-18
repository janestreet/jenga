
type t =
| Checking
| Jwait
| Running
| Usercode
| Built of What.t
| Error of Reason.t
