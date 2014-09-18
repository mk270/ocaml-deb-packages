
type selection_state =
	| Install
	| Hold
	| Deinstall
	| Purge

type debpkg
type t = debpkg list

exception Missing_field of (string * string)
exception Invalid_field of (string * string)

val package_name : debpkg -> string
val package_status : debpkg -> string
val init : unit -> t
