
type selection_state =
	| Install
	| Hold
	| Deinstall
	| Purge

type package_state =
	| Not_installed
	| Config_files
	| Half_installed
	| Unpacked
	| Half_configured
	| Triggers_awaited
	| Triggers_pending
	| Installed

type status = selection_state * package_state

type debpkg
type t = debpkg list

exception Missing_field of (string * string)
exception Invalid_field of (string * string)

val package_name : debpkg -> string
val package_status : debpkg -> status
val init : unit -> t
val string_of_selection_state : selection_state -> string
val string_of_package_state : package_state -> string
