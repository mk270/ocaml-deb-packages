
type debpkg
type t = debpkg list

exception Missing_field of (string * string)
exception Invalid_field of (string * string)

val package_name : debpkg -> string
val init : unit -> t
