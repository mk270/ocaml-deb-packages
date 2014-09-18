
type debpkg
type t = debpkg list

val package_name : debpkg -> string
val init : unit -> t
