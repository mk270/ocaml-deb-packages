
let status_filename = "/var/lib/dpkg/status"

type debpkg = {
	name : string;
	attributes : (string * string) list;
}

type t = debpkg list

let load_file f =
	let ic = open_in f in
	let n = in_channel_length ic in
	let s = String.create n in
		really_input ic s 0 n;
		close_in ic;
		s
			
let pkginfo_indices data =
	let rx = Str.regexp "^Package: " in
	let rec pkginfo_indices offset acc =
		try 
			let idx = Str.search_forward rx data offset in
				pkginfo_indices (idx + 1) (idx :: acc)
		with Not_found -> acc
	in
		pkginfo_indices 0 []
			
let pkgstrings data idxs =
	let last = String.length data in
	let rec pkgstrings offset acc = function
		| [] -> acc
		| hd :: tl ->
			let len = offset - hd in
			let s = String.sub data hd len in
				pkgstrings hd (s :: acc) tl
	in
		pkgstrings last [] idxs 
			
let dump_int = (fun i -> print_int i; print_newline ())
	
let lines_of_pkginfo pkginfo =
	let rx = Str.regexp "\n" in
	let ss = Str.split rx pkginfo in
	let starts_with_space s = 
		if not (s = "") 
		then s.[0] = ' ' 
		else false 
	in
	let raw_lines = List.map 
		(fun line -> (starts_with_space line, line)) ss
	in
	let rec reorg_lines acc = function
		| [] -> acc
		| (false, s) :: tl -> reorg_lines (s :: acc) tl
		| (true, s) :: tl -> 
			match acc with
			| [] -> assert false
			| prev :: rest -> reorg_lines ((prev ^ s) :: rest) tl
	in
		reorg_lines [] raw_lines
			
let kvps_of_pkglines pkglines =
	let rx = Str.regexp ": " in
	let kvp line =
		match Str.bounded_split rx line 2 with
		| left :: right :: [] -> Some (left, right)
		| [] -> None
		| _ -> assert false
	in
	let rec get_relevant acc = function
		| Some kvp :: tl -> get_relevant (kvp :: acc) tl
		| None :: tl -> get_relevant acc tl
		| [] -> acc
	in
		get_relevant [] (List.map kvp pkglines)
			
let make_package pkg_kvps =
	let name = List.assoc "Package" pkg_kvps in
		{ name = name;
		  attributes = pkg_kvps }
			
let init () =
	let data = load_file status_filename in
		pkginfo_indices data |>
		pkgstrings data |>
		List.map lines_of_pkginfo |>
		List.map kvps_of_pkglines |>
		List.map make_package
										
let package_name p = p.name
