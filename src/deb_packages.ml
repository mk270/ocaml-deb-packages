
let status_filename = "/var/lib/dpkg/status"

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

type debpkg = {
	name : string;
	status : status;
	attributes : (string * string) list;

}

type t = debpkg list

let selection_state_of_string = function
	| "install" -> Install
	| "deinstall" -> Deinstall
	| "hold" -> Hold
	| "purge" -> Purge
	| s -> failwith ("Unknown selection state: " ^ s)

let string_of_selection_state = function
	| Install -> "install"
	| Deinstall -> "deinstall"
	| Hold -> "hold"
	| Purge -> "purge"

let package_state_of_string = function
	| "not-installed" -> Not_installed
	| "config-files" -> Config_files
	| "half-installed" -> Half_installed
	| "unpacked" -> Unpacked
	| "half-configured" -> Half_configured
	| "triggers-awaited" -> Triggers_awaited
	| "triggers-pending" -> Triggers_pending
	| "installed" -> Installed
	| s -> failwith ("Unknown package state: " ^ s)

let string_of_package_state = function
	| Not_installed -> "not-installed"
	| Config_files -> "config-files"
	| Half_installed -> "half-installed"
	| Unpacked -> "unpacked"
	| Half_configured -> "half-configured"
	| Triggers_awaited -> "triggers-awaited"
	| Triggers_pending -> "triggers-pending"
	| Installed -> "installed"

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
			
exception Missing_field of (string * string)
exception Invalid_field of (string * string)

let parse_status s =
	let rx = Str.regexp " " in
	let words = Str.bounded_split rx s 3 in
		match words with
		| selection_state :: "ok" :: package_state :: [] ->
			(selection_state_of_string selection_state,
			 package_state_of_string package_state)
		| _ -> assert false

let make_package pkg_kvps =
	let name = List.assoc "Package" pkg_kvps in

	let permitted_fields = [
		"Provides"; "Original-Maintainer"; "Depends"; "Installed-Size";
		"Maintainer"; "Version"; "Description"; "Homepage";
		"Source"; "Config-Version"; "Multi-Arch"; "Pre-Depends";
		"Replaces"; "Breaks"; "Suggests"; "Conflicts"; "Python-Version";
		"Conffiles"; "Recommends"; "Essential"; "Ruby-Versions";
		"Gstreamer-Decoders"; "Gstreamer-Elements"; "Gstreamer-Version";
		"Gstreamer-Encoders"; "Gstreamer-Uri-Sinks"; "Gstreamer-Uri-Sources";
		"Enhances"; "Built-Using"; "Origin"; "Bugs";
		"Orig-Maintainer"; "Npp-Applications"; "Npp-Description";
		"Npp-File"; "Npp-Mimetype"; "Npp-Name"; "Xul-Appid";
	] and required_fields = [
		"Package"; "Status"; "Priority"; "Section";  
		"Architecture"; ] in
	let all_fields = permitted_fields @ required_fields in
	let used_fields = List.map fst pkg_kvps in
		List.iter (fun i -> 
			if not (List.mem_assoc i pkg_kvps)
			then raise (Missing_field (name, i))
			else ())  required_fields;

		List.iter (fun i ->
			if not (List.mem i all_fields)
			then raise (Invalid_field (name, i))
			else ()) used_fields;

	let status_codes = List.assoc "Status" pkg_kvps in
	let status = parse_status status_codes in
		{ name = name;
		  status = status;
		  attributes = pkg_kvps }
			
let init () =
	let data = load_file status_filename in
		pkginfo_indices data |>
		pkgstrings data |>
		List.map lines_of_pkginfo |>
		List.map kvps_of_pkglines |>
		List.map make_package
										
let package_name p = p.name
let package_status p = p.status
