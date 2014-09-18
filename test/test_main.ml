
let dump_packages () =
	let print_pkg_info pkg =
		print_endline (Deb_packages.package_name pkg)
	in
	let pkgs = Deb_packages.init () in
		List.iter print_pkg_info pkgs

let () =
	try dump_packages ()
	with 
	| Deb_packages.Missing_field (pkg, field) as e ->
		Printf.printf "Missing field: pkg: %s, field name: %s\n" pkg field;
		raise e
	| Deb_packages.Invalid_field (pkg, field) as e ->		
		Printf.printf "Invalid field: pkg: %s, field name: %s\n" pkg field;
		raise e
