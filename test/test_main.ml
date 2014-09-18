
let dump_packages () =
	let print_pkg_info pkg =
		print_endline (Deb_packages.package_name pkg)
	in
		Deb_packages.init () |>
		List.iter print_pkg_info

let () =
	try dump_packages ()
	with 
	| Deb_packages.Missing_field (pkg, field) as e ->
		Printf.printf "Missing field: pkg: %s, field name: %s\n" pkg field;
		raise e
	| Deb_packages.Invalid_field (pkg, field) as e ->		
		Printf.printf "Invalid field: pkg: %s, field name: %s\n" pkg field;
		raise e
