
let dump_packages () =
	let print_pkg_info pkg =
		let name = Deb_packages.package_name pkg 
		and selection_state, package_state = Deb_packages.package_status pkg
		in 
		let ss = Deb_packages.string_of_selection_state selection_state
		and ps = Deb_packages.string_of_package_state package_state
		in
			Printf.printf "%s %s %s\n" name ss ps
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
