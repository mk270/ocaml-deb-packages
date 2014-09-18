
let () =
	try
		let pkgs = Deb_packages.init () in
			List.iter (fun pkg ->
				print_endline (Deb_packages.package_name pkg)) pkgs
	with 
	| Deb_packages.Missing_field (pkg, field) as e ->
		Printf.printf "Missing field: pkg: %s, field name: %s\n" pkg field;
		raise e
	| Deb_packages.Invalid_field (pkg, field) as e ->		
		Printf.printf "Invalid field: pkg: %s, field name: %s\n" pkg field;
		raise e
