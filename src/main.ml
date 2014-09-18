
let () =
	let pkgs = Deb_packages.init () in
		List.iter (fun pkg ->
			print_endline (Deb_packages.package_name pkg)) pkgs
