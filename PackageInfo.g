SetPackageInfo( rec(
	PackageName := "newss",
	Version := "0.1",
	PackageDoc := rec(
		BookName := "newss",
		SixFile := "doc/manual.six",
		Autoload := true),
	Dependencies := rec(
		GAP := "4.5",
		NeededOtherPackages := [ ["GAPDoc", "1.3"] ],
		SuggestedOtherPackages := [] ),
	AvailabilityTest := ReturnTrue));
