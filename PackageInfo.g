SetPackageInfo( rec(
	PackageName := "newss",
	Subtitle := "A new Schreier-Sims implementation for GAP.",
	Date := "17/07/2017",
	Persons := [
		rec(
			LastName := "Jones",
			FirstNames := "Lucas",
			IsAuthor := true,
			IsMaintainer := true,
			Email := "ljnj@st-andrews.ac.uk")],
	Version := "0.1",
	PackageDoc := rec(
		BookName := "newss",
		ArchiveURLSubset := ["doc"],
		HTMLStart := "doc/chap0.html",
		PDFFile := "doc/manual.pdf",
		SixFile := "doc/manual.six",
		Autoload := true,
		LongTitle := ~.Subtitle),
	Dependencies := rec(
		GAP := "4.5",
		NeededOtherPackages := [ ["GAPDoc", ">=1.3"] ],
		SuggestedOtherPackages := [ ["orb", ">=4.7.6"], ["io", ">=4.4.6"] ] ),
	AvailabilityTest := ReturnTrue));
