# vim: ft=gap sts=2 et sw=2

NEWSS_TryModule := function (name)
  local fn;
  fn := Filename(DirectoriesPackageLibrary("newss"), Concatenation("../src/", name, ".so"));
  if fn = fail then
    ReadPackage("newss", Concatenation("lib", name, ".gi"));
  else
    BindGlobal(Concatenation("NEWSS_C_", name), true);
    LoadDynamicModule(fn);
  fi;
end;

ReadPackage("newss", "lib/ss.gi");
ReadPackage("newss", "lib/group.gi");
ReadPackage("newss", "lib/permword.gi");
ReadPackage("newss", "lib/stabchain.gi");
ReadPackage("newss", "lib/basetree.gi");
ReadPackage("newss", "lib/orbstab.gi");
NEWSS_TryModule("inner");

rlnewss := function ()
  RereadPackage("newss", "lib/ss.gd");
  RereadPackage("newss", "lib/ss.gi");
  RereadPackage("newss", "lib/orbstab.gd");
  RereadPackage("newss", "lib/orbstab.gi");
  RereadPackage("newss", "lib/group.gd");
  RereadPackage("newss", "lib/group.gi");
  RereadPackage("newss", "lib/permword.gd");
  RereadPackage("newss", "lib/permword.gi");
  RereadPackage("newss", "lib/stabchain.gd");
  RereadPackage("newss", "lib/stabchain.gi");
  RereadPackage("newss", "lib/basetree.gi");
  RereadPackage("newss", "lib/basetree.gi");
end;
