# vim: ft=gap sts=2 et sw=2

ReadPackage("newss", "lib/ss.gi");
ReadPackage("newss", "lib/orbstab.gi");
ReadPackage("newss", "lib/group.gi");

rlnewss := function ()
  RereadPackage("newss", "lib/ss.gd");
  RereadPackage("newss", "lib/ss.gi");
  RereadPackage("newss", "lib/orbstab.gd");
  RereadPackage("newss", "lib/orbstab.gi");
  RereadPackage("newss", "lib/group.gd");
  RereadPackage("newss", "lib/group.gi");
end;
