# vim: ft=gap sts=2 et sw=2

InstallGlobalFunction(StabilizerChainContains, function (bsgs, g)
  EnsureBSGSChainComputed(bsgs);
  return StabilizerChainStrip(bsgs, g).residue = ();
end);

InstallGlobalFunction(StabilizerChainOrder, function (bsgs)
  local order, U;
  EnsureBSGSChainComputed(bsgs);
  return Product(List(bsgs.orbits, O -> O.size));
end);

