# vim: ft=gap sts=2 et sw=2

InstallGlobalFunction(StabilizerChainContains, function (bsgs, g)
  EnsureBSGSChainComputed(bsgs);
  return StabilizerChainStrip(bsgs, g).residue = ();
end);

InstallGlobalFunction(StabilizerChainOrder, function (bsgs)
  local order, U;
  EnsureBSGSChainComputed(bsgs);
  return Product(bsgs.orbitsizes);
end);

