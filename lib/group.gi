# vim: ft=gap sts=2 et sw=2

InstallGlobalFunction(StabilizerChainContains, function (bsgs, g)
  EnsureBSGSChainComputed(bsgs);
  return StabilizerChainStrip(bsgs, g).residue = ();
end);

InstallGlobalFunction(StabilizerChainOrderNC, function (bsgs)
  return Product(List(bsgs!.chain, c -> c.orbit.size));
end);

InstallGlobalFunction(StabilizerChainOrder, function (bsgs)
  EnsureBSGSChainComputed(bsgs);
  return StabilizerChainOrderNC(bsgs);
end);

