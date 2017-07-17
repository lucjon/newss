# vim: ft=gap sts=2 et sw=2

InstallGlobalFunction(StabilizerChainContains, function (bsgs, g)
  if LargestMovedPoint(g) > LargestMovedPoint(bsgs.sgs) then
    return false;
  else
    return StabilizerChainStrip(bsgs, g).residue = ();
  fi;
end);

InstallGlobalFunction(StabilizerChainOrder, function (bsgs)
  local order, U;
  EnsureBSGSChainComputed(bsgs);
  return Product(bsgs.orbitsizes);
end);

