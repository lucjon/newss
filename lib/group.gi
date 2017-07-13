# vim: ft=gap sts=2 et sw=2

# StabilizerChainContains(bsgs, g)
# Returns true if the permutation g is in the group described by the BSGS
# structure bsgs, otherwise returns false.
InstallGlobalFunction(StabilizerChainContains, function (bsgs, g)
  if LargestMovedPoint(g) > LargestMovedPoint(bsgs.sgs) then
    return false;
  else
    return StabilizerChainStrip(bsgs, g).residue = ();
  fi;
end);

# StabilizerChainOrder(bsgs)
# Return the order of the group described by the given BSGS structure.
InstallGlobalFunction(StabilizerChainOrder, function (bsgs)
  local order, U;
  EnsureBSGSChainComputed(bsgs);
  return Product(bsgs.orbitsizes);
end);

