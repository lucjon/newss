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

InstallGlobalFunction(StabilizerChainStabilizer, function (bsgs, point)
  local fix_bsgs;
  fix_bsgs := BSGSWithBasePrefix(bsgs, [point]);
  if Size(fix_bsgs!.chain) >= 2 then
    return fix_bsgs!.chain[2].group;
  else
    return Group(());
  fi;
end);
