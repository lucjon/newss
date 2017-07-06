# vim: ft=gap sts=2 et sw=2
Read("orbstab.g");
Read("ss.g");

# VerifySCContains(G, H)
# Given a subgroup H of G, check that every element of G is correctly
# determined to be inside or outside H by the StabilizerChainContains function.
VerifySCContains := function (G, H)
  local H_sc, actually_in_H, think_in_H;
  H_sc := BSGSFromGAP(H);

  for x in Elements(G) do
    actually_in_H := x in H;
    think_in_H := StabilizerChainContains(H_sc, x);
    if actually_in_H <> think_in_H then
      Print(x, " in H = ", actually_in_H, " but we thought ", think_in_H, ".\n");
      return false;
    fi;
  od;

  return true;
end;

VerifyContainsPG := function (G)
  local deg;
  deg := LargestMovedPoint(GeneratorsOfGroup(G));
  return VerifySCContains(SymmetricGroup(deg), G);
end;


DoTest := function (name, fn, arg)
  Print(name, ": ");
  if fn(arg) then
    Print("ok.\n");
  else
    Print("fail.\n");
  fi;
end;

# These functions basically test the StabilizerChainStrip function (ss.g).
TestsContains := function ()
  DoTest("A_4", VerifyContainsPG, AlternatingGroup(4));
  DoTest("HCGT ex 4.1", VerifyContainsPG, Group([(1,3,7)(2,5), (3,4,6,7)]));
end;
