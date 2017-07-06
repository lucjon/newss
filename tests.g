# vim: ft=gap sts=2 et sw=2
Read("orbstab.g");
Read("ss.g");

# VerifySCContains(G, H)
# Given a group H which is a subset of G, check that every element of G is
# correctly determined to be inside or outside H by the StabilizerChainContains
# function.
VerifySCContains := function (G, H)
  local H_sc, actually_in_H, think_in_H;
  H_sc := BSGSFromGAP(H);

  for x in G do
    actually_in_H := x in H;
    think_in_H := StabilizerChainContains(H_sc, x);
    if actually_in_H <> think_in_H then
      Print(x, " in H = ", actually_in_H, " but we thought ", think_in_H, ".\n");
      return false;
    fi;
  od;

  return true;
end;

NUM_RANDOM_TEST_ELTS := 2^16;
VerifyContainsPG := function (G)
  local deg, Sn, X;
  deg := LargestMovedPoint(GeneratorsOfGroup(G));
  Sn := SymmetricGroup(deg);

  # For groups sitting in big S_n, it is not feasible to check all the points.
  # So pick lots of random ones instead.
  if deg <= 9 then
    return VerifySCContains(Sn, G);
  else
    X := List([1 .. NUM_RANDOM_TEST_ELTS], i -> PseudoRandom(Sn));
    return VerifySCContains(X, G);
  fi;
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
  DoTest("Mathieu deg. 9", VerifyContainsPG, MathieuGroup(9));
  DoTest("PrimitiveGroup(1024,2)", VerifyContainsPG, PrimitiveGroup(1024, 2));
end;
