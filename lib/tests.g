# vim: ft=gap sts=2 et sw=2
## tests.g
## Various tests for the stabiliser chain algorithms in the package.
## To run them all, read this file and execute DoAllTests().

# The tests themselves

# VerifySCOrder(G)
# Check the order we compute for a group versus that computed by GAP.
VerifySCOrder := function(G)
  return Order(G.group) = StabilizerChainOrder(G);
end;

# VerifySCContains(G, H)
# Given a group H which is a subset of G, check that every element of G is
# correctly determined to be inside or outside H by the StabilizerChainContains
# function.
VerifySCContains := function (G, H_sc)
  local H, actually_in_H, think_in_H, x;
  H := H_sc.group;

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

# NUM_RANDOM_TEST_ELTS
# An integer specifying how many random elements to pick from a large ambient
# group to be tested with VerifyContainsPG.
NUM_RANDOM_TEST_ELTS := 2^16;

# VerifyContainsPG(G)
# Run VerifySCContains on lots of elements inside and outside a given group G.
VerifyContainsPG := function (G_sc)
  local deg, Sn, X, G;
  G := G_sc.group;
  deg := LargestMovedPoint(GeneratorsOfGroup(G));
  Sn := SymmetricGroup(deg);

  # For groups sitting in big S_n, it is not feasible to check all the points.
  # So pick lots of random ones instead.
  if deg <= 9 then
    return VerifySCContains(Sn, G_sc);
  else
    X := List([1 .. NUM_RANDOM_TEST_ELTS], i -> PseudoRandom(Sn));
    return VerifySCContains(X, G_sc);
  fi;
end;


# The groups to test.
GROUPS := [
  ["A_4", AlternatingGroup(4)],
  ["HCGT ex 4.1", Group([(1,3,7)(2,5), (3,4,6,7)])],
  ["Mathieu deg. 9", MathieuGroup(9)],
  ["PrimitiveGroup(1024, 2)", PrimitiveGroup(1024, 2)],
  ["Suzuki", AtlasGroup("Suz")]
];

PickSomeGroups := function (nr)
  local bounds, counts, sources, count, bound, index, group, i, j;
  bounds := [50, 30];
  counts := [NrPrimitiveGroups, NrTransitiveGroups];
  sources := [PrimitiveGroup, TransitiveGroup];
  i := 1;

  while i <= nr do
    j := PseudoRandom([1 .. Size(sources)]);
    bound := PseudoRandom([1 .. bounds[j]]);
    count := counts[j](bound);
    if count > 0 then
      index := PseudoRandom([1 .. count]);
      group := sources[j](bound, index);
      Add(GROUPS, [Concatenation(NameFunction(sources[j]), "(", String(bound),
                                 ", ", String(index), ")"),
                   group]);
      i := i + 1;
    fi;
  od;
end;


# The test harness functions.
DoTest := function (name, fn, arg)
  Print(name, ": ");
  if fn(arg) then
    Print("ok.\n");
  else
    Print("fail.\n");
  fi;
end;

DoTests := function (tests, constructor)
  local test, group, add, stabchains, i;

  Print("Computing stabilizer chains [", NameFunction(constructor), "]:\n");
  add := function (L, x) Add(L, x); return true; end;
  stabchains := [];
  for group in GROUPS do
    Print(group[1]);
    Add(stabchains, constructor(group[2]));
    Print("\n");
  od;
  Print("\n");

  for test in tests do
    Print(NameFunction(test), ":\n");
    for i in [1 .. Size(GROUPS)] do
      DoTest(GROUPS[i][1], test, stabchains[i]);
    od;
    Print("\n");
  od;
end;

TESTS := [VerifyContainsPG, VerifySCOrder];
DoAllTests := function ()
  DoTests(TESTS, BSGSFromGroup);
end;
