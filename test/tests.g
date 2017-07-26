# vim: ft=gap sts=2 et sw=2

###
### Group selection
###
## A 'group source' is a function (degree?) which tries to return a group of
## the given degree --- or a random possible degree if not applicable --- 
## or fail if it cannot.

## 
## Some helper functions for handling group names
##

# PickName(G)
# Try and pick a nice Name for G if it doesn't have one.
PickName := function (G)
  if HasName(G) then
    return Name(G);
  else
    return Concatenation("<degree ", String(NrMovedPoints(G)), ">");
  fi;
end;

# GroupWithName(G, name)
# Give the group <C>G</C> the name <C>name</C> if it doesn't already have one,
# returning <C>G</G>.
GroupWithName := function (G, name)
  if not HasName(G) then
    SetName(G, name);
  fi;
  return G;
end;

# GroupSourceFromLibrary(library, library_count, max_degree)
# Return a group source from one of GAP's built in libraries, i.e. two
# functions Library(degree, index) and NrLibrary(degree).
GroupSourceFromLibrary := function (library, library_count, max_degree)
  return function (degree)
    local count, index, group;
    if degree = false then
      degree := PseudoRandom([1 .. max_degree]);
    fi;

    count := library_count(degree);
    if count = 0 then
      return fail;
    fi;

    index := PseudoRandom([1 .. count]);
    group := library(degree, index);

    if not HasName(group) then
      SetName(group, Concatenation(NameFunction(library), "(", String(degree), ", ", String(index), ")"));
    fi;

    return group;
  end;
end;

DEFAULT_ATLAS_LIST := "test/atlasnames.txt";

# LoadATLASGroupSource([list_filename])
# Return a group source picking random ATLAS groups from the list of ATLAS
# identifiers in the given text file, with each identifier on a line.
LoadATLASGroupSource := function (arg)
  local filename, handle, group_names, str;

  if Size(arg) = 0 then
    filename := DEFAULT_ATLAS_LIST;
  else
    filename := arg[1];
  fi;

  handle := InputTextFile(filename);
  if handle = fail then
    return fail;
  fi;
  group_names := [];

  str := Chomp(ReadLine(handle));
  while str <> fail and str <> "" do
    Add(group_names, str);
    str := Chomp(ReadLine(handle));
  od;
  CloseStream(handle);

  return function (degree)
    # We ignore the degree parameter for this source.
    local name;
    name := PseudoRandom(group_names);
    return GroupWithName(AtlasGroup(name), name);
  end;
end;

# ListGroupSource(list)
# Return a group source which picks from the given list.
ListGroupSource := function (list)
  return function (degree)
    local G;
    G := First(list, G -> NrMovedPoints(G) = degree);
    if G = fail then
      G :=  PseudoRandom(list);
    fi;
    return G;
  end;
end;


## With these fundamental group sources, we can build new group sources from
## them.

# PickManyGroups(source, degree, n)
# Pick n groups from a group source, without any fails.
GROUP_SOURCE_MAX_ATTEMPTS := 100;
PickManyGroups := function (source, degree, n)
  local groups, G, i;

  groups := [];
  i := 1;

  while Size(groups) < n and i < n * GROUP_SOURCE_MAX_ATTEMPTS do
    G := fail;
    while G = fail and i < n * GROUP_SOURCE_MAX_ATTEMPTS do
      G := source(degree);
      i := i + 1;
    od;
    Add(groups, G);
  od;

  if Size(groups) <> n then
    return fail;
  else
    return groups;
  fi;
end;

# PickOneGroup(source, degree)
# Like PickManyGroups, but return a single group and not a list.
PickOneGroup := function (source, degree)
  local groups;
  groups := PickManyGroups(source, degree, 1);
  if groups = fail then
    return fail;
  else
    return groups[1];
  fi;
end;


# DirectProductSource(source)
# Returns a group source which returns direct products of two groups from
# <C>source</C>.
DirectProductSource := function (source)
  return function (degree)
    local A, B, G;
    A := source(degree);
    B := source(degree);

    if A = fail or B = fail then
      return fail;
    fi;

    G := DirectProduct(A, B);
    SetName(G, Concatenation(PickName(A), " x ", PickName(B)));
    return G;
  end;
end;

# UnionGroupSource(source1[, source2[, ..., sourcen]])
# Returns a group source which returns a group from a random one of
# <C>source1</C>, ..., <C>sourcen</C>.
UnionGroupSource := function (arg)
  local sources;
  sources := Filtered(arg, s -> s <> fail);

  if Size(sources) = 0 then
    return fail;
  fi;

  return function (degree)
    local G, i;

    G := fail;
    i := 1;
    while G = fail and i < GROUP_SOURCE_MAX_ATTEMPTS do
      G := PseudoRandom(sources)(degree);
      i := i + 1;
    od;
    return G;
  end;
end;

# LimitSizeOfGroupSource(source, max_size_or_range)
# Returns a group source which returns a group from the given source as long as
# its size is less than max_size, or if a range is given, as long as its size
# is within the given range.
LimitSizeOfGroupSource := function (source, range)
  return function (degree)
    local G, i;

    if not IsRange(range) then
      range := [1 .. range];
    fi;

    G := fail;
    i := 1;
    while i < GROUP_SOURCE_MAX_ATTEMPTS and (G = fail or not HasSize(G)
          or not Size(G) in range) do
      G := source(degree);
      i := i + 1;
    od;

    return G;
  end;
end;

# ConjugatingGroupSource(source, p)
# Returns a group source which returns groups from <C>source</C>, except with a
# probability $\frac{\mathrm{p}}{100}$ that it has been conjugated by a random
# element of its enclosing symmetric group.
ConjugatingGroupSource := function (source, p)
  return function (degree)
    local G, H, Sn;

    G := source(degree);
    if G = fail then
      return fail;
    fi;

    if PseudoRandom([1 .. 100]) <= p then
      Sn := SymmetricGroup(LargestMovedPoint(G));
      H := G ^ PseudoRandom(Sn);
      SetName(H, Concatenation(PickName(G), " conj"));
      return H;
    else
      return G;
    fi;
  end;
end;

# XXX Workaround for a bug I don't entirely understand; works without it in GAP
# master, but not in HPC-GAP master or released GAP 4.8
TRANSPARTNUM[1] := 1;

PickBasicGroup := UnionGroupSource(
  GroupSourceFromLibrary(PrimitiveGroup, NrPrimitiveGroups, 768),
  GroupSourceFromLibrary(TransitiveGroup, NrTransitiveGroups, 30),
  LoadATLASGroupSource()
);

DefaultGroupSource := LimitSizeOfGroupSource(
  ConjugatingGroupSource(
    UnionGroupSource(
      PickBasicGroup,
      DirectProductSource(PickBasicGroup)
    ),
  20),
[10^4 .. 10^12]);

PickGroup := function ()
  return PickOneGroup(DefaultGroupSource, false);
end;

###
### The test driving code
###
# A test is a record ontaining test functions with arbitrary names. These test
# functions have the signature (bsgs) and return true or false, depending on
# whether or not they succeeded.

DEFAULT_TEST_OPTIONS := rec(
  number_of_groups := 100,
  fixed_groups := [
    GroupWithName(AlternatingGroup(4), "A4"),
    GroupWithName(MathieuGroup(9), "MathieuGroup(9)"),
    GroupWithName(SymmetricGroup(11), "S11"),
    GroupWithName(PrimitiveGroup(1024, 2), "PrimitiveGroup(1024, 2)"),
    TransitiveGroup(10,37)
  ],
  compute_gap_stabchains := true,
  filename := false,
  Print := Print,
  bsgs_options := rec()
);

# PerformTests(tests, opt)
# Runs the given list of tests. <C>opt</C> is a record with the following fields:
# * **number_of_groups**. The number of groups to test against (default 100).
# * **fixed_groups**. A list of groups to always test against.
# * **compute_gap_stabchains**. Whether to compute GAP stabilizer chains as
#   well to compare performance (default true).
# * **filename**. If not false, the filename to write the CSV of test results
#   to (default false).
# * **Print**. A function to call to print messages to the screen.
# * **bsgs_options**. Options record to supply to BSGSFromGroup when computing
#   the stabilizer chains. (default <C>rec()</C>).

PerformTests := function(tests, user_opt)
  local test_results, opt, groups, stab_chains, t, bsgs, result, test, G, i,
        test_name, success, our_time, gap_time;
  test_results := [];
  opt := ShallowCopy(user_opt);
  NEWSS_UpdateRecord(opt, DEFAULT_TEST_OPTIONS);

  # First pick the groups and compute their stabiliser chains.
  groups := ShallowCopy(opt.fixed_groups);
  opt.Print("*** Picking ", opt.number_of_groups - Size(groups), " random groups\n");
  Append(groups, PickManyGroups(DefaultGroupSource, false, opt.number_of_groups - Size(groups)));
  stab_chains := [];

  opt.Print("*** Computing stabilizer chains...\n");
  for G in groups do
    opt.Print(PickName(G), ":\n");
    t := Runtime();
    bsgs := BSGSFromGroup(G, opt.bsgs_options);
    our_time := Runtime() - t;

    t := Runtime();
    if opt.compute_gap_stabchains then
      StabChain(G);
    fi;
    gap_time := Runtime() - t;

    Add(stab_chains, bsgs);
    Add(test_results, rec(group := PickName(G),
                          group_degree := NrMovedPoints(G),
                          group_order := Size(G),
                          time_BSGSFromGroup := our_time,
                          time_StabChain := gap_time,
                          success_BSGSFromGroup := true,
                          success_StabChain := true,
                          success := true));
    opt.Print("    ok.\n");
  od;

  for i in [1 .. Size(stab_chains)] do
    bsgs := stab_chains[i];
    result := test_results[i];

    opt.Print(PickName(bsgs.group), ":\n");
    for test_name in RecNames(tests) do
      opt.Print("    ", test_name, "\n");
      test := tests.(test_name);
      t := Runtime();
      success := test(bsgs);
      t := Runtime() - t;

      result.(Concatenation("time_", test_name)) := t;
      result.(Concatenation("success_", test_name)) := success;
      result.success := result.success and success;

      opt.Print("      ", result.success, " in ", t, " ms.\n");
    od;
  od;

  if opt.filename <> false then
    PrintCSV(opt.filename, test_results);
  fi;

  return test_results;
end;


###
### Our library of tests
###

# NUM_RANDOM_TEST_ELTS
# An integer specifying how many random elements to pick from a large ambient
# group to be tested with VerifyContainsPG.
NUM_RANDOM_TEST_ELTS := 2^15;

NoTests := rec();

FailTests := rec(
  Fail := function (G)
    return false;
  end
);

DefaultTests := rec(
  Containment := function(H_sc)
    local G, H, actually_in_H, think_in_H, x, Sn;
    Sn := SymmetricGroup(LargestMovedPoint(H_sc.group));
    G := List([1 .. NUM_RANDOM_TEST_ELTS], i -> PseudoRandom(Sn));
    H := H_sc.group;

    for x in G do
      actually_in_H := x in H;
      think_in_H := StabilizerChainContains(H_sc, x);
      if actually_in_H <> think_in_H then
        return false;
      fi;
    od;

    return true;
  end,

  Order := function (bsgs)
    return StabilizerChainOrder(bsgs) = Size(bsgs.group);
  end
);
