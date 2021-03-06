# vim: ft=gap sts=2 et sw=2

LoadPackage("io");
LoadPackage("atlasrep");
ReadPackage("newss", "test/timeout.g");

NUM_RANDOM_TEST_ELTS := 2^15;
ContainmentTest := function (H, H_sc, n)
    local G, actually_in_H, think_in_H, x, Sn;
    Sn := SymmetricGroup(LargestMovedPoint(H_sc!.group));
    G := List([1 .. n], i -> PseudoRandom(Sn));

    for x in G do
      actually_in_H := x in H;
      think_in_H := StabilizerChainContains(H_sc, x);
      if actually_in_H <> think_in_H then
        return false;
      fi;
    od;

    return true;
end;


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
    return Concatenation("{degree ", String(NrMovedPoints(G)), "}");
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
DeclareInfoClass("NewssGroupSelInfo");
PickManyGroups := function (source, degree, n)
  local groups, names, G, i;

  groups := [];
  names := Set([]);
  i := 1;

  while Size(groups) < n and i < n * GROUP_SOURCE_MAX_ATTEMPTS do
    G := fail;
    while (G = fail or Name(G) in names) and i < n * GROUP_SOURCE_MAX_ATTEMPTS do
      G := source(degree);
      i := i + 1;
    od;
    Add(groups, G);
    AddSet(names, Name(G));

    if i mod 25 = 0 and i > 0 then
      Info(NewssGroupSelInfo, 2, "Picked ", Size(groups), " groups so far (", i, " attempts)");
    fi;
  od;

  if Size(groups) <> n then
    Info(NewssGroupSelInfo, 1, "Failed to pick the requested number of groups");
    return fail;
  else
    Info(NewssGroupSelInfo, 2, "Picked ", Size(groups), " groups");
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
NEWSS_ORDER_TEST_TIMEOUT := 1;
LimitSizeOfGroupSource := function (source, range)
  return function (degree)
    local G, i, H, size;

    if not IsRange(range) then
      range := [1 .. range];
    fi;

    G := fail;
    i := 1;
    while i < GROUP_SOURCE_MAX_ATTEMPTS do
      G := source(degree);
      i := i + 1;

      if HasSize(G) then
        if Size(G) in range then
          return G;
        fi;
      else
        # We don't want to bias timings of newss by giving it information
        # precomputed by GAP's implementations
        H := Group(GeneratorsOfGroup(G));
        size := Fork_CallWithTimeout(NEWSS_ORDER_TEST_TIMEOUT, Size, H);
        if size[1] and size[2] in range then
          return G;
        fi;
      fi;
    od;

    return G;
  end;
end;


# LimitDegreeOfGroupSource(source, max_size_or_range)
# Returns a group source which returns a group from the given source as long as
# its largest moved point is less than max_size, or if a range is given, as
# long as its size is within the given range.
LimitDegreeOfGroupSource := function (source, range)
  return function (degree)
    local G, i, H, size;

    if not IsRange(range) then
      range := [1 .. range];
    fi;

    G := fail;
    i := 1;
    while i < GROUP_SOURCE_MAX_ATTEMPTS do
      G := source(degree);
      i := i + 1;

      if LargestMovedPoint(G) in range then
        return G;
      fi;
    od;

    return fail;
  end;
end;


# WreathProductSource(source)
# Returns a group source which returns iterated wreath products of two groups
# from <A>source</A>.
WreathProductSource := function (source)
  local Wr, RandWr;

  Wr := function (F, H)
    local G;
    G := WreathProduct(F, H);
    SetName(G, Concatenation(PickName(F), " wr ", PickName(H)));
    return G;
  end;
    

  RandWr := function ()
    local H, F;
    H := LimitSizeOfGroupSource(source, 20)(false);
    F := DirectProductOp(List([1 .. PseudoRandom([1..Maximum(5, LargestMovedPoint(H))])], i -> H), H);
    return Wr(H, F);
  end;

  return function (degree)
    local G, i;
    return Wr(RandWr(), RandWr());
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

# RegeneratingGroupSource(source, p)
# Returns a group source which returns groups from <C>source</C>, except with a
# probability $\frac{\mathrm{p}}{100}$ that we actually make a new group object
# with the same generators. This way, neither newss or GAP have any intrinsic
# knowledge about the group (e.g. order, if the group is Sym/Alt, etc.) that
# might have been picked up e.g. from the ATLAS.
RegeneratingGroupSource := function (source, p)
  return function (degree)
    local G, H, q;
    G := source(degree);
    if G = fail then
      return fail;
    fi;

    q := Random([1 .. 100]);
    if q <= p then
      H := Group(GeneratorsOfGroup(G));
      SetName(H, Concatenation("< ", PickName(G), " >"));
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

VariedGroupSource := RegeneratingGroupSource(
  ConjugatingGroupSource(
    UnionGroupSource(
      PickBasicGroup,
      DirectProductSource(PickBasicGroup)
    ),
  20),
10);

DefaultGroupSource := LimitSizeOfGroupSource(VariedGroupSource, [10^4 .. 10^12]);

PickGroup := function ()
  return PickOneGroup(DefaultGroupSource, false);
end;

###
### Code for loading and saving lists of groups
###

# SaveGroupsList(groups, filename)
# Saves the given list of groups and names to a file.
SaveGroupsList := function (groups, filename)
  local result, handle;
  result := List(groups, function (G)
    local size;
    if HasSize(G) then
      size := Size(G);
    else
      size := fail;
    fi;
    return [PickName(G), size, GeneratorsOfGroup(G)];
  end);
  handle := IO_File(filename, "w");
  
  if handle <> fail then
    IO_Pickle(handle, result);
    IO_Close(handle);
    return true;
  else
    return fail;
  fi;
end;

# LoadGroupsList(filename)
# Loads a list of groups from the file with the given name.
LoadGroupsList := function (filename)
  local handle, input, groups, G, description;
  handle := IO_File(filename, "r");

  if handle <> fail then
    input := IO_Unpickle(handle);
    if input = fail then
      Error("Error unpickling groups list from file");
    fi;
    IO_Close(handle);

    groups := [];
    for description in input do
      G := Group(description[3]);
      SetName(G, description[1]);
      if description[2] <> fail then
        SetSize(G, description[2]);
      fi;
      Add(groups, G);
    od;
    return groups;
  else
    Error("Could not open groups list file for reading");
    return fail;
  fi;
end;

###
### The test driving code
###
# A test is a record ontaining test functions with arbitrary names. These test
# functions have the signature (bsgs) and return true or false, depending on
# whether or not they succeeded.

DEFAULT_TEST_OPTIONS := Immutable(rec(
  number_of_groups := 100,
  fixed_groups := [
    GroupWithName(TrivialGroup(IsPermGroup), "trivial group"),
    GroupWithName(AlternatingGroup(4), "A4"),
    GroupWithName(MathieuGroup(9), "MathieuGroup(9)"),
    GroupWithName(SymmetricGroup(11), "S11"),
    GroupWithName(PrimitiveGroup(1024, 2), "PrimitiveGroup(1024, 2)"),
    TransitiveGroup(10,37)
  ],
  compute_gap_stabchains := true,
  gap_stabchain_options := rec(),
  test_known_base := true,
  filename := false,
  load_groups_list := false,
  truncate_groups_list := false,
  group_source := DefaultGroupSource,
  # This is the percentage of groups for which we also force a deterministic
  # Schreier-Sims runthrough as well.
  deterministic_proportion := 10,
  Print := Print,
  bsgs_options := rec()
));


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

VERIFY_CONTAINMENT := "verify_containment";

PerformTests := function(tests, user_opt)
  local test_results, opt, groups, stab_chains, bsgs, result, G, i,
        tasks, group_task, our_time, gap_time, new_chain, t, region, success,
        deterministic_interval;
  tests := Immutable(tests);
  test_results := [];
  opt := ShallowCopy(DEFAULT_TEST_OPTIONS);
  NEWSS_UpdateRecord(opt, user_opt);

  if not IsBound(opt.bsgs_options.cache_bound) then
    # We do this to make sure that, while we hit the cache code, we also hit
    # the expiry code in DefaultTests.Stabilizer.
    opt.bsgs_options := rec(cache_bound := 2);
    if IsBound(user_opt.bsgs_options) then
      NEWSS_UpdateRecord(opt.bsgs_options, user_opt.bsgs_options);
    fi;
  fi;

  opt := Immutable(opt);

  # First pick the groups and compute their stabiliser chains.
  if opt.load_groups_list = false then
    groups := ShallowCopy(opt.fixed_groups);
    if opt.number_of_groups > Size(groups) then
      opt.Print("*** Picking ", opt.number_of_groups - Size(groups), " random groups\n");
      Append(groups, PickManyGroups(opt.group_source, false, opt.number_of_groups - Size(groups)));
    fi;
  else
    opt.Print("*** Loading groups from file\n");
    groups := LoadGroupsList(opt.load_groups_list);
    if IsBound(opt.truncate_groups_list) and opt.truncate_groups_list <> false then
      groups := groups { [1 .. Minimum(Size(groups), opt.truncate_groups_list)] };
    fi;
  fi;

  stab_chains := [];

  opt.Print("*** Computing stabilizer chains...\n");
  # First we start computing our chains in parallel.
  tasks := [];
  deterministic_interval := Int(Size(groups) * opt.deterministic_proportion / 100) + 1;
  for i in [1 .. Size(groups)] do
    Add(tasks, RunTask(function (G)
      local bsgs, orbit_length, result;
      opt.Print(PickName(G), ": BSGS started.\n");
      t := Runtime();
      bsgs := BSGSFromGroup(G, opt.bsgs_options);
      our_time := Runtime() - t;
      opt.Print(PickName(G), ": BSGS done in ", our_time, " ms.\n");

      if Size(bsgs!.chain) > 0 then
        orbit_length := bsgs!.chain[1].orbit.size;
      else
        orbit_length := 0;
      fi;

      atomic readonly G do
        result := rec(bsgs := bsgs,
                      group := PickName(G),
                      group_degree := NrMovedPoints(G),
                      base_length := Size(bsgs!.base),
                      orbit_top_length := orbit_length,
                      time_BSGSFromGroup := our_time,
                      success_BSGSFromGroup := true);
        if IsBound(bsgs!.options) then
          result.ss := NameFunction(bsgs!.options.SchreierSims);
          result.verify := NameFunction(bsgs!.options.Verify);
        fi;
        return result;
      od;
    end, groups[i]));
  od;

  # Now collect the results and compute the GAP chains on the main thread
  for i in [1 .. Size(groups)] do
    G := groups[i];

    t := Runtime();
    if opt.compute_gap_stabchains then
      StabChain(G, ShallowCopy(opt.gap_stabchain_options));
    fi;
    gap_time := Runtime() - t;
    result := TaskResult(tasks[i]);

    ShareObj(result.bsgs, Concatenation("bsgs", String(i)));
    NEWSS_UpdateRecord(result, rec(time_StabChain := gap_time,
                                   group_order := Size(G),
                                   success_StabChain := true));
    Add(test_results, result);

    LockAndMigrateObj(StabChainMutable(G), result.bsgs);
    Add(stab_chains, result.bsgs);
    Unbind(result.bsgs);

    opt.Print(PickName(G), ": StabChain done in ", gap_time, " ms.\n");
  od;


  tasks := [];
  for i in [1 .. Size(stab_chains)] do
    bsgs := stab_chains[i];
    group_task := RunTask(function (i, bsgs)
      local test_bsgs, test, t, success, vt, test_name, result, these_tests;
#      SilentErrors := false; BreakOnError := true;
      atomic readonly bsgs do
        result := rec(success := true);
        opt.Print(PickName(bsgs!.group), ":\n");

        these_tests := ShallowCopy(tests);
        if i mod deterministic_interval = 0 and i <> 0 then
          these_tests.Deterministic := function (bsgs)
            bsgs := BSGSFromGroup(bsgs!.group, NEWSS_DETERMINISTIC_OPTIONS);
            return ContainmentTest(bsgs!.group, bsgs, NUM_RANDOM_TEST_ELTS / 2);
          end;
        fi;

        for test_name in RecNames(these_tests) do
          # Some tests modify the BSGS; we want to copy it here so we don't count
          # the copying as part of the time taken by the test
          test_bsgs := CopyBSGS(bsgs);

          opt.Print(PickName(bsgs!.group), ": ", test_name, ": started.\n");
          test := these_tests.(test_name);
          t := Runtime();
          success := test(test_bsgs);
          t := Runtime() - t;

          # We want the timing information not to include the verification in a
          # lot of cases, so do a routine containment test out here.
          if success = VERIFY_CONTAINMENT then
            opt.Print(PickName(bsgs!.group), ": ", test_name, ": done in ", t, " ms.\n");

            vt := Runtime();
            success := ContainmentTest(bsgs!.group, test_bsgs, NUM_RANDOM_TEST_ELTS / 2);
            vt := Runtime() - vt;
            result.(Concatenation("vtime_", test_name)) := vt;

            opt.Print(PickName(bsgs!.group), ": ", test_name, ": ", success, " in ", vt, " ms.\n");
          else
            opt.Print(PickName(bsgs!.group), ": ", test_name, ": ", success, " in ", t, " ms.\n");
          fi;

          result.(Concatenation("time_", test_name)) := t;
          result.(Concatenation("success_", test_name)) := success;
          result.success := result.success and success;
        od;

        return result;
      od;
    end, i, bsgs);
    Add(tasks, group_task);
  od;

  for i in [1 .. Size(stab_chains)] do
    result := TaskResult(tasks[i]);
    NEWSS_UpdateRecord(test_results[i], result);
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

NoTests := rec();

FailTests := rec(
  Fail := function (G)
    return false;
  end
);

DefaultTests := rec(
  Containment := function (H_sc)
    return ContainmentTest(H_sc!.group, H_sc, NUM_RANDOM_TEST_ELTS);
  end,

  Order := function (bsgs)
    return StabilizerChainOrder(bsgs) = Size(bsgs!.group);
  end,

  Accessors := function (bsgs)
    # We just want to make sure these don't error
    ViewString(bsgs);
    GroupBSGS(bsgs);
    StrongGeneratorsBSGS(bsgs);
    BaseBSGS(bsgs);
    if Size(StabilizersBSGS(bsgs)) > 0 then;
      StabilizerBSGS(bsgs, 1);
    fi;
    return true;
  end,

  PermWord := function (bsgs)
    local factors, perm, pt;
    factors := List([1 .. 3], i -> PseudoRandom(bsgs!.group));
    perm := Product(factors);

    if PermWordAsPerm(factors) <> perm then
      return false;
    fi;

    if PermWordAsPerm(PermWordInverse(factors)) <> Inverse(perm) then
      return false;
    fi;

    if NrMovedPoints(bsgs!.group) = 0 then
      return true;
    fi;

    pt := PseudoRandom(MovedPoints(bsgs!.group));
    if PermWordImage(pt, factors) <> pt ^ perm then
      return false;
    fi;

    pt := PseudoRandom(MovedPoints(bsgs!.group));
    if PermWordPreImage(pt, factors) <> pt / perm then
      return false;
    fi;

    return true;
  end,

  RemoveRedundantGens := function (bsgs)
    RemoveRedundantGenerators(bsgs);
    return VERIFY_CONTAINMENT;
  end
);

KnownBaseTests := rec(
  KnownBase := function (bsgs)
    local new_chain;
    new_chain := BSGSFromGroup(bsgs!.group, rec( known_base := bsgs!.base ));

    if StabilizerChainOrder(bsgs) <> StabilizerChainOrder(new_chain) then
      return false;
    fi;
    return VERIFY_CONTAINMENT;
  end
);
WithKnownBaseTests := ShallowCopy(KnownBaseTests);
NEWSS_UpdateRecord(WithKnownBaseTests, DefaultTests);


SingleChangeOfBaseTest := function (bsgs)
  local gens, new_base, i, pt, stab, stabilised_pts;

  if Size(bsgs!.base) = 0 or NrMovedPoints(bsgs!.group) = 0 then
    # Not much more we can do here
    return true;
  fi;

  # First, we change up the base a bit
  gens := GeneratorsOfGroup(bsgs!.group);
  new_base := ShallowCopy(bsgs!.base);
  Shuffle(new_base);
  for i in [1 .. Int(Size(bsgs!.base)/10) + 1] do
    pt := PseudoRandom(MovedPoints(bsgs!.group));
    if not (pt in new_base) then
      Add(new_base, PseudoRandom(MovedPoints(bsgs!.group)));
    fi;
  od;

  if Size(bsgs!.base) > 2 then
    stabilised_pts := Difference(MovedPoints(bsgs!.chain[2].group), [bsgs!.base[1]]);
    if Size(stabilised_pts) > 0 then
      Add(new_base, PseudoRandom(stabilised_pts), 2);
    fi;
  fi;

  stab := Intersection(List(new_base, b -> Stabilizer(bsgs!.group, b)));
  if Size(stab) > 1 then
    # XXX not sure what the best thing to do is in this situation
    Print("!! not a base (stabilized by ", stab, ") !!\n");
    return true;
  fi;

  # Then perform the change of base and do the usual verification step
  bsgs := BSGSWithBase(bsgs, new_base);
  if bsgs!.base <> new_base then
    Print("*** ", new_base, "\n");
    Print("!! we didn't actually get the right base out, got ", bsgs!.base, " instead\n");
    return false;
  fi;
  return ContainmentTest(bsgs!.group, bsgs, NUM_RANDOM_TEST_ELTS / 4);
end;

ChangeOfBaseTests := rec(
  ChangesOfBase := function (bsgs)
    local i;
    for i in [1 .. 3] do
      if not SingleChangeOfBaseTest(bsgs) then
        return false;
      fi;
    od;
    return true;
  end,
  
  BaseSwap := function (bsgs)
    if Size(bsgs!.base) = 0 then
      return true;
    fi;
    NEWSS_PerformBaseSwap(bsgs, PseudoRandom([1 .. Size(bsgs!.base) - 1]));
    return VERIFY_CONTAINMENT;
  end,

  Stabilizer := function (bsgs)
    local points, i, pt, gap_stab, our_stab;
    points := MovedPoints(bsgs!.group);

    for i in [1 .. Minimum(4, Size(points))] do
      pt := PseudoRandom(points);
      gap_stab := Stabilizer(bsgs!.group, pt);
      our_stab := StabilizerChainStabilizer(bsgs, pt);
      if not ContainmentTest(gap_stab, BSGSFromGroup(our_stab), NUM_RANDOM_TEST_ELTS / 8) then
        return false;
      fi;
    od;
    return true;
  end,

  Conjugate := function (bsgs)
    local el, G, conj;

    if IsTrivial(bsgs!.group) then
      return true;
    fi;

    el := PseudoRandom(SymmetricGroup(LargestMovedPoint(bsgs!.group)));
    G := bsgs!.group ^ el;
    conj := ConjugateBSGS(bsgs, el);
    return ContainmentTest(G, conj, NUM_RANDOM_TEST_ELTS / 8);
  end
);
NEWSS_UpdateRecord(DefaultTests, ChangeOfBaseTests);

ToGAPStabChainTests := rec(
  ToGAPStabChain := function (bsgs)
    local gap_sc, G, g, i;
    G := Group(GeneratorsOfGroup(bsgs!.group));
    StabChainNewssOp(G, rec());
    gap_sc := StabChainMutable(G);
    bsgs := gap_sc.from_newss;

    if not IsBound(gap_sc.from_newss) or StabilizerChainOrder(bsgs) <> Size(G) then
      return false;
    fi;

    if bsgs!.base <> BaseStabChain(gap_sc) then
      Print("*** BSGS BASE: ", bsgs!.base, "\n");
      Print("*** GAP BASE: ", BaseStabChain(gap_sc), "\n");
      return false;
    fi;

    return ContainmentTest(G, bsgs, NUM_RANDOM_TEST_ELTS / 4);
  end
);

AllTests := ShallowCopy(DefaultTests);
NEWSS_UpdateRecord(AllTests, KnownBaseTests);
NEWSS_UpdateRecord(AllTests, ToGAPStabChainTests);
MakeImmutable(AllTests);
MakeImmutable(DefaultTests);
