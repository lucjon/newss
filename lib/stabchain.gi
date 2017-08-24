# vim: ft=gap sts=2 et sw=2
#
# This file contains strategy definitions and high-level functions for creating
# and manipulating stabiliser chain structures.
#

MakeReadWriteGlobal("NEWSS_MIN_DEGREE_RANDOM");
NEWSS_MIN_DEGREE_RANDOM := 10;

InstallValue(NEWSS_PERMWORD_REPRESENTATION, Immutable(rec(
  PermFromBasePoint := SchreierVectorWordFromBasePoint,
  PermToBasePoint := SchreierVectorWordToBasePoint,
  Strip := StabilizerChainStripWord,
  AsPerm := PermWordAsPerm,
  MulPerm := Concatenation,
  InvPerm := PermWordInverse,
  LiftPerm := x -> [x],
  ImagePerm := PermWordImage
)));

InstallValue(NEWSS_PERM_REPRESENTATION, Immutable(rec(
  PermFromBasePoint := SchreierVectorPermFromBasePoint,
  PermToBasePoint := SchreierVectorPermToBasePoint,
  Strip := StabilizerChainStrip,
  AsPerm := IdFunc,
  MulPerm := function (arg) return Product(arg); end,
  InvPerm := Inverse,
  LiftPerm := IdFunc,
  ImagePerm := OnPoints
)));
 
InstallValue(NEWSS_DEFAULT_OPTIONS, Immutable(rec(
  SchreierSims := RandomSchreierSims,
  Verify := NEWSS_VerifyByDeterministic,
  SelectBasePoint := NEWSS_FirstMovedPoint,
  SchreierVectorForLevel := NEWSS_SVForLevel,
  ExtendSchreierVector := NEWSS_ExtendSV,

  perm_representation := NEWSS_PERMWORD_REPRESENTATION,
  fall_back_to_deterministic := true,
  sift_threshold := 8,
  orbits_to_consider := 3,
  cache_bound := NEWSS_DEFAULT_TREE_BOUND,
  cache_depth := NEWSS_DEFAULT_TREE_DEPTH
)));

InstallValue(NEWSS_DETERMINISTIC_OPTIONS, Immutable(rec(
  SchreierSims := SchreierSims,
  Verify := ReturnTrue,
  SelectBasePoint := NEWSS_PickFromOrbits,
  SchreierVectorForLevel := NEWSS_SVForLevel,
  ExtendSchreierVector := NEWSS_ExtendSV,

  # We need this here since a user could specify a random algorithm in their
  # options, even in the case where we would have picked a deterministic one,
  # but might not provide these parameters
  perm_representation := NEWSS_PERMWORD_REPRESENTATION,
  fall_back_to_deterministic := NEWSS_DEFAULT_OPTIONS.fall_back_to_deterministic,
  sift_threshold := NEWSS_DEFAULT_OPTIONS.sift_threshold,
  orbits_to_consider := NEWSS_DEFAULT_OPTIONS.orbits_to_consider,
  cache_bound := NEWSS_DEFAULT_OPTIONS.cache_bound,
  cache_depth := NEWSS_DEFAULT_OPTIONS.cache_depth
)));


InstallGlobalFunction(BSGS, function (group, base, sgs)
  local obj, tree;
  tree := rec(children := [],
              members := [],
              tree := ~,
              count := 0,
              bound := NEWSS_DEFAULT_TREE_BOUND,
              depth := NEWSS_DEFAULT_TREE_DEPTH);
  obj := Objectify(BSGSType,
                   rec(group := group, base := base, sgs := sgs,
                       initial_gens := Immutable(Set(sgs)),
                       tree := tree));

  if Size(base) > 0 then
    # If we haven't actually got a base, wait until we calculate one.
    NEWSS_AddChainToTree(obj!.tree, obj);
  fi;

  return obj;
end);

InstallMethod(ViewString, "method for BSGS structures", [IsBSGS],
function (bsgs)
  if IsBound(bsgs!.chain) then
    return Concatenation("<BSGS with ", String(Size(bsgs!.chain)), " levels, base ", ViewString(bsgs!.base), ", ", String(Size(bsgs!.sgs)), " strong gens>");
  else
    return Concatenation("<BSGS with base ", ViewString(bsgs!.base), ", ", String(Size(bsgs!.sgs)), " strong gens>");
  fi;
end);

NEWSS_CopyMethod := function (meth)
  return function (bsgs)
    local new_record, try;
    new_record := rec();
    try := function (name)
      if IsBound(bsgs!.(name)) then
        new_record.(name) := meth(bsgs!.(name));
      fi;
    end;
    Perform(["group", "base", "sgs", "initial_gens", "chain", "options", "tree"], try);
    return Objectify(BSGSType, new_record);
  end;
end;

# XXX This should probably be installed for StructuralCopy but I couldn't get it to work...
InstallGlobalFunction(CopyBSGS, NEWSS_CopyMethod(StructuralCopy));

InstallGlobalFunction(BSGSFromGAP, function (group)
  local sc;
  sc := StabChain(group);
  return BSGS(group, BaseStabChain(sc), StrongGeneratorsStabChain(sc));
end);

InstallGlobalFunction(NEWSS_UpdateRecord, function (base, new)
  local name;
  for name in RecNames(new) do
    if not IsBound(base.(name)) then
      base.(name) := new.(name);
    fi;
  od;
end);

InstallGlobalFunction(BSGSFromGroup, function (arg)
  local group, B;

  if Size(arg) = 0 then
    Error("No group given to BSGSFromGroup");
  fi;

  group := arg[1];

  if Size(GeneratorsOfGroup(group)) = 0 then
    # We have the trivial group, but our implementations get a little upset
    # about not having any generators. It's easiest to special case this.
    # (Interestingly enough, the transitive groups library likes to call this S1.)
    B := BSGS(group, [], [()]);
    B!.chain := [];
    return B;
  fi;

  # First choose a strategy based on heuristics.
  B := BSGS(group, [], ShallowCopy(GeneratorsOfGroup(group)));

  if NrMovedPoints(group) <= NEWSS_MIN_DEGREE_RANDOM then
    B!.options := ShallowCopy(NEWSS_DETERMINISTIC_OPTIONS);
  else
    B!.options := ShallowCopy(NEWSS_DEFAULT_OPTIONS);
  fi;

  if HasSize(group) then
    B!.options.Verify := NEWSS_VerifyByOrder;
  fi;

  # If they have given us a strategy, use it, filling in any missing fields
  # from the heuristically-determined one.
  if Size(arg) > 1 then
    arg[2] := ShallowCopy(arg[2]);
    NEWSS_UpdateRecord(arg[2], B!.options);
    B!.options := arg[2];
  fi;

  if IsBound(B!.options.known_base) then
    B!.options.known_base := Immutable(B!.options.known_base);
    if not IsBound(B!.options.base) then
      B!.options.base := ShallowCopy(B!.options.known_base);
    fi;
    B!.options.IsIdentity := NEWSS_IsIdentityByKnownBase;
  else
    B!.options.IsIdentity := NEWSS_IsIdentityByMul;
  fi;

  if IsBound(B!.options.base) then
    B!.options.SelectBasePoint := NEWSS_SelectFromChosenBase;
  fi;

  # Configure the cache, unless it already has been.
  if B!.tree.count <= 1 then
    B!.tree!.bound := B!.options.cache_bound;
    B!.tree!.depth := B!.options.cache_depth;
  fi;

  NEWSS_UpdateRecord(B!.options, B!.options.perm_representation);
  Info(NewssInfo, 2, "Selected ", NameFunction(B!.options.SchreierSims), " for stabilizer chain computation");
  B!.options.SchreierSims(B);

  Info(NewssInfo, 2, "Verifying with ", NameFunction(B!.options.Verify));
  if not B!.options.Verify(B) and B!.options.fall_back_to_deterministic then
    # If we can't verify the chain is complete, then run the deterministic
    # algorithm to make sure we don't return an incomplete chain.
    Info(NewssInfo, 2, "Verification failed, performing determinstic pass");
    NEWSS_DETERMINISTIC_OPTIONS.SchreierSims(B);
    Info(NewssInfo, 2, "Finished deterministic pass");
  fi;

  NEWSS_AddChainToTree(B!.tree, B);
  return B;
end);

InstallGlobalFunction(GAPStabChainFromBSGS, function (bsgs)
  local labels, top, prev, current, pt, gen, image, next, genlabels,
        generators, i, j, to_compute;

  EnsureBSGSChainComputed(bsgs);

  if IsTrivial(bsgs!.group) then
    return rec(generators := [], genlabels := [], identity := (), labels := [()]);
  fi;

  top := rec();
  prev := top;
  current := prev;
  top.labels := Concatenation([()], bsgs!.sgs);

  for i in [1 .. Size(bsgs!.base)] do
    current.identity := ();
    current.labels := prev.labels;
    current.genlabels := List(bsgs!.chain[i].gens, function (g)
      if g = () then
        return 1;
      else
        return Position(bsgs!.sgs, g) + 1;
      fi;
    end);
    current.generators := ShallowCopy(bsgs!.chain[i].gens);

    current.orbit := [bsgs!.base[i]];
    current.transversal := [];
    current.translabels := [];
    current.transversal[bsgs!.base[i]] := ();
    current.translabels[bsgs!.base[i]] := 1;

    # We need to recompute the orbits `the other way around', which is how GAP
    # expects them. This could be slightly faster than the original computation
    # since we know the size of orbit to expect.
    to_compute := [bsgs!.base[i]];
    while Size(to_compute) > 0 and Size(current.orbit) < bsgs!.chain[i].orbit.size do
      pt := Remove(to_compute, 1);
      for gen in bsgs!.chain[i].gens do
        image := pt / gen;
        if not IsBound(current.transversal[image]) then
          current.transversal[image] := gen;
          current.translabels[image] := Position(current.labels, gen);
          Add(current.orbit, image);
          Add(to_compute, image);
        fi;
      od;
    od;

    if i <> Size(bsgs!.base) then
      next := rec();
      current.stabilizer := next;
      prev := current;
      current := next;
    else
      # The trivial group.
      current.stabilizer := rec(
        identity := (),
        labels := top.labels,
        genlabels := [],
        generators := []
      );
    fi;
  od;

  top.from_newss := bsgs;
  return top;
end);


InstallMethod(GroupBSGS, "for a BSGS structure", [IsBSGS], b -> b!.group);
InstallMethod(StrongGeneratorsBSGS, "for a BSGS structure", [IsBSGS], b -> Immutable(b!.sgs));
InstallMethod(BaseBSGS, "for a BSGS structure", [IsBSGS], b -> Immutable(b!.base));
InstallMethod(StabilizersBSGS, "for a BSGS structure", [IsBSGS], b -> Immutable(b!.chain));
InstallMethod(StabilizerBSGS, "for a BSGS structure", [IsBSGS, IsInt],
function (bsgs, i)
  return bsgs!.chain[i];
end);


StabChainNewssOp := function (G, gap_options)
  local options, S;

  if HasStabChainMutable(G) then
    return StabChainMutable(G);
  else
    options := rec();

    if IsBound(gap_options.knownBase) then
      options.known_base := gap_options.knownBase;
    fi;

    if IsBound(gap_options.base) then
      options.base := gap_options.base;
    fi;

    S := GAPStabChainFromBSGS(BSGSFromGroup(G, options));
    SetStabChainMutable(G, S);

    return S;
  fi;
end;

InstallGlobalFunction(EnableNewssOverloads, function ()
  InstallOtherMethod(StabChainOp, "permutation group and options", true,
                     [IsPermGroup, IsRecord], RankFilter(IsPermGroup) + 1,
                     StabChainNewssOp);
end);


###
### Functions for manipulating stabiliser chains
###

InstallGlobalFunction(NEWSS_ChangeBaseByPointSwap, function (bsgs, new_base)
  local pt, stabilized, i, j;
  for j in [1 .. Size(new_base)] do
    pt := new_base[j];
    stabilized := false;

    Info(NewssInfo, 3, "@", j, "(", pt, "):");
    Info(NewssInfo, 3, "  ", bsgs!.base);

    i := j;
    while i <= Size(bsgs!.base) do
      stabilized := ForAll(bsgs!.chain[i].gens, g -> pt ^ g = pt);
      if bsgs!.base[i] = pt or stabilized then
        break;
      fi;
      i := i + 1;
    od;

    if i <= Size(bsgs!.base) and bsgs!.base[i] = pt then
      # We still want to swap our point closer to the start, but the index will
      # be off since the code below assumes we added a point.
      Info(NewssInfo, 3, "  we already found the point ", pt, " at ", i);
      i := i - 1;
    elif stabilized then
      # When we get here, we are able to insert it after point i as a redundant
      # base point, since we know it is stabilized by the previous group. (This
      # may be at the end of the list.)
      Info(NewssInfo, 3, "  inserting redundant base point ", pt, " at ", i);
      NEWSS_InsertRedundantBasePoint(bsgs, i, pt);
    else
      # If we weren't even stabilized by some existing subgroup in the chain,
      # the best we can do is add it to the end of the base, and with trivial
      # stabilizer group
      Info(NewssInfo, 3, "  appending trivial base point ", pt, " at ", i);
      NEWSS_AppendTrivialBasePoint(bsgs, pt);
    fi;

    # Then we swap it back to its intended position.
    while i >= j and i < Size(bsgs!.base) do
      Info(NewssInfo, 3, "  swapping ", bsgs!.base[i], " at ", i, " with ", bsgs!.base[i + 1], " at ", i + 1);
      NEWSS_PerformBaseSwap(bsgs, i);
      i := i - 1;
    od;
  od;

  Info(NewssInfo, 3, "finished swaps with ", bsgs!.base);

  # If the list supplied is genuinely a base, then there will be a 'tail' of
  # extraneous trivial base points which were in the old base but not the new;
  # we should dispose of them now.
  while Size(bsgs!.base) > Size(new_base) do
    i := Size(new_base) + 1;
    if not IsTrivial(bsgs!.chain[i].group) then
      Error("irredundant extraneous base point ", bsgs!.base[i], "; was a base supplied?");
    fi;

    Info(NewssInfo, 3, "removing trivial base point ", bsgs!.base[i]);
    Remove(bsgs!.base, i);
    Remove(bsgs!.chain, i);
  od;
end);


# Insert a new point <C>pt</C> at position <C>i + 1</C> in the base of the BSGS
# structure <C>bsgs</C> with the same stabilizer group as in position i. This
# function does not check that bsgs!.stabilizers[i] does, in fact, stabilise
# <C>pt</C>.
InstallGlobalFunction(NEWSS_InsertRedundantBasePoint, function (bsgs, i, pt)
  local orb;
  # The stabilizer group is just a copy
  Add(bsgs!.base, pt, i + 1);
  Add(bsgs!.chain, rec(
    gens := ShallowCopy(bsgs!.chain[i].gens),
    group := bsgs!.chain[i].group,
    orbit := SchreierVectorForOrbit(~.gens, pt)), i + 1);
end);

# Append a new base point <C>pt</C> with trivial stabilizer to the chain.
InstallGlobalFunction(NEWSS_AppendTrivialBasePoint, function (bsgs, pt)
  local orb;
  Add(bsgs!.base, pt);
  Add(bsgs!.chain, rec(
    gens := [()],
    group := Group(()),
    orbit := NEWSS_EmptySchreierVector(~.gens, pt)));
end);



InstallGlobalFunction(ChangeBaseOfBSGS, function (bsgs, base)
  NEWSS_ChangeBaseByPointSwap(bsgs, base);
end);

InstallGlobalFunction(BSGSWithBase, function (bsgs, new_base)
  return BSGSWithBasePrefix(bsgs, new_base, true);
end);

InstallGlobalFunction(BSGSWithBasePrefix, function (bsgs, new_base, rest...)
  local known_base, chain, new_bsgs;
  known_base := false;
  if Size(rest) > 0 then
    known_base := rest[1];
  fi;

  chain := NEWSS_FindChainWithBasePrefix(bsgs!.tree, new_base);
  if chain = fail then
    new_bsgs := CopyBSGS(bsgs);
    if known_base then
      ChangeBaseOfBSGS(new_bsgs, new_base);
    else
      NEWSS_ChangeBaseByRecomputing(new_bsgs, new_base, false);
    fi;
    NEWSS_AddChainToTree(new_bsgs!.tree, new_bsgs);
    return new_bsgs;
  else
    return chain;
  fi;
end);

InstallGlobalFunction(NEWSS_ChangeBaseByRecomputing, function (bsgs, new_base, known_base)
  # For now, we just re-run Schreier–Sims with the given base. Knowing we
  # have a base makes this a lot faster, but in general we can do much better
  # than this.
  local old_base;
  old_base := bsgs!.base;
  bsgs!.base := [];
  bsgs!.sgs := GeneratorsOfGroup(bsgs!.group);
  bsgs!.chain := [];
  bsgs!.options.base := new_base;
  bsgs!.options.known_base := old_base;
  bsgs!.options.IsIdentity := NEWSS_IsIdentityByKnownBase;
  bsgs!.options.SelectBasePoint := NEWSS_SelectFromChosenBase;
  bsgs!.options.SchreierSims(bsgs);
end);

InstallGlobalFunction(RemoveRedundantGenerators, function (bsgs, keep_initial_gens)
  local i, new_gens, generator, sv, have_shrunk, j, k;

  i := Size(bsgs!.base) - 1;
  while i >= 1 do
    new_gens := Difference(bsgs!.chain[i].gens, bsgs!.chain[i + 1].gens);
    for j in [1 .. Size(new_gens)] do
      generator := Remove(new_gens, j);

      if keep_initial_gens and generator in bsgs!.initial_gens then
        continue;
      fi;

      sv := SchreierVectorForOrbit(new_gens, bsgs!.base[i]).sv;

      have_shrunk := false;
      for j in [1 .. Size(bsgs!.chain[i].orbit.sv)] do
        if IsBound(bsgs!.chain[i].orbit.sv[j]) and not IsBound(sv[j]) then
          have_shrunk := true;
          break;
        fi;
      od;

      if have_shrunk then
        Add(new_gens, generator, j);
      else
        Remove(bsgs!.sgs, Position(bsgs!.sgs, generator));
      fi;
    od;

    i := i - 1;
  od;

  return bsgs;
end);


InstallGlobalFunction(ComputeChainForBSGS, function (bsgs)
  local stabilizer, base_subset, i, gens;

  bsgs!.chain := [rec(gens := bsgs!.sgs, group := bsgs!.group)];

  for i in [1 .. Size(bsgs!.base)] do
    # The i-th stabilizer is generated by those generators in the SGS which fix
    # [b_1, ..., b_i], where b_k is the kth element of the base.
    base_subset := bsgs!.base { [1 .. i - 1] };
    gens := Filtered(bsgs!.sgs, g -> Stabilizes(g, base_subset));
    if Size(gens) = 0 then
      gens := [()];
    fi;

    if not IsBound(bsgs!.chain[i]) then
      bsgs!.chain[i] := rec();
    fi;
    bsgs!.chain[i].gens := gens;

    bsgs!.options.SchreierVectorForLevel(bsgs, i);
    ComputeStabForBSGS(bsgs, i);
  od;
end);


InstallGlobalFunction(ConjugateBSGS, function (bsgs, g)
  local new_bsgs;
  EnsureBSGSChainComputed(bsgs);

  new_bsgs := CopyBSGS(bsgs);
  new_bsgs!.base := List(bsgs!.base, b -> b ^ g);
  new_bsgs!.sgs := List(bsgs!.sgs, x -> x ^ g);
  new_bsgs!.group := Group(bsgs!.sgs);
  ComputeChainForBSGS(new_bsgs);
  NEWSS_AddChainToTree(bsgs!.tree, new_bsgs);

  return new_bsgs;
end);



# NEWSS_PerformBaseSwap(bsgs, i)
# Swap base points base[i] and base[i+1] using a randomised algorithm.
InstallGlobalFunction(NEWSS_PerformBaseSwap, function (bsgs, i)
  local T, beta, stab_sv, orb_sv, target_size, gen, to_stabilize;

  if i < 1 or i > Size(bsgs!.base) - 1 then
    Error("cannot swap base points out of range");
  fi;
  
  # The bulk of the effort is computing the new stabgens[i+1] and orbits[i+1]
  beta := bsgs!.base[i + 1];
  stab_sv := SchreierVectorForOrbit(bsgs!.chain[i].gens, beta);

  # The new group needs to contain G^(i+2);
  if IsBound(bsgs!.chain[i + 2]) then
    T := ShallowCopy(bsgs!.chain[i + 2].gens);
  else
    # but it might be trivial.
    T := [];
  fi;

  orb_sv := SchreierVectorForOrbit(T, bsgs!.base[i]);

  # We know the size to expect by the Orbit-Stabilizer theorem; see Seress,
  target_size := bsgs!.chain[i].orbit.size * bsgs!.chain[i + 1].orbit.size / stab_sv.size;

  while orb_sv.size < target_size do
    gen := RandomStabilizerElement(stab_sv);
    if gen <> () then
      Add(bsgs!.sgs, gen);
      ExtendSchreierVector(orb_sv, gen);
    fi;
  od;

  # Now perform the actual swapping
  if Size(T) = 0 then
    T := [()];
  fi;

  bsgs!.base[i + 1] := bsgs!.base[i];
  bsgs!.base[i] := beta;
  bsgs!.chain[i + 1].gens := T;
  bsgs!.chain[i + 1].group := Group(T);
  bsgs!.chain[i].orbit := stab_sv;
  bsgs!.chain[i + 1].orbit := orb_sv;
end);

InstallGlobalFunction(NEWSS_AppendEmptyChain, function (bsgs)
  Add(bsgs!.chain, rec(gens := []));
end);
