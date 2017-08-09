# vim: ft=gap sts=2 et sw=2
#
# This file contains strategy definitions and high-level functions for creating
# and manipulating stabiliser chain structures.
#

MakeReadWriteGlobal("NEWSS_MIN_DEGREE_RANDOM");
NEWSS_MIN_DEGREE_RANDOM := 10;

InstallValue(NEWSS_PERMWORD_REPRESENTATION, Immutable(rec(
  PermFromBasePoint := SchreierVectorWordFromBasePoint,
  Strip := StabilizerChainStripWord,
  AsPerm := PermWordAsPerm,
  MulPerm := Concatenation,
  InvPerm := PermWordInverse,
  LiftPerm := x -> [x],
  ImagePerm := PermWordImage
)));

InstallValue(NEWSS_PERM_REPRESENTATION, Immutable(rec(
  PermFromBasePoint := SchreierVectorPermFromBasePoint,
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
  ExtendBaseForLevel := NEWSS_FirstMovedPoint,
  SchreierVectorForLevel := NEWSS_SVForLevel,
  ExtendSchreierVector := NEWSS_ExtendSV,

  perm_representation := NEWSS_PERMWORD_REPRESENTATION,
  fall_back_to_deterministic := true,
  sift_threshold := 8,
  orbits_to_consider := 3
)));

InstallValue(NEWSS_DETERMINISTIC_OPTIONS, Immutable(rec(
  SchreierSims := SchreierSims,
  Verify := ReturnTrue,
  ExtendBaseForLevel := NEWSS_PickFromOrbits,
  SchreierVectorForLevel := NEWSS_SVForLevel,
  ExtendSchreierVector := NEWSS_ExtendSV,

  # We need this here since a user could specify a random algorithm in their
  # options, even in the case where we would have picked a deterministic one,
  # but might not provide these parameters
  perm_representation := NEWSS_PERMWORD_REPRESENTATION,
  fall_back_to_deterministic := NEWSS_DEFAULT_OPTIONS.fall_back_to_deterministic,
  sift_threshold := NEWSS_DEFAULT_OPTIONS.sift_threshold,
  orbits_to_consider := NEWSS_DEFAULT_OPTIONS.orbits_to_consider
)));


InstallGlobalFunction(BSGS, function (group, base, sgs)
  return rec(group := group, base := base, sgs := sgs,
             initial_gens := Immutable(Set(sgs)), has_chain := false);
end);

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
    B.stabgens := [[]];
    B.orbits := [];
    B.orbitsizes := [];
    B.have_chain := true;
    return B;
  fi;

  # First choose a strategy based on heuristics.
  B := BSGS(group, [], ShallowCopy(GeneratorsOfGroup(group)));

  if NrMovedPoints(group) <= NEWSS_MIN_DEGREE_RANDOM then
    B.options := ShallowCopy(NEWSS_DETERMINISTIC_OPTIONS);
  else
    B.options := ShallowCopy(NEWSS_DEFAULT_OPTIONS);
  fi;

  if HasSize(group) then
    B.options.Verify := NEWSS_VerifyByOrder;
  fi;

  # If they have given us a strategy, use it, filling in any missing fields
  # from the heuristically-determined one.
  if Size(arg) > 1 then
    arg[2] := ShallowCopy(arg[2]);
    NEWSS_UpdateRecord(arg[2], B.options);
    B.options := arg[2];
  fi;

  if IsBound(B.options.known_base) then
    B.options.known_base := Immutable(B.options.known_base);
    B.base := ShallowCopy(B.options.known_base);
    B.options.IsIdentity := NEWSS_IsIdentityByKnownBase;
  else
    B.options.IsIdentity := NEWSS_IsIdentityByMul;
  fi;

  NEWSS_UpdateRecord(B.options, B.options.perm_representation);
  Info(NewssInfo, 2, "Selected ", NameFunction(B.options.SchreierSims), " for stabilizer chain computation");
  B.options.SchreierSims(B);

  Info(NewssInfo, 2, "Verifying with ", NameFunction(B.options.Verify));
  if not B.options.Verify(B) and B.options.fall_back_to_deterministic then
    # If we can't verify the chain is complete, then run the deterministic
    # algorithm to make sure we don't return an incomplete chain.
    Info(NewssInfo, 2, "Verification failed, performing determinstic pass");
    NEWSS_DETERMINISTIC_OPTIONS.SchreierSims(B);
    Info(NewssInfo, 2, "Finished deterministic pass");
  fi;

  return B;
end);

InstallGlobalFunction(GAPStabChainFromBSGS, function (bsgs)
  local labels, top, prev, current, pt, gen, image, next, genlabels,
        generators, i, j, to_compute;

  EnsureBSGSChainComputed(bsgs);

  if Size(bsgs.base) = 0 then
    return rec(generators := [], genlabels := [], identity := (), labels := [()]);
  fi;

  top := rec();
  prev := top;
  current := prev;
  top.labels := Concatenation([()], bsgs.sgs);

  for i in [1 .. Size(bsgs.base)] do
    current.identity := ();
    current.labels := prev.labels;
    current.genlabels := List(bsgs.stabgens[i], g -> Position(bsgs.sgs, g) + 1);
    current.generators := ShallowCopy(bsgs.stabgens[i]);

    current.orbit := [bsgs.base[i]];
    current.transversal := [];
    current.translabels := [];
    current.transversal[bsgs.base[i]] := ();
    current.translabels[bsgs.base[i]] := 1;

    # We need to recompute the orbits `the other way around', which is how GAP
    # expects them. This could be slightly faster than the original computation
    # since we know the size of orbit to expect.
    to_compute := [bsgs.base[i]];
    while Size(to_compute) > 0 and Size(current.orbit) < bsgs.orbitsizes[i] do
      pt := Remove(to_compute, 1);
      for gen in bsgs.stabgens[i] do
        image := pt / gen;
        if not IsBound(current.transversal[image]) then
          current.transversal[image] := gen;
          current.translabels[image] := Position(current.labels, gen);
          Add(current.orbit, image);
          Add(to_compute, image);
        fi;
      od;
    od;

    if i <> Size(bsgs.base) then
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

  top.from_newss := true;
  return top;
end);


StabChainNewssOp := function (G, options)
  local S;
  # We ignore the options record for now given that they will be GAP options
  # that don't really make any sense for us. In future, could perhaps try and
  # translate, or use our own options if any are given.
  if HasStabChainMutable(G) then
    return StabChainMutable(G);
  else
    S := GAPStabChainFromBSGS(BSGSFromGroup(G));
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

InstallGlobalFunction(ChangeBaseByPointSwap, function (bsgs, new_base)
  local pt, stabilized, i, j;
  for j in [1 .. Size(new_base)] do
    pt := new_base[j];
    stabilized := false;

    for i in [j .. Size(bsgs.base)] do
      stabilized := ForAll(bsgs.stabgens[i], g -> pt ^ g = pt);
      if bsgs.base[i] = pt or stabilized then
        break;
      fi;
    od;

    if bsgs.base[i] = pt then
      # We still want to swap our point closer to the start, but the index will
      # be off since the code below assumes we added a point.
      i := i - 1;
    elif stabilized then
      # When we get here, we are able to insert it after point i as a redundant
      # base point, since we know it is stabilized by the previous group. (This
      # may be at the end of the list.)
      Info(NewssInfo, 3, "inserting redundant base point ", pt, " at ", i);
      NEWSS_InsertRedundantBasePoint(bsgs, i, pt);
    else
      # If we weren't even stabilized by some existing subgroup in the chain,
      # the best we can do is add it to the end of the base, and with trivial
      # stabilizer group
      Info(NewssInfo, 3, "appending trivial base point ", pt, " at ", i);
      NEWSS_AppendTrivialBasePoint(bsgs, pt);
    fi;

    # Then we swap it back to its intended position.
    while i >= j do
      Info(NewssInfo, 3, "swapping ", pt, " at ", i, " with ", bsgs.base[i + 1], " at ", i + 1);
      NEWSS_PerformBaseSwap(bsgs, i);
      i := i - 1;
    od;
  od;

  # If the list supplied is genuinely a base, then there will be a 'tail' of
  # extraneous trivial base points which were in the old base but not the new;
  # we should dispose of them now.
  i := Size(bsgs.base);
  while IsTrivial(bsgs.stabilizers[i]) and not (bsgs.base[i] in new_base) do
    Remove(bsgs.base, i);
    Remove(bsgs.stabgens, i);
    Remove(bsgs.stabilizers, i);
    Remove(bsgs.orbits, i);
    Remove(bsgs.orbitsizes, i);
    i := i - 1;
  od;
end);


# Insert a new point <C>pt</C> at position <C>i + 1</C> in the base of the BSGS
# structure <C>bsgs</C> with the same stabilizer group as in position i. This
# function does not check that bsgs.stabilizers[i] does, in fact, stabilise
# <C>pt</C>.
InstallGlobalFunction(NEWSS_InsertRedundantBasePoint, function (bsgs, i, pt)
  local orb;
  # The stabilizer group is just a copy
  Add(bsgs.base, pt, i + 1);
  Add(bsgs.stabgens, ShallowCopy(bsgs.stabgens[i]), i + 1);
  Add(bsgs.stabilizers, bsgs.stabilizers[i], i + 1);
  # but the orbit requires some actual computation, since it will be different.
  orb := NEWSS_SchreierVector([], 0, bsgs.stabgens[i + 1], [pt]);
  Add(bsgs.orbits, orb.sv, i + 1);
  Add(bsgs.orbitsizes, orb.size, i + 1);
end);

# Append a new base point <C>pt</C> with trivial stabilizer to the chain. T
InstallGlobalFunction(NEWSS_AppendTrivialBasePoint, function (bsgs, pt)
  local orb;
  Add(bsgs.base, pt);
  Add(bsgs.stabgens, [()]);
  Add(bsgs.stabilizers, Group(()));
  orb := [];
  orb[pt] := -1;
  Add(bsgs.orbits, orb);
  Add(bsgs.orbitsizes, 1);
end);



InstallGlobalFunction(ChangeBaseOfBSGS, function (bsgs, new_base)
  ChangeBaseByPointSwap(bsgs, new_base);
end);

InstallGlobalFunction(ChangeBaseByRecomputing, function (bsgs, new_base)
  # For now, we just re-run Schreier–Sims with the given base. Knowing we
  # have a base makes this a lot faster, but in general we can do much better
  # than this.
  bsgs.base := new_base;
  bsgs.sgs := GeneratorsOfGroup(bsgs.group);
  bsgs.stabgens := [];
  bsgs.stabilizers := [];
  bsgs.orbits := [];
  bsgs.orbitsizes := [];
  bsgs.options.known_base := new_base;
  bsgs.options.IsIdentity := NEWSS_IsIdentityByKnownBase;
  bsgs.options.SchreierSims(bsgs);
end);

InstallGlobalFunction(RemoveRedundantGenerators, function (bsgs, keep_initial_gens)
  local i, new_gens, generator, sv, have_shrunk, j, k;

  i := Size(bsgs.base) - 1;
  while i >= 1 do
    new_gens := Difference(bsgs.stabgens[i], bsgs.stabgens[i + 1]);
    for j in [1 .. Size(new_gens)] do
      generator := Remove(new_gens, j);

      if keep_initial_gens and generator in bsgs.initial_gens then
        continue;
      fi;

      sv := NEWSS_SchreierVector([], 0, new_gens, [bsgs.base[i]]).sv;

      have_shrunk := false;
      for j in [1 .. Size(bsgs.orbits[i])] do
        if IsBound(bsgs.orbits[j]) and not IsBound(sv[j]) then
          have_shrunk := true;
          break;
        fi;
      od;

      if have_shrunk then
        Add(new_gens, generator, j);
      else
        Remove(bsgs.sgs, Position(bsgs.sgs, generator));
      fi;
    od;

    i := i - 1;
  od;

  return bsgs;
end);


InstallGlobalFunction(ComputeChainForBSGS, function (bsgs)
  local stabilizer, base_subset, i, gens;

  bsgs.stabgens := [bsgs.sgs];
  bsgs.stabilizers := [bsgs.group];
  bsgs.orbits := [];
  bsgs.orbitsizes := [];

  for i in [1 .. Size(bsgs.base)] do
    # The i-th stabilizer is generated by those generators in the SGS which fix
    # [b_1, ..., b_i], where b_k is the kth element of the base.
    base_subset := bsgs.base { [1 .. i - 1] };
    gens := Filtered(bsgs.sgs, g -> Stabilizes(g, base_subset));
    if Size(gens) = 0 then
      gens := [()];
    fi;
    bsgs.stabgens[i] := gens;

    bsgs.options.SchreierVectorForLevel(bsgs, i);
    ComputeStabForBSGS(bsgs, i);
  od;
end);


InstallGlobalFunction(ConjugateBSGS, function (bsgs, g)
  EnsureBSGSChainComputed(bsgs);
  bsgs.base := List(bsgs.base, b -> b ^ g);
  bsgs.sgs := List(bsgs.sgs, x -> x ^ g);
  bsgs.group := Group(bsgs.sgs);
  ComputeChainForBSGS(bsgs);
end);


InstallGlobalFunction(CopyBSGS, function (bsgs)
  return StructuralCopy(bsgs);
end);


# NEWSS_PerformBaseSwap(bsgs, i)
# Swap base points base[i] and base[i+1] using a randomised algorithm.
InstallGlobalFunction(NEWSS_PerformBaseSwap, function (bsgs, i)
  local T, beta, stab_sv, orb_sv, target_size, gen;

  if i < 1 or i > Size(bsgs.base) - 1 then
    Error("cannot swap base points out of range");
  fi;
  
  if IsBound(bsgs.stabgens[i + 2]) then
    T := ShallowCopy(bsgs.stabgens[i + 2]);
  else
    T := [];
  fi;

  # The bulk of the effort is computing the new stabgens[i+1] and orbits[i+1]
  beta := bsgs.base[i + 1];
  stab_sv := NEWSS_SchreierVector([], 0, bsgs.stabgens[i], [beta]);
  orb_sv := NEWSS_SchreierVector([], 0, T, [bsgs.base[i]]);
  # We know the size to expect by the Orbit-Stabilizer theorem
  target_size := bsgs.orbitsizes[i] * bsgs.orbitsizes[i + 1] / stab_sv.size;

  while orb_sv.size < target_size do
    gen := RandomStabilizerElement(bsgs.stabgens[i], stab_sv.sv, beta);
    if gen <> () then
      Add(T, gen);
      Add(bsgs.sgs, gen);
      orb_sv := NEWSS_ExtendSchreierVector(orb_sv.sv, orb_sv.size, T, gen);
      Print("attempting to adjoin ", gen, "\n");
    fi;
  od;

  # Now perform the actual swapping
  bsgs.base[i + 1] := bsgs.base[i];
  bsgs.base[i] := beta;
  bsgs.stabgens[i + 1] := T;
  bsgs.orbits[i] := stab_sv.sv;
  bsgs.orbitsizes[i + 1] := stab_sv.sv;
  bsgs.orbits[i + 1] := orb_sv.sv;
  bsgs.orbitsizes[i + 1] := orb_sv.size;
end);
