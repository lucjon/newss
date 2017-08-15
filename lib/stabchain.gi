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
             initial_gens := Immutable(Set(sgs)));
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
    B.chain := [];
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
    current.genlabels := List(bsgs.chain[i].gens, g -> Position(bsgs.sgs, g) + 1);
    current.generators := ShallowCopy(bsgs.chain[i].gens);

    current.orbit := [bsgs.base[i]];
    current.transversal := [];
    current.translabels := [];
    current.transversal[bsgs.base[i]] := ();
    current.translabels[bsgs.base[i]] := 1;

    # We need to recompute the orbits `the other way around', which is how GAP
    # expects them. This could be slightly faster than the original computation
    # since we know the size of orbit to expect.
    to_compute := [bsgs.base[i]];
    while Size(to_compute) > 0 and Size(current.orbit) < bsgs.chain[i].orbit.size do
      pt := Remove(to_compute, 1);
      for gen in bsgs.chain[i].gens do
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

InstallGlobalFunction(ChangeBaseOfBSGS, function (bsgs, new_base)
  # For now, we just re-run Schreier–Sims with the given base. Knowing we
  # have a base makes this a lot faster, but in general we can do much better
  # than this.
  bsgs.base := new_base;
  bsgs.sgs := GeneratorsOfGroup(bsgs.group);
  bsgs.chain := [];
  bsgs.options.known_base := new_base;
  bsgs.options.IsIdentity := NEWSS_IsIdentityByKnownBase;
  bsgs.options.SchreierSims(bsgs);
end);

InstallGlobalFunction(RemoveRedundantGenerators, function (bsgs, keep_initial_gens)
  local i, new_gens, generator, sv, have_shrunk, j, k;

  i := Size(bsgs.base) - 1;
  while i >= 1 do
    new_gens := Difference(bsgs.chain[i].gens, bsgs.chain[i + 1].gens);
    for j in [1 .. Size(new_gens)] do
      generator := Remove(new_gens, j);

      if keep_initial_gens and generator in bsgs.initial_gens then
        continue;
      fi;

      sv := SchreierVectorForOrbit(new_gens, bsgs.base[i]).sv;

      have_shrunk := false;
      for j in [1 .. Size(bsgs.chain[i].orbit.sv)] do
        if IsBound(bsgs.chain[i].orbit.sv[j]) and not IsBound(sv[j]) then
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

  bsgs.chain := [rec(gens := bsgs.sgs, group := bsgs.group)];

  for i in [1 .. Size(bsgs.base)] do
    # The i-th stabilizer is generated by those generators in the SGS which fix
    # [b_1, ..., b_i], where b_k is the kth element of the base.
    base_subset := bsgs.base { [1 .. i - 1] };
    gens := Filtered(bsgs.sgs, g -> Stabilizes(g, base_subset));
    if Size(gens) = 0 then
      gens := [()];
    fi;

    if not IsBound(bsgs.chain[i]) then
      bsgs.chain[i] := rec();
    fi;
    bsgs.chain[i].gens := gens;

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
  
  if IsBound(bsgs.chain[i + 2]) then
    T := ShallowCopy(bsgs.chain[i + 2].gens);
  else
    T := [];
  fi;

  # The bulk of the effort is computing the new stabgens[i+1] and orbits[i+1]
  beta := bsgs.base[i + 1];
  stab_sv := SchreierVectorForOrbit(bsgs.chain[i].gens, beta);
  orb_sv := SchreierVectorForOrbit(T, bsgs.base[i]);
  # We know the size to expect by the Orbit-Stabilizer theorem
  target_size := bsgs.chain[i].orbit.size * bsgs.chain[i + 1].orbit.size / stab_sv.size;

  while orb_sv.size < target_size do
    gen := RandomStabilizerElement(bsgs.chain[i].orbit);
    if gen <> () then
      Add(bsgs.sgs, gen);
      ExtendSchreierVector(orb_sv, gen);
    fi;
  od;

  # Now perform the actual swapping
  bsgs.base[i + 1] := bsgs.base[i];
  bsgs.base[i] := beta;
  bsgs.chain[i + 1].gens := T;
  bsgs.chain[i].orbit := stab_sv;
  bsgs.chain[i + 1].orbit := orb_sv;
end);

InstallGlobalFunction(NEWSS_AppendEmptyChain, function (bsgs)
  Add(bsgs.chain, rec(gens := []));
end);
