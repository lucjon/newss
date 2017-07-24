# vim: ft=gap sts=2 et sw=2

# The algorithm is split into a few different parts, each with multiple
# possible implementations, which can be varied depending on e.g. what is known
# about the group we are working with. A `strategy' is a record describing
# which implementation of each algorithm should be used.
# This file is laid out as follows:
#   *  Definitions of strategies and high level user functions
#        These high level functions create and wrap strategies.
#   *  Functions for manipulating stabiliser chains
#
#   *  Implementations of each algorithm
#        SchreierSims(bsgs)
#          The main Schreier-Sims algorithm.
#        Verify(bsgs)
#          The verification routine, which generally ensures the resulting
#          stabiliser chain is correct.
#        ExtendBaseForLevel(bsgs, level, culprit)
#          Finds a new base point when adding a new generator at the given
#          level (or 0 if we are starting from an empty base). If known, the
#          argument culprit is a permutation which fixes all the existing base
#          points; otherwise it has the value false.
#
#   *  Helper functions common to many implementations
#
# Only the functions in the first two categories should be considered `public'.


###
### Strategy definitions and high-level functions
###

MakeReadWriteGlobal("NEWSS_MIN_DEGREE_RANDOM");
NEWSS_MIN_DEGREE_RANDOM := 10;

InstallValue(NEWSS_DEFAULT_OPTIONS, rec(
  SchreierSims := RandomSchreierSims,
  Verify := NEWSS_VerifyByDeterministic,
  ExtendBaseForLevel := NEWSS_FirstMovedPoint,

  fall_back_to_deterministic := true,
  sift_threshold := 8,
  orbits_to_consider := 3
));

InstallValue(NEWSS_DETERMINISTIC_OPTIONS, rec(
  SchreierSims := SchreierSims,
  Verify := ReturnTrue,
  ExtendBaseForLevel := NEWSS_PickFromOrbits,

  # We need this here since a user could specify a random algorithm in their
  # options, even in the case where we would have picked a deterministic one,
  # but might not provide these parameters
  fall_back_to_deterministic := NEWSS_DEFAULT_OPTIONS.fall_back_to_deterministic,
  sift_threshold := NEWSS_DEFAULT_OPTIONS.sift_threshold,
  orbits_to_consider := NEWSS_DEFAULT_OPTIONS.orbits_to_consider
));


InstallGlobalFunction(BSGS, function (group, base, sgs)
  return rec(group := group, base := base, sgs := sgs,
             initial_gens := Immutable(Set(sgs)), has_chain := false);
end);

InstallGlobalFunction(BSGSFromGAP, function (group)
  local sc;
  sc := StabChain(group);
  return BSGS(group, BaseStabChain(sc), StrongGeneratorsStabChain(sc));
end);

InstallGlobalFunction(BSGSFromGroup, function (arg)
  local group, B, base_options, options, key;

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
  if NrMovedPoints(group) <= NEWSS_MIN_DEGREE_RANDOM then
    base_options := ShallowCopy(NEWSS_DETERMINISTIC_OPTIONS);
  else
    base_options := ShallowCopy(NEWSS_DEFAULT_OPTIONS);
  fi;

  if HasSize(group) then
    base_options.Verify := NEWSS_VerifyByOrder;
  fi;

  # If they have given us a strategy, use it, filling in any missing fields
  # from the heuristically-determined one.
  if Size(arg) > 1 then
    options := ShallowCopy(arg[2]);
    for key in RecNames(base_options) do
      if not IsBound(options.(key)) then
        options.(key) := base_options.(key);
      fi;
    od;
  else
    options := base_options;
  fi;

  B := BSGS(group, [], ShallowCopy(GeneratorsOfGroup(group)));
  B.options := options;

  options.SchreierSims(B);

  if not options.Verify(B) and options.fall_back_to_deterministic then
    # If we can't verify the chain is complete, then run the deterministic
    # algorithm to make sure we don't return an incomplete chain.
    NEWSS_DETERMINISTIC_OPTIONS.SchreierSims(B);
  fi;

  return B;
end);


###
### Functions for manipulating stabiliser chains
###

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

      sv := NOrbitStabilizer(new_gens, bsgs.base[i], OnPoints, true).sv;

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


###
### Implementations of algorithms
###

##
## SchreierSims
##

InstallGlobalFunction(SchreierSims, function (bsgs)
  local i, added_generator, stripped, iterators, g, l;

  bsgs.sgs := List(bsgs.sgs);
  if not IsBound(bsgs.stabgens) then
    bsgs.stabgens := [];
  fi;

  if Size(bsgs.base) = 0 then
    bsgs.options.ExtendBaseForLevel(bsgs, 0, false);
  fi;

  ComputeChainForBSGS(bsgs);

  # So Strip() etc., don't interfere
  bsgs.has_chain := true;

  # The condition we need to verify for our structure to be a genuine BSGS is
  # that the stabiliser of the i-th base point in the i-th "stabiliser" is the
  # (i+1)-th stabiliser. We check this by checking the membership of each
  # generator of the stabiliser is there, a generating set being given by
  # Schreier's lemma (see SchreierGenerators).
  # This condition will be an invariant of this loop; it is true at the start
  # since by the time we are here, we know the (k+ 1)th stabiliser is trivial
  # (k being the size of the base). Since this holds, the StabilizerChainStrip
  # procedure can still be used to check membership in the (i+1)th group.
  i := Size(bsgs.base);

  iterators := [];
  while i >= 1 do
    Info(NewssInfo, 3, "Starting SS loop (index ", i, ")");
    added_generator := false;

    if not IsBound(iterators[i]) then
      iterators[i] := SchreierGenerators(bsgs, i);
    fi;

    for g in iterators[i] do
      if g = () then continue; fi;
      stripped := StabilizerChainStrip(bsgs, g);

      # If the stripped permutation is not the identity, it was not in the next
      # group --- so we adjoin it.
      if stripped.residue <> () then
        Info(NewssInfo, 3, "Adjoining generator ", g);

        Add(bsgs.sgs, stripped.residue);

        # Additionally, if the strip procedure made it to the last iteration,
        # we know it fixes all the existing base points and that we need to
        # extend our basis again.
        if stripped.level > Size(bsgs.base) then
          bsgs.options.ExtendBaseForLevel(bsgs, i, stripped.residue);
        fi;

        for l in [i + 1 .. stripped.level] do
          Add(bsgs.stabgens[l], stripped.residue);
          ComputeStabOrbForBSGS(bsgs, l);
          # We might be able to avoid this as well.
          iterators[l] := SchreierGenerators(bsgs, l);
        od;
        i := stripped.level;
        added_generator := true;
        break;
      fi;
    od;

    # If we didn't adjoin any more generators, they must all have been in the
    # group and we can move on to verify the condition for the next group.
    if not added_generator then
      i := i - 1;
    fi;
  od;

  # Once we finish the loop, we know we have a correct base, SGS and stabilizer chain.
  Info(NewssInfo, 2, "Computed stabiliser chain.");
  return bsgs;
end);


InstallGlobalFunction(RandomSchreierSims, function (bsgs)
  local no_sifted_this_round, can_verify_order, verified, g, stripped, l;

  bsgs.sgs := List(bsgs.sgs);
  bsgs.stabgens := [];

  if Size(bsgs.base) = 0 then
    bsgs.options.ExtendBaseForLevel(bsgs, 0, false);
  fi;

  ComputeChainForBSGS(bsgs);
  bsgs.has_chain := true;

  no_sifted_this_round := 0;
  can_verify_order := HasSize(bsgs.group);
  verified := false;

  while no_sifted_this_round < bsgs.options.sift_threshold do
    g := PseudoRandom(bsgs.group);
    stripped := StabilizerChainStrip(bsgs, g);

    if stripped.residue <> () then
      Add(bsgs.sgs, stripped.residue);

      if stripped.level > Size(bsgs.base) then
        bsgs.options.ExtendBaseForLevel(bsgs, stripped.level, stripped.residue);
      fi;

      for l in [2 .. stripped.level] do
        Add(bsgs.stabgens[l], stripped.residue);
        ComputeStabOrbForBSGS(bsgs, l);
      od;
      no_sifted_this_round := 0;
    else
      no_sifted_this_round := no_sifted_this_round + 1;
    fi;

    # We know we're correct if the orders match.
    if can_verify_order and Product(bsgs.orbitsizes) = Size(bsgs.group) then
      break;
    fi;
  od;

  return bsgs;
end);

##
## Verify
##

InstallGlobalFunction(NEWSS_VerifyByOrder, function (bsgs)
  return HasSize(bsgs.group) and Product(bsgs.orbitsizes) = Size(bsgs.group);
end);

InstallGlobalFunction(NEWSS_VerifyByDeterministic, function (bsgs)
  NEWSS_DETERMINISTIC_OPTIONS.SchreierSims(bsgs);
  return true;
end);

##
## ExtendBaseForLevel
##

InstallGlobalFunction(NEWSS_FirstMovedPoint, function (bsgs, level, culprit)
  local i, point;

  if culprit = false then
    if level = 0 then
      # If we have an empty generating set we deserve to bail out here.
      culprit := bsgs.sgs[1];
    else
      i := Size(bsgs.sgs);
      culprit := bsgs.sgs[i];

      while Stabilizes(culprit, Reversed(bsgs.base)) and i > 0 do
        i := i - 1;
        culprit := bsgs.sgs[i];
      od;

      if i = 0 then
        Error("could not find a generator which does not fix the base");
      fi;
    fi;
  fi;

  point := First(MovedPoints(culprit), pt -> not (pt in bsgs.base));

  if point = fail then
    Error("could not find point not fixed by culprit");
  fi;

  Add(bsgs.base, point);
  if level > 0 then
    Add(bsgs.stabgens, []);
  fi;

  return point;
end);

InstallGlobalFunction(NEWSS_PickFromOrbits, function (bsgs, level, culprit)
  local point, orbit_level, min_level, i;

  if culprit = false or not IsBound(bsgs.orbits) or Size(bsgs.orbits) < level then
    return NEWSS_FirstMovedPoint(bsgs, level, culprit);
  fi;

  point := 0;
  orbit_level := level;
  min_level := Maximum(1, level - bsgs.options.orbits_to_consider - 1);

  repeat
    for i in [1 .. Size(bsgs.orbits[orbit_level])] do
      if IsBound(bsgs.orbits[orbit_level][i]) and bsgs.orbits[orbit_level][i] <> -1 then
        if i ^ culprit <> i then
          point := i;
          break;
        fi;
      fi;
    od;

    if point <> 0 then
      break;
    fi;
    orbit_level := orbit_level - 1;
  until orbit_level < min_level;

  if point = 0 then
    point := First(MovedPoints(culprit), pt -> not (pt in bsgs.base));
  fi;

  Add(bsgs.base, point);
  Add(bsgs.stabgens, []);

  return point;
end);


###
### Helper functions
###

# EnsureBSGSChainComputed(bsgs)
# Given a BSGS structure, compute the basic stabilizers and basic orbits if
# they have not already been (see ComputeChainForBSGS). Returns nothing; the
# chain is stored in the BSGS structure (see the function BSGS).
InstallGlobalFunction(EnsureBSGSChainComputed, function (bsgs)
  if not bsgs.has_chain then
    ComputeChainForBSGS(bsgs);
    bsgs.has_chain := true;
  fi;
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

    ComputeStabOrbForBSGS(bsgs, i);
  od;
end);

# ComputeStabOrbForBSGS(bsgs, i)
# Given a BSGS structure, compute the basic stabilizer with the given
# generators and the corresponding basic orbit for the i-th stabilizer group.
InstallGlobalFunction(ComputeStabOrbForBSGS, function (bsgs, i)
  local base_subset, gens, orbstab, j;
  Info(NewssInfo, 3, "Computing staborb for ", bsgs, " index ", i);

  # We special case the first entry.
  if i = 1 then
    bsgs.stabgens[i] := bsgs.sgs;
    bsgs.stabilizers[i] := bsgs.group;
  else
    bsgs.stabilizers[i] := Group(bsgs.stabgens[i]);
  fi;

  # Then compute the orbit.
  orbstab := NOrbitStabilizer(bsgs.stabgens[i], bsgs.base[i], OnPoints, true);
  bsgs.orbits[i] := orbstab.sv;
  bsgs.orbitsizes[i] := Size(orbstab.orbit);
end);


InstallGlobalFunction(SchreierGenerators, function (bsgs, i)
  # Take this out here so it has a name; it's easier to read the profiling
  # then.
  local SchreierGenerators_Next;
  SchreierGenerators_Next := function (iter)
    local x, u_beta_x, gen;

    if iter!.gen_iter = false or IsDoneIterator(iter!.gen_iter) then
      while iter!.orbit_index <= Size(bsgs.orbits[i]) do
        iter!.orbit_index := iter!.orbit_index + 1;
        if IsBound(bsgs.orbits[i][iter!.orbit_index]) then
          break;
        fi;
      od;

      if iter!.orbit_index > Size(bsgs.orbits[i]) then
        # Quite messy. Unfortunately checking for this case properly in
        # IsDoneIterator would get even messier.
        return ();
      fi;

      iter!.u_beta := SchreierVectorPermFromBasePoint(bsgs.stabgens[i],
                                                      bsgs.orbits[i],
                                                      iter!.orbit_index);
      iter!.gen_iter := Iterator(bsgs.stabgens[i]);
    fi;

    x := NextIterator(iter!.gen_iter);
    u_beta_x := SchreierVectorPermFromBasePoint(bsgs.stabgens[i],
                                                bsgs.orbits[i],
                                                iter!.orbit_index ^ iter!.u_beta);

    gen := iter!.u_beta * x * u_beta_x^(-1);
    Info(NewssInfo, 3, "Yielding Schreier gen. ", gen, " for stab ", i, " = <", bsgs.stabgens[i], ">");
    return gen;
  end;

  return IteratorByFunctions(rec(
    gen_iter := false,
    orbit_index := 0,
    NextIterator := SchreierGenerators_Next,
    IsDoneIterator := function (iter)
      return iter!.orbit_index > Size(bsgs.orbits[i]) and
             iter!.gen_iter <> false and IsDoneIterator(iter!.gen_iter);
    end,
    ShallowCopy := function (iter)
      return rec(
        gen_iter := ShallowCopy(iter!.gen_iter),
        orbit_index := iter!.orbit_index,
        orbit := iter!.orbit,
        NextIterator := iter!.NextIterator,
        IsDoneIterator := iter!.IsDoneIterator,
        ShallowCopy := iter!.ShallowCopy,
        PrintObj := iter!.PrintObj
      );
    end,
    PrintObj := function (iter)
      Print("<iterator over Schreier generators [group ", i, "]>");
    end));
end);


# StabilizerChainStrip(bsgs, g)
InstallGlobalFunction(StabilizerChainStrip, function (bsgs, g)
  local h, i, beta, u;
  h := g;
  i := 0;

  for i in [1 .. Size(bsgs.base)] do
    beta := bsgs.base[i] / h;
    if not IsBound(bsgs.orbits[i][beta]) then
      return rec(residue := h, level := i);
    fi;
    
    u := SchreierVectorPermFromBasePoint(bsgs.stabgens[i], bsgs.orbits[i], beta);
    h := u * h;
  od;

  return rec(residue := h, level := i + 1);
end);

