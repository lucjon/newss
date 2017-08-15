# vim: ft=gap sts=2 et sw=2

# The algorithm is split into a few different parts, each with multiple
# possible implementations, which can be varied depending on e.g. what is known
# about the group we are working with. A `strategy' is a record describing
# which implementation of each algorithm should be used.
#
# This file contains the private implementations of the core algorithms, in
# particular the Schreier-Sims algorithms. For the high-level functions which
# create and manipulate stabilizer chains, see stabchain.g[di]. For functions
# which perform actual group computations using these chains, see group.g[di].
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
#        SchreierVectorForLevel(bsgs, level)
#          Finds a Schreier vector for the basic orbit of the level-th base
#          point in the level-th stabilizer group.
#        ExtendSchreierVector(bsgs, level, gen)
#          Extend the level-th basic orbit after adding a Schreier generator
#          gen.
#        IsIdentity(bsgs, perm_rep)
#          Takes a permutation in our working representation and determines if
#          it is the identity. If it is, return rec(is_identity := true). If
#          not, return rec(is_identity := false, perm := p), where p is a GAP
#          permutation corresponding to perm_rep.
#
#   *  Helper functions common to many implementations
#
# Only the functions in the first two categories should be considered `public'.

###
### Implementations of algorithms
###

##
## SchreierSims
##

InstallGlobalFunction(SchreierSims, function (bsgs)
  local i, added_generator, stripped, iterators, g, l, need_to_adjoin, perm, id_result;

  if not IsBound(bsgs.chain) then
    bsgs.chain := [];
  fi;

  bsgs.sgs := Filtered(bsgs.sgs, x -> x <> ());
  if Size(bsgs.sgs) = 0 then
    # The trivial group!
    return;
  fi;


  if Size(bsgs.base) = 0 then
    bsgs.options.ExtendBaseForLevel(bsgs, 0, false);
  fi;

  ComputeChainForBSGS(bsgs);

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
      stripped := bsgs.options.Strip(bsgs, g);

      # If the stripped permutation is not the identity, it was not in the next
      # group --- so we adjoin it.
      id_result := bsgs.options.IsIdentity(bsgs, stripped.residue);
      if not id_result.is_identity then
        Info(NewssInfo, 3, "Adjoining generator ", g);
        perm := id_result.perm;
        Add(bsgs.sgs, perm);

        # Additionally, if the strip procedure made it to the last iteration,
        # we know it fixes all the existing base points and that we need to
        # extend our basis again.
        if stripped.level > Size(bsgs.base) then
          bsgs.options.ExtendBaseForLevel(bsgs, i, perm);
        fi;

        for l in [i + 1 .. stripped.level] do
          Add(bsgs.chain[l].gens, perm);
          bsgs.options.ExtendSchreierVector(bsgs, l, perm);
          ComputeStabForBSGS(bsgs, l);
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

  if Size(bsgs.base) = 0 then
    bsgs.options.ExtendBaseForLevel(bsgs, 0, false);
  fi;

  ComputeChainForBSGS(bsgs);

  no_sifted_this_round := 0;
  can_verify_order := HasSize(bsgs.group);
  verified := false;

  while no_sifted_this_round < bsgs.options.sift_threshold do
    g := PseudoRandom(bsgs.group);
    stripped := StabilizerChainStrip(bsgs, g);

    if not bsgs.options.IsIdentity(bsgs, stripped.residue).is_identity then
      Add(bsgs.sgs, stripped.residue);

      if stripped.level > Size(bsgs.base) then
        bsgs.options.ExtendBaseForLevel(bsgs, stripped.level, stripped.residue);
      fi;

      for l in [2 .. stripped.level] do
        Add(bsgs.chain[l].gens, stripped.residue);
        bsgs.options.ExtendSchreierVector(bsgs, l, stripped.residue);
        ComputeStabForBSGS(bsgs, l);
      od;
      no_sifted_this_round := 0;
    else
      no_sifted_this_round := no_sifted_this_round + 1;
    fi;

    # We know we're correct if the orders match.
    if can_verify_order and StabilizerChainOrderNC(bsgs) = Size(bsgs.group) then
      break;
    fi;
  od;

  return bsgs;
end);


##
## IsIdentity
##
InstallGlobalFunction(NEWSS_IsIdentityByMul, function (bsgs, perm)
  if not IsPerm(perm) then
    perm := bsgs.options.AsPerm(perm);
  fi;

  return rec( is_identity := perm = (), perm := perm );
end);

InstallGlobalFunction(NEWSS_IsIdentityByKnownBase, function (bsgs, perm_rep)
  local image_fn, b;

  # In RandomSchreierSims, we don't use the permutation representations.
  if IsPerm(perm_rep) then
    image_fn := OnPoints;
  else
    image_fn := bsgs.options.ImagePerm;
  fi;

  for b in bsgs.options.known_base do
    if image_fn(b, perm_rep) <> b then
      if not IsPerm(perm_rep) then
        perm_rep := bsgs.options.AsPerm(perm_rep);
      fi;

      return rec(
        is_identity := false,
        perm := perm_rep
      );
    fi;
  od;

  # If we get here, it must be the identity since we haven't moved any base
  # points.
  return rec( is_identity := true );
end);


##
## Verify
##

InstallGlobalFunction(NEWSS_VerifyByOrder, function (bsgs)
  return HasSize(bsgs.group) and StabilizerChainOrderNC(bsgs) = Size(bsgs.group);
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
    NEWSS_AppendEmptyChain(bsgs);
  fi;

  return point;
end);

InstallGlobalFunction(NEWSS_PickAscending, function (bsgs, level, culprit)
  local i;
  if culprit = false then
    return NEWSS_FirstMovedPoint(bsgs, level, culprit);
  else
    for i in [1 .. LargestMovedPoint(bsgs.group)] do
      if i ^ culprit <> i then
        Add(bsgs.base, i);
        if level > 0 then
          NEWSS_AppendEmptyChain(bsgs);
        fi;
        return i;
      fi;
    od;
    
    Error("could not find point not fixed by culprit");
  fi;
end);

InstallGlobalFunction(NEWSS_PickFromOrbits, function (bsgs, level, culprit)
  local point, orbit_level, min_level, i, sv;

  if culprit = false or not IsBound(bsgs.chain) or Size(bsgs.chain) < level then
    return NEWSS_FirstMovedPoint(bsgs, level, culprit);
  fi;

  point := 0;
  orbit_level := level;
  min_level := Maximum(1, level - bsgs.options.orbits_to_consider - 1);

  repeat
    sv := bsgs.chain[orbit_level].orbit.sv;
    for i in [1 .. Size(sv)] do
      if IsBound(sv[i]) and sv[i] <> -1 then
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
  NEWSS_AppendEmptyChain(bsgs);

  return point;
end);


##
## SchreierVectorForLevel
##


InstallGlobalFunction(NEWSS_SVForLevel, function (bsgs, i)
  local sv;
  sv := SchreierVectorForOrbit(bsgs.chain[i].gens, bsgs.base[i]);
  bsgs.chain[i].orbit := sv;
  return sv;
end);


if IsBound(ORB) then
  InstallGlobalFunction(NEWSS_SVFromOrb, function (bsgs, i)
    local O, orb_sv, sv, j, size;
    O := Orb(bsgs.chain[i].gens, bsgs.base[i], OnPoints, rec( schreier := true ));
    orb_sv := Enumerate(O)!.schreiergen;
    size := 1;

    sv := [];
    for j in [1 .. Size(O!.tab)] do
      if O!.tab[j] <> 0 then
        sv[j] := orb_sv[O!.tab[j]];
        size := size + 1;
      fi;
    od;

    # orb uses fail instead of -1 for this sentinel
    sv[bsgs.base[i]] := -1;
    bsgs.chain[i].orbit := rec(sv := sv, size := size);
    return sv;
  end);
fi;

###
### ExtendSchreierVector
###

InstallGlobalFunction(NEWSS_ExtendSV, function (bsgs, i, gen)
  local sv;
  if not IsBound(bsgs.chain[i].orbit) then
    bsgs.options.SchreierVectorForLevel(bsgs, i);
  else
    ExtendSchreierVector(bsgs.chain[i].orbit, gen);
  fi;
end);


###
### Helper functions
###

# EnsureBSGSChainComputed(bsgs)
# Given a BSGS structure, compute the basic stabilizers and basic orbits if
# they have not already been (see ComputeChainForBSGS). Returns nothing; the
# chain is stored in the BSGS structure (see the function BSGS).
InstallGlobalFunction(EnsureBSGSChainComputed, function (bsgs)
  if not IsBound(bsgs.chain) then
    ComputeChainForBSGS(bsgs);
  fi;
end);

# ComputeStabForBSGS(bsgs, i)
# Given a BSGS structure, compute the basic stabilizer with the given
# generators for the i-th stabilizer group.
InstallGlobalFunction(ComputeStabForBSGS, function (bsgs, i)
  local base_subset, gens, orbstab, j;

  # We special case the first entry.
  if i = 1 then
    bsgs.chain[i].gens := bsgs.sgs;
    bsgs.chain[i].group := bsgs.group;
  else
    bsgs.chain[i].group := Group(bsgs.chain[i].gens);
  fi;
end);


InstallGlobalFunction(SchreierGenerators, function (bsgs, i)
  # Take this out here so it has a name; it's easier to read the profiling
  # then.
  local SchreierGenerators_Next;
  SchreierGenerators_Next := function (iter)
    local x, u_beta_x, gen, image, sv, chain;

    chain := bsgs.chain[i];
    if iter!.gen_iter = false or IsDoneIterator(iter!.gen_iter) then
      sv := chain.orbit.sv;
      while iter!.orbit_index <= Size(sv) do
        iter!.orbit_index := iter!.orbit_index + 1;
        if IsBound(sv[iter!.orbit_index]) then
          break;
        fi;
      od;

      if iter!.orbit_index > Size(sv) then
        # Quite messy. Unfortunately checking for this case properly in
        # IsDoneIterator would get even messier.
        return ();
      fi;

      iter!.u_beta := bsgs.options.PermFromBasePoint(chain.orbit,
                                                     iter!.orbit_index);
      iter!.gen_iter := Iterator(chain.gens);
    fi;

    x := bsgs.options.LiftPerm(NextIterator(iter!.gen_iter));
    image := bsgs.options.ImagePerm(iter!.orbit_index, iter!.u_beta);
    u_beta_x := bsgs.options.PermFromBasePoint(chain.orbit, image);

    gen := bsgs.options.MulPerm(iter!.u_beta, x, bsgs.options.InvPerm(u_beta_x));
    Info(NewssInfo, 3, "Yielding Schreier gen. ", gen, " for stab ", i, " = <", chain.gens, ">");
    return gen;
  end;

  return IteratorByFunctions(rec(
    gen_iter := false,
    orbit_index := 0,
    NextIterator := SchreierGenerators_Next,
    IsDoneIterator := function (iter)
      return iter!.orbit_index > Size(bsgs.chain[i].orbit.sv) and
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


InstallGlobalFunction(StabilizerChainStrip, function (bsgs, g)
  local h, i, beta, u;
  h := g;
  i := 0;

  for i in [1 .. Size(bsgs.base)] do
    beta := bsgs.base[i] / h;
    if not IsBound(bsgs.chain[i].orbit.sv[beta]) then
      return rec(residue := h, level := i);
    fi;
    
    u := SchreierVectorPermFromBasePoint(bsgs.chain[i].orbit, beta);
    h := u * h;
  od;

  return rec(residue := h, level := i + 1);
end);


InstallGlobalFunction(StabilizerChainStripWord, function (bsgs, g)
  local h, i, beta, u;
  h := g;
  i := 0;

  if not IsList(h) then
    h := [h];
  fi;

  for i in [1 .. Size(bsgs.base)] do
    beta := PermWordPreImage(bsgs.base[i], h);
    if not IsBound(bsgs.chain[i].orbit.sv[beta]) then
      return rec(residue := h, level := i);
    fi;
    
    u := SchreierVectorWordFromBasePoint(bsgs.chain[i].orbit, beta);
    h := Concatenation(u, ShallowCopy(h));
  od;

  return rec(residue := h, level := i + 1);
end);
