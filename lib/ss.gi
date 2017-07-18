# vim: ft=gap sts=2 et sw=2

InstallGlobalFunction(BSGS, function (group, base, sgs)
  return rec(group := group, base := base, sgs := sgs,
             initial_gens := Immutable(Set(sgs)), has_chain := false);
end);

InstallGlobalFunction(BSGSFromGAP, function (group)
  local sc;
  sc := StabChain(group);
  return BSGS(group, BaseStabChain(sc), StrongGeneratorsStabChain(sc));
end);

MakeReadWriteGlobal("BSGS_MIN_DEGREE_RANDOM");
MakeReadWriteGlobal("BSGS_RANDOM_SS_THRESHOLD");
BSGS_MIN_DEGREE_RANDOM := 10;
BSGS_RANDOM_SS_THRESHOLD := 8;

InstallGlobalFunction(BSGSFromGroup, function (group)
  local B;

  B := BSGS(group, [], ShallowCopy(GeneratorsOfGroup(group)));

  if LargestMovedPoint(group) <= BSGS_MIN_DEGREE_RANDOM or
     not RandomSchreierSims(B, BSGS_RANDOM_SS_THRESHOLD).verified then
    SchreierSims(B);
  fi;

  return B;
end);

InstallGlobalFunction(SchreierSims, function (bsgs)
  local i, added_generator, stripped, iterators, g, l;

  bsgs.sgs := List(bsgs.sgs);
  if not IsBound(bsgs.stabgens) then
    bsgs.stabgens := [];
  fi;
  ExtendBaseIfStabilized(bsgs);
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

        # Additionally, if the strip procedure made it to the last iteration,
        # we know it fixes all the existing base points and that we need to
        # extend our basis again.
        if stripped.level > Size(bsgs.base) then
          ExtendBase(bsgs, stripped.residue);
        fi;

        Add(bsgs.sgs, stripped.residue);

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


# RandomSchreierSims(bsgs, w)
InstallGlobalFunction(RandomSchreierSims, function (bsgs, w)
  local no_sifted_this_round, g, stripped, l, verified;

  bsgs.sgs := List(bsgs.sgs);
  bsgs.stabgens := [];
  ExtendBaseIfStabilized(bsgs);
  ComputeChainForBSGS(bsgs);
  bsgs.has_chain := true;

  no_sifted_this_round := 0;
  verified := false;

  while no_sifted_this_round < w do
    g := PseudoRandom(bsgs.group);
    stripped := StabilizerChainStrip(bsgs, g);

    if stripped.residue <> () then
      if stripped.level > Size(bsgs.base) then
        ExtendBase(bsgs, stripped.residue);
      fi;

      Add(bsgs.sgs, stripped.residue);

      for l in [2 .. stripped.level] do
        Add(bsgs.stabgens[l], stripped.residue);
        ComputeStabOrbForBSGS(bsgs, l);
      od;
      no_sifted_this_round := 0;
    else
      no_sifted_this_round := no_sifted_this_round + 1;
    fi;

    # We know we're correct if the orders match.
    if HasSize(bsgs.group) and Product(bsgs.orbitsizes) = Size(bsgs.group) then
      verified := true;
      break;
    fi;
  od;

  return rec(bsgs := bsgs, verified := verified);
end);


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

# ExtendBaseIfStabilized(bsgs)
# Extends the base of a BSGS structure until no permutation in the SGS fixes
# all of the base points.
InstallGlobalFunction(ExtendBaseIfStabilized, function (bsgs)
  local s;

  for s in bsgs.sgs do
    if s <> () and Stabilizes(s, bsgs.base) then
      ExtendBase(bsgs, s);
    fi;
  od;
end);

# ExtendBase(bsgs, culprit)
# Extends the base of a BSGS structure to include a new point, given a
# permutation culprit which fixes all current base points.
InstallGlobalFunction(ExtendBase, function (bsgs, culprit)
  local x;
  # First, find an appropriate point to add (there must be one here)
  x := Difference(MovedPoints(culprit), bsgs.base)[1];
  Info(NewssInfo, 3, "Extending base to include ", x);
  # Then do the bookkeeping
  Add(bsgs.base, x);
  Add(bsgs.stabgens, []);
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
