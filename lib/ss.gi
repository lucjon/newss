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
#        SchreierVectorForLevel(bsgs, level)
#          Finds a Schreier vector for the basic orbit of the level-th base
#          point in the level-th stabilizer group.
#        ExtendSchreierVector(bsgs, level, gen)
#          Extend the level-th basic orbit after adding a Schreier generator
#          gen.
#
#   *  Helper functions common to many implementations
#
# Only the functions in the first two categories should be considered `public'.


###
### Strategy definitions and high-level functions
###

MakeReadWriteGlobal("NEWSS_MIN_DEGREE_RANDOM");
NEWSS_MIN_DEGREE_RANDOM := 10;

InstallValue(NEWSS_PERMWORD_REPRESENTATION, rec(
  PermFromBasePoint := SchreierVectorWordFromBasePoint,
  Strip := StabilizerChainStripWord,
  AsPerm := PermWordAsPerm,
  MulPerm := Concatenation,
  InvPerm := PermWordInverse,
  LiftPerm := x -> [x],
  ImagePerm := PermWordImage
));

InstallValue(NEWSS_PERM_REPRESENTATION, rec(
  PermFromBasePoint := SchreierVectorPermFromBasePoint,
  Strip := StabilizerChainStrip,
  AsPerm := IdFunc,
  MulPerm := function (arg) return Product(arg); end,
  InvPerm := Inverse,
  LiftPerm := IdFunc,
  ImagePerm := OnPoints
));
 
InstallValue(NEWSS_DEFAULT_OPTIONS, rec(
  SchreierSims := RandomSchreierSims,
  Verify := NEWSS_VerifyByDeterministic,
  ExtendBaseForLevel := NEWSS_FirstMovedPoint,
  SchreierVectorForLevel := NEWSS_SVForLevel,
  ExtendSchreierVector := NEWSS_ExtendSchreierVector,

  perm_representation := NEWSS_PERMWORD_REPRESENTATION,
  fall_back_to_deterministic := true,
  sift_threshold := 8,
  orbits_to_consider := 3
));

InstallValue(NEWSS_DETERMINISTIC_OPTIONS, rec(
  SchreierSims := SchreierSims,
  Verify := ReturnTrue,
  ExtendBaseForLevel := NEWSS_PickFromOrbits,
  SchreierVectorForLevel := NEWSS_SVForLevel,
  ExtendSchreierVector := NEWSS_ExtendSchreierVector,

  # We need this here since a user could specify a random algorithm in their
  # options, even in the case where we would have picked a deterministic one,
  # but might not provide these parameters
  perm_representation := NEWSS_PERMWORD_REPRESENTATION,
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


###
### Implementations of algorithms
###

##
## SchreierSims
##

InstallGlobalFunction(SchreierSims, function (bsgs)
  local i, added_generator, stripped, iterators, g, l, need_to_adjoin, perm;

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
      stripped := bsgs.options.Strip(bsgs, g);

      # If the stripped permutation is not the identity, it was not in the next
      # group --- so we adjoin it.
      perm := bsgs.options.AsPerm(stripped.residue);
      if perm <> () then
        Info(NewssInfo, 3, "Adjoining generator ", g);
        Add(bsgs.sgs, perm);

        # Additionally, if the strip procedure made it to the last iteration,
        # we know it fixes all the existing base points and that we need to
        # extend our basis again.
        if stripped.level > Size(bsgs.base) then
          bsgs.options.ExtendBaseForLevel(bsgs, i, perm);
        fi;

        for l in [i + 1 .. stripped.level] do
          Add(bsgs.stabgens[l], perm);
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
        bsgs.options.ExtendSchreierVector(bsgs, l, stripped.residue);
        ComputeStabForBSGS(bsgs, l);
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


##
## SchreierVectorForLevel
##

InstallGlobalFunction(NEWSS_SchreierVector, function (sv, size, gens, to_compute)
  local pt, gen, image, j;
  if Size(to_compute) = 1 and Size(sv) = 0 then
    sv[to_compute[1]] := -1;
    size := 1;
  fi;

  while Size(to_compute) > 0 do
    pt := Remove(to_compute, 1);
    for j in [1 .. Size(gens)] do
      gen := gens[j];
      image := pt ^ gen;
      if not IsBound(sv[image]) then
        Add(to_compute, image);
        sv[image] := j;
        size := size + 1;
      fi;
    od;
  od;
  
  return rec( sv := sv, size := size );
end);

InstallGlobalFunction(NEWSS_SVForLevel, function (bsgs, i)
  local sv;
  sv := NEWSS_SchreierVector([], 0, bsgs.stabgens[i], [bsgs.base[i]]);
  bsgs.orbits[i] := sv.sv;
  bsgs.orbitsizes[i] := sv.size;
  return sv.sv;
end);


if IsBound(ORB) then
  InstallGlobalFunction(NEWSS_SVFromOrb, function (bsgs, i)
    local O, orb_sv, sv, j, size;
    O := Orb(bsgs.stabgens[i], bsgs.base[i], OnPoints, rec( schreier := true ));
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
    bsgs.orbits[i] := sv;
    bsgs.orbitsizes[i] := size;
    return sv;
  end);
fi;

###
### ExtendSchreierVector
###

InstallGlobalFunction(NEWSS_ExtendSVByRecomputing, function (bsgs, i, gen)
  bsgs.options.SchreierVectorForLevel(bsgs, i);
end);

InstallGlobalFunction(NEWSS_ExtendSchreierVector, function (bsgs, i, gen)
  local sv, to_compute, size, n, pt, image, j, g;

  if not IsBound(bsgs.orbits[i]) then
    NEWSS_ExtendSVByRecomputing(bsgs, i, gen);
  fi;

  sv := bsgs.orbits[i];
  to_compute := [];
  size := bsgs.orbitsizes[i];
  n := Size(bsgs.stabgens[i]);

  for pt in [1 .. Size(sv)] do
    if not IsBound(sv[pt]) then
      continue;
    fi;

    image := pt ^ gen;
    if not IsBound(sv[image]) then
      Add(to_compute, image);
      sv[image] := n;
      size := size + 1;
    fi;
  od;

  bsgs.orbitsizes[i] := NEWSS_SchreierVector(sv, size, bsgs.stabgens[i], to_compute).size;
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

    bsgs.options.SchreierVectorForLevel(bsgs, i);
    ComputeStabForBSGS(bsgs, i);
  od;
end);

# ComputeStabForBSGS(bsgs, i)
# Given a BSGS structure, compute the basic stabilizer with the given
# generators for the i-th stabilizer group.
InstallGlobalFunction(ComputeStabForBSGS, function (bsgs, i)
  local base_subset, gens, orbstab, j;

  # We special case the first entry.
  if i = 1 then
    bsgs.stabgens[i] := bsgs.sgs;
    bsgs.stabilizers[i] := bsgs.group;
  else
    bsgs.stabilizers[i] := Group(bsgs.stabgens[i]);
  fi;
end);


InstallGlobalFunction(SchreierGenerators, function (bsgs, i)
  # Take this out here so it has a name; it's easier to read the profiling
  # then.
  local SchreierGenerators_Next;
  SchreierGenerators_Next := function (iter)
    local x, u_beta_x, gen, image;

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

      iter!.u_beta := bsgs.options.PermFromBasePoint(bsgs.stabgens[i],
                                                     bsgs.orbits[i],
                                                     iter!.orbit_index);
      iter!.gen_iter := Iterator(bsgs.stabgens[i]);
    fi;

    x := bsgs.options.LiftPerm(NextIterator(iter!.gen_iter));
    image := bsgs.options.ImagePerm(iter!.orbit_index, iter!.u_beta);
    u_beta_x := bsgs.options.PermFromBasePoint(bsgs.stabgens[i],
                                               bsgs.orbits[i],
                                               image);

    gen := bsgs.options.MulPerm(iter!.u_beta, x, bsgs.options.InvPerm(u_beta_x));
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


InstallGlobalFunction(StabilizerChainStripWord, function (bsgs, g)
  local h, i, beta, u;
  h := g;
  i := 0;

  if not IsList(h) then
    h := [h];
  fi;

  for i in [1 .. Size(bsgs.base)] do
    beta := PermWordPreImage(h, bsgs.base[i]);
    if not IsBound(bsgs.orbits[i][beta]) then
      return rec(residue := h, level := i);
    fi;
    
    u := SchreierVectorWordFromBasePoint(bsgs.stabgens[i], bsgs.orbits[i], beta);
    h := Concatenation(u, ShallowCopy(h));
  od;

  return rec(residue := h, level := i + 1);
end);
