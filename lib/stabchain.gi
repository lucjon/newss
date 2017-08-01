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
  ExtendSchreierVector := NEWSS_ExtendSchreierVector,

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
  ExtendSchreierVector := NEWSS_ExtendSchreierVector,

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
  local top, prev, current, next, i, j, gen;

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
    current.transversal := [()];
    current.translabels := [1];
    for j in [1 .. Size(bsgs.orbits[i])] do
      if j <> bsgs.base[i] and IsBound(bsgs.orbits[i][j]) then
        gen := bsgs.stabgens[i][bsgs.orbits[i][j]];
        Add(current.orbit, j);
        current.transversal[j] := gen;
        current.translabels[j] := Position(bsgs.sgs, gen) + 1;
      fi;
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
  local ConjugateByG;
  ConjugateByG := function (L)
    local i;
    for i in [1 .. Size(L)] do
      if IsBound(L[i]) then
        L[i] := L[i] ^ g;
      fi;
    od;
  end;

  EnsureBSGSChainComputed(bsgs);
  ConjugateByG(bsgs.base);
  ConjugateByG(bsgs.sgs);
  bsgs.group := Group(bsgs.sgs);
  ComputeChainForBSGS(bsgs);
end);
