# vim: ft=gap sts=2 et sw=2

# NOrbitStabilizer(X_or_G, alpha)
# Given a a permutation group G with generating set X acting on \Omega, and an
# element alpha of \Omega, compute a record with the following members:
#  orbit:  the orbit of alpha in G,
#  moves:  the element moves[i] is a permutation such that (moves[i])^alpha = O[i],
#  sv:  a Schreier vector for alpha in G,
#  stab:  the stabiliser of alpha in G.
# This corresponds to the procedures OrbitStabilizer, OrbitSv in §4.1 of Holt,
# et al.
NOrbitStabilizer := function (X, alpha)
  local orbit, moves, sv, stab_gens, i, x, x_index, beta, beta_index, image, location;

  orbit := [alpha];
  moves := [()];
  sv := [];
  stab_gens := [];

  # We really just want a generating set, so if we are handed a group pick one
  # out.
  if IsGroup(X) then
    X := GeneratorsOfGroup(X);
  fi;

  # Now initialize the Schreier vector
  for i in [1 .. LargestMovedPoint(X)] do
    sv[i] := 0;
  od;
  sv[alpha] := -1;
  
  # Perform the orbit computation.
  x_index := 1;
  for x in X do
    beta_index := 1;
    for beta in orbit do
      image := beta ^ x;
      location := Position(orbit, image);

      if location = fail then
        Add(orbit, image);
        Add(moves, moves[beta_index] * x);
        sv[image] := x_index;
      else
        AddSet(stab_gens, moves[beta_index] * x * (moves[location])^(-1));
      fi;

      beta_index := beta_index + 1;
    od;
    
    x_index := x_index + 1;
  od;

  return rec(orbit := orbit, moves := moves, sv := sv, stab := Group(stab_gens));
end;

# NOrbit(X_or_G, alpha)
# Given a permutation group G with generating set X acting on \Omega, and an
# element alpha of \Omega, computes the orbit of alpha in G.
NOrbit := function (X, alpha)
  return NOrbitStabilizer(X, alpha).orbit;
end;

# NStabilizer(X_or_G, alpha)
# Given a permutation group G with generating set X acting on \Omega, and an
# element alpha of \Omega, returns the stabilizer of alpha in G; i.e. the
# subgroup of G whose elements fix alpha.
NStabilizer := function (X, alpha)
  return NOrbitStabilizer(X, alpha).stab;
end;

