# vim: ft=gap sts=2 et sw=2

# NOrbitStabilizer(X_or_G, alpha, action, compute_sv)
# Given a a permutation group G with generating set X acting on \Omega (by a
# function action), and an element alpha of \Omega, compute a record with the
# following members:
#  orbit:  the orbit of alpha in G,
#  moves:  the element moves[i] is a permutation such that (moves[i])^alpha = O[i],
#  sv:  a Schreier vector for alpha in G (if compute_sv is true),
#  stabilizer:  the stabiliser of alpha in G.
# This corresponds to the procedures OrbitStabilizer, OrbitSv in §4.1 of Holt,
# et al. Note that this function can only compute the Schreier vector if \Omega
# is a set of natural numbers.
NOrbitStabilizer := function (X, alpha, action, compute_sv)
  local orbit, moves, sv, stab_gens, i, x, x_index, beta, beta_index, image, in_orbit, location;

  orbit := [alpha];
  moves := [()];
  stab_gens := [];
  if compute_sv then
    sv := [];
  else
    sv := false;
  fi;

  # We really just want a generating set, so if we are handed a group pick one
  # out.
  if IsGroup(X) then
    X := GeneratorsOfGroup(X);
  fi;

  # Now initialize the Schreier vector
  if compute_sv then
    for i in [1 .. LargestMovedPoint(X)] do
      sv[i] := 0;
    od;
    sv[alpha] := -1;
  fi;
  
  # Perform the orbit computation.
  x_index := 1;
  for x in X do
    beta_index := 1;
    for beta in orbit do
      image := action(beta, x);

      # Reuse Schreier vector for the orbit membership test if we're computing it.
      if compute_sv then
        in_orbit := sv[image] = 0;
      else
        location := Position(orbit, image);
        in_orbit := location <> fail;
      fi;

      if not in_orbit then
        Add(orbit, image);
        Add(moves, moves[beta_index] * x);

        if compute_sv then
          sv[image] := x_index;
        fi;
      else
        if compute_sv then
          location := Position(orbit, image);
        fi;

        AddSet(stab_gens, moves[beta_index] * x * (moves[location])^(-1));
      fi;

      beta_index := beta_index + 1;
    od;
    
    x_index := x_index + 1;
  od;

  return rec(orbit := orbit, moves := moves, sv := sv,
             stabilizer := Group(stab_gens));
end;

# NOrbit(X_or_G, alpha)
# Given a permutation group G with generating set X acting on \Omega, and an
# element alpha of \Omega, computes the orbit of alpha in G.
NOrbit := function (X, alpha)
  return NOrbitStabilizer(X, alpha, OnPoints, false).orbit;
end;

# NStabilizer(X_or_G, alpha)
# Given a permutation group G with generating set X acting on \Omega, and an
# element alpha of \Omega, returns the stabilizer of alpha in G; i.e. the
# subgroup of G whose elements fix alpha.
NStabilizer := function (X, alpha)
  return NOrbitStabilizer(X, alpha, OnPoints, false).stabilizer;
end;

# NSetStabiliser(X_or_G, A)
# Given a permutation group G with generating set X acting on \Omega, and a
# subset A of \Omega, return the setwise 
