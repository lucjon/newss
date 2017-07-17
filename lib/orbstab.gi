# vim: ft=gap sts=2 et sw=2

InstallGlobalFunction(NOrbitStabilizer, function (X, alpha, action, compute_sv)
  local orbit, moves, sv, stab_gens, i, x, x_index, beta, beta_index, image,
        in_orbit, location, stabilizer;

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
    sv[alpha] := -1;
  fi;
  
  # Perform the orbit computation.
  beta_index := 1;
  for beta in orbit do
    x_index := 1;
    for x in X do
      image := action(beta, x);

      # Reuse Schreier vector for the orbit membership test if we're computing it.
      if compute_sv then
        in_orbit := IsBound(sv[image]);
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

      x_index := x_index + 1;
    od;
    
    beta_index := beta_index + 1;
  od;

  if Size(stab_gens) = 0 then
    stabilizer := TrivialGroup();
  else
    stabilizer := Group(stab_gens);
  fi;

  return rec(orbit := orbit, moves := moves, sv := sv,
             stabilizer := stabilizer);
end);

InstallGlobalFunction(SchreierVectorPermFromBasePoint, function (X, sv, beta)
  local u, k;

  # Bail out early if beta is not in the orbit.
  if not IsBound(sv[beta]) then
    return false;
  fi;

  u := ();
  k := sv[beta];
  while k <> -1 do
    u := X[k] * u;
    beta := beta / X[k];
    k := sv[beta];
  od;

  return u;
end);

InstallGlobalFunction(NOrbit, function (X, alpha)
  return NOrbitStabilizer(X, alpha, OnPoints, false).orbit;
end);

InstallGlobalFunction(NStabilizer, function (X, alpha)
  return NOrbitStabilizer(X, alpha, OnPoints, false).stabilizer;
end);

InstallGlobalFunction(NSetStabilizer, function (X, A)
  return NOrbitStabilizer(X, A, OnSets, false).stabilizer;
end);

InstallGlobalFunction(Stabilizes, function (g, O)
  local o;

  for o in O do
    if o^g <> o then
      return false;
    fi;
  od;

  return true;
end);
