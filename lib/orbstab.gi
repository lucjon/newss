# vim: ft=gap sts=2 et sw=2

# NEWSS_SchreierVector(sv, size, gens, to_compute)
# Extend a Schreier vector <C>sv</C> of size <C>size</C> to include the orbits
# of the elements in <C>to_compute</C> under the natural action of the
# permutations <C>gens</C>. To compute an orbit afresh, call with an empty
# vector, e.g.
#   NEWSS_SchreierVector([], 0, gens, [beta]);
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

InstallGlobalFunction(NEWSS_ExtendSchreierVector, function (sv, size, gens, gen)
  local to_compute, image, n, pt;
  to_compute := [];
  n := Size(gens);

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

  return NEWSS_SchreierVector(sv, size, gens, to_compute);
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

InstallGlobalFunction(SchreierVectorWordFromBasePoint, function (X, sv, beta)
  local u, k;

  if not IsBound(sv[beta]) then
    return false;
  fi;

  u := [];
  k := sv[beta];
  while k <> -1 do
    Add(u, X[k], 1);
    beta := beta / X[k];
    k := sv[beta];
  od;

  return u;
end);

InstallGlobalFunction(RandomStabilizerElement, function (X, sv, beta)
  local g, h;
  g := PseudoRandom(Group(X));
  h := SchreierVectorPermFromBasePoint(X, sv, beta ^ g);
  return g * Inverse(h);
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
