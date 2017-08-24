# vim: ft=gap sts=2 et sw=2

InstallGlobalFunction(NEWSS_SchreierVector, function (sv, to_compute)
  local pt, gen, image, j;

  while Size(to_compute) > 0 do
    pt := Remove(to_compute, 1);
    for j in [1 .. Size(sv.gens)] do
      gen := sv.gens[j];
      image := pt / gen;
      if not IsBound(sv.sv[image]) then
        Add(to_compute, image);
        sv.sv[image] := j;
        sv.size := sv.size + 1;
      fi;
    od;
  od;
end);

InstallGlobalFunction(NEWSS_EmptySchreierVector, function (gens, point)
  local sv;
  sv := [];
  sv[point] := -1;
  return rec(
    gens := gens,
    point := point,
    size := 1,
    sv := sv
  );
end);

InstallGlobalFunction(SchreierVectorForOrbit, function (gens, point)
  local sv;
  sv := NEWSS_EmptySchreierVector(gens, point);
  NEWSS_SchreierVector(sv, [point]);
  return sv;
end);

InstallGlobalFunction(ExtendSchreierVector, function (sv, gen)
  local to_compute, image, n, pt;
  to_compute := [];
  Add(sv.gens, gen);
  n := Size(sv.gens);

  for pt in [1 .. Size(sv.sv)] do
    if not IsBound(sv.sv[pt]) then
      continue;
    fi;

    image := pt / gen;
    if not IsBound(sv.sv[image]) then
      Add(to_compute, image);
      sv.sv[image] := n;
      sv.size := sv.size + 1;
    fi;
  od;

  NEWSS_SchreierVector(sv, to_compute);
end);

InstallGlobalFunction(SchreierVectorPermToBasePoint, function (sv, beta)
  local u, k;

  # Bail out early if beta is not in the orbit.
  if not IsBound(sv.sv[beta]) then
    return false;
  fi;

  u := ();
  k := sv.sv[beta];
  while k <> -1 do
    u := u * sv.gens[k];
    beta := beta ^ sv.gens[k];
    k := sv.sv[beta];
  od;

  return u;
end);

InstallGlobalFunction(SchreierVectorPermFromBasePoint, function (sv, beta)
  return Inverse(SchreierVectorPermToBasePoint(sv, beta));
end);

InstallGlobalFunction(SchreierVectorWordToBasePoint, function (sv, beta)
  local u, k;

  if not IsBound(sv.sv[beta]) then
    return false;
  fi;

  u := [];
  k := sv.sv[beta];
  while k <> -1 do
    Add(u, sv.gens[k]);
    beta := beta ^ sv.gens[k];
    k := sv.sv[beta];
  od;

  return u;
end);

InstallGlobalFunction(SchreierVectorWordFromBasePoint, function (sv, beta)
  return PermWordInverse(SchreierVectorWordToBasePoint(sv, beta));
end);

InstallGlobalFunction(RandomStabilizerElement, function (sv)
  local g, h;
  g := PseudoRandom(Group(sv.gens));
  h := SchreierVectorPermFromBasePoint(sv, sv.point ^ g);
  return g * Inverse(h);
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
