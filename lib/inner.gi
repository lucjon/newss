# vim: ft=gap sts=2 et sw=2

InstallGlobalFunction(NEWSS_SchreierVector, function (sv, to_compute)
  local pt, gen, image, j;

  while Size(to_compute) > 0 do
    pt := Remove(to_compute, 1);
    for j in [1 .. Size(sv.gens)] do
      gen := sv.invgens[j];
      image := pt ^ gen;
      if not IsBound(sv.sv[image]) then
        Add(to_compute, image);
        sv.sv[image] := j;
        sv.size := sv.size + 1;
      fi;
    od;
  od;
end);

InstallGlobalFunction(ExtendSchreierVector, function (sv, gen, invgen)
  local to_compute, image, n, pt;
  to_compute := [];
  Add(sv.gens, gen);
  Add(sv.invgens, invgen);
  n := Size(sv.gens);

  for pt in [1 .. Size(sv.sv)] do
    if not IsBound(sv.sv[pt]) then
      continue;
    fi;

    image := pt ^ invgen;
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
