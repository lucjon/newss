# vim: ft=gap sts=2 et sw=2

InstallGlobalFunction(NEWSS_EmptySchreierVector, function (gens, invgens, point)
  local sv, j;

  sv := [];
  sv[point] := -1;
  return rec(
    gens := gens,
    invgens := invgens,
    point := point,
    size := 1,
    sv := sv
  );
end);

InstallGlobalFunction(SchreierVectorForOrbit, function (gens, invgens, point)
  local sv;
  sv := NEWSS_EmptySchreierVector(gens, invgens, point);
  NEWSS_SchreierVector(sv, [point]);
  return sv;
end);

InstallGlobalFunction(SchreierVectorPermFromBasePoint, function (sv, beta)
  return Inverse(SchreierVectorPermToBasePoint(sv, beta));
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
