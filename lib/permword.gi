# vim: ft=gap sts=2 et sw=2
InstallGlobalFunction(PermWordInverse, function (word)
  return Reversed(List(word, Inverse));
end);

InstallGlobalFunction(PermWordPreImage, function (pt, word)
  local perm;
  for perm in Reversed(word) do
    pt := pt / perm;
  od;
  return pt;
end);

InstallGlobalFunction(PermWordImage, function (pt, word)
  local perm;
  for perm in word do
    pt := pt ^ perm;
  od;
  return pt;
end);

InstallGlobalFunction(PermWordAsPerm, function (word)
  local perm, i;
  
  perm := word[1];
  for i in [2 .. Size(word)] do
    perm := perm * word[i];
  od;
  return perm;
end);
