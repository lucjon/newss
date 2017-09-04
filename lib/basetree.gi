# vim: ft=gap sts=2 et sw=2

InstallGlobalFunction(NEWSS_AddChainToTree, function(tree, bsgs)
  local current, base, depth, pt, next, new_record;

  Info(NewssInfo, 2, "Adding ", ViewString(bsgs), " to tree (", tree.count, "/", tree.bound, ")");

  if Size(bsgs!.base) = 0 then
    return false;
  fi;

  if tree.bound <> 0 and tree.count >= tree.bound then
    NEWSS_RemoveOldChain(tree);
  fi;

  current := tree;
  base := ShallowCopy(bsgs!.base);
  depth := 1;

  while Size(base) > 1 and depth < tree.depth do
    pt := Remove(base, 1);
    if not IsBound(current.children[pt]) then
      current.children[pt] := [];
    fi;
    next := rec(point := pt, children := [], parent := current);
    Add(current.children[pt], next);
    current := next;
    depth := depth + 1;
  od;

  if not IsBound(current.children[base[1]]) then
    current.children[base[1]] := [];
  elif bsgs in current.children[base[1]] then
    return false;
  fi;
  new_record := rec(point := base[1], chain := bsgs, tree := tree, parent := current);
  Add(current.children[base[1]], new_record);
  Add(tree.members, bsgs);
  tree.count := tree.count + 1;
  return true;
end);


InstallGlobalFunction(NEWSS_RemoveOldChain, function (tree)
  local target, container, node;
  target := Remove(tree.members, 1);
  Info(NewssInfo, 3, "Evicting ", ViewString(target), " from the base tree");
  node := NEWSS_FindChainWithBasePrefix(tree, target!.base, 1);
  container := node.parent.children[node.point];
  Remove(container, Position(container, node));
  tree.count := tree.count - 1;
end);


NEWSS_NodeLevel := function (node)
  if IsBound(node.parent) then
    return 1 + NEWSS_NodeLevel(node.parent);
  else
    return 0;
  fi;
end;

InstallGlobalFunction(NEWSS_FindChainWithBasePrefix, function(tree, prefix, rest...)
  local current, pt, found, child, children, result, i;
  current := tree;

  if Size(rest) = 0 then
    i := 1;
  else
    i := rest[1];
  fi;

  if i > Size(prefix) then
    return fail;
  fi;
  pt := prefix[i];

  # We are either a root or intermediate node...
  if IsBound(current.children) then
    # Are there any edges labelled with our current point?
    if IsBound(current.children[pt]) then
      # Yes!
      if Size(prefix) - i > 0 then
        # If we have any more points in our prefix, then we need to continue
        # our search. Any subtree labelled `pt' could lead to a suitable chain.
        for child in current.children[pt] do
          result := NEWSS_FindChainWithBasePrefix(child, prefix, i + 1);
          if result <> fail then
            return result;
          fi;
        od;
        # If we get here though, none did.
        return fail;
      else
        # If we don't have any more points in our prefix, then *any* chain in
        # this subtree will suffice. So we just drill down until we reach a
        # leaf node.
        for i in [1 .. Size(current.children[pt])] do
          if not IsBound(current.children[pt][i]) then
            continue;
          fi;

          child := current.children[pt][i];
          while not IsBound(child.chain) do
            children := First(child.children, x -> Size(x) > 0);
            if children = fail then
              child := fail;
              break;
            else
              child := children[1];
            fi;
          od;

          if child <> fail then
            return child;
          fi;
        od;
        return fail;
      fi;
    # If no edges are labelled with the current point, then there can't be any
    # suitable chains.
    else
      return fail;
    fi;
  # We might also be a leaf node. If so, then check if its base has our desired
  # prefix.
  elif IsBound(current.chain) and StartsWith(current.chain!.base, prefix) then
    return current;
  # If it doesn't then we've reached a dead end.
  else
    return fail;
  fi;
end);


