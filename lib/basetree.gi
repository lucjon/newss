# vim: ft=gap sts=2 et sw=2

# A tree record contains the following fields:
#  * **children**. A list of child nodes indexed by the first base point.
#  * **depth**. The maximum depth of the tree.
# 

# NEWSS_AddChainToTree(tree, bsgs[, base])
# Add a BSGS to the given BSGS tree, with the given base point.
InstallGlobalFunction(NEWSS_AddChainToTree, function(tree, bsgs)
  local current, base, depth, pt, next;

  current := tree;
  base := ShallowCopy(bsgs!.base);
  depth := 1;

  while Size(base) > 1 and depth < tree.depth do
    pt := Remove(base, 1);
    if not IsBound(current.children[pt]) then
      current.children[pt] := [];
    fi;
    next := rec(point := pt, children := []);
    Add(current.children[pt], next);
    current := next;
    depth := depth + 1;
  od;

  if not IsBound(current.children[base[1]]) then
    current.children[base[1]] := [];
  fi;
  Add(current.children[base[1]], rec(point := base[1], chain := bsgs));
  tree.count := tree.count + 1;
  return true;
end);


# NEWSS_FindChainWithBasePrefix(tree, prefix)
# Find a BSGS with the given prefix in the tree, or fail if no such BSGS has
# been calculated.
InstallGlobalFunction(NEWSS_FindChainWithBasePrefix, function(tree, prefix, rest...)
  local current, i, pt, found, child, result;
  current := tree;

  i := 1;
  if Size(rest) > 0 then
    i := rest[1];
  fi;
  pt := prefix[i];

  # We are either a root or intermediate node...
  if IsBound(current.children) then
    # Are there any edges labelled with our current point?
    if IsBound(current.children[pt]) then
      # Yes!
      if Size(prefix) > 1 then
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
        child := current.children[pt][1];
        while not IsBound(child.chain) do
          child := First(child.children, ReturnTrue)[1];
        od;
        return child.chain;
      fi;
    # If no edges are labelled with the current point, then there can't be any
    # suitable chains.
    else
      return fail;
    fi;
  # We might also be a leaf node. If so, then check if its base has our desired
  # prefix.
  elif IsBound(current.chain) and StartsWith(current.chain!.base, prefix) then
    return current.chain;
  # If it doesn't then we've reached a dead end.
  else
    return fail;
  fi;
end);


