# vim: ft=gap sts=2 et sw=2

# A tree record contains the following fields:
#  * **children**. A list of child nodes indexed by the first base point.
#  * **depth**. The maximum depth of the tree.
# 

# NEWSS_AddChainToTree(tree, bsgs[, base])
# Add a BSGS to the given BSGS tree, with the given base point.
InstallGlobalFunction(NEWSS_AddChainToTree, function (tree, bsgs)
  local current, base, depth, pt, next, new_record;

  if Size(bsgs!.base) = 0 then
    return fail;
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
    next := rec(point := pt, children := []);
    Add(current.children[pt], next);
    current := next;
    depth := depth + 1;
  od;

  if not IsBound(current.children[base[1]]) then
    current.children[base[1]] := [];
  fi;
  new_record := rec(point := base[1], chain := bsgs, tree := tree);
  Add(current.children[base[1]], new_record);
  Add(tree.members, bsgs);
  tree.count := tree.count + 1;
  return true;
end);


# NEWSS_RemoveOldChain(tree)
# Remove an 'old' chain (one which hasn't been used in a while). The precise
# semantics here are not guaranteed; we will choose the least recently used.
InstallGlobalFunction(NEWSS_RemoveOldChain, function (tree)
  local target, container;
  target := Remove(tree.members, 1);
  container := NEWSS_FindChainWithBasePrefix(tree, target!.base, 1, true);
  Remove(container, Position(container, target));
end);


# NEWSS_FindChainWithBasePrefix(tree, prefix[, offset[, return_enclosing_list]])
# Find a BSGS with the given prefix in the tree, or fail if no such BSGS has
# been calculated.
InstallGlobalFunction(NEWSS_FindChainWithBasePrefix, function (node, prefix, rest...)
  local current, i, pt, found, child, result, return_container, container;

  i := 1;
  return_container := false;
  if Size(rest) > 0 then
    i := rest[1];
    if Size(rest) > 1 then
      return_container := true;
    fi;
  fi;
  pt := prefix[i];

  # We are either a root or intermediate node...
  if IsBound(node.children) then
    # Are there any edges labelled with our node point?
    if IsBound(node.children[pt]) then
      # Yes!
      if Size(prefix) > 1 then
        # If we have any more points in our prefix, then we need to continue
        # our search. Any subtree labelled `pt' could lead to a suitable chain.
        for child in node.children[pt] do
          result := NEWSS_FindChainWithBasePrefix(child, prefix, i + 1);
          if result <> fail then
            if return_container then
              result := node.children[pt];
            fi;
            break;
          fi;
        od;
        # If we get here though, none did.
      else
        # If we don't have any more points in our prefix, then *any* chain in
        # this subtree will suffice. So we just drill down until we reach a
        # leaf node.
        container := node.children[pt];
        child := node.children[pt][1];
        while not IsBound(child.chain) do
          container := child.children;
          child := First(container, ReturnTrue)[1];
        od;
        if return_container then
          result := container;
        else
          result := child.chain;
        fi;
      fi;
    # If no edges are labelled with the node point, then there can't be any
    # suitable chains.
    else
      result := fail;
    fi;
  # We might also be a leaf node. If so, then check if its base has our desired
  # prefix.
  elif IsBound(node.chain) and StartsWith(node.chain!.base, prefix) and not return_container then
    result := node.chain;
  # If it doesn't then we've reached a dead end.
  else
    result := fail;
  fi;

  # Update the most-recently-used list
  if result <> fail and not return_container then
    Remove(node.tree.members, Position(node.tree.members, result));
    Add(node.tree.members, result);
  fi;

  return result;
end);


