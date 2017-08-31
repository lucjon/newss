# vim: ft=gap sts=2 et sw=2

#! @Chapter Miscellany
#! @Section Base trees

#! @Arguments tree, bsgs
#! @Returns <C>true</C> if <A>bsgs</A> was successfully added to the tree, or
#! <C>false</C> otherwise
#! @Description
#! Add <A>bsgs</A> to the stabilizer chain cache tree <A>tree</A>, indexed by
#! its base. This may not succeed if there is already a stabilizer chain with
#! this base in the tree, or if the given BSGS has an empty base.
DeclareGlobalFunction("NEWSS_AddChainToTree");

#! @Arguments tree, prefix[, offset]
#! @Returns a tree node whose base starts with <A>prefix</A>, or
#! <C>fail</C>
#! @Description
#! Attempt to find a stabilizer chain whose base starts with <A>prefix</A> if
#! one exists in the tree, comparing base points starting at index
#! <A>offset</A> (which defaults to <C>1</C>). If no such stabilizer chain
#! exists, return <C>fail</C>. Otherwise, return a leaf node containing the
#! chain. A leaf node is a record containing the following fields:
#!  * **chain**. The BSGS object with the suitable base.
#!  * **point**. The base point under which the node was indexed; this is
#!    either the last point in the stabilizer chain's base, or the point at
#!    the $i$-th position, where $i$ is the maximum depth of the tree,
#!    whichever is earlier.
#! * **tree**. The tree to the node is attached (one would expect this to be
#!   <C>tree</C>).
#! * **parent**. The node's parent in the tree, such that this node <C>node</C>
#!   is contained within <C>node.parent.children[node.point]</C>.
DeclareGlobalFunction("NEWSS_FindChainWithBasePrefix");

#! @Arguments tree
#! @Returns nothing
#! @Description
#! Attempts to remove the oldest (in the sense of least-recently-accessed)
#! stabilizer chain from the given cache tree.
DeclareGlobalFunction("NEWSS_RemoveOldChain");

#! @Description
#! The default maximum depth of a stabilizer chain tree. This can be controlled
#! on an individual basis with the <C>cache_depth</C> option to <Ref
#! Func="BSGSFromGroup"/>.
BindGlobal("NEWSS_DEFAULT_TREE_DEPTH", 5);

#! @Description
#! The default upper bound on the number of stabilizer chains a tree can
#! contain. This can be controlled on an individual basis with the
#! <C>cache_bound</C> option to <Ref Func="BSGSFromGroup"/>.
BindGlobal("NEWSS_DEFAULT_TREE_BOUND", 20);
