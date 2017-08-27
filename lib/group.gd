# vim: ft=gap sts=2 et sw=2

#! @Chapter Computing with Stabilizer Chains

#! @Section Properties of groups

#! @Arguments bsgs
#! @Returns the order of $G$
#! @Description
#! Return the order of the group $G$ described by the given BSGS structure.
DeclareGlobalFunction("StabilizerChainOrder");

#! @Arguments bsgs
#! @Returns the order of $G$
#! @Description
#! Return the order of the group $G$ described by the given BSGS structure,
#! without checking whether the BSGS structure has had a stabilizer chain
#! computed.
DeclareGlobalFunction("StabilizerChainOrderNC");


#! @Section Stabilizers

#! @Argmuents bsgs, point
#! @Returns the point stabilizer of <A>point</A> in the group described by
#! <A>bsgs</A>
#! @Description
#! Computes the point stabilizer of the given point in the group described by
#! <A>bsgs</A>, which must act on a set containing <A>point</A>.
DeclareGlobalFunction("StabilizerChainStabilizer");


#! @Section Permutations
#! @Arguments bsgs, g
#! @Returns a boolean
#! @Description
#! Returns <K>true</K> if the permutation $g$ is in the group described by the
#! BSGS structure <C>bsgs</C>, otherwise returns <K>false</K>.
DeclareGlobalFunction("StabilizerChainContains");

