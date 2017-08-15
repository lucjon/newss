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


#! @Section Permutations
#! @Arguments bsgs, g
#! @Returns a boolean
#! @Description
#! Returns <K>true</K> if the permutation $g$ is in the group described by the
#! BSGS structure <C>bsgs</C>, otherwise returns <K>false</K>.
DeclareGlobalFunction("StabilizerChainContains");

