# vim: ft=gap sts=2 et sw=2
#! @Chapter Stabilizer Chains

#! @Section Creating stabilizer chains

#! @Arguments bsgs, group, sgs
#! @Returns an initialized BSGS structure
#! @Description
#! Initialize a BSGS structure for a group, given a base and strong generating
#! set for it. A BSGS structure is a record containing the following fields:
#!
#! * **group**:        The group $G$ for which <C>base</C> and <C>sgs</C> are a
#!                     base and SGS,
#! * **sgs**:          A strong generating set for $G$ relative to <C>base</C>,
#! * **base**:         A base for $G$,
#! * **has_chain**:    <K>true</K> if a stabiliser chain has been computed yet 
#!                     for this base and SGS, otherwise <K>false</K>.
#! * **initial_gens**: An immutable list containing the first generating set given
#!                     for the group, whether it was a strong generating set or not,
#!                     e.g. the initial generators given before
#!                     <Ref Func="SchreierSims"/> or
#!                     <Ref Func="RemoveRedundantGenerators"/> is called.
#! * ***stabilizers**: The stabilizer chain corresponding to base and sgs, i.e. a
#!                     sequence of subgroups $[G^{(1)}, G^{(2)}, ..., G^{(k+1)}]$
#!                     where
#!                        $$1 = G^{(k+1)} \le G^{(k)} \le \cdots \le G^{(1)} = G,$$
#!                     with $k$ being the size of <C>sgs</C>.
#! * ***stabgens**:    A list whose $i$-th element is a list of generators for the
#!                     $i$-th stabilizer group.
#! * ***orbits**:      A list whose $i$-th element is a Schreier vector for the orbit of
#!                     <C>base[i]</C> in $G^{(i)} =$ <C>stabilizers[i]</C>.
#! * ***orbitsizes**:  A list whose $i$-th element is the number of elements in the orbit
#!                     of <C>base[i]</C> in $G^{(i)}$.
#! * ***options**:     If a stabilizer chain was computed for this BSGS using
#!                     <Ref Func="BSGSFromGroup"/>, then the field <C>options</C>
#!                     contains the final set of options used in the computation; see
#!                     <Ref Sect="Chapter_Stabilizer_Chains_Section_Options_for_stabilizer_chain_creation"/>.
#!
#! The fields marked * are present only if <C>has_chain = </C><K>true</K>; see the
#! function <Ref Func="ComputeChainForBSGS"/>. This function does not compute the
#! stabilizer chain --- structures initialized here have <C>has_chain</C> =
#! <K>false</K>. 
DeclareGlobalFunction("BSGS");

#! @Arguments bsgs
#! @Returns a BSGS structure with full stabilizer chain
#! @Description
#! For testing purposes, initialises a BSGS structure with stabilizer chain
#! using the &GAP; builtin functions.
DeclareGlobalFunction("BSGSFromGAP");

#! @Arguments group[, options]
#! @Returns a BSGS structure with full stabilizer chain
#! @Description
#! Initialises a BSGS structure from an existing group's generating set, and
#! compute a chain for it using the Schreier-Sims algorithm (see 
#! <Ref Func="SchreierSims"/>). For more information on the options which can
#! be passed in, see the section
#! <Ref Sect="Chapter_Stabilizer_Chains_Section_Options_for_stabilizer_chain_creation"/>.
DeclareGlobalFunction("BSGSFromGroup");

#! @Arguments bsgs
#! @Returns a GAP stabilizer chain structure
#! @Description
#! Takes a BSGS structure and, finding a base and strong generating set if
#! necessary (using the algorithms in this package), returns a &GAP; stabilizer
#! chain record representing the computed chain. (For testing purposes, the
#! topmost stabilizer chain record has an additional component
#! <C>from_newss := true</C>.)
DeclareGlobalFunction("GAPStabChainFromBSGS");

#! @Description
#! Overrides the default &GAP; stabilizer chain methods with the ones from this
#! package.
DeclareGlobalFunction("EnableNewssOverloads");


#! @Section Manipulating stabilizer chains

#! @Arguments bsgs, new_base
#! @Returns nothing
#! @Description
#! Modifies the given BSGS so that it contains a strong generating set and
#! stabilizer chain relative to the base <C>new_base</C>. This function does not
#! attempt to verify that <C>new_base</C> is in fact a base for the group.
DeclareGlobalFunction("ChangeBaseOfBSGS");

#! @Arguments bsgs
#! @Returns nothing
#! @Description
#! Given a BSGS structure, compute the basic stabilizers (i.e. the stabilizer
#! chain) and basic orbits. Returns nothing; the chain is stored in the BSGS
#! structure (see the function <Ref Func="BSGS"/>).
DeclareGlobalFunction("ComputeChainForBSGS");

#! @Arguments bsgs, g
#! @Returns nothing
#! @Description
#! Conjugates the given stabilizer chain <C>bsgs</C> for a group $G$ by the
#! permutation <C>g</C>, such that its base $[\beta_1, \ldots, \beta_n]$
#! is now $[\beta_1^g, \ldots, \beta_n^g]$, and we have a stabilizer chain for
#! $G^g$.
DeclareGlobalFunction("ConjugateBSGS");

#! @Arguments bsgs
#! @Returns a new BSGS structure
#! @Description
#! Creates a deep copy of the given BSGS structure.
DeclareGlobalFunction("CopyBSGS");

#! @Arguments bsgs, keep_initial_gens
#! @Returns a BSGS structure
#! @Description
#! Attempts to remove redundant generators from the given BSGS structure with
#! stabilizer chain, to produce a smaller strong generating set. If the
#! <C>keep_initial_gens</C> parameter is <K>true</K>, then do not attempt to
#! remove any generator in the BSGS structure's <C>initial_gens</C> set (see
#! <Ref Func="BSGS"/>).
DeclareGlobalFunction("RemoveRedundantGenerators");


DeclareGlobalFunction("NEWSS_PerformBaseSwap");
