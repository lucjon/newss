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

#! @Arguments group
#! @Returns a BSGS structure with full stabilizer chain
#! @Description
#! Initialises a BSGS structure from an existing group's generating set, and
#! compute a chain for it using the Schreier-Sims algorithm (see 
#! <Ref Func="SchreierSims"/>).
DeclareGlobalFunction("BSGSFromGroup");


#! @Section The Schreier-Sims algorithm

#! @Arguments bsgs
#! @Returns a BSGS structure with stabilizer chain
#! @Description
#! Attempt to extend the given BSGS structure into a genuine base and strong
#! generating set respectively for $G$ using the Schreier–Sims algorithm.
DeclareGlobalFunction("SchreierSims");

#! @Arguments bsgs, i
#! @Returns an iterator over the relevant Schreier generators
#! @Description
#! Compute the (possibly trivial) Schreier generators for the stabilizer of the
#! $i$-th base point in the $i$-th stabilizer group in the given BSGS structure.
DeclareGlobalFunction("SchreierGenerators");


#! @Section The randomized Schreier-Sims algorithm

#! @Description
#! The minimum degree of permutation group to use the randomised Schreier-Sims
#! algorithm on; i.e., if the group acts on fewer points than this, use the
#! deterministic algorithm (<Ref Func="SchreierSims"/>), and otherwise use the
#! randomised algorithm (<Ref Func="RandomSchreierSims"/>). The default value
#! for this setting is 10.
DeclareGlobalVariable("BSGS_MIN_DEGREE_RANDOM");

#! @Description
#! The number of unchanged sifted elements to require before finishing the
#! randomised Schreier-Sims algorithm; i.e. <Ref Func="RandomSchreierSims"/> 
#! with this value will return an incomplete stabilizer chain with probability
#! $2^{-w}$, where $w$ is this number <C>BSGS_RANDOM_SS_THRESHOLD</C>. The
#! default value for this setting is 8.
DeclareGlobalVariable("BSGS_RANDOM_SS_THRESHOLD");

#! @Arguments bsgs, w
#! @Returns a BSGS structure with stabilizer chain
#! @Description
#! As in <Ref Func="SchreierSims"/>, compute a base and stong generating set
#! for the given BSGS structure, along with a stabilizer chain, with the
#! proviso that the chain could be incomplete. (See
#! <Ref Var="BSGS_RANDOM_SS_THRESHOLD"/>.)
DeclareGlobalFunction("RandomSchreierSims");

#! @Arguments bsgs
#! @Returns nothing
#! @Description
#! Given a BSGS structure, compute the basic stabilizers (i.e. the stabilizer
#! chain) and basic orbits. Returns nothing; the chain is stored in the BSGS
#! structure (see the function <Ref Func="BSGS"/>).
DeclareGlobalFunction("ComputeChainForBSGS");


#! @Section Manipulating stabilizer chains

#! @Arguments bsgs, keep_initial_gens
#! @Returns a BSGS structure
#! @Description
#! Attempts to remove redundant generators from the given BSGS structure with
#! stabilizer chain, to produce a smaller strong generating set. If the
#! <C>keep_initial_gens</C> parameter is <K>true</K>, then do not attempt to
#! remove any generator in the BSGS structure's <C>initial_gens</C> set (see
#! <Ref Func="BSGS"/>).
DeclareGlobalFunction("RemoveRedundantGenerators");

#! @Chapter Computing with Stabilizer Chains
#! @Section Permutations

#! @Arguments bsgs, g
#! @Returns a record describing the strip result
#! @Description
#! Corresponds to the procedure Strip in §4.4.1 of Holt et al. or the builtin
#! function SiftedPermutation. Here, <C>bsgs</C> is a BSGS structure for a
#! group $G$ and $g$ is an element of $\mathrm{Sym}(\Omega)$, where $G$
#! acts on $\Omega$. The result is a record containing the fields:
#! * **residue**:    The permutation after the strip operation. This is () if
#!                    and only if $g$ is truly an element of $G$.
#! * **level**:      The iteration at which the stripping stopped.
#! Note that unlike in e.g. <Ref Func="StabilizerChainContains" />, we do not
#! check to see if <C>bsgs</C> has a full stabilizer chain computed, as this
#! function is called many times in tight loops.
DeclareGlobalFunction("StabilizerChainStrip");

DeclareGlobalFunction("ComputeStabOrbForBSGS");
DeclareGlobalFunction("EnsureBSGSChainComputed");
DeclareGlobalFunction("ExtendBase");
DeclareGlobalFunction("ExtendBaseIfStabilized");
DeclareInfoClass("NewssInfo");
