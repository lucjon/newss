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

#! @Arguments group[, options]
#! @Returns a BSGS structure with full stabilizer chain
#! @Description
#! Initialises a BSGS structure from an existing group's generating set, and
#! compute a chain for it using the Schreier-Sims algorithm (see 
#! <Ref Func="SchreierSims"/>). For more information on the options which can
#! be passed in, see the section
#! <Ref Sect="Chapter_Stabilizer_Chains_Section_Options_for_stabilizer_chain_creation"/>.
DeclareGlobalFunction("BSGSFromGroup");


#! @Section Options for stabilizer chain creation
#! 
#! Various aspects of the creation of stabilizer chains can be controlled by
#! supplying an `options' record to <Ref Func="BSGSFromGroup"/>. There are two
#! kinds of field in the record: some are functions, which specify which
#! implementations of particular algorithms to use, and the rest are parameters
#! which affect things like the probability of error in the computation.
#! The following options of the former type can be specified:
#!
#! * **<C>SchreierSims</C>**. Usually either <Ref Func="SchreierSims"/> or
#!   <Ref Func="RandomSchreierSims"/>, depending on whether the determinstic
#!   or randomized algorithm should be used. See
#!   <Ref Sect="Chapter_Stabilizer_Chains_Section_Implementations_of_the_Schreier-Sims_algorithm"/>.
#! * **<C>Verify</C>**. The function used to verify the stabilizer chain once
#!   computed. See
#!   <Ref Sect="Chapter_Stabilizer_Chains_Section_Verification_procedures"/>.
#! * **<C>ExtendBaseForLevel</C>**. The function used to select new base points
#!   when a Schreier generator is found which fixes all the existing ones.
#! * **<C>SchreierVectorForLevel</C>**. The function used to compute Schreier
#!   vectors for basic orbits. See
#!   <Ref Sect="Chapter_Stabilizer_Chains_Section_Orbit_computation_procedures"/>.
#!
#! The following tuning parameters can be specified:
#!
#! * **fall_back_to_deterministic**. If <K>true</K>, then the deterministic
#!   algorithm will be run on the stabilizer chain if it is found to be
#!   incomplete by the Verify procedure. Note that if this parameter is
#!   <K>false</K>, the resulting stabilizer chain may be incorrect!
#! * **sift_threshold**. In the randomized algorithm, the number of potential
#!   generators which must be sifted to the identity for the chain to be
#!   considered complete. If this parameter has the value $w$, the result of the
#!   randomized Schreier-Sims algorithm is correct with probability $2^{-w}$.
#! * **perm_representation**. The package can represent permutations either
#!   purely as &GAP; permutation objects, or as permutation words (see section
#!   <Ref Sect="Chapter_Miscellany_Section_Permutation_words"/>), which is
#!   faster for small-base groups; specify either
#!   <C>NEWSS_PERM_REPRESENTATION</C> or <C>NEWSS_PERMWORD_REPRESENTATION</C>
#!   respectively to override the default.
#!
#! All of these fields are optional; if any are missing, they are taken from
#! whichever of the following default options structures are chosen by
#! <Ref Func="BSGSFromGroup"/> for the group supplied.

#! @Description
#! The default set of options used by <Ref Func="BSGSFromGroup"/> to compute
#! the stabilizer chain of most groups. It uses the randomized Schreier-Sims
#! algorithm, with a sift threshold of 8, and verifies the result with a pass
#! of the deterministic algorithm.
DeclareGlobalVariable("NEWSS_DEFAULT_OPTIONS");

#! @Description
#! The default set of options used by <Ref Func="BSGSFromGroup"/> to compute
#! the stabilizer chain of small-degree groups. It uses the deterministic
#! Schreier-Sims algorithm, but otherwise the same parameters as usual.
DeclareGlobalVariable("NEWSS_DETERMINISTIC_OPTIONS");

#! @Description
#! The maximum degree of permutation group to consider `small', i.e. the
#! maximum degree for which <C>NEWSS_DETERMINISTIC_OPTIONS</C> will be used by
#! default.
DeclareGlobalVariable("NEWSS_MIN_DEGREE_RANDOM");

#! @Description
#! Represent permutations as plain &GAP; permutation objects. This record has
#! fields <C>PermFromBasePoint</C>, <C>Strip</C>, <C>AsPerm</C> and
#! <C>LiftPerm</C>. The first two methods correspond to the functions
#! <Ref Func="SchreierVectorPermFromBasePoint"/> and
#! <Ref Func="StabilizerChainStrip"/>, and the latter two convert to and from
#! native &GAP; permutation objects (although these are no-ops in this case).
#! For now, it also has fields <C>MulPerm</C>, <C>InvPerm</C> and
#! <C>ImagePerm</C> which perform the obvious functions, although a better way
#! of dealing with these would be to do away with these fields and instead
#! create a new &GAP; type for permutation words with the appropriate operations
#! installed, so that the native syntax can be used.
DeclareGlobalVariable("NEWSS_PERM_REPRESENTATION");

#! @Description
#! Represent permutations as permutation words. See
#! <Ref Var="NEWSS_PERM_REPRESENTATION"/> for the list of fields this record
#! contains.
DeclareGlobalVariable("NEWSS_PERMWORD_REPRESENTATION");


#! @Section Manipulating stabilizer chains

#! @Arguments bsgs
#! @Returns nothing
#! @Description
#! Given a BSGS structure, compute the basic stabilizers (i.e. the stabilizer
#! chain) and basic orbits. Returns nothing; the chain is stored in the BSGS
#! structure (see the function <Ref Func="BSGS"/>).
DeclareGlobalFunction("ComputeChainForBSGS");

#! @Arguments bsgs, keep_initial_gens
#! @Returns a BSGS structure
#! @Description
#! Attempts to remove redundant generators from the given BSGS structure with
#! stabilizer chain, to produce a smaller strong generating set. If the
#! <C>keep_initial_gens</C> parameter is <K>true</K>, then do not attempt to
#! remove any generator in the BSGS structure's <C>initial_gens</C> set (see
#! <Ref Func="BSGS"/>).
DeclareGlobalFunction("RemoveRedundantGenerators");


#! @Section Implementations of the Schreier-Sims algorithm

#! @Arguments bsgs
#! @Returns a BSGS structure with stabilizer chain
#! @Description
#! Attempt to extend the given BSGS structure into a genuine base and strong
#! generating set respectively for $G$ using the deterministic Schreier–Sims
#! algorithm.
DeclareGlobalFunction("SchreierSims");

#! @Arguments bsgs, i
#! @Returns an iterator over the relevant Schreier generators
#! @Description
#! Compute the (possibly trivial) Schreier generators for the stabilizer of the
#! $i$-th base point in the $i$-th stabilizer group in the given BSGS structure.
DeclareGlobalFunction("SchreierGenerators");

#! @Arguments bsgs
#! @Returns a BSGS structure
#! @Description
#! As in <Ref Func="SchreierSims"/>, compute a base and stong generating set
#! for the given BSGS structure, along with a stabilizer chain, with the
#! proviso that the chain could be incomplete, with probability $2^{-w}$. Here,
#! $w$ is the value of the <C>sift_threshold</C> option.
DeclareGlobalFunction("RandomSchreierSims");


#! @Description
#! The number of unchanged sifted elements to require before finishing the
#! randomised Schreier-Sims algorithm; i.e. <Ref Func="RandomSchreierSims"/> 
#! with this value will return an incomplete stabilizer chain with probability
#! $2^{-w}$, where $w$ is this number <C>BSGS_RANDOM_SS_THRESHOLD</C>. The
#! default value for this setting is 8.
DeclareGlobalVariable("BSGS_RANDOM_SS_THRESHOLD");


#! @Section Verification procedures
#!
#! Depending on the prior knowledge we have about the group for which the
#! stabilizer chain is being computed, more efficient procedures to verify the
#! correctness of the stabilizer chain may be available. By default, the package
#! makes the following choice:
#! * If we are using the deterministic algorithm, perform no verification at
#!   all. In this case, the <C>ReturnTrue</C> procedure is used.
#! * If we are using the randomized algorithm and we know the order of the
#!   group a priori, we use <Ref Func="NEWSS_VerifyByOrder"/>. You can inform
#!   GAP, and hence the package, of a group's order by using the builtin
#!   <Ref Func="SetSize"/> function.
#! * Otherwise, we use <Ref Func="NEWSS_VerifyByDeterministic"/>.

#! @Arguments bsgs
#! @Returns <K>true</K> if the stabilizer chain is complete, otherwise
#! <K>false</K>
#! @Description
#! Verify the stabilizer chain against the expected order of the group by
#! calculating the order of the group using the size of the computed orbits.
DeclareGlobalFunction("NEWSS_VerifyByOrder");

#! @Arguments bsgs
#! @Returns <K>true</K> if the stabilizer chain is complete, otherwise
#! <K>false</K>
#! @Description
#! Verify the stabilizer chain by running the deterministic Schreier-Sims
#! algorithm on the chain. When the deterministic algorithm is given a
#! candidate base and strong generating set, it runs much more quickly than
#! it does from scratch.
DeclareGlobalFunction("NEWSS_VerifyByDeterministic");

#! @Section Orbit computation procedures
#! The default orbit algorithm is our own naive implementation
#! <Ref Func="NEWSS_SchreierVector"/>. If the <C>orb</C> package is available
#! when <C>newss</C> is loaded, then <Ref Func="NEWSS_SVFromOrb"/> can also be
#! selected. 

#! @Arguments bsgs, level
#! @Returns a Schreier vector for the <C>level</C>th basic orbit
#! @Description
#! Compute a Schreier vector the <C>level</C>th basic orbit by simple
#! enumeration.
DeclareGlobalFunction("NEWSS_SVForLevel");

#! @Arguments bsgs, level
#! @Returns a Schreier vector for the <C>level</C>th basic orbit
#! @Description
#! Compute a Schreier vector for the <C>level</C>th basic orbit using the orbit
#! enumeration functions in the <C>orb</C> package.
DeclareGlobalFunction("NEWSS_SVFromOrb");

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

#! @Arguments bsgs, g
#! @Returns a record describing the strip result
#! @Description Performs the same strip computation as
#! <Ref Func="StabilizerChainStrip"/>, except that the input and resulting
#! residue permutations are permutation words; see
#! <Ref Sect="Chapter_Miscellany_Section_Permutation_words"/>.
DeclareGlobalFunction("StabilizerChainStripWord");

DeclareGlobalFunction("ComputeStabForBSGS");
DeclareGlobalFunction("EnsureBSGSChainComputed");
DeclareGlobalFunction("NEWSS_ExtendSVByRecomputing");
DeclareGlobalFunction("NEWSS_ExtendSchreierVector");
DeclareGlobalFunction("NEWSS_FirstMovedPoint");
DeclareGlobalFunction("NEWSS_PickFromOrbits");
DeclareGlobalFunction("NEWSS_SchreierVector");
DeclareGlobalFunction("NEWSS_UpdateRecord");
DeclareInfoClass("NewssInfo");
