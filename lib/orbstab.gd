# vim: ft=gap sts=2 et sw=2

#! @Chapter Orbits and Stabilizers

#! @Section Stabilizers

#! @Arguments g, O
#! @Returns a boolean
#! @Description
#! Suppose $G$ is a group acting on a set $\Omega$. Then given an element $g$
#! of $G$ and a subset $O$ of $\Omega$, determine whether $G$ fixes $O$
#! pointwise.
DeclareGlobalFunction("Stabilizes");


#! @Section Schreier vectors

#! @Arguments X, sv, beta
#! @Returns a permutation $u$ such $u^{\beta_i} = $<C> beta</C>
#! @Description
#! Given a permutation group $G$ with generating set $X$ acting on $\Omega$, a
#! Schreier vector <C>sv</C> for the orbit of an element $\alpha$ in $G$, and
#! another element <C>beta</C> in this orbit, returns a permutation $u$ such
#! that <C>alpha ^ u = beta</C>. If beta is not in the orbit, returns
#! <K>false</K>.
DeclareGlobalFunction("SchreierVectorPermFromBasePoint");

#! @Arguments X, sv, beta
#! @Returns a permutation word $u$ such that the successive image of $\beta_i$
#! under each permutation in $u$ is <C>beta</C>.
#! @Description
#! Given a permutation group $G$ with generating set $X$ acting on $\Omega$, a
#! Schreier vector <C>sv</C> for the orbit of an element $\alpha$ in $G$, and
#! another element <C>beta</C> in this orbit, returns a permutation word $u$ such
#! that the image of <C>alpha</C> under <C>u</C> is <C>beta</C> --- see section
#! <Ref Sect="Chapter_Miscellany_Section_Permutation_words"/> on permutation words.
#! If <C>beta</C> is not in the given orbit, return <K>false</K>.
DeclareGlobalFunction("SchreierVectorWordFromBasePoint");

#! @Arguments X, sv, beta
#! @Returns a permutation in $G_{\rm beta}$, where $G = \langle X \rangle$.
#! @Description
#! Given a Schreier vector <C>sv</C> for the orbit of <C>beta</C> under a group
#! $G$ return a random element of the subgroup of $G$ stabilizing <C>beta</C>.
#! The parameter <C>X</C> specifies the group $G$ as a list of generators,
#! as used to construct <C>sv</C>.
DeclareGlobalFunction("RandomStabilizerElement");

#! @Arguments sv, size, gens, to_compute
#! @Returns a record with fields <C>sv</C> containing a Schreier vector and
#! <C>size</C>, the number of points in the orbit
#! @Description
#! (This is an internal function.)
#! Extend a Schreier vector <C>sv</C> of size <C>size</C> to include the orbits
#! of the elements in <C>to_compute</C> under the natural action of the
#! permutations <C>gens</C>. To compute an orbit afresh, call with an empty
#! vector, e.g.
#! <Listing>
#!   NEWSS_SchreierVector([], 0, gens, [beta]);
#! </Listing>
DeclareGlobalFunction("NEWSS_SchreierVector");
DeclareGlobalFunction("NEWSS_ExtendSchreierVector");
