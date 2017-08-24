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
#! Suppose $G = \langle X \rangle$ is a subgroup of
#! $\operatorname{Sym}(\Omega)$. Schreier vectors are an efficient way of
#! representing orbits of a point $\omega \in \Omega$ such that, for any point
#! $x$ in the orbit, it is easy to recover a permutation $u \in G$ such that
#! $\omega^u = x$. Here, they are a list <C>L</C> such that <C>X[omega] =
#! -1</C> and <C>X[L[x]]</C> is a permutation taking $x$ `closer' to $\omega$.
#! Finding the image of $x$ under this permuation, and looking it up again in
#! the list, we get a permutation taking the image closer to $\omega$.
#! Continuing this process until we reach -1, and multiplying the permutations
#! obtained along the way, we eventually compute a permutation in $G$ mapping
#! $x$ to $\omega$.
#! Most of the functions in this section operate on `Schreier vector records',
#! which are records containing the following fields:
#!  * **<C>point</C>**. The point $\omega$ whose orbit the structure
#!    describes.
#!  * **<C>gens</C>**. The list $X$ of generators for $G$ the orbit is computed
#!    with respect to.
#!  * **<C>sv</C>**. The list <C>L</C> as above.
#!  * **<C>size</C>**. The size of the orbit.

#! @Arguments X, pt
#! @Returns a Schreier vector record
#! @Description
#! Computes the orbit of the point <A>pt</A> under the permutations <A>X</A>,
#! returning a Schreier vector record describing it.
DeclareGlobalFunction("SchreierVectorForOrbit");

#! @Arguments sv, gen
#! @Returns nothing
#! @Description
#! Given a Schreier vector record <A>sv</A>, extends the orbit to include
#! images under a new generator <A>gen</A>.
DeclareGlobalFunction("ExtendSchreierVector");


#! @Arguments sv, beta
#! @Returns a permutation $u$ such $u^{\beta_i} = $<C> beta</C>
#! @Description
#! Given a Schreier vector record <C>sv</C> for the orbit of an element
#! $\alpha$, and another element <C>beta</C> in this orbit, returns a
#! permutation $u$ such that <C>alpha ^ u = beta</C>. If beta is not in the
#! orbit, returns
#! <K>false</K>.
DeclareGlobalFunction("SchreierVectorPermFromBasePoint");

DeclareGlobalFunction("SchreierVectorPermToBasePoint");
DeclareGlobalFunction("SchreierVectorWordToBasePoint");


#! @Arguments sv, beta
#! @Returns a permutation word $u$ such that the successive image of $\beta_i$
#! under each permutation in $u$ is <C>beta</C>.
#! @Description
#! Given a Schreier vector record <C>sv</C> for the orbit of an element
#! $\alpha$, and another element <C>beta</C> in this orbit, returns a
#! permutation word $u$ such that the image of <C>alpha</C> under <C>u</C> is
#! <C>beta</C> --- see section <Ref
#! Sect="Chapter_Miscellany_Section_Permutation_words"/> on permutation words.
#! If <C>beta</C> is not in the given orbit, return <K>false</K>.
DeclareGlobalFunction("SchreierVectorWordFromBasePoint");

#! @Arguments sv
#! @Returns a permutation in $G_{\rm beta}$, where $G = \langle X \rangle$.
#! @Description
#! Given a Schreier vector record <C>sv</C> for the orbit of <C>beta</C> in
#! a group $G$, return a random element of the subgroup of $G$ stabilizing
#! <C>beta</C>.
DeclareGlobalFunction("RandomStabilizerElement");

#! @Arguments sv, to_compute
#! @Returns a Schreier vector record
#! @Description
#! (This is an internal function.)
#! Extend an orbit described by a Schreier vector record <A>sv</A> of the
#! elements in <C>to_compute</C> under the natural action of the permutations
#! <C>gens</C>.
DeclareGlobalFunction("NEWSS_SchreierVector");

#! @Arguments gens, point
#! @Returns a Schreier vector record
#! @Description
#! (This is an internal function.)
#! Construct a Schreier vector record for the orbit of <A>point</A> under the
#! permutations <A>gens</A>, without actually computing the orbit.
DeclareGlobalFunction("NEWSS_EmptySchreierVector");
