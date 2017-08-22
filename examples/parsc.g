# vim: ft=gap sw=2 et

# The goal of this example is to demonstrate how to use HPC-GAP and the newss
# package to perform parallel computations on permutation groups using
# stabiliser chains.
LoadPackage("newss");

# For this example, we shall use the Mathieu group M_24 --- one of the sporadic
# simple groups --- which is of nontrivial size. In the first instance, we
# shall compute the stabilisers of various points under the group's action in
# parallel, by computing stabiliser chains with various different bases. M_24
# acts on the set {1, ..., 24}.

# First, we compute one stabiliser chain for the monster group. We make the
# generator list immutable here so we can read the list from other threads
# without mutual exclusion.
M24_gens := Immutable([
  (1,4)(2,7)(3,17)(5,13)(6,9)(8,15)(10,19)(11,18)(12,21)(14,16)(20,24)(22,23),
  (1,4,6)(2,21,14)(3,9,15)(5,18,10)(13,17,16)(19,24,23)
]);
B := BSGSFromGroup(Group(M24_gens));

# Now we can access a single known base for B. 
M24_base := Immutable(BaseBSGS(B));


# To compute the stabilisers of a point p, we ask newss to compute a stabiliser
# chain with respect to the base obtained by moving p to the front of our
# original base. The first nontrivial entry in the resulting stabiliser chain
# will then be the subgroup of M_24 which fixes p. We perform each of these
# computations in parallel by running each one in a separate HPC-GAP "task".
tasks := List([1 .. 24], i -> RunTask(function (i)
  local our_base, bsgs;
    our_base := Concatenation([i], Filtered(M24_base, j -> i <> j));
    bsgs := BSGSFromGroup(Group(M24_gens), rec ( known_base := our_base ));
    return StabilizerBSGS(bsgs, 2).gens;
end, i));

# Once each task has been started, we can wait for each task to complete using
# the TaskResult function.
stabilisers := List(tasks, TaskResult);
Print(stabilisers, "\n");
