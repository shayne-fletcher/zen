# Depth first search

In a depth-first search the idea is to fully explore the edges of the
most recently discovered node *v* before 'backtracking' to explore
edges leaving the node from which *v* was discovered. To do a
depth-first search means keeping careful track of what vertices have
been visited and when.

We compute timestamps for each vertex discovered in the search. A
discovered vertex has two timestamps associated with it : its
discovery time (in map `d`) and its finishing time (in map `f`) (a
vertex is finished when its adjacency list has been completely
examined). These timestamps are often useful in graph algorithms and
aid in reasoning about the behavior of depth-first search.

We color nodes during the search to help in the bookeeping (map
`color`). All vertices of the graph are initially `White`. When a
vertex is discovered it is marked `Gray` and when it is finished, it
is marked `Black`.

If vertex *v* is discovered in the adjacency list of previously
discovered node *u*, this fact is recorded in the predecessor subgraph
(map `pred`).
