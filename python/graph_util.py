# -*- coding: utf-8 -*-
"""
Created on Sat Sep 21 19:14:09 2019

@author: owaishussain@outlook.com
"""

from igraph import *
from enum import Enum

class Trait(Enum):
    DEGREE = 1
    BETWEENNESS = 2
    CLOSENESS = 3
    CORENESS = 4
    EIGENVALUE = 5
    PAGERANK = 6
    COLLECTIVE_INFLUENCE = 7


def create_graph(vertices, edges):
    '''
    Returns an igraph object
    vertices is the set of vertices
    edges is the set of edges
    '''
    pass


def create_graph_edgelist(edgelist):
    '''
    Returns an igraph object
    edgelist is the list of tuples of edges to form the graph
    '''
    pass


def create_graph_file(file):
    '''
    Returns an igraph object
    file is the file to read the graph from
    '''
    pass


def fit_power_law(graph):
    '''
    Plots degree distribution and returns power-law exponent of given graph
    graph is the igraph object
    '''
    pass


def get_graph_summary(graph):
    '''
    Returns a summary of common metrics of given graph
    graph is the igraph object
    '''
    pass


def get_graph_traits(graph, traits=[Trait.DEGREE, Trait.BETWEENNESS, Trait.CLOSENESS, Trait.PAGERANK], normalize=False):
    '''
    Returns several traits from given graph
    graph is the igraph object
    vertices is the set of vertices to compute traits for
    traits is the list of traits to calculate for given parameters
    '''
    pass


def get_vertex_traits(graph, vertices, traits=[Trait.DEGREE, Trait.BETWEENNESS, Trait.CLOSENESS, Trait.PAGERANK], normalize=False):
    '''
    Returns several traits from given graph and set of vertices
    graph is the igraph object
    vertices is the set of vertices to compute traits for
    traits is the list of traits to calculate for given parameters
    normalize when True, the data will be normalized between 0 and 1
    '''
    pass


def get_largest_component(graph):
    '''
    Returns a subgraph, which is the largest connected component of a graph
    graph is the igraph object
    '''
    pass


def generate_tree(size, children=2, directed=False):
    '''
    Returns a tree graph
    size is the number of vertices in the graph
    children is the number of child vertices each parent vertex will have
    directed when True, the generated graph will be directed
    '''
    pass


def generate_ring(size, distance=2, directed=False):
    '''
    Returns a ring graph
    size is the number of vertices in the graph
    children is the number of child vertices each parent vertex will have
    directed when True, the generated graph will be directed
    '''
    pass


def generate_clique(size):
    '''
    Returns a clique graph
    size is the number of vertices in the graph
    '''
    pass


def generate_random(size, probability=0.1, directed=False, cycles=False):
    '''
    Returns an Erdos Renyi random graph
    size is the number of vertices in the graph
    probability is the random probability of an edge between two vertices
    directed when True, the generated graph will be directed
    cycles when True, the generated graph will contain cycles of connected vertices
    '''
    pass


def generate_scale_free(size, preference_power=1, directed=False, cycles=False):
    '''
    Returns a scale-free graph based on Barabasi model, i.e. rewiring a random graph, while keeping the degree distribution consistent
    size is the number of vertices in the graph
    preference_power is the power of preference of attachment. Default value 1 denotes that the preference is linear
    directed when True, the generated graph will be directed
    cycles when True, the generated graph will contain cycles of connected vertices
    '''
    pass


def generate_small_world(size, probability=0.1, directed=False, cycles=False):
    '''
    Returns a small world graph based on Watts and Strogatz model, i.e. rewiring a random graph, while keeping the degree distribution consistent
    size is the number of vertices in the graph
    probability is the random probability of an edge between two vertices
    directed when True, the generated graph will be directed
    cycles when True, the generated graph will contain cycles of connected vertices
    '''
    pass


def generate_holme_and_kim(size, probability=0.1, directed=False, cycles=False):
    '''
    Returns a scale-free graph with relatively high clustering
    size is the number of vertices in the graph
    probability is the random probability of an edge between two vertices
    directed when True, the generated graph will be directed
    cycles when True, the generated graph will contain cycles of connected vertices
    '''
    pass


def plot_graph(graph, save_to_file=False, file_name):
    '''
    Plot the graph on screen
    save_to_file when true, the graph will be saved on disk as an image
    file_name is the name of the file with path where graph will be stored. Not applicable if save_to_file is False
    '''
    pass


def plot_distribution(graph, trait=Trait.DEGREE):
    '''
    Plots distribution of graph for given trait
    graph is the igraph object
    trait is the Trait for which the plot will be generated
    '''
    pass


def get_communities_multilevel(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each vertex belongs to
    graph is the igraph object
    Description: Multilevel community detection is based on Louvaine's algorithm, it is better at scaling and avoids formation of super communities. In this method, instead of merging communities, vertices are moved between communities such that each vertex makes a local decision that maximizes its own contribution to the modularity score.
    When this procedure gets stuck (i.e. none of the vertices change their membership), then all the communities are collapsed into single vertices and the process continues (thus the name multilevel).
    '''
    pass


def get_communities_edgebetweenness(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each vertex belongs to
    graph is the igraph object
    Description: Edge-betweenness community detection is a hierarchical decomposition process where edges are removed in the decreasing order of their edge betweenness scores. This is motivated by the fact that edges connecting different groups are more likely to be contained in multiple shortest paths simply because in many cases they are the only option to go from one group to another.
    This method yields good results but is very slow because of the computational complexity of edge betweenness calculations and because the betweenness scores have to be re-calculated after every edge removal. Another disadvantage is that it builds a full dendrogram and does not tell where to cut the dendrogram to obtain the final groups, so we use some other measure (e.g. modularity score of the partitions) to decide that
    '''
    pass


def get_communities_eigenvector(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each vertex belongs to
    graph is the igraph object
    Description: Leading Eigenvector community detection is a top-down hierarchical approach that optimizes the modularity function. In each step, the graph is split into two parts in a way that the separation itself yields a significant increase in the modularity. The split is determined by evaluating the leading eigenvector of modularity matrix, and there is also a stopping condition which prevents tightly connected groups to be split further.
    Due to the eigenvector calculations involved, it might not work on degenerate graphs where the ARPACK eigenvector solver is unstable. On non-degenerate graphs, it is likely to yield a higher modularity score than the fast greedy method, although it is a bit slower.
    '''
    pass


def get_communities_fastgreedy(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each vertex belongs to
    graph is the igraph object
    Description: Fastgreedy community detection is a bottom-up hierarchical approach. It tries to optimize function modularity function in greedy manner. Initially, every vertex belongs to a separate community, and communities are merged iteratively such that each merge is locally optimal (i.e. has high increase in modularity value).
    The algorithm stops when it is not possible to increase the modularity any more, so it gives you a grouping as well as a dendrogram. The method is fast and it is the method that is usually tried as a first approximation because it has no parameters to tune.
    However, it has a limitation that communities below a given size threshold will always be merged with neighboring communities
    '''
    pass


def get_communities_spinglass(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each vertex belongs to
    graph is the igraph object
    Description: Spinglass community detection is an approach based on Potts model. Each vertex can be in one of 'c' spin states, and the edges specify which pairs of vertices would prefer to stay in the same spin state and which ones prefer to have different spin states.
    The model is then simulated for a given number of steps, and the spin states of the vertices in the end define the communities.
    The consequences are that 1) There will never be more than 'c' communities in the end, although you can set c to as high as 200; 2) There may be less than 'c' communities in the end as some of the spin states may become empty; 3) In disconnected networks, it is not guaranteed that vertices in disconencted parts of the networks have different spin states.
    The method is not particularly fast and not deterministic, but has a tunable resolution parameter that determines the cluster sizes.
    '''
    pass


def get_communities_walktrap(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each vertex belongs to
    graph is the igraph object
    Description: Walktrap community detection' is the's general idea is that if you perform random walks on the graph, then the walks are more likely to stay within the same community because there are only a few edges that lead outside a given community.
    Walktrap runs short random walks of 3-4-5 steps (depending on parameters) and uses the results of these random walks to merge separate communities in a bottom-up manner like fastgreedy.community. We can use the modularity score to select where to cut the dendrogram.
    It is a bit slower than the fast greedy approach but also a bit more accurate.
    '''
    pass


def get_communities_labelpropagation(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each vertex belongs to
    graph is the igraph object
    Description: Label propagation community detection is a simple approach in which every vertex is assigned one of 'k' labels. The method then proceeds iteratively and re-assigns labels to vertices in a way that each vertex takes the most frequent label of its neighbors in a synchronous manner. The method stops when the label of each vertex is one of the most frequent labels in its neighborhood.
    It is very fast but yields different results based on the initial configuration (which is decided randomly), therefore it should be run a large number of times before labeling.
    '''
    pass


def get_communities_largescale(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each vertex belongs to
    graph is the igraph object
    Description: This function detects communities in large-scale graphs.
    '''
    pass


def get_communities_clique(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each vertex belongs to
    graph is the igraph object
    Description: This function detects communities using cliques.
    '''
    pass

