# -*- coding: utf-8 -*-
"""
Created on Sat Sep 21 19:14:09 2019

@author: owaishussain@outlook.com
"""

from enum import Enum
from igraph import Graph

import numpy as np
import powerlaw
import random
import plot_util as pu
import networkx as nx


class Trait(Enum):
    DEGREE = 1
    BETWEENNESS = 2
    CLOSENESS = 3
    CORENESS = 4
    EIGENVALUE = 5
    PAGERANK = 6
    COLLECTIVE_INFLUENCE = 7


def normalize(x):
    '''
    Normalizes and returns the values in given list between 0 and 1
    Args:
        x: list of values
    '''
    x = np.array(x)
    x = (x - min(x)) / (max(x) - min(x))
    return x.tolist()


def attach_names_to_graph(graph):
    '''
    If the attribute graph.vs["name"] does not exist, then the node indices are set as node names for all nodes in in graph.vs
    '''
    try:
        graph.vs["name"]
    except KeyError:
        graph.vs["name"] = [i for i in range(0,len(graph.vs))]
    return graph


def convert_igraph_to_networkx(graph):
    '''
    Converts the given igraph object and returns NetworkX object
    '''
    nxg = nx.DiGraph()
    graph = attach_names_to_graph(graph)
    names = graph.vs["name"]
    nxg.add_nodes_from(names)
    nxg.add_edges_from([(names[x[0]], (names[x[1]])) for x in graph.get_edgelist()])
    return nxg


def convert_networkx_to_igraph(nx_graph):
    '''
    Converts the given NetworkX object and returns igraph object
    '''
    graph = Graph()
    graph.add_vertices(nx_graph.nodes())
    graph.add_edges(nx_graph.edges())
    return graph


def create_graph(nodes, edges):
    '''
    Returns an igraph object
    Args:
        nodes: the number of nodes to add
        edges: list of edges like [(0,2), (1,2)]
    '''
    graph = Graph()
    graph.add_vertices(nodes)
    graph.add_edges(edges)
    return graph


def create_edgelist_graph(edgelist_file, directed=False, weights=False):
    '''
    Returns an igraph object by reading edges from a file
    Args:
        edgelist_file: is the name of the file containing edges (from_id to_id)
        directed: is the network directed
        weights: is the network weighted
    '''
    graph = Graph.Read_Ncol(edgelist_file, directed=directed, weights=weights)
    return graph


def fit_power_law(graph, plot=True):
    '''
    Plots degree distribution and returns power-law exponent of given graph
    Args:
        graph: the igraph object
        plot: whether to plot the graph
    Returns:
        alpha: the power law exponent
    '''
    fit = powerlaw.Fit(np.array(graph.degree()) + 1, xmin=1, discrete=True)
    if (plot):
        fit.power_law.plot_pdf(color='b', linestyle='--', label='fit ccdf')
        fit.plot_pdf(color='r')
    return fit.power_law.alpha


def get_graph_summary(graph):
    '''
    Returns a summary of common metrics of given graph
    Args:
        graph:the igraph object
    '''
    summary = get_graph_traits(graph, traits=[Trait.DEGREE, Trait.BETWEENNESS, Trait.CLOSENESS, Trait.CORENESS, Trait.EIGENVALUE],
                               normalize=True)
    summary["nodes"] = graph.vcount()
    summary["edges"] = graph.ecount()
    summary["edge_node_ratio"] = graph.ecount() / graph.vcount()
    summary["average_degree"] = sum(summary["degree"]) / graph.vcount()
    summary["highest_degree"] = max(summary["degree"])
    summary["density"] = graph.density(loops=False)
    summary["diameter"] = graph.diameter()
    summary["transitivity"] = graph.transitivity_undirected(mode="nan")
    cliques = graph.cliques(min=3, max=3)
    summary["graph_triads"] = len(cliques)
    summary["average_distance"] = None
    summary["girth"] = None
    return summary


def plot_graph(graph, save_to_file=False, file_name="graph.png"):
    '''
    Plot the graph on screen. The igraph object is first converted into NetworkX object, which is then plotted
        graph: the igraph object
        file_name: the name of the file with path where graph will be stored. Not applicable if save_to_file is False
        save_to_file: when true, the graph will be saved on disk as an image
    '''
    nxg = convert_igraph_to_networkx(graph)
    pu.plot_random(nxg, save_to_file=save_to_file, file_name=file_name)


def plot_distribution(graph, trait=Trait.DEGREE):
    '''
    Plots distribution of graph for given trait
    graph is the igraph object
    trait is the Trait for which the plot will be generated
    '''
    pass


def get_graph_traits(graph, traits=[Trait.DEGREE, Trait.BETWEENNESS, Trait.CLOSENESS, Trait.PAGERANK], normalize=False):
    '''
    Returns several traits from given graph
    Args:
        graph: the igraph object
        traits: the list of traits to calculate for given parameters
        normalize: whether to normalize the traits between 0 and 1
    '''
    graph_traits = {}
    if (Trait.DEGREE in traits):
        graph_traits["degree"] = graph.degree()
    if (Trait.BETWEENNESS in traits):
        graph_traits["betweenness"] = graph.betweenness()
    if (Trait.CLOSENESS in traits):
        graph_traits["closeness"] = graph.closeness()
    if (Trait.CORENESS in traits):
        graph_traits["coreness"] = graph.coreness()
    if (Trait.EIGENVALUE in traits):
        graph_traits["eigenvalue"] = graph.evcent(return_eigenvalue=True)[0]
    if (Trait.PAGERANK in traits):
        graph_traits["pagerank"] = graph.pagerank()
    # TODO COLLECTIVE_INFLUENCE
    if (normalize):
        for key, values in graph_traits.items():
            graph_traits[key] = normalize(graph_traits[key])
    return graph_traits


def get_largest_component(graph):
    '''
    Returns a subgraph, which is the largest connected component of a graph
    Args:
        graph: the igraph object
    '''
    graph = Graph()
    cl = graph.clusters()
    return cl.giant()


def generate_tree(size, children=2, directed=False):
    '''
    Returns a tree graph
    Args:
        size: the number of nodes in the graph
        children: the number of child nodes each parent node will have
        directed: whether the generated graph will be directed
    '''
    graph = Graph.Tree(n=size, children=children)
    return graph


def generate_ring(size, distance=2, directed=False):
    '''
    Returns a ring graph
    Args:
        size: is the number of nodes in the graph
        children: is the number of child nodes each parent node will have
        directed: whether the generated graph will be directed
    '''
    graph = Graph.Ring(size, mutual=False, circular=True, directed=directed)
    return graph


def generate_random(size, probability=0.1, directed=False):
    '''
    Returns an Erdos Renyi random graph
    Args:
        size: the number of nodes in the graph
        probability: the random probability of an edge between two nodes
        directe: whether the generated graph will be directed
    '''
    graph = Graph.Erdos_Renyi(size, p=probability, directed=directed, loops=False)
    return graph


def generate_scale_free(size, preference_power=1, directed=False):
    '''
    Returns a scale-free graph based on Barabasi model, i.e. rewiring a random graph, while keeping the degree distribution consistent
    Args:
        size: is the number of nodes in the graph
        preference_power: is the power of preference of attachment. Default value 1 denotes that the preference is linear
        directed: whether the generated graph will be directed
    '''
    graph = Graph.Barabasi(n=size, zero_appeal=preference_power, implementation="psumtree", outpref=True, directed=directed, start_from=None)
    return graph


def generate_small_world(size, neighborhood=1, probability=0.1, directed=False):
    '''
    Returns a small world graph based on Watts and Strogatz model, i.e. rewiring a random graph, while keeping the degree distribution consistent
    Args:
        size: the number of nodes in the graph
        neighborhood: the distance (number of steps) within which two nodes will be connected
        probability: the random probability of an edge between two nodes
        directed: whether the generated graph will be directed
    '''
    graph = Graph.Watts_Strogatz(dim=1, size=size, nei=neighborhood, p=probability, directed=directed)
    return graph


def generate_small_world_scale_free(size, probability=0.1, seed=None, directed=False):
    '''
    Returns a scale-free graph with relatively high clustering
    Args:
        size: the number of nodes in the graph
        probability: the random probability of an edge between two nodes
        directed: whether the generated graph will be directed
    '''
    if seed is not None:
        random.seed(seed)
    pass


def get_communities_multilevel(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each node belongs to
    Args:
        graph: the igraph object
    Description: Multilevel community detection is based on Louvaine's algorithm, it is better at scaling and avoids formation of super communities. In this method, instead of merging communities, nodes are moved between communities such that each node makes a local decision that maximizes its own contribution to the modularity score.
    When this procedure gets stuck (i.e. none of the nodes change their membership), then all the communities are collapsed into single nodes and the process continues (thus the name multilevel).
    '''
    pass


def get_communities_edgebetweenness(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each node belongs to
    Args:
        graph: the igraph object
    Description: Edge-betweenness community detection is a hierarchical decomposition process where edges are removed in the decreasing order of their edge betweenness scores. This is motivated by the fact that edges connecting different groups are more likely to be contained in multiple shortest paths simply because in many cases they are the only option to go from one group to another.
    This method yields good results but is very slow because of the computational complexity of edge betweenness calculations and because the betweenness scores have to be re-calculated after every edge removal. Another disadvantage is that it builds a full dendrogram and does not tell where to cut the dendrogram to obtain the final groups, so we use some other measure (e.g. modularity score of the partitions) to decide that
    '''
    pass


def get_communities_eigenvector(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each node belongs to
    Args:
        graph: the igraph object
    Description: Leading Eigenvector community detection is a top-down hierarchical approach that optimizes the modularity function. In each step, the graph is split into two parts in a way that the separation itself yields a significant increase in the modularity. The split is determined by evaluating the leading eigenvector of modularity matrix, and there is also a stopping condition which prevents tightly connected groups to be split further.
    Due to the eigenvector calculations involved, it might not work on degenerate graphs where the ARPACK eigenvector solver is unstable. On non-degenerate graphs, it is likely to yield a higher modularity score than the fast greedy method, although it is a bit slower.
    '''
    pass


def get_communities_fastgreedy(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each node belongs to
    Args:
        graph: the igraph object
    Description: Fastgreedy community detection is a bottom-up hierarchical approach. It tries to optimize function modularity function in greedy manner. Initially, every node belongs to a separate community, and communities are merged iteratively such that each merge is locally optimal (i.e. has high increase in modularity value).
    The algorithm stops when it is not possible to increase the modularity any more, so it gives you a grouping as well as a dendrogram. The method is fast and it is the method that is usually tried as a first approximation because it has no parameters to tune.
    However, it has a limitation that communities below a given size threshold will always be merged with neighboring communities
    '''
    # Attach names if missing
    graph = attach_names_to_graph(graph)
    dendrogram = Graph.community_fastgreedy(graph)
    clusters = dendrogram.as_clustering()
    # Create membership vector, group in node space of igraph object
    graph.vs["group"] = clusters.membership
    if plot:
        pu.plot_community_layout(graph)
    return graph


def get_communities_spinglass(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each node belongs to
    Args:
        graph: the igraph object
    Description: Spinglass community detection is an approach based on Potts model. Each node can be in one of 'c' spin states, and the edges specify which pairs of nodes would prefer to stay in the same spin state and which ones prefer to have different spin states.
    The model is then simulated for a given number of steps, and the spin states of the nodes in the end define the communities.
    The consequences are that 1) There will never be more than 'c' communities in the end, although you can set c to as high as 200; 2) There may be less than 'c' communities in the end as some of the spin states may become empty; 3) In disconnected networks, it is not guaranteed that nodes in disconencted parts of the networks have different spin states.
    The method is not particularly fast and not deterministic, but has a tunable resolution parameter that determines the cluster sizes.
    '''
    pass


def get_communities_walktrap(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each node belongs to
    Args:
        graph: the igraph object
    Description: Walktrap community detection' is the's general idea is that if you perform random walks on the graph, then the walks are more likely to stay within the same community because there are only a few edges that lead outside a given community.
    Walktrap runs short random walks of 3-4-5 steps (depending on parameters) and uses the results of these random walks to merge separate communities in a bottom-up manner like fastgreedy.community. We can use the modularity score to select where to cut the dendrogram.
    It is a bit slower than the fast greedy approach but also a bit more accurate.
    '''
    pass


def get_communities_labelpropagation(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each node belongs to
    Args:
        graph: the igraph object
    Description: Label propagation community detection is a simple approach in which every node is assigned one of 'k' labels. The method then proceeds iteratively and re-assigns labels to nodes in a way that each node takes the most frequent label of its neighbors in a synchronous manner. The method stops when the label of each node is one of the most frequent labels in its neighborhood.
    It is very fast but yields different results based on the initial configuration (which is decided randomly), therefore it should be run a large number of times before labeling.
    '''
    pass


def get_communities_largescale(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each node belongs to
    Args:
        graph: the igraph object
    Description: This function detects communities in large-scale graphs.
    '''
    pass


def get_communities_clique(graph, plot=True):
    '''
    Returns an igraph object with additional 'group' property attached, representing the community each node belongs to
    Args:
        graph: the igraph object
    Description: This function detects communities using cliques.
    '''
    pass


if __name__ == "__main__":
    graph = Graph.Watts_Strogatz(dim=2, size=5, nei=2, p=0.15)
    graph = get_communities_fastgreedy(graph, plot=True)
    summary = get_graph_traits(graph)
