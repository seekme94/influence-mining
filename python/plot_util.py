# -*- coding: utf-8 -*-
"""
Created on Sat Oct 19 20:42:46 2019

@author: Owais
"""


import matplotlib.pyplot as plt
import networkx as nx
from operator import itemgetter
from networkx.drawing.nx_agraph import graphviz_layout # requries pydot
from graph_util import convert_igraph_to_networkx
from igraph import Graph


def plot(G, node_color="blue", node_size=50, line_color="grey", line_widths=0, width=0.1, save_to_file=False, file_name="graph.png"):
    if (isinstance(G, Graph)):
        G = convert_igraph_to_networkx(G)
    options = {'node_color': node_color, 'node_size': node_size, 'line_color': line_color,
               'linewidths': line_widths, 'width': width}
    nx.draw(G, **options)
    plt.show()
    if (save_to_file):
        plt.savefig(file_name, format="PNG")


def plot_circular(G, node_color="blue", node_size=50, line_color="grey", line_widths=0, width=0.1, save_to_file=False, file_name="graph.png"):
    if (isinstance(G, Graph)):
        G = convert_igraph_to_networkx(G)
    options = {'node_color': node_color, 'node_size': node_size, 'line_color': line_color,
               'linewidths': line_widths, 'width': width}
    nx.draw_circular(G, **options)
    plt.show()
    if (save_to_file):
        plt.savefig(file_name, format="PNG")


def plot_edge_colormap(G, save_to_file=False, file_name="graph.png"):
    # best for star graphs, e.g. nx.star_graph(20)
    if (isinstance(G, Graph)):
        G = convert_igraph_to_networkx(G)
    colors = range(int(pow(len(G), 1/2))) # Sqrt of the size of graph
    pos = nx.spring_layout(G)
    nx.draw(G, pos, node_color='#A0CBE2', edge_color=colors, width=4, edge_cmap=plt.cm.Blues, with_labels=False)
    plt.show()
    if (save_to_file):
        plt.savefig(file_name, format="PNG")


def plot_circular_tree(G, save_to_file=False, file_name="graph.png"):
    # best for tree graphs, e.g. nx.balanced_tree(2, 7)
    if (isinstance(G, Graph)):
        G = convert_igraph_to_networkx(G)
    pos = graphviz_layout(G, prog='twopi', args='')
    plt.figure(figsize=(8, 8))
    nx.draw(G, pos, node_size=20, alpha=0.5, node_color="blue", with_labels=False)
    plt.axis('equal')
    plt.show()
    if (save_to_file):
        plt.savefig(file_name, format="PNG")


def plot_degree_rank(G, save_to_file=False, file_name="graph.png"):
    # best for random graphs, e.g. nx.gnp_random_graph(100, 0.02)
    if (isinstance(G, Graph)):
        G = convert_igraph_to_networkx(G)
    degree_sequence = sorted([d for n, d in G.degree()], reverse=True)
    plt.loglog(degree_sequence, 'b-', marker='o')
    plt.title("Degree rank plot")
    plt.ylabel("degree")
    plt.xlabel("rank")
    # draw graph in inset
    plt.axes([0.45, 0.45, 0.45, 0.45])
    plt.axis('off')
    plt.show()
    if (save_to_file):
        plt.savefig(file_name, format="PNG")


def plot_ego_net(G, save_to_file=False, file_name="graph.png"):
    # best for graphs with hubs, e.g. nx.generators.barabasi_albert_graph(100, 3)
    if (isinstance(G, Graph)):
        G = convert_igraph_to_networkx(G)
    node_and_degree = G.degree()
    (largest_hub, degree) = sorted(node_and_degree, key=itemgetter(1))[-1]
    hub_ego = nx.ego_graph(G, largest_hub)
    pos = nx.spring_layout(hub_ego)
    nx.draw(hub_ego, pos, node_color='b', node_size=50, with_labels=False)
    nx.draw_networkx_nodes(hub_ego, pos, nodelist=[largest_hub], node_size=300, node_color='r')
    plt.show()
    if (save_to_file):
        plt.savefig(file_name, format="PNG")


def plot_random(G, save_to_file=False, file_name="graph.png"):
    # best for general random graps, e.g. G = nx.random_geometric_graph(200, 0.15)
    if (isinstance(G, Graph)):
        G = convert_igraph_to_networkx(G)
    pos = nx.get_node_attributes(G, 'pos')
    dmin = 1
    ncenter = 0
    for n in pos:
        x, y = pos[n]
        d = (x - 0.5)**2 + (y - 0.5)**2
        if d < dmin:
            ncenter = n
            dmin = d
    p = dict(nx.single_source_shortest_path_length(G, ncenter))
    plt.figure(figsize=(8, 8))
    nx.draw_networkx_edges(G, pos, nodelist=[ncenter], alpha=0.4)
    nx.draw_networkx_nodes(G, pos, nodelist=list(p.keys()), node_size=80, node_color=list(p.values()), cmap=plt.cm.Reds_r)
    plt.xlim(-0.05, 1.05)
    plt.ylim(-0.05, 1.05)
    plt.axis('off')
    plt.show()
    if (save_to_file):
        plt.savefig(file_name, format="PNG")


def plot_weighted(G, save_to_file=False, file_name="graph.png"):
    # Weights should be normalized between 0 and 1
    if (isinstance(G, Graph)):
        G = convert_igraph_to_networkx(G)
    elarge = [(u, v) for (u, v, d) in G.edges(data=True) if d['weight'] > 0.5]
    esmall = [(u, v) for (u, v, d) in G.edges(data=True) if d['weight'] <= 0.5]
    pos = nx.spring_layout(G)  # positions for all nodes
    nx.draw_networkx_nodes(G, pos, node_size=700)
    nx.draw_networkx_edges(G, pos, edgelist=elarge, width=6)
    nx.draw_networkx_edges(G, pos, edgelist=esmall, width=6, alpha=0.5, edge_color='b', style='dashed')
    nx.draw_networkx_labels(G, pos, font_size=20, font_family='sans-serif')
    plt.axis('off')
    plt.show()
    if (save_to_file):
        plt.savefig(file_name, format="PNG")


def plot_community_layout(G, save_to_file=False, file_name="graph.png"):
    '''
    Plots the graph according to the respective community structure
    Args:
        G: the graph object. Must contain graph.vs["group"] containing cluster IDs
    '''
    partitions = dict()
    for i in range(G.vcount()):
        partitions[G.vs["name"][i]] = G.vs["group"][i]
    if (isinstance(G, Graph)):
        G = convert_igraph_to_networkx(G)
    node_positions = dict()
    communities = dict()
    for node, community in partitions.items():
        try:
            communities[community] += [node]
        except KeyError:
            communities[community] = [node]
    for ci, nodes in communities.items():
        subgraph = G.subgraph(nodes)
        pos_subgraph = nx.spring_layout(subgraph, scale=1.)
        node_positions.update(pos_subgraph)

    community_positions = dict()
    # create a weighted graph, in which each node corresponds to a community,
    # and each edge weight to the number of edges between communities
    between_community_edges = dict()
    for (ni, nj) in G.edges():
        ci = partitions[ni]
        cj = partitions[nj]
        if ci != cj:
            try:
                between_community_edges[(ci, cj)] += [(ni, nj)]
            except KeyError:
                between_community_edges[(ci, cj)] = [(ni, nj)]
    communities = set(partitions.values())
    hypergraph = nx.DiGraph()
    hypergraph.add_nodes_from(communities)
    for (ci, cj), edges in between_community_edges.items():
        hypergraph.add_edge(ci, cj, weight=len(edges))
    # find layout for communities
    pos_communities = nx.spring_layout(hypergraph, scale=3.)
    # set node positions to position of community
    for node, community in partitions.items():
        community_positions[node] = pos_communities[community]
    pos = dict()
    for node in G.nodes():
        pos[node] = community_positions[node] + node_positions[node]
    nx.draw(G, pos, node_color=list(partitions.values()))
    plt.show()
    if (save_to_file):
        plt.savefig(file_name, format="PNG")


# Test em all
if __name__ == '__main__':
    plot(nx.grid_2d_graph(5, 5))
    plot_edge_colormap(nx.star_graph(20))
    plot_circular(nx.balanced_tree(2, 5))
    plot_circular_tree(nx.balanced_tree(2, 7))
    plot_degree_rank(nx.gnp_random_graph(100, 0.02))
    plot_ego_net(nx.generators.barabasi_albert_graph(100, 5))
    plot_random(nx.random_geometric_graph(200, 0.15))


    G = nx.Graph()
    set1 = ['a', 'a', 'c', 'c', 'c', 'a']
    set2 = ['b', 'c', 'd', 'e', 'f', 'd']
    weights = [0.6, 0.2, 0.1, 0.7, 0.9, 0.3]
    for i in range(0, 6):
        G.add_edge(set1[i], set2[i], weight=weights[i])
    plot_weighted(G)

