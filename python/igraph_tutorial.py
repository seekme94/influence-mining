from igraph import *
import os

wd = os.getcwd()
print(wd)


def create_graph(nodes, edges):
    '''
    nodes is the set of nodes
    edges is the set of edges
    '''
    # Create a new graph
    g = Graph()
    # Add nodes
    g.add_vertices(nodes)
    g.add_edges(edges)
    return g


# Read a graph
g = Graph.Read_Ncol("sample_edgelist.txt", directed=False)

g = create_graph(nodes=4, edges=[(1, 3), (2, 3), (1, 4), (2, 4)])
g = Graph()
# Add nodes
g.add_vertices(4)
print(g)
g.add_edges([(1, 3), (2, 3), (1, 4), (2, 4)])
