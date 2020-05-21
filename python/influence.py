# -*- coding: utf-8 -*-
"""
Created on Sat Sep 21 19:14:09 2019

@author: owaishussain@outlook.com
"""

import time
import numpy as np
import heapq
import matplotlib.pyplot as plt
from igraph import Graph
from enum import Enum

class SeedMethod(Enum):
    RANDOM = 0
    DEGREE = 1
    BETWEENNESS = 2
    CLOSENESS = 3
    CORENESS = 4
    EIGENVALUE = 5
    PAGERANK = 6
    COLLECTIVE_INFLUENCE = 7
    ADAPTIVE_DEGREE = 8
    ADAPTIVE_BETWEENNESS = 9
    ADAPTIVE_CLOSENESS = 10
    ADAPTIVE_CORENESS = 11
    ADAPTIVE_EIGENVALUE = 12
    ADAPTIVE_PAGERANK = 13
    ADAPTIVE_COLLECTIVE_INFLUENCE = 14


class InfluenceModel(Enum):
    INDEPENDENT_CASCADE = 1 # Independent cascade model
    LINEAR_THRESHOLD = 2 # Linear threshold model
    RESILIENCE = 3 # Resilience model


def get_resilience(graph, node):
    '''
    Retuns the resilience, i.e. the size of the largest connected component in the graph after removing given node
    graph is the igraph object
    '''
    pass


def collective_influence_degree(graph, neighbor_distance, node):
    '''
    Returns the CI value, the product of degree of target node and total sum of degrees of neighborhood
    graph is the igraph object
    neighbor_distance is the distance to which the neighborhood nodes are searched for
    node is the target node in graph
    '''
    pass


def select_seed(graph, k, method=SeedMethod.DEGREE):
    '''
    Returns a list of k-nodes which have the highest value of given SeedMethod in a graph
    graph is the igraph object
    k is the number of seed nodes to select from graph
    method is the trait to use on graph nodes
    '''
    pass


def greedy_influence(graph, k, probability=0.2, model=InfluenceModel.RESILIENCE, iterations=10):
    '''
    Returns a list of influential nodes in a graph using Greedy approach
    graph is the igraph object
    k is the number of most influential nodes in the graph
    iterations is the depth of neighbourhood to which a node can spread its influence. If exhaustive run is required, set this to -1.
    probability is the probability of a node to successfully influence another node
    model is the algorithm to determine the influence
    '''
    if model == InfluenceModel.INDEPENDENT_CASCADE:
        return greedy_independent_cascade(graph=graph, k=k, probability=probability, iterations=iterations)
    if model == InfluenceModel.LINEAR_THRESHOLD:
        pass
    if model == InfluenceModel.RESILIENCE:
        pass


def greedy_independent_cascade(graph, k, probability=0.2, iterations=10):
    '''
    Find k-nodes with the largest spread under Independent Cascade model from a graph
    using the Greedy Algorithm.
    '''
    elapsed = []
    spreads = []
    solution = []
    start_time = time.time()
    for _ in range(k):
        best_node = -1
        best_spread = -np.inf
        nodes = set(range(graph.vcount())) - set(solution)
        for node in nodes:
            spread = spread_independent_cascade(graph, solution + [node], prob, iterations)
            if spread > best_spread:
                best_spread = spread
                best_node = node
        solution.append(best_node)
        spreads.append(best_spread)
        elapse = round(time.time() - start_time, 3)
        elapsed.append(elapse)
    return solution, spreads, elapsed


def greedy_independent_cascade_celf(graph, k, probability=0.2, iterations=10):
    '''
    Find k nodes with the largest spread (determined by IC) from a igraph graph
    using the Cost Effective Lazy Forward Algorithm, a.k.a Lazy Greedy Algorithm.
    '''
    start_time = time.time()
    # Find the first node with greedy algorithm
    gains = []
    for node in range(graph.vcount()):
        spread = spread_independent_cascade(graph, [node], probability, iterations)
        # Push each node and its spread to a heap
        heapq.heappush(gains, (-spread, node))

    # Pop the heap to get the node with the top spread
    spread, node = heapq.heappop(gains)
    solution = [node]
    spread = -spread
    spreads = [spread]
    # Record the number of times the spread is computed
    lookups = [graph.vcount()]
    elapsed = [round(time.time() - start_time, 3)]
    for _ in range(k - 1):
        node_lookup = 0
        matched = False
        while not matched:
            node_lookup += 1
            # here we need to compute the marginal gain of adding the current node
            # to the solution, instead of just the gain, i.e. we need to subtract
            # the spread without adding the current node
            _, current_node = heapq.heappop(gains)
            spread_gain = spread_independent_cascade(
                graph, solution + [current_node], prob, n_iters) - spread
            # check if the previous top node stayed on the top after pushing
            # the marginal gain to the heap
            heapq.heappush(gains, (-spread_gain, current_node))
            matched = gains[0][1] == current_node
        # spread stores the cumulative spread
        spread_gain, node = heapq.heappop(gains)
        spread -= spread_gain
        solution.append(node)
        spreads.append(spread)
        lookups.append(node_lookup)
        elapse = round(time.time() - start_time, 3)
        elapsed.append(elapse)
    return solution, spreads, elapsed, lookups


def optimal_maximization(graph, k, iterations=3, model=InfluenceModel.RESILIENCE, parallel=False):
    '''
    Returns a list of most influential nodes in a graph - optimal solution
    graph is the igraph object
    k is the number of most influential nodes in the graph
    iterations is the depth of neighbourhood to which a node can spread its influence. If exhaustive run is required, set this to -1.
    model is the algorithm to determine the influence
    parallel defines whether the process will run on single thread or multiple
    '''
    pass


def spread_independent_cascade(graph, seed, probability=0.2, iterations=10):
    '''
    Returns average spread of given seed nodes in the graph under Independent Cascade model
    graph is the igraph object
    seed is the list of nodes initially activated
    iterations is the number of times to repeat the process in order to calculate spread
    '''
    total_spead = 0
    for i in range(iterations):
        np.random.seed(i)
        active = seed[:]
        new_active = seed[:]
        while new_active:
            activated_nodes = []
            for node in new_active:
                neighbors = graph.neighbors(node, mode='out')
                success = np.random.uniform(0, 1, len(neighbors)) < probability
                activated_nodes += list(np.extract(success, neighbors))
            new_active = list(set(activated_nodes) - set(active))
            active += new_active
        total_spead += len(active)
    return total_spead / iterations


def spread_linear_threshold(graph, seed, iterations=10):
    '''
    Returns average spread of given seed nodes in the graph under Linear Threshold model
    graph is the igraph object
    seed is the list of nodes initially activated
    runs is the number of times to repeat the process in order to calculate average spread
    '''
    pass


def influence(graph, k, seed, seed_method=SeedMethod.DEGREE, model=InfluenceModel.INDEPENDENT_CASCADE, optimal=False, probability=0.2, parallel=False):
    '''
    Returns a list of influential nodes in a graph
    graph is the igraph object
    k is the number of most influential nodes in the graph
    iterations is the depth of neighbourhood to which a node can spread its influence. If exhaustive run is required, set this to -1.
    seed is the list of nodes initially activated. When not provided, seed_method is used to create seed from the graph
    seed_method is the method to select list of nodes to activate initially. If the list of seed nodes are already provided, then this is ignored
    model is the algorithm to determine the influence
    optimal defines whether the optimal set of influential nodes is required. This should not be set to True for graphs of sizes over 100
    probability is the probability of a node to successfully influence another node
    parallel defines whether the process will run on single thread or multiple
    '''

    # IC Algorithm: Independent Cascade model takes a network (graph) as input and some value k.
    # From G, k fraction of nodes are initially activated by some method. Next, we attempt to activate more nodes in the neighbourhood of these nodes.
    # Each active node attempts to activate each of its neighbour nodes with a global probability p (this is 0.5 for coin toss method)
    # Whether an attempt succeeds or fails, a node cannot be attempted for activation twice by any of the active neighbours.

    # LT Algorithm: Linear Threshold model takes a network (graph) as input and some value k.
    # From G, k fraction of nodes are initially activated randomly. Then we attempt to activate more nodes in the neighbourhood of these nodes.
    # A node v actiates only if sum of weights of its active neighbour nodes equals or exceeds its threshold (assigned randomly here).
    # In the given function, if the fraction of active nodes in neighbourhood equals or exceeds the threshold, the inactive node becomes active
    # The process continues for t steps, in each step, the nodes activated in step t-1 also take part in diffusion process
    pass



source = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 4, 5]
target = [2, 3, 4, 5, 6, 7, 8, 9, 2, 3, 4, 5, 6, 7, 8, 9, 6, 7, 8, 9]
graph = Graph(directed=True)
graph.add_nodes(10)
graph.add_edges(zip(source,  target))

k = 2
prob = 0.2
n_iters = 100
np.random.seed(1234)
graph = Graph.Erdos_Renyi(n=50, m=100, directed=True)
k = 5
prob = 0.1
nodes, spreads, elapsed = greedy_independent_cascade(graph, k, prob, n_iters)
print("nodes: {n}; spreads: {s}; elapsed: {e}".format(n=nodes, s=spreads, e=elapsed))
nodes, spreads, elapsed, lookups = greedy_independent_cascade_celf(graph, k, prob, n_iters)
print("nodes: {n}; spreads: {s}; elapsed: {e}; lookups: {l}".format(n=nodes, s=spreads, e=elapsed, l=lookups))
