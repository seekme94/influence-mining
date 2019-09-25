# -*- coding: utf-8 -*-
"""
Created on Sat Sep 21 19:14:09 2019

@author: owaishussain@outlook.com
"""

from igraph import *
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


def get_resilience(graph, vertex):
    '''
    Retuns the resilience, i.e. the size of the largest connected component in the graph after removing given vertex
    graph is the igraph object
    '''
    pass


def collective_influence_degree(graph, neighbor_distance, vertex):
    '''
    Returns the CI value, the product of degree of target vertex and total sum of degrees of neighborhood
    graph is the igraph object
    neighbor_distance is the distance to which the neighborhood vertices are searched for
    vertex is the target vertex in graph
    '''
    pass


def select_seed(graph, budget, method=SeedMethod.DEGREE):
    '''
    Returns a list of k-vertices (total vertices * budget) which have the highest value of given SeedMethod in a graph
    graph is the igraph object
    budget is the percentage of most influential vertices in the graph
    method is the trait to use on graph vertices
    '''
    pass


def greedy_influence(graph, budget, iterations=3, probability=0.2, model=InfluenceModel.RESILIENCE):
    '''
    Returns a list of influential vertices in a graph using Greedy approach
    graph is the igraph object
    budget is the percentage of most influential vertices in the graph
    iterations is the depth of neighbourhood to which a vertice can spread its influence. If exhaustive run is required, set this to -1.
    probability is the probability of a vertex to successfully influence another vertex
    model is the algorithm to determine the influence
    '''
    pass


def optimal_maximization(graph, budget, iterations=3, model=InfluenceModel.RESILIENCE, parallel=False):
    '''
    Returns a list of most influential vertices in a graph - optimal solution
    graph is the igraph object
    budget is the percentage of most influential vertices in the graph
    iterations is the depth of neighbourhood to which a vertice can spread its influence. If exhaustive run is required, set this to -1.
    model is the algorithm to determine the influence
    parallel defines whether the process will run on single thread or multiple
    '''
    pass


def simulate_independent_cascade(graph, seed):
    '''
    Returns a list of vertices activated during simulation under Independent Cascade model
    graph is the igraph object
    seed is the list of vertices initially activated
    Description: given a weighted graph G and a set of active vertices V, each vertex u in V attempts to activate its neighbours with probability equal to the weight on its edge.
    If a coin toss with this probability is successful, the the inactive neighbour gets activated. Once active, a vertex does not deactivate.
    '''
    pass


def simulate_linear_threshold(graph, seed):
    '''
    Returns a list vertices activated during simulation under Linear Threshold model
    graph is the igraph object
    seed is the list of vertices initially activated
    Description: given a weighted graph G, a set of active vertices V and a set of inactive vertices U, each vertex u in U is activated with a probability equal to collective weight of its neighbours v, which are in V.
    If a coin toss with this probability is successful, the the inactive neighbour gets activated. Once active, a vertex does not deactivate.
    '''
    pass


def spread_independent_cascade(graph, seed, plus_variant=False, iterations=10):
    '''
    Returns average spread of given seed vertices in the graph under Independent Cascade (plus) model
    graph is the igraph object
    seed is the list of vertices initially activated
    plus_variant defines whether to use IC+, i.e. next step is based on previous best
    runs is the number of times to repeat the process in order to calculate average spread
    '''
    pass


def spread_linear_threshold(graph, seed, iterations=10):
    '''
    Returns average spread of given seed vertices in the graph under Linear Threshold model
    graph is the igraph object
    seed is the list of vertices initially activated
    runs is the number of times to repeat the process in order to calculate average spread
    '''
    pass


def influence(graph, budget=0.1, seed, seed_method=SeedMethod.DEGREE, model=InfluenceModel.RESILIENCE, optimal=False, probability=0.2, parallel=False):
    '''
    Returns a list of influential vertices in a graph
    graph is the igraph object
    budget is the percentage of most influential vertices in the graph
    iterations is the depth of neighbourhood to which a vertice can spread its influence. If exhaustive run is required, set this to -1.
    seed is the list of vertices initially activated. When not provided, seed_method is used to create seed from the graph
    seed_method is the method to select list of vertices to activate initially. If the list of seed vertices are already provided, then this is ignored
    model is the algorithm to determine the influence
    optimal defines whether the optimal set of influential vertices is required. This should not be set to True for graphs of sizes over 100
    probability is the probability of a vertex to successfully influence another vertex
    parallel defines whether the process will run on single thread or multiple
    '''

    # IC Algorithm: Independent Cascade model takes a network (graph) as input and some budget (k).
    # From G, k fraction of vertices are initially activated by some method. Next, we attempt to activate more vertices in the neighbourhood of these vertices.
    # Each active vertex attempts to activate each of its neighbour vertices with a global probability p (this is 0.5 for coin toss method)
    # Whether an attempt succeeds or fails, a vertex cannot be attempted for activation twice by any of the active neighbours.

    # LT Algorithm: Linear Threshold model takes a network (graph) as input and some budget (k).
    # From G, k fraction of vertices are initially activated randomly. Then we attempt to activate more vertices in the neighbourhood of these vertices.
    # A vertex v actiates only if sum of weights of its active neighbour vertices equals or exceeds its threshold (assigned randomly here).
    # In the given function, if the fraction of active vertices in neighbourhood equals or exceeds the threshold, the inactive vertex becomes active
    # The process continues for t steps, in each step, the vertices activated in step t-1 also take part in diffusion process
    pass

