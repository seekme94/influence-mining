import unittest
from igraph import Graph
import numpy as np
import graph_util as gu
import influence as inf


class TestInfluence(unittest.TestCase):
    
    def setUp(self):
        # Nodes 0 and 1 are obviously influential
        source = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 4, 5]
        target = [2, 3, 4, 5, 6, 7, 8, 9, 2, 3, 4, 5, 6, 7, 8, 9, 6, 7, 8, 9]
        self.graph = Graph(directed=True)
        self.graph.add_vertices(10)
        self.graph.add_edges(zip(source,  target))
        self.graph.get_adjlist()
        np.random.seed(1234)
        self.graph2 = Graph.Erdos_Renyi(n=50, m=100, directed=True)

    def test_get_resilience(self):
        pass

    def test_collective_influence_degree(self):
        pass

    def test_select_seed(self):
        pass

    def test_optimal_maximization(self):
        pass

    def test_greedy_influence(self):
        pass

    def test_greedy_independent_cascade(self):
        nodes, spreads, elapsed = inf.greedy_independent_cascade(self.graph2, k=5, probability=0.2, iterations=100)
        for spread in spreads:
            self.assertGreater(spread, 1)

    def test_greedy_independent_cascade_celf(self):
        nodes, spreads, elapsed, lookups = inf.greedy_independent_cascade_celf(self.graph2, k=5, probability=0.2, iterations=100)
        for spread in spreads:
            self.assertGreater(spread, 1)

    def test_spread_independent_cascade(self):
        seed = [0]
        spread = inf.spread_independent_cascade(self.graph, seed, probability=0.2)
        self.assertGreaterEqual(spread, 2)

    def test_spread_linear_threshold(self):
        pass

    def test_influence(self):
        pass


if __name__ == '__main__':
    unittest.main()
