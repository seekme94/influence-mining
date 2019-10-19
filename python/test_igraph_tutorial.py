import unittest
from igraph import Graph
import numpy as np
import graph_util as gu


class TestSum(unittest.TestCase):

    def test_normalize(self):
        normal = gu.normalize([1,1,2,3])
        normal

    def test_create_graph(self):
        g = gu.create_graph(vertices=5, edges=[(1, 3), (2, 3), (1, 4), (2, 4)])
        self.assertTrue(isinstance(g, Graph))

    def test_create_edgelist_graph(self):
        g = gu.create_edgelist_graph("sample_edgelist.txt", directed = False, weights = False)
        self.assertTrue(isinstance(g, Graph))

    def test_get_graph_traits_degree(self):
        g = gu.create_graph(vertices=3, edges=[(0, 1), (1, 2), (0, 2)])
        result = gu.get_graph_traits(g, traits=[gu.Trait.DEGREE], normalize=False)
        self.assertEquals(result["degree"], [2, 2, 2])

    def test_get_graph_traits_betweenness(self):
        n = 50
        degree_mean = 5
        m = list(np.random.poisson(degree_mean, n))
        g = Graph.Barabasi(n, m, outpref=True, directed=False, start_from=None)
        result = gu.get_graph_traits(g, traits=[gu.Trait.BETWEENNESS], normalize=False)
        self.assertEquals(len(result["betweenness"]), n)

    def test_get_graph_traits_eigenvalue(self):
        n = 50
        degree_mean = 3
        m = list(np.random.poisson(degree_mean, n))
        g = Graph.Barabasi(n, m, outpref=True, directed=False, start_from=None)
        result = gu.get_graph_traits(g, traits=[gu.Trait.EIGENVALUE], normalize=False)
        self.assertEquals(len(result["eigenvalue"]), n)

    def test_get_graph_traits_pagerank(self):
        n = 50
        degree_mean = 3
        m = list(np.random.poisson(degree_mean, n))
        g = Graph.Barabasi(n, m, outpref=True, directed=False, start_from=None)
        result = gu.get_graph_traits(g, traits=[gu.Trait.PAGERANK], normalize=False)
        self.assertEquals(len(result["pagerank"]), n)

    def test_fit_power_law(self):
        g = Graph.Watts_Strogatz(dim=2, size=100, nei=3, p=0.5)
        alpha = gu.fit_power_law(g, plot=False)
        self.assertGreater(alpha, 1)
        self.assertLess(alpha, 2)

if __name__ == '__main__':
    unittest.main()

graph = Graph.Watts_Strogatz(dim=2, size=50, nei=2, p=0.15)
graph.summary()
summary = gu.get_graph_traits(graph)
