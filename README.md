# Project: influence-mining
## Version:
> 0.1.0

The purpose of this project is to provide an interface to perform influence mining operations on networks, preferably Social networks.

The source contains the following files:
  1. Influence.R
  2. Under construction

```
Influence.R
```
The file contains source code for implementation of two basic influence mining models: Independent Cascade model and Linear Threshold model[1]
```
influence (edgesFile, budget, steps, model, p)
```
This is a wrapper function to call influence_LT and influence_IC functions
- edgesFile: complete path to the dataset file, which MUST be in form of white-space separated edge list, representing a network
- budget: the percentage of total nodes from the network that should be initially activated in order to start the diffusion process. Default value is 5%
- steps: the time steps for which, the diffusion process should run. Default value is 1
- model: the influence model to run the dataset on. Value MUST either be "LT" or "IC". Default value is "LT"
seed_method: the selection method for seed (initial nodes). Value can be "random", "degree", "closeness", "betweenness", "coreness" or "eigenvector". Default value is "random"
- p: the probability of activation of a neighbour node. This is applicable only to IC model. Default value is 0.5

> Output: summary of influence process, including no. of nodes, edges, seed set size, nodes influenced and time taken

```
influence_LT
```
This function calculates influence (number of nodes in the network expected to be activated) under Linear Threshold model. For parameters, see influence function.

```
influence_IC
```
This function calculates influence (number of nodes in the network expected to be activated) under Independent Cascade model. For parameters, see influence function.

```
select_seed
```
This function returns a set of nodes, to be used as seed in influence functions on the basis of given seed selection method
- G: a graph object of library *igraph*
- k: percentage of seed nodes from the network to be chosen
- seed_method: see influence function

> Output: subset vector of nodes in a graph

### Examples:
1. Calculate influence under defaults (model="LT", budget=5, steps=1 and seed_method="random")
```
influence(edgesFile="C:/Datasets/twitter_edges.csv")
```
2. Calculate influence under IC model, budget=10% for 2 time steps and seed_method="random"
```
influence(edgesFile="C:/Datasets/twitter_edges.csv", budget=10, steps=2, model="IC")
```
3. Calculate influence under IC model to select 10% nodes for 2 time steps and seed selection criteria to be nodes with highest degree
```
influence(edgesFile="C:/Datasets/twitter_edges.csv", budget=10, steps=2, model="IC", seed_method="degree")
```
4. Calculate influence under LT model to select 5% nodes for 1 time steps and seed selection criteria to be nodes with highest betweenness
```
influence(edgesFile="C:/Datasets/twitter_edges.csv", seed_method="betweenness")
```

### References:
[1] Kempe, D., Kleinberg, J., & Tardos, É. (2003). Maximizing the Spread of Influence through a Social Network. In Proceedings of the ninth ACM SIGKDD international conference on Knowledge discovery and data mining - KDD ’03 (p. 137). New York, New York, USA: ACM Press. doi:10.1145/956755.956769
