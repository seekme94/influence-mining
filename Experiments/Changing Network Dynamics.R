# This experiment learns the impact of small changes to the network which do and do not impact influential nodes

# Hypothesises:
# 1. Attaching a node to all nodes with degree = 1 will not change influential nodes
# 2. From 3 random nodes such that there is need for only single edge to make a triad, adding that edge will not change influential nodes
# 3. Connecting few nodes with degree = 1 and greater distances between them will change influential nodes
# 4. 

# Generate a synthetic graph
graph <- generate_random(100, 0.1)
# 