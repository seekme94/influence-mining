require(testthat)
require(iterpc)
require(foreach)

source('util/graph_util.R')
source('util/db_util.R')
source('util/heuristics.R')
source('util/community_detection.R')
source('util/graph_util.R')
source('util/influence_maximization.R')

test_that("ic_spread returns some spread", {
  graph <- generate_random(50, 0.2)
  degrees <- degree(graph)
  spread <- ic_spread(graph, V(graph)[which(degrees > 12)])
  expect_gt(spread, 1)
})

test_that("ic_spread does not spread to all nodes", {
  graph <- generate_random(50, 0.2)
  degrees <- degree(graph)
  spread <- ic_spread(graph, V(graph)[which(degrees > 12)])
  expect_lt(spread, vcount(graph))
})

test_that("ic_spread_plus returns some spread", {
  graph <- generate_random(50, 0.2)
  degrees <- degree(graph)
  spread <- ic_spread_plus(graph, V(graph)[which(degrees > 12)])
  expect_gt(spread, 1)
})

test_that("ic_spread_plus does not spread to all nodes", {
  graph <- generate_random(50, 0.2)
  degrees <- degree(graph)
  spread <- ic_spread_plus(graph, V(graph)[which(degrees > 12)])
  expect_lt(spread, vcount(graph))
})

test_that("lt_spread returns some spread", {
  graph <- generate_random(50, 0.2)
  degrees <- degree(graph)
  spread <- lt_spread(graph, V(graph)[which(degrees > 12)])
  expect_gt(spread, 1)
})

test_that("lt_spread does not spread to all nodes", {
  graph <- generate_random(50, 0.2)
  degrees <- degree(graph)
  spread <- lt_spread(graph, V(graph)[which(degrees > 12)])
  expect_lt(spread, vcount(graph))
})

test_that("simulate_ic activates some nodes", {
  graph <- generate_random(50, 0.2)
  active <- V(graph)[which(degree(graph) >= 24)]
  activated <- simulate_ic(graph, active)
  expect_gt(activated, 1)
})

test_that("simulate_ic does not activate all nodes", {
  graph <- generate_random(50, 0.2)
  active <- V(graph)[which(degree(graph) >= 24)]
  activated <- simulate_ic(graph, active)
  expect_lt(activated, vcount(graph))
})

test_that("simulate_lt activates some nodes", {
  graph <- generate_random(100, 0.2)
  active <- V(graph)[which(degree(graph) >= 24)]
  activated <- simulate_lt(graph, active, threshold=0.4)
  expect_gt(activated, 1)
})

test_that("simulate_lt does not activate all nodes", {
  graph <- generate_random(100, 0.2)
  active <- V(graph)[which(degree(graph) >= 24)]
  activated <- simulate_lt(graph, active, threshold=0.4)
  expect_lt(activated, vcount(graph))
})



size <- 50
budget <- size * 0.1
prob <- 1 / budget
steps <- round(sqrt(size)) # Since influence drops exponentially, therefore below this range, it will be negligible

# Generate several graphs of various types
set.seed(100)
graph <- generate_random(size, prob)
combinations <- getall(iterpc(vcount(graph), budget))
combinations <- cbind(combinations, 0)
combinations <- cbind(combinations, 0)
max_spread_ic <- 0
max_spread_lt <- 0
samples <- sample(1:nrow(combinations), 1000)
for (i in samples) {
  seed <- combinations[i, 1:budget]
  spread_ic <- influence_ic(graph, seed, budget, 0.5)$influence
  combinations[i,(budget + 1)] <- spread_ic
  if (spread_ic > max_spread_ic) {
    max_spread_ic <- spread_ic
    print(paste("Max IC spread:", max_spread_ic))
  }
  spread_lt <- influence_lt(graph, seed, steps, 0.5)$influence
  combinations[i,(budget + 2)] <- spread_lt
  if (spread_lt > max_spread_lt) {
    max_spread_lt <- spread_lt
    print(paste("Max LT spread:", max_spread_lt))
  }
}
