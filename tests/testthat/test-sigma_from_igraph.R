
test_graph_to_json = function() {
  nodes = data.frame(key = c('1', '2'), x = 3:4, y = 5:6, size = 7:8)
  nodes %<>% graph_to_json
  expected = list(list(key = '1', attributes = list(x = 3, y = 5, size = 7)),
                  list(key = '2', attributes = list(x = 4, y = 6, size = 8)))
  expect_identical(nodes, expected)

  nodes = data.frame(key = c('1', '2'), x = 3:4, y = 5:6, size = 7:8)
  nodes %<>% graph_to_json
  expected = list(list(key = '1', attributes = list(x = 3, y = 5, size = 7)),
                  list(key = '2', attributes = list(x = 4, y = 6, size = 8)))
  expect_identical(nodes, expected)

  edges = data.frame(key = c('1', '20'), source = 3:4, target = 5:6, size = 7:8)
  edges %<>% graph_to_json('edges')
  expected = list(list(key = '1', source = '3', target = '5',
                       attributes = list(size = 7)),
                  list(key = '20', source = '4', target = '6',
                       attributes = list(size = 8)))
  expect_identical(edges, expected)

}
test_that('graph_to_json', test_graph_to_json())

test_sigma_from_igraph = function() {

}
test_that('sigma_from_igraph', test_sigma_from_igraph())
