
test_sigma_from_igraph = function() {

  # test writing
  ## this is what we want
  nodes = list(list(key = 1, attributes = list(size = 2)),
               list(key = 2, attributes = list(size = 3)))
  jsonlite::toJSON(nodes, pretty = TRUE, auto_unbox = TRUE)

  ## this is now superseded by nodes_to_json
  nodes = head(iris, 20)
  nodes$key = seq_len(nrow(nodes))
  nodes = lapply(as.data.frame(trimws(t(nodes))), function(vec) {
      setNames(vec, names(nodes)) %>%
          { list(key = .[['key']], attributes = as.list(.[names(iris)])) }
    })
  names(nodes) = NULL
  nodes = lapply(nodes, function(node) {
      node$attributes[1:4] %<>% lapply(as.numeric)
      node
    })

  graphOut <- list(nodes)
  names(graphOut) <- c('nodes')
  out <- jsonlite::toJSON(graphOut, pretty = TRUE, auto_unbox = TRUE)
  ## need to keep unbox

  testthat::expect_true(TRUE)




  # test reading
  ## when importing, a strange data frame object, consisting of two data frames
  ## but works well
  nodes <- jsonlite::fromJSON(out)$nodes
  nodes = list(list(key = 1, attributes = list(x = 1, y = 1, size = 2)),
               list(key = 2, attributes = list(x = 2, y = 2, size = 3)))
  out = jsonlite::toJSON(nodes, pretty = TRUE, auto_unbox = TRUE)
  nodes <- jsonlite::fromJSON(out)

  testthat::expect_true(TRUE)
}
test_that('sigma_from_igraph', test_sigma_from_igraph())
