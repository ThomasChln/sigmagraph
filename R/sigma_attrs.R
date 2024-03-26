#' Modify the node size of a sigmagraph object.
#'
#' Modify the node size of an existing sigmagraph object by providing either:
#' (1) A single size to use for all nodes; (2) a vector of node sizes; or (3) a
#' metric to use to scale the nodes.
#'
#' @param sigma_obj   Sigmagraph object, returned by sigma_from_igraph function
#' @param min_size    Minimum node size on the graph (for scaling)
#' @param max_size    Maximum node size on the graph (for scaling)
#' @param one_size    A single size to use for all nodes
#' @param size_vector An optional vector with the sizes for each node
#' @param size_metric The metric to use when sizing the nodes.
#'                    Options are: degree, closeness, betweenness, pageRank, or
#'                    eigenCentrality.
#'
#' @return A sigmagraph object with modified node sizes
#'
#' @examples
#' library(igraph)
#' library(sigmagraph)
#'
#' data(lesMis)
#'
#' layout <- layout_nicely(lesMis)
#'
#' # one size for all nodes
#' sig <- sigma_from_igraph(graph = lesMis, layout = layout) %>%
#'   add_node_size(oneSize = 3)
#' sig
#'
#' # using a vector
#' custom_size <- log10(degree(lesMis))
#' sig <- sigma_from_igraph(graph = lesMis, layout = layout) %>%
#'  add_node_size(sizeVector = custom_size)
#' sig
#'
#' @export
add_node_size <- function(sigma_obj, min_size = 1, max_size = 3,
  one_size = NULL, size_vector = NULL) {

  json_obj = jsonlite::fromJSON(sigma_obj$x$data)
  edges <- json_obj$edges
  nodes <- json_obj$nodes
  directed <- json_obj$directed
  
  if (!is.null(one_size)) {

    nodes$attributes$size = one_size
    sigma_obj$x$options$minNodeSize <- one_size
    sigma_obj$x$options$maxNodeSize <- one_size

  } else if (!is.null(size_vector)) {

    nodes$attributes$size <- size_vector
    sigma_obj$x$options$minNodeSize <- min_size
    sigma_obj$x$options$maxNodeSize <- max_size

  }

  sgraph <- list(nodes = nodes, edges = edges, directed = directed)
  sigma_obj$x$data <- jsonlite::toJSON(sgraph, auto_unbox = TRUE)

  sigma_obj
}

#' Modify the node labels of a sigmagraph object.
#'
#' Modify the node labels of an existing sigmagraph object by providing an
#' attribute from the initial igraph to use as the labels.
#'
#' @param sigma_obj   Sigmagraph object, returned by sigma_from_igraph function
#' @param label_attr  Attribute to use to replace node labels
#'
#' @return Sigmagraph object with modified node labels
#'
#' @examples
#' library(igraph)
#' library(sigmagraph)
#'
#' data(lesMis)
#'
#' sig <- sigma_from_igraph(graph = lesMis) %>%
#'   add_node_labels(label_attr = 'label')
#' sig
#'
#' @export
add_node_labels <- function(sigma_obj, label_attr = NULL) {

  json_obj = jsonlite::fromJSON(sigma_obj$x$data)
  nodes <- json_obj$nodes

  nodes$attributes$label <- if (length(label_attr) > 1) {
    concat_labels(sigma_obj$x$graph$vertices, label_attr)
  } else {
    as.character(sigma_obj$x$graph$vertices[, label_attr])
  }

  sgraph <- list(nodes = nodes, edges = json_obj$edges,
                 directed = json_obj$directed)
  sigma_obj$x$data <- jsonlite::toJSON(sgraph, auto_unbox = TRUE)

  sigma_obj
}


concat_labels = function(df_nodes, label_attr) {
  apply(df_nodes, 1, function(row) {
      paste0(names(label_attr), row[label_attr], collapse = '\n')
    })
}

#' Modify the edge size of a sigmagraph object.
#'
#' Modify the edge size of a sigmagraph by providing a single size
#'
#' @param sigma_obj Sigmagraph object
#' @param one_size  A single size to use for all edges
#'
#' @return Sigmagraph with modified edge sizes
#'
#' @examples
#' library(igraph)
#' library(sigmagraph)
#'
#' data(lesMis)
#'
#' sig <- sigma_from_igraph(graph = lesMis) %>%
#'   add_edge_size(one_size = 0.1)
#' sig
#'
#' @export
add_edge_size <- function(sigma_obj, one_size = NULL) {

  json_obj = jsonlite::fromJSON(sigma_obj$x$data)
  edges <- json_obj$edges
  nodes <- json_obj$nodes
  directed <- json_obj$directed

  if (one_size == 0) edges$attributes$hidden = TRUE
  edges$attributes$size <- one_size
  sigma_obj$x$options$minEdgeSize <- one_size
  sigma_obj$x$options$maxEdgeSize <- one_size

  graph <- list(nodes = nodes, edges = edges, directed = directed)
  sigma_obj$x$data <- jsonlite::toJSON(graph, auto_unbox = TRUE)

  sigma_obj
}

#' Modify the edge colors of a sigmagraph object.
#'
#' Modify the edge colors of a sigmagraph object by providing a single color.
#'
#' @param sigma_obj Sigmagraph object
#' @param one_color A single color to color all of the nodes (hex format)
#'
#' @return Sigmagraph with modified edge colors
#'
#' @examples
#' library(igraph)
#' library(sigmagraph)
#'
#' data(lesMis)
#'
#' sig <- sigma_from_igraph(graph = lesMis) %>%
#'   add_edge_color(one_color = "#ccc")
#' sig
#'
#' @export
add_edge_color <- function(sigma_obj, one_color = NULL) {

  json_obj = jsonlite::fromJSON(sigma_obj$x$data)
  edges <- json_obj$edges
  nodes <- json_obj$nodes
  directed <- json_obj$directed

  edges$attributes$color <- one_color

  graph <- list(nodes = nodes, edges = edges, directed = directed)
  sigma_obj$x$data <- jsonlite::toJSON(graph, auto_unbox = TRUE)

  sigma_obj
}
