#' Create a UI element for a sigmagraph visualization in Shiny
#'
#' @param outputId ID of the UI element
#' @param width    Width of the UI element
#' @param height   Height of the UI element
#'
#' @export
sigmagraphOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, 'sigmagraph', width, height,
                    package = 'sigmagraph')
}

#' Render a sigmagraph visualization in Shiny
#'
#' @param expr   An expression that creates a sigmagraph visualization
#' @param env    Defaults to parent.frame() (cf. Shiny docs)
#' @param quoted Defaults to FALSE (cf. Shiny docs)
#'
#' @export
renderSigmagraph <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) }
  shinyRenderWidget(expr, parseNetOutput, env, quoted = TRUE)
}

#' Build a sigmagraph object from an igraph object
#'
#' Basic sigma.js visualization of an igraph object, with pipeable syntax.
#'
#' @param graph     Igraph object
#' @param layout    Output of an igraph layout function (default: layout_nicely)
#' @param width     Width of the output graph (default: fit container)
#' @param height    Height of the output graph (default: fit container)
#' @param elementId Do not specify, used by the htmlwidgets package
#'
#' @return Htmlwidget object, meant to be called directly to render a default
#'   visualization, or passed to other functions to change attributes
#'   (colors, sizes, interactivity, etc.).
#'
#' @examples
#' library(igraph)
#' library(sigmagraph)
#'
#' data(lesMis)
#'
#' layout <- layout_nicely(lesMis)
#' sig <- sigma_from_igraph(graph = lesMis, layout = layout)
#'
#' sig
#' @export
sigma_from_igraph <- function(graph, layout = NULL, width = NULL,
                              height = NULL, elementId = NULL){

  directed_flag <- igraph::is.directed(graph)
  graph_parse <- igraph::as_data_frame(graph, what = 'both')

  edges <- graph_parse$edges[, c('from', 'to')]
  edges[c('from', 'to')] %<>% lapply(as.character)
  colnames(edges) <- c('source', 'target')
  edges$key <- seq_len(nrow(edges))
  edges$size <- 1
  edges$color <- '#636363'

  nodes <- graph_parse$vertices
  nodes$label <- row.names(nodes)
  layout = if (is.null(layout)) igraph::layout_nicely(graph) else layout
  nodes <- cbind(nodes[, 'label', drop = FALSE], layout)
  colnames(nodes) <- c('label', 'x', 'y')

  nodes$key <- seq_len(nrow(nodes))
  nodes$size <- 1
  nodes[c('x', 'y')] %<>% lapply(as.numeric)
  nodes$color <- '#3182bd'

  edges$source %<>% match(nodes$label) %>% `[`(nodes$key, .)
  edges$target %<>% match(nodes$label) %>% `[`(nodes$key, .)

  nodes$label %<>% as.character
  edges[c('source', 'target')] %<>% lapply(as.character)

  # adapt changes for v2.4.0
  nodes %<>% nodes_to_json 
  edges %<>% edges_to_json 

  graphOut <- list(nodes = nodes, edges = edges, directed = directed_flag)

  options <- list(minNodeSize = 1, maxNodeSize = 3, minEdgeSize = 3,
                  maxEdgeSize = 1, neighborEvent = 'onClick',
                  neighborStart = 'clickNode', neighborEnd = 'clickStage',
                  doubleClickZoom = TRUE, mouseWheelZoom = TRUE,
                  edgeArrows = 'def')

  out <- jsonlite::toJSON(graphOut, pretty = TRUE, auto_unbox = TRUE)
  x <- list(data = out, options = options, graph = graph_parse)

  createWidget(name = 'parseNet', x, width = width, height = height,
               package = 'sigmagraph', elementId = elementId)
}

# returns a list of nodes of 2 elements: key and attributes
nodes_to_json = function(nodes) {

  nodes_attr = setdiff(names(nodes), 'key')

  nodes = lapply(as.data.frame(trimws(t(nodes))), function(vec) {

      node = setNames(vec, names(nodes)) %>%
          { list(key = .[['key']], attributes = as.list(.[nodes_attr])) }

      node$attributes[c('x', 'y', 'size')] %<>% lapply(as.numeric)

      node
    })
  names(nodes) = NULL

  nodes
}

# returns a list of edges of 2 elements: key, source, target and attributes
edges_to_json = function(edges) {

  edges_attr = setdiff(names(edges), c('key', 'source', 'target'))

  edges = lapply(as.data.frame(trimws(t(edges))), function(vec) {

      edge = setNames(vec, names(edges)) %>% {
          list(key = .[['key']], source = .[['source']],
               target = .[['target']], attributes = as.list(.[edges_attr]))
        }

      edge$attributes[c('size')] %<>% lapply(as.numeric)

      edge
    })
  names(edges) = NULL

  edges
}

