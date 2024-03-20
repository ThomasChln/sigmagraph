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
  shinyRenderWidget(expr, sigmagraphOutput, env, quoted = TRUE)
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
#' library(sigmagraph)
#' data(lesMis)
#'
#' sig <- sigma_from_igraph(graph = upgrade_graph(lesMis))
#' sig
#' @export
sigma_from_igraph <- function(graph, layout = NULL, width = NULL,
                              height = NULL, elementId = NULL) {

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
  nodes$size <- 3
  nodes[c('x', 'y')] %<>% lapply(as.numeric)
  nodes$color <- '#3182bd'

  edges$source %<>% match(nodes$label) %>% `[`(nodes$key, .)
  edges$target %<>% match(nodes$label) %>% `[`(nodes$key, .)

  nodes$label %<>% as.character
  edges[c('source', 'target')] %<>% lapply(as.character)

  # adapt changes for v2.4.0
  nodes %<>% graph_to_json 
  edges %<>% graph_to_json('edges')

  graphOut <- list(nodes = nodes, edges = edges, directed = directed_flag)

  options <- list(minNodeSize = 1, maxNodeSize = 3, minEdgeSize = 3,
                  maxEdgeSize = 1, neighborEvent = 'onClick',
                  neighborStart = 'clickNode', neighborEnd = 'clickStage',
                  doubleClickZoom = TRUE, mouseWheelZoom = TRUE,
                  edgeArrows = 'def')

  out <- jsonlite::toJSON(graphOut, pretty = TRUE, auto_unbox = TRUE)
  x <- list(data = out, options = options, graph = graph_parse)

  createWidget(name = 'sigmagraph', x, width = width, height = height,
               package = 'sigmagraph', elementId = elementId)
}

# separate df in keys fields and attributes
graph_to_json = function(df_input, type = c('nodes', 'edges')) {

  type = match.arg(type)
  fields = switch(type, nodes = 'key',
                  edges = c('key', 'source', 'target'))
  num_attrs = switch(type, nodes = c('x', 'y', 'size'), edges = 'size')
  
  names_attr = setdiff(names(df_input), fields)

  # t breaks if not characters
  df_input$key %<>% as.character
  df_json = as.data.frame(trimws(t(df_input))) %>%
      lapply(graph_row_to_json, names(df_input), fields, names_attr, num_attrs)

  setNames(df_json, NULL)
}

graph_row_to_json = function(vec, names_df, fields, names_attr, num_attrs) {

  vec = setNames(vec, names_df)
  vec = append(as.list(vec[fields]),
               list(attributes = as.list(vec[names_attr])))

  vec$attributes[num_attrs] = lapply(vec$attributes[num_attrs], as.numeric)

  vec 
}
