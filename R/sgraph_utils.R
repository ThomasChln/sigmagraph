#' Add nodes information to the igraph object
#'
#' Modify the node attributes of an existing igraph object by providing a
#' dataframe
#'
#' @param igraph   Igraph object to modify
#' @param df_nodes Data frame to add to nodes
#' @fields fileds  Columns of df_nodes to add. First must be the node identifier.
#'
#' @return A sigmagraph object with modified node labels
#'
#' @examples
#' library(sigmagraph)
#' data(lesMis)
#'
#' df_nodes = cbind.data.frame(name = names(igraph::V(lesMis)),
#'   log10_degree = degree(lesMis)$res))
#'
#' igraph = add_igraph_info(lesMis, df_nodes)
#'
#' sig <- sigma_from_igraph(lesMis) %>%
#'   add_node_size(size_vector = 'log10_degree')
#'
#' @export
add_igraph_info = function(igraph, df_nodes, fields = names(df_nodes)) {

  #df_igraph = cbind.data.frame(name = names(igraph::V(igraph)),
  #  degree = igraph::centralization.degree(igraph)$res)
  df_igraph = data.frame(name = vertex_attr(igraph)[[1]])

  df_igraph %<>% merge(df_nodes[fields], by.x = 'name', by.y = fields[1],
      all.x = TRUE, all.y = FALSE, sort = FALSE)

  param_names = names(df_igraph)[-1]

  for (param in param_names) {
    igraph %<>% igraph::set_vertex_attr(param, value = df_igraph[[param]])
  }

  igraph
}

interpolate_palette = function(n_unique, palette) {
  if (n_unique <= length(palette)) return(tail(palette, n_unique))
  grDevices::colorRampPalette(palette)(n_unique)
}

get_color_map = function(colors,
  palette = RColorBrewer::brewer.pal(8, 'Dark2')) {

  unique_colors = unique(colors)
  palette = interpolate_palette(length(unique_colors), palette)
  data.frame(group = unique_colors, color = palette[seq_along(unique_colors)],
    stringsAsFactors = FALSE)
}

sigma_nodes_colors = function(sigmaObj, attr_name = NULL, color_map = NULL,
  palette = RColorBrewer::brewer.pal(8, 'Dark2')) {

  json_data = jsonlite::fromJSON(sigmaObj$x$data)

  colors = sigmaObj$x$graph$vertices[, attr_name]
  if (is.null(color_map)) color_map = get_color_map(colors, palette)
  json_data$nodes$attributes$color = color_map %$% color[match(colors, group)]

  sigmaObj$x$data <- jsonlite::toJSON(json_data, pretty = TRUE, auto_unbox = TRUE)
  sigmaObj
}
                                                                                                                                    
sigma_mutuals = function(igraph, color_map = NULL, niter = 5e3,
                         label = 'word', clusters = TRUE, arrows = FALSE,
                         node_size = NULL) {

  set.seed(0)                                                                   
  sigma_graph = igraph %>%          
    sigma_from_igraph(layout = igraph::layout_with_fr(., niter = niter))

  if (clusters == TRUE) sigma_graph %<>% sigma_nodes_colors('clusters', color_map)

  if (label != 'word') sigma_graph %<>% add_node_labels(label)

  if (!is.null(node_size)) {
      if (is.numeric(node_size)) {
        sigma_graph %<>% add_node_size(one_size = node_size)
      } else {
        sigma_graph %<>% add_node_size(size_vector = igraph::vertex_attr(igraph, node_size))
      }
  }
  
  sigma_graph
}             



igraph_words = function(words_graph, largest_connected = TRUE, min_degree = 0,
  max_nodes = 1500) {

  igraph = igraph::graph_from_data_frame(words_graph$df_links)

  # remove isolated users
  if (min_degree > 0) {
    igraph %<>% igraph::delete_vertices(
      which(igraph::centralization.degree(.)$res < min_degree))
  }

  # remove using modularity
  if (length(igraph::V(igraph)) > max_nodes) {
    metric = igraph::cluster_walktrap(igraph)$modularity
    threshold = sort(metric, decreasing = TRUE)[max_nodes]
    igraph %<>% igraph::delete_vertices(which(metric < threshold))
  }

  # keep only largest connected graph
  if (largest_connected) {
    igraph %<>% igraph::components() %$%
      which(membership != which.max(csize)) %>% names %>%
      igraph::delete_vertices(igraph, .)
  }

  igraph %<>% igraph::simplify()

  if ('df_nodes' %in% names(words_graph)) {
      igraph %<>% add_igraph_info(words_graph$df_nodes,
                                  names(words_graph$df_nodes))
  }

  igraph
}

