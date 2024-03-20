
kgraph_to_igraph = function(l_fit_embeds) {

  df_projs = l_fit_embeds$df_projs

  df_projs %<>% order_dataframe(relevant_pattern = 'suicid')

  df_nodes = data.frame(name = unique(unlist(df_projs[1:2])))
  # color by CUIs
  df_nodes$clusters = grepl('[)]$', df_nodes$name)

  igraph = setNames(df_projs, c(1:2, 'weight')) %>%
      list(df_links = ., df_nodes = df_nodes) %>% l_graph_to_igraph
}



order_dataframe = function(df_x, cols = 1:2, relevant_pattern = NULL) {

  # put strings matching relevant patterns in first column
  if (!is.null(relevant_pattern)) {

    df_x %<>% subset(grepl(relevant_pattern, .[[cols[1]]]) |
                     grepl(relevant_pattern, .[[cols[2]]]))

    uniq_lvls = unique(unlist(df_x[cols]))
    relevant_lvls = grep(relevant_pattern, uniq_lvls)
    uniq_lvls = c(uniq_lvls[relevant_lvls], uniq_lvls[-relevant_lvls])

    df_x[cols] %<>% lapply(function(lvls) factor(lvls, uniq_lvls) %>% as.numeric)
    df_x %<>% order_dataframe
    df_x[cols] %<>% lapply(function(lvls) uniq_lvls[lvls])

  } else {
    df_x[cols] = apply(df_x[cols], 1, sort) %>%
      t %>% as.data.frame %>% setNames(names(df_x[cols]))
  }

  df_x
}


l_graph_to_igraph = function(l_graph) {

  igraph = igraph::graph_from_data_frame(l_graph$df_links)
  igraph %<>% add_igraph_info(l_graph$df_nodes)
}
