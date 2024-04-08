
# attenuate edges, highlight multiple connected nodes
highlight_multiple_connected = function(df_links, selected_phenos) {

  df_links_pheno = subset(df_links, from %in% selected_phenos)

  if (is.null(names(selected_phenos))) {

    full_connect_nodes = df_links_pheno %$%
      { names(which(table(to) == length(selected_phenos))) }
 
    mult_connect_nodes = df_links_pheno %$%
      { names(which(table(to) > 1)) }

  } else {

    df_links_pheno$group = df_links_pheno %$%
        stringi:::stri_replace_all_fixed(from, selected_phenos, names(selected_phenos), vectorize_all = FALSE)
    df_group_uniq = unique(df_links_pheno[c('group', 'to')])
    full_connect_nodes = df_group_uniq %$%
      { names(which(table(to) == length(unique(group)))) }
 
    mult_connect_nodes = df_group_uniq %$%
      { names(which(table(to) > 1)) }
  }

  df_links$color = if (length(mult_connect_nodes) > 20) "#efefef" else "#ddd"
  mult_highlight = if (length(mult_connect_nodes) > 20) "#bbb" else "#444"
  full_highlight = if (length(full_connect_nodes) > 20) "#ddd" else "#444"

  df_links$color[df_links$to %in% mult_connect_nodes] = mult_highlight
  df_links$color[df_links$to %in% full_connect_nodes] = full_highlight

  df_links$zindex = (df_links$to %in% full_connect_nodes) + (df_links$to %in% mult_connect_nodes)
  df_links
}

# apply spring weights (revert, best not to add target nodes weak links)
convert_to_spring_weights = function(df_links, selected_phenos = NULL) {
  df_links$weight %<>% { max(.) - . + 1}
  if (length(selected_phenos) <= 1) return(df_links)

  df_links_targets = if (length(selected_phenos) == 2) {
      as.list(selected_phenos)
  } else {
      combn(selected_phenos, 2) |> t()
  }
  df_links_targets %<>% as.data.frame %>% setNames(c('from', 'to'))

  df_links_targets$weight = max(df_links$weight)# + diff(range(df_links$weight))
  df_links %<>% rbind(df_links_targets)
}

scale_graph = function(weights) {
  #{ (. - min(.)) / (max(.) - min(.)) } %>% `+`(.5) %>% `*`(10)
  upper_bound_mult = if (length(weights) > 1e3) 2 else 4
  lower_bound_const = if (length(weights) > 1e3) 2 else 3
  scale(weights, center = FALSE) %>% `*`(upper_bound_mult) %>%
      ifelse(. < lower_bound_const, lower_bound_const, .)
}

kgraph_to_lgraph = function(l_fit_embeds) {

  df_projs = l_fit_embeds$df_projs

  df_projs %<>% order_dataframe(relevant_pattern = 'suicid')

  df_nodes = data.frame(name = unique(unlist(df_projs[1:2])))
  # color by CUIs
  df_nodes$clusters = grepl('[)]$', df_nodes$name)

  igraph = setNames(df_projs, c(1:2, 'weight')) %>%
      list(df_links = ., df_nodes = df_nodes)
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
