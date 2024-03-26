devtools::load_all()
#library(sigmagraph)


shiny_map_server = function(input, output, session) {

  dirpath = system.file('data', package = 'sigmagraph')
  kgraph = get(load(file.path(dirpath, 'epmc_1700_cuis_kg.rds')))
  lgraph = kgraph_to_lgraph(kgraph)

  # test multiline labels and color_map
  lgraph$df_nodes$cui = lgraph$df_nodes$name %>%
      gsub('.*[(](C[0-9]+)[)]$', '\\1', .) %>%
      ifelse(lgraph$df_nodes$clusters, ., 'NA')

  lgraph$df_nodes$name %<>% gsub(' [(](C[0-9]+)[)]$', '', .)
  lgraph$df_links[[1]] %<>% gsub(' [(](C[0-9]+)[)]$', '', .)
  lgraph$df_links[[2]] %<>% gsub(' [(](C[0-9]+)[)]$', '', .)

  igraph = l_graph_to_igraph(lgraph)

  color_map = data.frame(group = c(TRUE, FALSE), color = c('#0000ff', '#ffff00'))
  sgraph = sigma_mutuals(igraph, niter = 500, node_size = 7,
                         color_map = color_map,
                         label = c('name', `CUI: ` = 'cui'))

  output$kg = renderSigmagraph(sgraph)

  output
}


shiny::shinyApp(
  ui = shiny::fillPage(sigmagraphOutput('kg', height = '100%')),
  server = shiny_map_server)

