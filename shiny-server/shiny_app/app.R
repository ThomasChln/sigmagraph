library(sigmagraph)


shiny_map_server = function(input, output, session) {

  dirpath = system.file('data', package = 'sigmagraph')
  kgraph = get(load(file.path(dirpath, 'epmc_1700_cuis_kg.rds')))
  igraph = kgraph_to_igraph(kgraph)
  sgraph = sigma_mutuals(igraph, niter = 500, node_size = 7)

  output$kg = renderSigmagraph(sgraph)

  output
}


shiny::shinyApp(
  ui = shiny::fillPage(sigmagraphOutput('kg', height = '100%')),
  server = shiny_map_server)

