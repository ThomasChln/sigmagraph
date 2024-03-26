devtools::load_all()
library(shiny)
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
                         color_map = color_map, label_color = 'ff0000',
                         label = c('name', `CUI: ` = 'cui'))

  # test edge size, seems size below 1 have no impact
  # sgraph %<>% add_edge_size(one_size = 5)
  # sgraph %<>% add_edge_size(one_size = 0.1)

  # size 0 hides them completely
  # sgraph %<>% add_edge_size(one_size = 0)
  
  # test edge color
  sgraph %<>% add_edge_color(one_color = "#ccc")

  color_map %<>% cbind(data.frame(x = 1, y = 1))
  color_map$group %<>% factor(unique(.))

  gglegend = ggplot2::ggplot(color_map, ggplot2::aes(x, y, color = group)) +
      ggplot2::geom_point(size = 10) +
      ggplot2::scale_color_manual(name = 'CUI', values = color_map$color) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.text.position = 'top',
                     legend.title = ggplot2::element_text(size = 20),
                     legend.text = ggplot2::element_text(size = 15))

  #gglegend = ggplot2::ggplotGrob(gglegend) %>% gtable::gtable_filter('guide')
  gglegend = cowplot::get_legend(gglegend)
      
  

  output$kg = renderSigmagraph(sgraph)
  output$gglegend = renderPlot(grid::grid.draw(gglegend))

  output
}


shinyApp(
  ui = fillPage(sigmagraphOutput('kg', height = '100%'),
                absolutePanel(id = "legend", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto", width = 80, height = "auto",
                              plotOutput("gglegend", height = '300px')
                              )
                ),
  server = shiny_map_server)

