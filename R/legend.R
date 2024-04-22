
get_legend = function(colors_map, clusters) {

  colors_map %<>% subset(group %in% clusters)
  colors_map %<>% cbind(data.frame(x = 1, y = 1))
  colors_map$group %<>% factor(unique(.))

  gglegend = ggplot2::ggplot(colors_map, ggplot2::aes(x, y, color = group)) +
      ggplot2::geom_point(size = 10) +
      ggplot2::scale_color_manual(name = NULL, values = colors_map$color) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.text.position = 'top',
                     legend.title = ggplot2::element_text(size = 20),
                     legend.text = ggplot2::element_text(size = 15))

  gglegend = cowplot::get_legend(gglegend)
}

