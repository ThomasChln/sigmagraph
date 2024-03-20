# sigmagraph

R package: sigma.js network visualization v2.4.0

Htmlwidget wrapper for the sigma.js network visualization v2.4.0, inspired by the sigmaNet package.

## Installation

Development version, using the remotes package in R:

```r
  remotes::install_git('https://gitlab.com/thomaschln/sigmagraph.git')
```

## Usage

Build a sigmagraph object from an igraph object

```r
  library(igraph)
  library(sigmagraph)
 
  data(lesMis)
 
  sig <- sigma_from_igraph(lesMis)
  sig
```

Modify the node size of a sigmagraph object.

```r
 
  # one size for all nodes
  sig %>% add_node_size(one_size = 7)
 
  # using a vector
  df_nodes = cbind.data.frame(name = vertex_attr(lesMis, 'id'),
    degree = degree(lesMis))

  # seems sigma.js is not scaling automatically with min_size and max_size
  # do it manually for now
  df_nodes$degree %<>% scale(center = FALSE) %>% `*`(3) %>% `+`(3)

  igraph = add_igraph_info(lesMis, df_nodes)

  sig <- sigma_from_igraph(lesMis) %>%
   add_node_size(size_vector = vertex_attr(igraph, 'degree'), min_size = 3, max_size = 8)
  sig
```

Modify the node labels of a sigmagraph object.

```r
  sig %>%
    add_node_labels(label_attr = 'label')
```

Use in Shiny and Flexdashboard (see examples in shiny-server folder)

## Development status

This is a prototype version and I plan on making a full release that will
integrate a vignette, tests and additional functionalities as sigmaNet-like
modifiers and new sigma.js features (especially reducers and clusters).
Hit the star button if you'd like to see it on CRAN as soon as possible !

## Acknowledgements

Development funded by [PARSE Health](https://parse-health.org/) and led by Prof. Cai.
Code heavily inspired by sigmaNet package.

## License

This package is free and open source software, licensed under GPL-3.
