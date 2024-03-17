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
 
  layout <- layout_nicely(lesMis)
  sig <- sigma_from_igraph(graph = lesMis, layout = layout)
 
  sig
```

Modify the node size of a sigmagraph object.

```r
  library(magrittr)
 
  # one size for all nodes
  sig <- sigma_from_igraph(graph = lesMis, layout = layout) %>%
    add_node_size(oneSize = 3)
  sig
 
  # using a size attribute
  sig <- sigma_from_igraph(graph = lesMis, layout = layout) %>%
    add_node_size(size_metric = 'degree', min_size = 2, max_size = 8)
  sig
 
  # using a vector
  custom_size <- log10(degree(lesMis))
  sig <- sigma_from_igraph(graph = lesMis, layout = layout) %>%
   add_node_size(sizeVector = custom_size)
  sig
```

Modify the node labels of a sigmagraph object.

```r
  sig <- sigma_from_igraph(graph = lesMis, layout = layout) %>%
    add_node_labels(label_attr = 'label')
  sig
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
