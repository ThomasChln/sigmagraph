
from rocker/shiny-verse:4.3.2
run apt-get update && \
  # for vignette
  apt-get install -y --no-install-recommends texlive texlive-latex-recommended texlive-fonts-extra qpdf \
  # for igraph
  libxml2-dev libglpk-dev

run R -e "install.packages(c('flexdashboard', 'igraph', 'jsonlite', 'RColorBrewer'))"

add ./DESCRIPTION /sigmagraph/DESCRIPTION
run R -e "devtools::install_deps('sigmagraph', dependencies = TRUE)"
add ./ /sigmagraph
run R -e "devtools::install('sigmagraph', dependencies = TRUE)"
