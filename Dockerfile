
from rocker/shiny-verse:4.3.2
run apt-get update && \
  # for vignette
  apt-get install -y --no-install-recommends texlive texlive-latex-recommended texlive-fonts-extra qpdf \
  # for igraph
  libxml2-dev libglpk-dev

run R -e "install.packages(c('flexdashboard', 'igraph', 'jsonlite', 'RColorBrewer'))"
run R -e "install.packages('cowplot')"

add ./ /sigmagraph
run R -e "devtools::install('sigmagraph', dependencies = TRUE)"
