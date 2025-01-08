source('modules/data_preprocessing.R')
source('modules/consensus_clustering.R')
source('modules/limma_analysis.R')
source('modules/auto_limma_analysis.R')
source('modules/information.R')

#Author: JG Lab
#Year: 2023
cran_packages = c("shiny", "shinythemes", "ggplot2", "dplyr","openxlsx","data.table", "tidyverse", "ggpubr",
                  "devtools", "ggfortify", "partitions", "DT", "shinycssloaders", "cluster", "survival", "randomForest",
                  "missForest", "glmnet", "Rcpp", "foreach", "itertools", "iterators", "Matrix", "devtools", "plotly",
                  "magick", "stringi", "manhattanly", "gplots", "RColorBrewer", "grid", "ragg", "htmltools", "bsplus",
                  "zip", "gridExtra", "shinyjs", "shinyBS", "shinycssloaders", "shinybusy")
github_packages = list(
  "WangLab-MSSM/DreamAI/Code", # Specify GitHub packages in "username/repository" format
  "rstudio/shinythemes"
)
github_packages_with_opts = list(
  "jdreyf/ezlimma", # Specify GitHub packages in "username/repository" format
  "jdreyf/ezlimmaplot",
  "jdreyf/Hitman",
  "jdreyf/jdcbioinfo"
)
bioc_packages = c("impute", "EnhancedVolcano", "limma", "preprocessCore", "vsn", "ggtree", "ConsensusClusterPlus")

install_cran_packages = function(packages) {
  missing_cran = packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missing_cran)) {
    install.packages(missing_cran)
  }
}

install_github_packages = function(packages) {
  if (!"remotes" %in% installed.packages()[, "Package"]) {
    install.packages("remotes")  # Install remotes if missing
  }
  for (pkg in packages) {
    repo = strsplit(pkg, "/")[[1]][2]  # Extract the package name
    if (!repo %in% installed.packages()[, "Package"]) {
      remotes::install_github(pkg)
    }
  }
}

install_github_packages_with_opts = function(packages) {
  if (!"remotes" %in% installed.packages()[, "Package"]) {
    install.packages("remotes")  # Install remotes if missing
  }
  for (pkg in packages) {
    repo = strsplit(pkg, "/")[[1]][2]  # Extract the package name
    if (!repo %in% installed.packages()[, "Package"]) {
      remotes::install_github(pkg, build_opts = c("--no-resave-data", "--no-manual"))
    }
  }
}

install_bioc_packages = function(packages) {
  if (!"BiocManager" %in% installed.packages()[, "Package"]) {
    install.packages("BiocManager")
  }
  missing_bioc = packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missing_bioc)) {
    BiocManager::install(missing_bioc)
  }
}

#Install missing packages
install_cran_packages(cran_packages)
install_github_packages(github_packages)
install_github_packages(github_packages_with_opts)
install_bioc_packages(bioc_packages)

required_packages = c(
  cran_packages,
  sapply(github_packages, function(x) strsplit(x, "/")[[1]][2]),
  sapply(github_packages_with_opts, function(x) strsplit(x, "/")[[1]][2]),
  bioc_packages
)
#Load packages
lapply(required_packages, library, character.only = TRUE)

about_page = tabPanel(
  title = "Help",
  titlePanel("Help Page"),
  informationUI("information"),
  tags$footer("Created with R Shiny, ","2024 January")
)

preprocessing_page = tabPanel(
  title = "Data Preperation",
  titlePanel("Data Preprocessing"),
  data_preprocessingUI("data_preprocessing")
)

limma_page = tabPanel(
  title = "Differential Expression",
  titlePanel("Limma Analysis"),
  value = "limma",
  data_limmaUI("data_limma")
)

auto_limma_page = tabPanel(
  title = "Automated Combinatory Differential Expression",
  titlePanel("Automated Combinatory Differential Expression Analysis"),
  value = "autolimma",
  data_auto_limmaUI("data_auto_limma")
)

consensus_clustering_page = tabPanel(
  title = "Consensus Clustering",
  titlePanel("Perform Consensus Clustering"),
  value = "consclustering",
  data_clusteringUI("data_clustering")
)

ui = navbarPage(
  title = "FlexStat 1.0",
  theme = shinytheme('united'),
  id = "inTabset",
  preprocessing_page,
  limma_page,
  auto_limma_page,
  consensus_clustering_page,
  about_page,
)

server = function(input, output, session) {

  # Reactive variables
  # variables = reactiveValues(
  #   data_table = NULL,
  #   auto_data_table = NULL,
  #   pipeline_data = NULL,
  #   pipline_ant = NULL,
  #   replicates = NULL,
  #   pipeline = F,
  #   functional_dataset = NULL,
  #   fit2C = NULL,
  #   results = NULL,
  #   original_dataset = NULL,
  #   imputed_dataset = NULL,
  #   normalized_dataset = NULL,
  #   limma_dataset = NULL
  # )
  
  variables = reactiveValues(
    data_table = NULL,
    auto_data_table = NULL,
    pipeline_data = NULL,
    pipline_ant = NULL,
    replicates = NULL,
    pipeline = F,
    functional_dataset = NULL,
    fit2C = NULL,
    results = NULL,
    original_dataset = NULL,
    imputed_dataset = NULL,
    normalized_dataset = NULL,
    limma_dataset = NULL,
    preproc_pdf = NULL,
    dep_pdf = NULL,
    preproc_boxplot_1 = NULL,
    preproc_density_1 = NULL,
    preproc_pca_1 = NULL,
    preproc_boxplot_2 = NULL,
    preproc_density_2 = NULL,
    preproc_pca_2 = NULL,
    top_box_plots = NULL,
    heatmap = NULL,
    volcano_plot = NULL,
    pca = NULL,
    input_file_name = "Sample_data"
  )
  
  data_preprocessing_server("data_preprocessing", variables)
  
  data_limma_server("data_limma", variables)

  data_auto_limma_server("data_auto_limma", variables)
  
  data_clustering_server("data_clustering", variables)
}

shinyApp(ui = ui, server = server)

