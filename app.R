library(openxlsx)
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(devtools)
library(ggfortify)
library(partitions)
library(DT)
library(shinycssloaders)
library(proBatch)
library(DreamAI)
library(plotly)
library(magick)
library(stringi)
library(manhattanly)
library(gplots)
library(RColorBrewer)
library(grid)
library(ragg)
library(htmltools)
library(bsplus)
library(EnhancedVolcano)
library(zip)
library(gridExtra)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinybusy)
library(jdcbioinfo)
library(limma)
library(preprocessCore)
library(vsn)
library(ggtree)

source('modules/data_preprocessing.R')
source('modules/consensus_clustering.R')
source('modules/limma_analysis.R')
source('modules/auto_limma_analysis.R')
source('modules/information.R')

#Author: JG Lab
#Year: 2023
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
    limma_dataset = NULL
  )
  
  data_preprocessing_server("data_preprocessing", variables)
  
  data_limma_server("data_limma", variables)

  data_auto_limma_server("data_auto_limma", variables)
  
  data_clustering_server("data_clustering", variables)
}

shinyApp(ui = ui, server = server)

