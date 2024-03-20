######################################################################################  
# Consensus Clustering
###################################################################################### 

library(shinyjs)

# Module UI function
data_clusteringUI <- function(id) {
  
  ns = NS(id)
  
  htmltools::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        title = "Inputs",
        shinyjs::useShinyjs(),
        shiny::fileInput(ns("cluster_csv_input"), 
                  p(
                    span("Select CSV File"),
                    span(icon("info-circle"), id = "icon7", style = "color: #E95420")
                  ),accept = ".csv"),
        shinyBS::bsPopover(ns("icon7"), NULL, "Samples should be in rows, proteins should be on columns. Maximum file size is 64MB.", placement = "right"),
        htmltools::div(style = "margin-top: -50px"),
        shiny::fluidRow(
          column(
            width = 4,
            shiny::checkboxInput(ns("cluster_use_sample_data"), "Use Sample Data", F)
          ),
          offset = 0,
          style = 'padding:0px;'
        ),
        shiny::uiOutput(ns("cluster_slider")),
        shinyBS::bsPopover(ns("icon8"), NULL, "The maximum number of clusters. The choice is arbitrary based on the research question at hand.", placement = "right"),
        shiny::selectInput(
          ns("cluster_alg_var"),
          p(
            span("Clust Algorithm"),
            span(icon("info-circle"), id = "icon9", style = "color: #E95420")
          ),
          choices = c(
            "Hierachical Clustering",
            "Partition Around Medoids Clustering",
            "K-means Clustering"
          )
        ),
        shinyBS::bsPopover(ns("icon9"), NULL, "The type of clustering algorithm to use.", placement = "right"),
        shiny::selectInput(
          ns("cluster_distance_var"),
          p(
            span("Distance Measurement"),
            span(icon("info-circle"), id = "icon10", style = "color: #E95420")
          ),
          choices = c('pearson', 'spearman','kendall')
        ),
        shinyBS::bsPopover(ns("icon10"), NULL, "This option describes the distance metric to be used.", placement = "right"),
        shiny::selectInput(
          ns("cluster_inner_linkage_var"),
          p(
            span("Inner Linkage",),
            span(icon("info-circle"), id = "icon11", style = "color: #E95420")
          ),
          choices = c(
            'ward.D2',
            'ward.D',
            "single",
            "complete",
            "average",
            "mcquitty" ,
            "median",
            "centroid"
          )
        ),
        shinyBS::bsPopover(ns("icon11"), NULL, "This option species the linkage method to use initerative agglomerative hierarchical clustering. Not applicable to other cluster algorithms.", placement = "right"),
        shiny::selectInput(
          ns("cluster_final_linkage_var"),
          p(
            span("Final Linkage"),
            span(icon("info-circle"), id = "icon12", style = "color: #E95420")
          ),
          choices = c(
            'ward.D2',
            'ward.D',
            "single",
            "complete",
            "average",
            "mcquitty" ,
            "median",
            "centroid"
          )
        ),
        shinyBS::bsPopover(ns("icon12"), NULL, "This option species the linkage method to use in the final agglomerative hierarchical clustering.", placement = "right"),
        br(),
        shiny::actionButton(
          ns("run_consclustering_button"),
          "Perform Clustering",
          icon = icon("play"), style="background-color: #E95420"
        )
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("conditional_contents_template3")),
        shiny::uiOutput(ns('cluster_images'))
      )
    )
  )
}

# Module server function
data_clustering_server <- function(id, variables) {
  shiny::moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      ns <- NS(id)
      show_template3 = shiny::reactiveVal(TRUE)
      
      output$conditional_contents_template3 = shiny::shiny::renderUI({
        if (show_template3()) {
          tagList(h3(strong("Sample Data")),
                  tableOutput(ns("contents_template3")))
        } else{
          NULL
        }
      })
      
      output$contents_template3 = shiny::renderTable({
        read.csv("data/consensus_data.csv")
      })
      
      cluster_csv_input = shiny::reactive({
        req(input$cluster_csv_input)
        fread(input$cluster_csv_input$datapath)
      })
      
      shiny::observeEvent(cluster_csv_input(), {
        show_template3(FALSE)
      })
      
      get_cluster_dataset = function() {
        if (!input$cluster_use_sample_data) {
          req(input$cluster_csv_input)
          data_df = read.csv(
            input$cluster_csv_input$datapath,
            head = TRUE,
            sep = ",",
            row.names = 1
          )
        } else {
          data_df = read.csv(
            "data/consensus_data.csv",
            head = TRUE,
            sep = ",",
            row.names = 1
          )
        }
        return(data_df)
      }
      
      shiny::observeEvent(input$cluster_use_sample_data, {
        if (input$cluster_use_sample_data) {
          reset(id = "cluster_csv_input")
          show_template3(TRUE)
        }
      })
      
      cluster_k_var = shiny::eventReactive(input$run_consclustering_button, input$cluster_k_var)
      cluster_alg_var = shiny::eventReactive(input$run_consclustering_button, input$cluster_alg_var)
      cluster_distance_var = shiny::eventReactive(input$run_consclustering_button,
                                           input$cluster_distance_var)
      cluster_inner_linkage_var = shiny::eventReactive(input$run_consclustering_button,
                                                input$cluster_inner_linkage_var)
      cluster_final_linkage_var = shiny::eventReactive(input$run_consclustering_button,
                                                input$cluster_final_linkage_var)
      
      output$cluster_slider = shiny::shiny::renderUI({
        shiny::sliderInput(
          ns("cluster_k_var"),
          p(
            span("Maximum No. of Clusters"),
            span(icon("info-circle"), id = "icon8", style = "color: #E95420")
          ),
          min = 2,
          max = if (!is.null(get_cluster_dataset())) ncol(get_cluster_dataset()) else 15,
          value = if (!is.null(get_cluster_dataset())) floor(ncol(get_cluster_dataset())/2) else 6,
          step= 1
        )
      })
      
      shiny::observe({
        if (xor(input$cluster_use_sample_data,!is.null(input$cluster_csv_input$datapath))){
          shinyjs::enable(id="run_consclustering_button")
        } else {
          shinyjs::disable(id="run_consclustering_button")
        }
      })
      
      shiny::observeEvent(input$run_consclustering_button, {
        show_template3(FALSE)
        start = Sys.time()
        
        source('helper_functions/consensus_clustering.R')
        data = get_cluster_dataset()
        data[is.na(data)] = 0

        clust_alg = cluster_alg_var()
        ca = c("hc", "pam", "km")
        names(ca) = c(
          "Hierachical Clustering",
          "Partition Around Medoids Clustering",
          "K-means Clustering"
        )
        print('Started...')
        path = perform_consensus_clustering_png(
          as.matrix(data),
          gsub("\\..*", "", input$cluster_csv_input$name),
          k = cluster_k_var(),
          clusterAlg = ca[[clust_alg]],
          distance = cluster_distance_var(),
          innerLinkage = cluster_inner_linkage_var(),
          finalLinkage = cluster_final_linkage_var()
        )
        print('done..')
        variables$cluster_path = path
        
        output$cluster_images = shiny::renderUI({
          all_images = list.files(variables$cluster_path,
                                  pattern = "*png",
                                  full.names = TRUE)
          shinycssloaders::withSpinner(tagList(
            downloadButton(ns("clusterDownload"), label = "Download Results"),
            br(),
            br(),
            
            shiny::renderImage(list(
              src = all_images[1],
              alt = "Can't show file!",
              width = 300,
              height = 300
            ), deleteFile = FALSE),
            all_images[-1] %>% map(function(path) {
              renderImage(list(
                src = path,
                alt = "Can't show file!",
                width = 480,
                height = 500
              ), deleteFile = FALSE)
            })
          ),color = "#FF4500")
          
        })
        
        output$clusterDownload = shiny::downloadHandler(
          filename = function() {
            paste0("ConsensusClusterResults_", Sys.Date(), ".zip")
          },
          content = function(file) {
            files = list.files(variables$cluster_path, recursive = TRUE)
            files = paste(variables$cluster_path, files, sep = "/")
            return (zip::zip(file ,files ,mode="cherry-pick"))
          },
          contentType = "application/zip"
        )
      }
      )  
    })
  }