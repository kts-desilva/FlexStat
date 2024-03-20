#############################################################################################
# Data Preprocessing
#############################################################################################

source('modules/data_imputation.R')
source('modules/data_normalization.R')
source('modules/data_transformation.R')

#Constants
not_sel = "Not Selected"

data_preprocessingUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      shinyjs::useShinyjs(),
      fileInput(ns("preproc_data_csv_input"), 
                p(
                  span("Select CSV File to Import"),
                  span(icon("info-circle"), id = ns("icon0"), style = "color: #E95420")
                ), accept = ".csv"),
      bsPopover(ns("icon0"), NULL, "Samples should be in rows, proteins should be on columns. A column with condition/class should be present. Sample input is shown in the left panel as sample data. Maximum file size is 64MB.", placement = "right"),
      div(style = "margin-top: -50px"),
      fluidRow(
        column(width = 4, checkboxInput(ns("preproc_show_head_var"), "Show head", T)),
        column(width = 4, textOutput("")),
        column(width = 4, checkboxInput(ns("preproc_use_sample_data"), 
                                        p(
                                          span("Use Sample Data"),
                                          span(icon("info-circle"), id = ns("icon15"), style = "color: #E95420")
                                        ), F),
               bsPopover(ns("icon15"), NULL, "Use sample data to execute the analysis.", placement = "right"),),
        offset = 0,
        style = 'padding:0px;'
      ),
      fluidRow(
        column(
          width = 4,
          checkboxInput(ns("preproc_show_transpose_var"), "Transpose data", F)
        ),
        column(width = 4, checkboxInput(ns("preproc_show_log2_var"), "Log2 Transform", F)),
        column(width = 4, checkboxInput(ns("preproc_show_log10_var"), "Log10 Transform", F)),
        offset = 0,
        style = 'padding:0px;'
      ),
      shinyjs::useShinyjs(),
      selectizeInput(
        ns('preproc_remove_cols_var'),
        'Select columns to remove',
        choices = c(not_sel),
        options = list(maxOptions = 5),
        multiple = TRUE
      ),
      selectInput(ns("preproc_class_var"), "Class Variable", choices = c(not_sel)),
      selectInput(ns("preproc_batch_var"), "Experimental Batch Variable", choices = c(not_sel)),
      radioButtons(
        ns("preproc_normalization_method_var"), "Select Data Normalization Method",
        choices = list(
          "Median Normalization" = 1,
          "Quantile Normalization" = 2,
          "Internal Reference Normalization" = 3,
          "Variance Stabilization Normalization" = 4
        ), selected = 1
      ),
      conditionalPanel(condition = "input.preproc_normalization_method_var==3",
                       selectizeInput(
                         ns('preproc_ir_var'),
                         'Select internal references',
                         choices = c(not_sel),
                         options = list(maxOptions = 5),
                         multiple = TRUE
                       )),
      sliderInput(ns("preproc_missing_value_threshold_var"), "Missing value threshold",
                  min = 0,max = 1,value = 0.5),
      textOutput(ns('preproc_retain_protein')),
      radioButtons(
        ns("preproc_imputation_method_var"), "Select Data Imputation Method",
        choices = list(
          "Random draw from a normal distribution" = 1, 
          "K-nearest neighbour" = 2,
          "MissForest" = 3
        ), selected = 1
      ),
      actionButton(ns("run_preproc_button"), "Preprocess Data", icon = icon("play"), 
                   style="background-color: #E95420"),
      hidden(div(id=ns('preproc_message'), textOutput(ns("preproc_text")), style="color:green"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Data",
          br(),
          uiOutput(ns("conditional_contents_template4")),
          uiOutput(ns("contents_conditional_h6")),
          withSpinner(tableOutput(ns("preproc_contents")), color = "#FF4500"),
          data_transformationUI(ns("data_transformation"))
        ),
        tabPanel(
          title = "Preprocessed Data",
          br(),
          data_normalizationUI(ns("data_normalization")),
          data_imputationUI(ns("data_imputation"))
        ),
        data_normalization_qcUI(ns("data_normalization_qc")),
        data_imputation_qcUI(ns("data_imputation_qc"))
      )
    )
  )
}

data_preprocessing_server <- function(id, variables ) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
      # Constants
      # MAX FILE SIZE FOR UPLOAD
      options(shiny.maxRequestSize = 64 * 1024 ^ 2)

      # Reactive values
      preproc_show_template = shiny::reactiveVal(TRUE)
      
      ns <- NS(id)
      
      output$conditional_contents_template4 = shiny::renderUI({
        if (preproc_show_template()) {
          tagList(h3(strong("Sample Data")),
                  tableOutput(ns("contents_template4")))
        } else{
          NULL
        }
      })
      
      output$contents_template4 = shiny::renderTable({
        read.csv("data/sample_preproc_data.csv", row.names = 1)
      }) 
      
      output$contents_conditional_h6 = shiny::renderUI({
        if (!preproc_show_template()) {
          h3(strong("Original Data"))
        } else{
          NULL
        }
      })
      
      preproc_data_input = shiny::reactive({
        req(input$preproc_data_csv_input)
        fread(input$preproc_data_csv_input$datapath)
      })
      
      shiny::observeEvent(preproc_data_input(), {
        char_column_df = data.frame(preproc_data_input(), row.names=1)  %>%
          select_if(is.character)
        
        preproc_show_template(FALSE)
        
        choices = c(not_sel, names(char_column_df))
        updateSelectInput(inputId = "preproc_class_var", choices = choices)
        
        choices = c(not_sel, names(char_column_df))
        updateSelectInput(inputId = "preproc_batch_var", choices = choices)
        
        choices2 = c(sub(" ", "_", names(char_column_df)))
        updateSelectizeInput(session, inputId = "preproc_remove_cols_var", choices = choices2,
                             server = TRUE)
        
        updateSelectInput(inputId = "preproc_ir_var", choices = (preproc_data_input()[,1]))
      })
      
      shiny::observeEvent(input$preproc_use_sample_data, {
        if (input$preproc_use_sample_data) {
          reset(id = "preproc_data_csv_input")
          reset(id = "preproc_contents")
          preproc_show_template(TRUE)
          variables$functional_dataset = NULL
          
          data_df = read.csv("data/sample_preproc_data.csv", row.names = 1)
          char_column_df = data_df %>% select_if(is.character)
          
          choices = c(not_sel, names(char_column_df))
          updateSelectInput(inputId = "preproc_class_var", choices = choices)
          
          choices = c(not_sel, names(char_column_df))
          updateSelectInput(inputId = "preproc_batch_var", choices = choices)
          
          choices2 = c(sub(" ", "_", names(char_column_df)))
          updateSelectInput(inputId = "preproc_remove_cols_var", choices = choices2)
          
          updateSelectInput(inputId = "preproc_ir_var", choices = rownames(data_df))
        } else {
          variables$functional_dataset = NULL
        }
        
      })
      
      shiny::observeEvent(input$preproc_use_sample_data, {
        if (input$preproc_use_sample_data) {
          shinyjs::disable("preproc_show_transpose_var")
        } else {
          shinyjs::enable("preproc_show_transpose_var")
        }
      })
      
      shiny::observeEvent(input$preproc_show_log2_var, {
        if (input$preproc_show_log2_var) {
          shinyjs::disable("preproc_show_log10_var")
        } else{
          shinyjs::enable("preproc_show_log10_var")
        }
      })
      
      shiny::observeEvent(input$preproc_show_log10_var, {
        if (input$preproc_show_log10_var) {
          shinyjs::disable("preproc_show_log2_var")
        } else{
          shinyjs::enable("preproc_show_log2_var")
        }
      })
      
      output$preproc_contents = shiny::renderTable({
        if (!preproc_show_template()) {
          req(input$preproc_data_csv_input)
          
          df = read.csv(
            input$preproc_data_csv_input$datapath,
            head = TRUE,
            sep = ",",
            row.names = 1
          )
          return(head(df))
        }
        
      }, rownames = TRUE)
      
      preproc_class_var = shiny::eventReactive(input$run_preproc_button, input$preproc_class_var)
      preproc_batch_var = shiny::eventReactive(input$run_preproc_button, input$preproc_batch_var)
      preproc_missing_value_threshold_var = shiny::eventReactive(input$run_preproc_button, input$preproc_missing_value_threshold_var)
      preproc_imputation_method_var = shiny::eventReactive(input$run_preproc_button, input$preproc_imputation_method_var)
      preproc_normalization_method_var = shiny::eventReactive(input$run_preproc_button, input$preproc_normalization_method_var)
      preproc_ir_var = shiny::eventReactive(input$run_preproc_button, input$preproc_ir_var)
      
      shiny::observe({
        if(input$preproc_class_var!=not_sel && input$preproc_batch_var!=not_sel){
          shinyjs::enable("run_preproc_button")
        } else {
          shinyjs::disable("run_preproc_button")
        }
      })
      
      shiny::observeEvent(input$run_preproc_button, {
        shinyjs::toggle('preproc_message')
        output$preproc_text = renderText({"Check the Preprocessed Data Tab"})
        
      })
      
      # Data transformation module
      data_transformation_server("data_transformation", input$preproc_show_transpose_var, input$preproc_show_log2_var,
                                 input$preproc_show_log10_var,input$preproc_use_sample_data,
                                 input$preproc_data_csv_input, input$preproc_remove_cols_var,
                                 variables)
      
      # Data normalization module
      data_normalization_server("data_normalization", input$run_preproc_button, input$preproc_use_sample_data,
                                input$preproc_data_csv_input,input$preproc_missing_value_threshold_var,
                                input$preproc_normalization_method_var,
                                input$preproc_batch_var, input$preproc_ir_var, variables)
      data_normalization_qc_server("data_normalization_qc",input$preproc_class_var,input$run_preproc_button,
                                   variables)
      
      # Data imputation module
      data_imputation_server("data_imputation", variables, input$preproc_imputation_method_var)
      data_imputation_qc_server("data_imputation_qc",input$preproc_class_var, variables)
      
      
  })
}

