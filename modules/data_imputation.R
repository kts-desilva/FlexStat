#############################################################################################
# Data imputation 
#############################################################################################


# Module UI function
data_imputationUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    h3(strong("Imputed Data")),
    withSpinner(DTOutput(ns("preproc_contents3")), color = "#FF4500")
    
  )
}


data_imputation_qcUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tabPanel(
    title = "Imputation Quality Control Plots",
    br(),
    h3(strong("Data Imputation")),
    textOutput(strong(ns("preproc_imputation_colors"))),
    withSpinner(plotOutput(ns("preproc_boxplot")), color = "#FF4500"),
    withSpinner(plotOutput(ns("preproc_density")), color = "#FF4500"),
    withSpinner(plotOutput(ns("preproc_pca")), color = "#FF4500")
  )
}

# Module server function
data_imputation_server <- function(id, variables, preproc_imputation_method_var) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
      output$preproc_contents3 = renderDT(server = FALSE, {
        data = preproc_table_3()
        if (!is.null(data$x)) {
          data
        } else {
          datatable(data.frame(Test = "No Data to show"))
        }
      })
      
      # Imputed data table
      preproc_table_3 = eventReactive(variables$normalized_dataset, {
        df = variables$functional_dataset
        
        if(!is.null(df)){
          num_column_df = names(df %>% select_if(is.numeric))
          char_columns = df %>% select_if(is.character)
          
          df = df[,colnames(df) %in% num_column_df]
          df = as.data.frame(t(df))
          
          if(preproc_imputation_method_var %in% c("K-nearest neighbour",
                                                        "MissForest")){
            imputed_df = DreamAI(
              df,
              k = 10,
              maxiter_MF = 10,
              ntree = 100,
              maxnodes = NULL,
              maxiter_ADMIN = 30,
              tol = 10 ^ (-2),
              gamma_ADMIN = NA,
              gamma = 50,
              CV = FALSE,
              fillmethod = "row_mean",
              maxiter_RegImpute = 10,
              conv_nrmse = 1e-6,
              iter_SpectroFM = 40,
              method = preproc_imputation_method_var,
              out = "Ensemble"
            )
            imputed_df = as.data.frame(imputed_df$Ensemble)
          } else {
            imputed_df = as.data.frame(jdcbioinfo::impute_normal(df, width = 0.3, downshift = 1.8, seed = 100))
          }
          
          imputed_df = as.data.frame(t(imputed_df))
          imputed_df = cbind(imputed_df,char_columns)
          variables$functional_dataset = imputed_df
          variables$imputed_dataset = imputed_df
        } else {
          imputed_df = NULL
        }
        
        dtt = datatable(
          imputed_df,
          extensions = "Buttons",
          options = list(
            rowsGroup = list(0),
            dom = 'lBfrtip',
            buttons = list(
              list(
                extend = "csv",
                text = "Download Imputed Data",
                filename = "preprocessed_imputed_data",
                exportOptions = list(modifier = list(
                  order = "index",
                  page = "all",
                  search = "none"
                ))
              )
            ),
            language = list(emptyTable = "No records to display")
          )
        )
        if (!is.null(imputed_df) && nrow(imputed_df) > 0)
          dtt = dtt %>% formatRound(columns = which(sapply(imputed_df, is.numeric)), digits = 4)
        return(dtt)
      })
    }
  )    
}

data_imputation_qc_server <- function(id, preproc_class_var, variables) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      output$preproc_imputation_colors = renderText({
        paste("Colored by ", preproc_class_var)
      })
      
      
      output$preproc_boxplot = renderPlot({
        validate(need(!is.null(variables$imputed_dataset), 'Boxplot: No data exists, please upload data or use sample data'))
        preproc_boxplot_1()
      })
      
      preproc_boxplot_1 = eventReactive(variables$normalized_dataset, {
        df = variables$imputed_dataset
        
        if(!is.null(df)){
          num_column_df = df %>% select_if(is.numeric)
          char_columns = df %>% select_if(is.character)
          
          ori_df = variables$normalized_dataset
          
          num_column_df2 = ori_df %>% select_if(is.numeric)
          char_columns2 = ori_df %>% select_if(is.character)
          
          par(mfrow=c(1,2))
          
          boxplot(t(num_column_df2), col = as.character(as.numeric(as.factor(char_columns2[,preproc_class_var]))), 
                  notch = F, main = "Before Imputation",
                  xlab = 'Sample', ylab = 'log2 of Intensity')
          
          boxplot(t(num_column_df), col = as.character(as.numeric(as.factor(char_columns[,preproc_class_var]))), 
                  notch = F, main = "After Imputation",
                  xlab = 'Sample', ylab = 'log2 of Intensity')
        }
        
      })
      
      output$preproc_density = renderPlot({
        validate(need(!is.null(variables$imputed_dataset), 'Density Plot: No data exists, please upload data or use sample data'))
        preproc_density_1()
      })
      
      preproc_density_1 = eventReactive(variables$normalized_dataset, {
        df = variables$imputed_dataset
        
        if(!is.null(df)){
          num_column_df = df %>% select_if(is.numeric)
          char_columns = df %>% select_if(is.character)
          
          ori_df = variables$normalized_dataset
          
          num_column_df2 = ori_df %>% select_if(is.numeric)
          char_columns2 = ori_df %>% select_if(is.character)
          
          par(mfrow=c(1,2))
          plotDensities(t(num_column_df2), col = as.character(as.numeric(as.factor(char_columns2[,preproc_class_var]))), 
                        main = "Before Imputation - Density Plot", legend=F)
          plotDensities(t(num_column_df), col = as.character(as.numeric(as.factor(char_columns[,preproc_class_var]))), 
                        main = "After Imputation - Density Plot", legend=F)
        }
        
      })
      
      output$preproc_pca = renderPlot({
        validate(need(!is.null(variables$imputed_dataset), 'PCA Plot: No data exists, please upload data or use sample data'))
        preproc_pca_1()
      })
      
      preproc_pca_1 = eventReactive(variables$normalized_dataset, {
        df = variables$imputed_dataset
        
        if(!is.null(df)){
          num_column_df = df %>% select_if(is.numeric)
          char_columns = df %>% select_if(is.character)
          
          ori_df = variables$normalized_dataset
          
          num_column_df2 = ori_df %>% select_if(is.numeric)
          char_columns2 = ori_df %>% select_if(is.character)
          
          par(mfrow=c(1,2))
          plotMDS(t(num_column_df2), labels= NULL, pch= 15,col = as.character(as.numeric(as.factor(char_columns2[,preproc_class_var]))), main = "Before Imputation - Principal Component Analysis")
          plotMDS(t(num_column_df), labels= NULL, pch= 15,col = as.character(as.numeric(as.factor(char_columns[,preproc_class_var]))), main = "After Imputation - Principal Component Analysis")
        }
        
      })
      
  })
  
}

