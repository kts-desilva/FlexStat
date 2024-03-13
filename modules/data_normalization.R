#############################################################################################
# Data normalization 
#############################################################################################

# Module UI function
data_normalizationUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    h3(strong("Normalized Data")),
    withSpinner(DTOutput(ns("preproc_contents2")), color = "#FF4500")
    
  )
}

data_normalization_qcUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tabPanel(
    title = "Normalization Quality Control Plots",
    br(),
    h3(strong("Data Normalization")),
    textOutput(strong(ns("preproc_normalization_colors"))),
    withSpinner(plotOutput(ns("preproc_boxplot2")), color = "#FF4500"),
    withSpinner(plotOutput(ns("preproc_density2")), color = "#FF4500"),
    withSpinner(plotOutput(ns("preproc_pca2")), color = "#FF4500")
  )
}




# Module server function
data_normalization_server <- function(id, run_preproc_button, preproc_use_sample_data,preproc_data_csv_input,
                                      preproc_missing_value_threshold_var, preproc_normalization_method_var,
                                      preproc_batch_var, preproc_ir_var, variables) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
      #############################################################################################
      # Data Normalization
      #############################################################################################
      output$preproc_contents2 = renderDT(server = FALSE, {
        data = preproc_table_2()
        if (!is.null(data$x)) {
          data
        } else {
          datatable(data.frame(Test = "No Data to show"))
        }
      })
      
      preproc_table_2 = eventReactive(run_preproc_button, {
        
        if(is.null(variables$functional_dataset)){ 
          if (!preproc_use_sample_data) {
            req(preproc_data_csv_input)
            df = read.csv(
              preproc_data_csv_input$datapath,
              head = TRUE,
              sep = ",",
              row.names = 1
            )
          } else{
            df = read.csv("data/sample_preproc_data.csv", row.names = 1)
          }
        } else{
          df = variables$functional_dataset
        }
        
        variables$original_dataset = df
        
        if(!is.null(df)){
          
          num_column_df = names(df %>% select_if(is.numeric))
          char_columns = df %>% select_if(is.character)
          
          df_ori = df
          
          df = df[,colnames(df) %in% num_column_df]
          df = as.data.frame(t(df))
          
          df = df[which(rowMeans(is.na(df)) < preproc_missing_value_threshold_var), ]
          
          if(preproc_normalization_method_var == 1){
            normalized_df = normalizeMedianValues(df)
            normalized_df = as.data.frame(t(normalized_df))
          } else if(preproc_normalization_method_var == 2){
            normalized_df = normalize.quantiles(as.matrix(df),copy=TRUE)
            normalized_df = as.data.frame(normalized_df)
            colnames(normalized_df) = colnames(df)
            rownames(normalized_df) = rownames(df)
            normalized_df = as.data.frame(t(normalized_df))
          } else if(preproc_normalization_method_var == 3){
            if(!is.null(preproc_batch_var)){
              colSums = list()
              data_sl = data.frame()
              i = 1
              for (u in unique(char_columns[,preproc_batch_var])){
                d = df[,rownames(char_columns[,preproc_batch_var==u])]
                d = d[,!colnames(d) %in% c(preproc_batch_var)]
                colSums[i] = as.data.frame(colSums(d, na.rm = T))
                i = i+1
              }
              
              target = mean(unlist(colSums))
              
              for (u in unique(char_columns[,preproc_batch_var])){
                d = df[,char_columns[,preproc_batch_var]==u]
                d = d[,!colnames(d) %in% c(preproc_batch_var)]
                norm_facs = target / colSums(d, na.rm = T)
                exp1_sl = sweep(d, 2, norm_facs, FUN = "*")
                exp1_sl = as.data.frame(exp1_sl)
                
                exp1_sl = as.data.frame(t(exp1_sl))
                exp1_sl[,preproc_batch_var] = u
                
                data_sl = rbind(data_sl, exp1_sl)
              }
              
              target2 = colMeans(data_sl[preproc_ir_var,!(names(data_sl) %in% c(preproc_batch_var))])
              data_sl_tmm = data.frame()
              
              j = 1
              for (k in preproc_ir_var){
                u = unique(char_columns[,preproc_batch_var])[j]
                d = data_sl[data_sl[,preproc_batch_var]==u,]
                d = d[,!colnames(d) %in% c(preproc_batch_var)]
                
                norm_facs2 = target2 / d[k,]
                norm_facs2 = as.numeric(as.vector(norm_facs2[1,]))
                exp1_sl = sweep(d, 2, norm_facs2, FUN = "*")
                
                data_sl_tmm = rbind(data_sl_tmm, as.data.frame(exp1_sl))
                j=j+1
              }
              normalized_df = data_sl_tmm
            } else {
              normalized_df = normalizeMedianValues(df)
              normalized_df = as.data.frame(t(normalized_df))
            }
          } else {
            fit = vsn2(as.matrix(df))
            normalized_df = as.data.frame(predict(fit, newdata=as.matrix(df)))
            normalized_df = as.data.frame(t(normalized_df))
          }
          
          normalized_df = cbind(normalized_df,char_columns[rownames(normalized_df),])
          
          variables$normalized_dataset = normalized_df
          variables$functional_dataset = normalized_df
          
        } else {
          normalized_df = NULL
        }
        
        dtt = datatable(
          normalized_df,
          extensions = "Buttons",
          options = list(
            rowsGroup = list(0),
            dom = 'lBfrtip',
            buttons = list(
              list(
                extend = "csv",
                text = "Download Normalized Data",
                filename = "preprocessed_imputed_normalized_data",
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
        
        if (!is.null(normalized_df) && nrow(normalized_df) > 0)
          dtt = dtt %>% formatRound(columns = which(sapply(normalized_df, is.numeric)), digits = 4)
        return(dtt)
      })
    })
  
  }


data_normalization_qc_server <- function(id, preproc_class_var,run_preproc_button, variables) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      output$preproc_normalization_colors = renderText({
        paste("Colored by ", preproc_class_var)
      })
      
      output$preproc_boxplot2 = renderPlot({
        validate(need(!is.null(variables$normalized_dataset), 'Boxplot: No data exists, please upload data or use sample data'))
        preproc_boxplot_2()
      })
      
      preproc_boxplot_2 = eventReactive(run_preproc_button, {
        df = variables$normalized_dataset
        
        if(!is.null(df)){
          df = df[order(df[,preproc_class_var]),]
          
          num_column_df = df %>% select_if(is.numeric)
          char_columns = df %>% select_if(is.character)
          
          ori_df = variables$original_dataset
          ori_df = ori_df[order(ori_df[,preproc_class_var]),]
          
          num_column_df2 = ori_df %>% select_if(is.numeric)
          char_columns2 = ori_df %>% select_if(is.character)
          
          par(mfrow=c(1,2))
          
          boxplot(t(num_column_df2), col = as.character(as.numeric(as.factor(char_columns2[,preproc_class_var]))), 
                  notch = F, main = "Before Normalization",
                  xlab = 'Sample', ylab = 'log2 of Intensity')
          
          boxplot(t(num_column_df), col = as.character(as.numeric(as.factor(char_columns[,preproc_class_var]))), 
                  notch = F, main = "After Normalization",
                  xlab = 'Sample', ylab = 'log2 of Intensity')
        }
        
      })
      
      output$preproc_density2 = renderPlot({
        validate(need(!is.null(variables$normalized_dataset), 'Density Plot: No data exists, please upload data or use sample data'))
        preproc_density_2()
      })
      
      preproc_density_2 = eventReactive(run_preproc_button, {
        df = variables$normalized_dataset
        
        if(!is.null(df)){
          df = df[order(df[,preproc_class_var]),]
          
          num_column_df = df %>% select_if(is.numeric)
          char_columns = df %>% select_if(is.character)
          
          ori_df = variables$original_dataset
          ori_df = ori_df[order(ori_df[,preproc_class_var]),]
          
          num_column_df2 = ori_df %>% select_if(is.numeric)
          char_columns2 = ori_df %>% select_if(is.character)
          
          par(mfrow=c(1,2))
          plotDensities(t(num_column_df2), col = as.character(as.numeric(as.factor(char_columns2[,preproc_class_var]))), 
                        main = "Before Normalization - Density Plot", legend=F)
          plotDensities(t(num_column_df), col = as.character(as.numeric(as.factor(char_columns[,preproc_class_var]))), 
                        main = "After Normalization - Density Plot", legend=F)
        }
        
      })

      output$preproc_pca2 = renderPlot({
        validate(need(!is.null(variables$normalized_dataset), 'PCA Plot: No data exists, please upload data or use sample data'))
        preproc_pca_2()
      })
      
      preproc_pca_2 = eventReactive(run_preproc_button, {
        df = variables$normalized_dataset
        
        if(!is.null(df)){
          df = df[order(df[,preproc_class_var]),]
          
          num_column_df = df %>% select_if(is.numeric)
          char_columns = df %>% select_if(is.character)
          
          ori_df = variables$original_dataset
          ori_df = ori_df[order(ori_df[,preproc_class_var]),]
          
          num_column_df2 = ori_df %>% select_if(is.numeric)
          char_columns2 = ori_df %>% select_if(is.character)
          
          par(mfrow=c(1,2))
          plotMDS(t(num_column_df2), labels= NULL,pch= 15, col = as.character(as.numeric(as.factor(char_columns2[,preproc_class_var]))), main = "Before Normalization - Principal Component Analysis")
          plotMDS(t(num_column_df), labels= NULL,pch= 15, col = as.character(as.numeric(as.factor(char_columns[,preproc_class_var]))), main = "After Normalization - Principal Component Analysis")
        }
        
      })
      
    })
  }