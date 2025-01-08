#############################################################################################
# Data transformation
#############################################################################################

# Module UI function
data_transformationUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    conditionalPanel(condition = "input.preproc_show_transpose_var || input.preproc_show_log2_var || input.preproc_show_log10_var",
                     h3(strong("Transformed Data"),
                        downloadButton(ns("downloadTransformedMatrix"), "Download Transformed Matrix"))),
    withSpinner(tableOutput(ns("preproc_contents_ready")), color = "#FF4500")
    
  )
}

data_transformation_server <- function(id, preproc_show_transpose_var, preproc_show_log2_var,
                                       preproc_show_log10_var,preproc_use_sample_data,
                                       preproc_data_csv_input, preproc_remove_cols_var,
                                       variables ) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      output$downloadTransformedMatrix = downloadHandler(
        filename = paste(variables$input_file_name, "transformed_data.csv", sep="_"),
        content = function(file) {
          write.csv(variables$functional_dataset, file)
        }
      )
      
      output$preproc_contents_ready = renderTable({
        if (preproc_show_transpose_var || preproc_show_log2_var || preproc_show_log10_var) {
          if (!preproc_use_sample_data) {
            req(preproc_data_csv_input)
            data_df = read.csv(
              preproc_data_csv_input$datapath,
              head = TRUE,
              sep = ",",
              row.names = 1
            )
          } else{
            data_df = read.csv("../data/sample_preproc_data.csv", row.names = 1)
          }
          
          if (!is.null(preproc_remove_cols_var) &
              !("Not Selected" %in% preproc_remove_cols_var)) {
            data_df = data_df[,-which(names(data_df) %in% preproc_remove_cols_var)]
          }
          if (preproc_show_transpose_var) {
            data_df = as.data.frame(t(data_df))
            variables$functional_dataset = data_df
          }
          if (preproc_show_log2_var) {
            data_df = as.data.frame(mutate_if(data_df, is.numeric, log2))
            variables$functional_dataset = data_df
          }
          if (preproc_show_log10_var) {
            data_df = as.data.frame(mutate_if(data_df, is.numeric, log10))
            variables$functional_dataset = data_df
          }
          return(data_df)
        }
        
      }, rownames = TRUE)
      
    }
    
  )}


