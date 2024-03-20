#############################################################################################
# Limma Analysis
#############################################################################################

source('helper_functions/multiclass_limma.R')

#Constants
not_sel = "Not Selected"

data_limmaUI <- function(id) {

  ns <- NS(id)
  
  htmltools::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        title = "Inputs",
        shinyjs::useShinyjs(),
        fileInput(ns("limma_csv_input"), 
                  p(
                    span("Select CSV File to Import"),
                    span(icon("info-circle"), id = ns("icon0"), style = "color: #E95420")
                  ), accept = ".csv"),
        shinyBS::bsPopover(ns("icon0"), NULL, "Samples should be in rows, proteins should be on columns. A column with condition/class should be present. Sample input is shown in the left panel as sample data. Maximum file size is 64MB.", placement = "right"),
        htmltools::div(style = "margin-top: -50px"),
        shiny::fluidRow(
          column(width = 4, checkboxInput(ns("show_head_var"), "Show head", T)),
          column(width = 4, textOutput("")),
          column(width = 4, checkboxInput(ns("use_sample_data"), 
                                          p(
                                            span("Use Sample Data"),
                                            span(icon("info-circle"), id = ns("icon01"), style = "color: #E95420")
                                          ), F),
                 bsPopover(ns("icon01"), NULL, "Use sample data to execute the analysis.", placement = "right"),),
          offset = 0,
          style = 'padding:0px;'
        ),
        shinyjs::useShinyjs(),
        shiny::selectizeInput(
          ns('remove_cols_var'),
          'Select columns to remove',
          choices = NULL,
          options = list(maxOptions = 5),
          multiple = TRUE
        ),
        shiny::selectInput(ns("class_var"), "Class Variable", choices = c(not_sel)),
        htmltools::div(textOutput(ns("print_class_var")), style="margin-top: -15px;margin-bottom: 10px; color:red;"),
        shiny::selectInput(
          ns("class_of_interest_var"),
          "Class of Interest",
          choices = c(not_sel)
        ),
        shiny::selectInput(ns("contrast_var"), "Contrast variable", choices = c(not_sel)),
        shiny::checkboxInput(ns("contrast_other_classes_var"),  p(
          span("Contrast other classes"),
          span(icon("info-circle"), id = "icon1", style = "color: #E95420")
        ), F),
        bsPopover(ns("icon1"), NULL, "Treat other classes expect the Class of Interest as controls", placement = "right"),
        numericInput(ns("lfc_var"), p(
          span("Log fold-change variable"),
          span(icon("info-circle"), id = ns("icon2"), style = "color: #E95420")
        ), "Any"), 
        shinyBS::bsPopover(ns("icon2"), NULL, "The choice is arbitrary. However, lower thresholds might result in more false positives. 
      Usual choices are between 1 and 2. Default is set to Any LFC (Empty).", placement = "right"),
        shiny::numericInput(ns("pvalue_var"), p(
          span("P-value variable"),
          span(icon("info-circle"), id = ns("icon3"), style = "color: #E95420")
        ), "Any", max = 1), 
        shinyBS::bsPopover(ns("icon3"), NULL, "Referred to the adjusted p-value. Usual choice is 0.05. Default is set to any p-value (Empty).", placement = "right"),
        shiny::selectInput(
          ns("padjust_method_var"),
          p(
            span("Adjust P-values for Multiple Comparisons"),
            span(icon("info-circle"), id = "icon13", style = "color: #E95420")
          ),
          choices = c(
            "Benjamini-Hochberg",
            "Bonferroni",
            "Benjamini-Yekutieli",
            "Holm",
            "None"
          ),
          selected = "Benjamini-Hochberg"
        ),
        shinyBS::bsPopover(ns("icon13"), NULL, "Adjusted p-value is (highly) recommended. Raw p-value cutoffs yield many false positives and give a general trend, 
      not any statistical significance.", placement = "right"),
        shiny::actionButton(ns("run_limma_button"), "Perform Limma", icon = icon("play"), style="background-color: #E95420"),
        shinyjs::hidden(div(id=ns('limma_message'), textOutput(ns("limma_text")), style="color:green"))
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Data",
            br(),
            uiOutput(ns("conditional_contents_template")),
            uiOutput(ns("contents_conditional_h5")),
            withSpinner(tableOutput(ns("contents")), color = "#FF4500"),
            
          ),
          
          shiny::tabPanel(title = "Results",
                   br(),
                   withSpinner(DTOutput(ns("limma_table")), color = "#FF4500")),
          shiny::tabPanel(
            title = "Top 50",
            br(),
            downloadButton(ns("downloadPlots"), "Download Plots"),
            checkboxInput(ns("plotUpregulated"), "Upregulated", value = FALSE),
            checkboxInput(ns("plotDownregulated"), "Downregulated", value = FALSE),
            br(),
            withSpinner(plotOutput(ns("limma_boxplot")), color = "#FF4500")
          ),
          shiny::tabPanel(title = "Volcano Plot",
                   withSpinner(
                     plotlyOutput(ns("limma_volcanoplot"), height = '700'), color = "#FF4500"
                   )),
          shiny::tabPanel(
            title = "Annotated Volcano Plot",
            uiOutput(ns("volcano_cutoffs")),
            withSpinner(plotOutput(ns("limma_ant_volcanoplot"), height = '700'), color =
                          "#FF4500")
          ),
          shiny::tabPanel(title = "Heatmap",
                   withSpinner(
                     plotOutput(ns("limma_heatmap"), height = '700'), color = "#FF4500"
                   )),
          shiny::tabPanel(
            title = "PCA",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  ns("limma_pca_protein_group_var"),
                  "Protein Functional Group",
                  choices = c(
                    'None',
                    'Kinases',
                    'Protoeases',
                    'Transcription Factors',
                    'Hydrolases',
                    'Phosphatases'
                  )
                )
              ),
              column(width = 2, checkboxInput(ns("limma_pca_label"), "Show label", T)),
              column(
                width = 4,
                radioButtons(
                  ns("limma_pca_frame"),
                  "Draw boundary",
                  choices = list(
                    "Elipse" = 1,
                    "Lasso" = 2,
                    "None" = 3
                  ),
                  inline = TRUE,
                  selected = 1
                )
              )
            ),
            fluidRow(column(
              width = 4, textOutput(ns("limma_pca_protein_group_num"))
            )),
            withSpinner(plotOutput(ns("limma_pca"), height = '700'), color = "#FF4500")
            
          ),
          shiny::tabPanel(
            title = "Functional Annotation Analysis",
            h3("Perform functional annotation analysis on DEP proteins."),
            withSpinner(uiOutput(ns("func_annot_analysis")), color ="#FF4500")
          )
        )
      )
    )
    
  )
}

data_limma_server <- function(id, variables ) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # Constants
      # MAX FILE SIZE FOR UPLOAD
      options(shiny.maxRequestSize = 64 * 1024 ^ 2)
      
      # shiny::reactive values
      show_template = shiny::reactiveVal(TRUE)
      
      ns = NS(id)
      
      shiny::observeEvent(input$use_sample_data, {
        if (input$use_sample_data) {
          reset(id = "limma_csv_input")
          reset(id = "contents")
          show_template(TRUE)
          variables$functional_dataset = NULL
          
          data_df = read.csv("data/sample_data.csv", row.names = 1)
          char_column_df = data_df %>% select_if(is.character)
          
          choices = c(not_sel, names(char_column_df))
          shiny::shiny::updateSelectInput(inputId = "class_var", choices = choices)
          
          choices = c(not_sel, names(char_column_df))
          shiny::shiny::updateSelectInput(inputId = "class_of_interest_var", choices = choices)
          
          choices2 = c(sub(" ", "_", names(char_column_df)))
          shiny::shiny::updateSelectInput(inputId = "remove_cols_var", choices = choices2)
        } else {
          variables$functional_dataset = NULL
        }
        
      })
      
      output$contents = shiny::renderTable({
        if (!show_template()) {
          df = variables$limma_dataset
          
          if (input$show_head_var) {
            return(df[1:min(nrow(df),10),1:min(ncol(df),10)])
          } else {
            return(df)
          }
        }
        
      }, rownames = TRUE)
      
      
      output$contents_ready = shiny::renderTable({
        if (!input$use_sample_data) {
          data_df = variables$limma_dataset
        } else {
          data_df = read.csv("data/sample_data.csv", row.names = 1)
        }
        
        if (!is.null(input$remove_cols_var) &
            !("Not Selected" %in% input$remove_cols_var)) {
          data_df = data_df[,-which(names(data_df) %in% input$remove_cols_var)]
        }
        
        if (input$show_head_var) {
          return(head(data_df))
        } else {
          return(data_df)
        }
        
      }, rownames = TRUE)
      
      output$conditional_contents_template = shiny::renderUI({
        if (show_template()) {
          shiny::tagList(h3(strong("Sample Data")),
                         tableOutput(ns("contents_template")))
        } else{
          NULL
        }
      })
      
      output$contents_template = shiny::renderTable({
        read.csv("data/sample_data.csv", row.names = 1)
      })
      
      output$contents_conditional_h5 = shiny::renderUI({
        if (!show_template()) {
          shiny::h3(strong("Original Data"))
        } else{
          NULL
        }
      })
      
      class_var = shiny::eventReactive(input$run_limma_button, input$class_var)
      contrast_var = shiny::eventReactive(input$run_limma_button, input$contrast_var)
      lfc_var = shiny::eventReactive(input$run_limma_button, input$lfc_var)
      pvalue_var = shiny::eventReactive(input$run_limma_button, input$pvalue_var)
      contrast_other_classes_var = shiny::eventReactive(input$run_limma_button, input$contrast_other_classes_var)
      class_of_interest_var = shiny::eventReactive(input$run_limma_button, input$class_of_interest_var)
      padjust_method_var = shiny::eventReactive(input$run_limma_button, input$padjust_method_var)
      remove_cols_var = shiny::eventReactive(input$run_limma_button,input$remove_cols_var)
      sort_results_by_var = shiny::eventReactive(input$run_limma_button, input$sort_results_by_var)
      
      limma_data_input = shiny::reactive({
        req(input$limma_csv_input)
        fread(input$limma_csv_input$datapath)
      })
      
      shiny::observeEvent(input$class_var, {
        if (input$class_var != "Not Selected") {
          if (!input$use_sample_data) {
            data_df = variables$limma_dataset
          } else {
            data_df = read.csv("data/sample_data.csv", row.names = 1)
          }
          
          n = length(table(data_df[input$class_var]))
          count = 0
          for(i in (1: n)){
            if(table(data_df[input$class_var])[i]>1){
              count = count+1
            }
          }
          
          if(count==n){
            shinyjs::enable('class_of_interest_var')
            shinyjs::enable('contrast_var')
            shiny::updateSelectInput(session,
                              inputId = "class_of_interest_var",
                              choices = unique(data_df[input$class_var]))
            shiny::updateSelectInput(session,
                              inputId = "contrast_var",
                              choices = unique(data_df[input$class_var]))
          }
          
        }
      })
      
      
      output$print_class_var = shiny::renderText({
        if (input$class_var != "Not Selected") {
          if (!input$use_sample_data) {
            data_df = variables$limma_dataset
          } else {
            data_df = read.csv("data/sample_data.csv", row.names = 1)
          }

          n = length(table(data_df[input$class_var]))
          count = 0
          for(i in (1: n)){
            if(table(data_df[input$class_var])[i]>1){
              count = count+1
            }
          }
          shiny::validate(
            need(count!=n,"")
          )
          shinyjs::disable('class_of_interest_var')
          shinyjs::disable('contrast_var')
          return("Chosen variable is not applicable for limma analysis")
        }
      })
      
      shiny::observeEvent(input$contrast_other_classes_var, {
        if (input$contrast_other_classes_var == T) {
          shinyjs::disable('contrast_var')
        } else {
          shinyjs::enable('contrast_var')
        }
      }, ignoreNULL = T)
      
      shiny::observe({
        if (input$class_var != not_sel &&
            input$contrast_var != not_sel &&
            input$class_of_interest_var != not_sel) {
          shinyjs::enable("run_limma_button")
        }
        else{
          shinyjs::disable("run_limma_button")
        }
      })
      
      limma_data_input = shiny::reactive({
        req(input$limma_csv_input)
        fread(input$limma_csv_input$datapath)
      })
      
      shiny::observeEvent(limma_data_input(), {
        
        variables$limma_dataset= data.frame(limma_data_input(), row.names=1)
        char_column_df = variables$limma_dataset %>% select_if(is.character)
        
        show_template(FALSE)
        
        choices = c(not_sel, names(char_column_df))
        shiny::shiny::updateSelectInput(inputId = "class_var", choices = choices)
        
        choices = c(not_sel, names(char_column_df))
        shiny::shiny::updateSelectInput(inputId = "class_of_interest_var", choices = choices)
        
        choices2 = c(sub(" ", "_", names(limma_data_input())))
        shiny::updateSelectizeInput(session, inputId = "remove_cols_var", choices = choices2,
                             server = TRUE)
      })
      
      shiny::observeEvent(input$run_limma_button, {
        shinyjs::toggle('limma_message')
        output$limma_text = shiny::renderText({"Check the Results Tab"})
      })
      
      get_dataset = function() {
        if (!input$use_sample_data) {
          data_df = variables$limma_dataset  
          
          if (!is.null(input$remove_cols_var)) {
            data_df = data_df[,-which(names(data_df) %in% input$remove_cols_var)]
          }
        } else{
          data_df = read.csv("data/sample_data.csv", row.names = 1)
        }
        return(data_df)
      }
      
      perform_limma_1 = function(class_var,
                                 contrast_var,
                                 lfc_var,
                                 pvalue_var,
                                 contrast_other_classes_var,
                                 class_of_interest_var,
                                 padjust_method_var = "Benjamini-Hochberg") {
        if (!is.null(variables$functional_dataset)) {
          data_df = variables$functional_dataset
        } else {
          data_df = get_dataset()
        }
        
        data_df[, class_var] = gsub(" ", "", data_df[, class_var])
        classes = unique(data_df[class_var])
        classes = classes[[class_var]]
        
        class_of_interest_var = gsub(" ", "", class_of_interest_var)
        contrast_var = gsub(" ", "", contrast_var)
        
        if (contrast_other_classes_var &
            class_of_interest_var == contrast_var) {
          contrast_var = "control"
        }
        contrast_string = paste(class_of_interest_var, paste("-", contrast_var))
        
        if (is.na(lfc_var) && is.na(pvalue_var)) {
          results = perform_limma(
            data_df,
            class_of_interest_var,
            contrast_var,
            contrast_string,
            contrast_other_classes_var,
            class_var,
            classes,
            padjust_method_var
          )
        } else{
          results = perform_specific_limma(
            data_df,
            class_of_interest_var,
            contrast_var,
            contrast_string,
            contrast_other_classes_var,
            class_var,
            classes,
            lfc_var,
            pvalue_var,
            padjust_method_var
          )
        }
        
        variables$fit2C = results$fit2C
        results = results$topProteins
        variables$results = results
        
        data.table(results)
      }
      
      limma_table_1 = shiny::eventReactive(input$run_limma_button, {
        start = Sys.time()
        data_table = perform_limma_1(
          class_var(),
          input$contrast_var,
          input$lfc_var,
          input$pvalue_var,
          input$contrast_other_classes_var,
          class_of_interest_var(),
          padjust_method_var()
        )
        data_table = data_table[complete.cases(data_table),]
        variables$data_table = data_table
        
        dtt = DT::datatable(
          data_table,
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            rowsGroup = list(0),
            dom = 'lBfrtip',
            buttons = list(
              list(
                extend = "csv",
                text = "Download Current Page",
                filename = "limma_results_current_page",
                exportOptions = list(modifier = list(page = "current"))
              ),
              list(
                extend = "csv",
                text = "Download Full Results",
                filename = "limma_results_all",
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
        
        if (nrow(data_table) > 0)
          dtt = dtt %>% formatRound(columns = which(sapply(data_table, is.numeric)), digits = 4)
        print('limma time')
        print(Sys.time() - start)
        return(dtt)
      })
      
      output$download = shiny::downloadHandler(
        filename = "limma_filtered_protein_matrix.csv",
        content = function(file) {
          filtered = variables$data_table[input$limma_table_rows_current, 'Gene']
          df = get_dataset()
          char_column_df = df %>% select_if(is.character)
          df = df[, c(names(char_column_df), filtered$Gene)]
          write.csv(df, file)
        }
      )
      
      generate_box_plots = function(top_genes){
        data_df = get_dataset()
        plot_list = list()
        
        if (!input$contrast_other_classes_var) {
          data_df = data_df[data_df[, class_var()] %in% c(class_of_interest_var(), contrast_var()), ]
        }
        
        for (i in top_genes) {
          pp = ggplot(data_df,
                      aes_string(
                        x = class_var(),
                        y = i,
                        color = class_var()
                      )) +
            geom_boxplot() + labs(y = i, x = "Label") + theme(text = element_text(size = 18))
          plot_list[[which(i == top_genes)[[1]]]] = pp
        }
        
        if (length(top_genes) %% 3 == 0) {
          rows = length(top_genes) / 3
        } else {
          rows = (length(top_genes) / 3) + 1
        }
        gga = ggarrange(
          plotlist = plot_list,
          # labels = c("A", "B", "C", "D","E", "F", "G", "H","I", "J"),
          ncol = 3,
          nrow = rows,
          common.legend = TRUE,
          legend = "bottom"
        )
        return(gga)
      }
      
      limma_boxplots_1 = shiny::eventReactive(input$run_limma_button, {
        shiny::validate(need(!is.null(variables$data_table) && nrow(variables$data_table) != 0, 'No records to display'))
        bp = NULL
        if (!is.null(variables$data_table) && (nrow(variables$data_table) != 0)) {
          start = Sys.time()
          top_genes_df = head(variables$data_table, 50)
          top_genes = as.vector(top_genes_df$Gene)
          bp = generate_box_plots(top_genes)
          print(Sys.time() - start)
        } else {
          img = grid::rasterGrob(png::readPNG("norecords.png"))
          bp = gridExtra::grid.arrange(ggplot(), img, heights = c(1, 1))
          shinyjs::disable("plotUpregulated")
          shinyjs::disable("plotDownregulated")
        }
        return(bp)
      })
      
      filteredBoxplots = function() {
        bp = NULL
        top_genes = c()
        if (input$plotDownregulated && !input$plotUpregulated) {
          top_genes_df = head(variables$data_table, 50)
          top_genes_df = top_genes_df[top_genes_df$logFC < 0, ]
          top_genes = as.vector(top_genes_df$Gene)
          bp = generate_box_plots(top_genes)
        } else if (!input$plotDownregulated && input$plotUpregulated) {
          top_genes_df = head(variables$data_table, 50)
          top_genes_df = top_genes_df[top_genes_df$logFC > 0, ]
          top_genes = as.vector(top_genes_df$Gene)
          bp = generate_box_plots(top_genes)
        }
        return (list('plots' = bp, 'n_genes' = length(top_genes)))
      }
      
      shiny::observeEvent(input$plotUpregulated, {
        if (xor(input$plotUpregulated, input$plotDownregulated)) {
          bp = filteredBoxplots()
          output$limma_boxplot = shiny::renderPlot(bp$plots, height = 100 * bp$n_genes)
        } else {
          output$limma_boxplot = shiny::renderPlot(limma_boxplots_1(), height = 100 * min(nrow(variables$data_table), 50))
        }
      }, ignoreInit = TRUE)
      
      shiny::observeEvent(input$plotDownregulated, {
        if (xor(input$plotDownregulated, input$plotUpregulated)) {
          bp = filteredBoxplots()
          output$limma_boxplot = shiny::renderPlot(bp$plots, height = 100 * bp$n_genes)
        } else {
          output$limma_boxplot = shiny::renderPlot(limma_boxplots_1(), height = 100 *
                                              min(nrow(variables$data_table), 50))
        }
      }, ignoreInit = TRUE)
      
      output$limma_table = DT::renderDT(server = FALSE, {
        data = limma_table_1()
        if (!is.null(data$x)) {
          data
        } else {
          datatable(data.frame(Test = "No Data to show"))
        }
      })
      
      boxplotHeight = function() {
        if (!is.null(variables$data_table) && (nrow(variables$data_table) > 0)) {
          return (100 * min(nrow(variables$data_table), 50))
        }
        return (100)
      }
      
      output$limma_boxplot = shiny::renderPlot({
        shiny::validate(need(!is.null(variables$data_table), 'No records to display'))
        limma_boxplots_1()
      }, height = boxplotHeight)
      
      output$limma_volcanoplot = plotly::renderPlotly({
        shiny::validate(need(nrow(variables$data_table) != 0, 'No records to display'))
        
        results6 = variables$data_table
        results6$FC = 2 ^ results6$logFC
        results6$P = 2 ^ results6$P.Value
        
        effectsize = if (!is.na(input$lfc_var))
          c(-1 * as.numeric(input$lfc_var), as.numeric(input$lfc_var))
        else
          c(-0.5, 0.5)
        effectivep = as.numeric(input$pvalue_var)
        
        results6_vol = manhattanly::volcanor(
          results6,
          p = "P.Value",
          effect_size = "logFC",
          annotation1 = "FC",
          annotation2 = "P",
          snp = "Gene"
        )
        manhattanly::volcanoly(
          results6_vol,
          col = c("#808080"),
          genomewideline = -log10(effectivep),
          highlight_color = "red",
          effect_size_line = effectsize
        ) %>% layout(hoverlabel = list(bgcolor = "white"))
      })
      
      output$volcano_cutoffs = shiny::shiny::renderUI({
        fit2C = variables$fit2C
        fluidRow(column(
          width = 4,
          sliderInput(
            ns("pvalue_cutoff"),
            "p-value cutoff (-log10):",
            min = if (!is.null(fit2C)) - round(log10(max(fit2C$p.value, na.rm = T)),2) else 0,
            max = if (!is.null(fit2C)) - round(log10(min(fit2C$p.value, na.rm = T)),2) else 1,
            value = if (!is.null(fit2C)) ((round(min(abs(fit2C$p.value), na.rm = T), 2) + round(min(abs(fit2C$p.value), na.rm = T), 2))/2) else 0.5
          )
        ),
        column(
          width = 4,
          sliderInput(
            ns("lfc_cutoff"),
            "log fold change cutoff:",
            min = if (!is.null(fit2C))
              round(min(abs(fit2C$coef), na.rm = T), 2)
            else 0 ,
            max = if (!is.null(fit2C))
              round(max(abs(fit2C$coef), na.rm = T), 2)
            else 1,
            value = if (!is.null(fit2C)) ((round(min(abs(fit2C$coef), na.rm = T), 2) + round(min(abs(fit2C$coef), na.rm = T), 2))/2) else 0.5
          )
        ))
      })
      
      output$func_annot_analysis = shiny::shiny::renderUI({
        results_df = variables$results
        
        if(!is.na(input$lfc_var) && is.na(input$pvalue_var) ){
          results_df = results_df[abs(results_df$logFC) >= input$lfc_var,]
        } else if(is.na(input$lfc_var) && !is.na(input$pvalue_var) ){
          results_df = results_df[results_df$adj.P.Val <= input$pvalue_var,]
        } else if(!is.na(input$lfc_var) && !is.na(input$pvalue_var) ){
          results_df = results_df[(abs(results_df$logFC) >= input$lfc_var)&&
                                    (results_df$adj.P.Val <= input$pvalue_var),]
        } else {
          results_df = results_df[results_df$adj.P.Val <0.05,]
        }
        
        url = paste("https://david.ncifcrf.gov/api.jsp?type=UNIPROT_ACCESSION&ids=",
                    paste(rownames(results_df), collapse=","),"&tool=summary")
        
        tags$ul(
          tags$li(tags$h4(tags$b("DAVID Functional Annotation Analysis: ")), tags$h5("Sherman, Brad T., et al. 'DAVID: a web server for functional enrichment analysis and functional annotation of gene lists (2021 update).' Nucleic acids research 50.W1 (2022): W216-W221."),
                  tags$a(tags$h5("navigate to DAVID platform"),href=url, target="_blank")),
          
          tags$li(tags$h4(tags$b("ShinyGO Functional Enrichment Analysis: ")), tags$h5("Ge, Steven Xijin, Dongmin Jung, and Runan Yao. 'ShinyGO: a graphical gene-set enrichment tool for animals and plants.' Bioinformatics 36.8 (2020): 2628-2629."),
                  tags$a(tags$h5("navigate to ShinyGO platform")
                         , href="http://bioinformatics.sdstate.edu/go/", target="_blank")),
          
          tags$li(tags$h4(tags$b("STRING Protein-Protein Interaction Networks Functional Enrichment Analysis: ")), tags$h5("Szklarczyk, Damian, et al. 'The STRING database in 2023: protein-protein association networks and functional enrichment analyses for any sequenced genome of interest.' Nucleic acids research 51.D1 (2023): D638-D646."),
                  tags$a(tags$h5("navigate to STRING platform")
                         , href="https://string-db.org/cgi/input?sessionId=btCTWkV96Y7k&input_page_active_form=multiple_identifiers", target="_blank")),
        )
        
      })
      
      output$limma_ant_volcanoplot = shiny::renderPlot(res = 96, {
        shiny::validate(need(nrow(variables$data_table) != 0, 'No records to display'))
        fit2C = variables$fit2C
        
        data = get_dataset()
        data <- t(data)
        results_df <- data.frame (
          log2FoldChange  = c(fit2C$coef),
          pvalue = c(fit2C$p.value),
          delabel = rownames(fit2C)
        )
        rownames(results_df) <- rownames(fit2C)
        results_df$diffexpressed <- "NO"
        results_df$diffexpressed[results_df$log2FoldChange > input$lfc_cutoff &
                                   results_df$pvalue < 10 ** (-1 * input$pvalue_cutoff)] <- "UP"
        results_df$diffexpressed[results_df$log2FoldChange < (-1 * input$lfc_cutoff) &
                                   results_df$pvalue < 10 ** (-1 * input$pvalue_cutoff)] <- "DOWN"
        
        EnhancedVolcano::EnhancedVolcano(
          results_df,
          lab = rownames(results_df),
          x = 'log2FoldChange',
          y = 'pvalue',
          title = paste("DEP in", class_of_interest_var()),
          pCutoff = 10 ** (-1 * input$pvalue_cutoff),
          FCcutoff = if(is.null(input$lfc_cutoff)) 0.5 else input$lfc_cutoff,
          pointSize = 3.0,
          labSize = 6.0,
          ylim = c(0, -1 * log10(min(fit2C$p.value) * 0.1)),
          xlim = c(-1 * (max(fit2C$coef) + 0.5), 1 * (max(fit2C$coef) +
                                                        0.5)),
        )
        
      })
      
      output$limma_heatmap = shiny::renderPlot(res = 96, {
        data_df = get_dataset()
        data_df[, input$class_var] = gsub(" ", "", data_df[, input$class_var])
        
        classes = unique(data_df[input$class_var])
        
        charColumns = data_df %>% select_if(is.character)
        data_df_int = data_df[,-which(names(data_df) %in%  colnames(charColumns))]
        data_df[, input$class_var] = as.factor(data_df[, input$class_var])
        
        results_df = variables$results
        
        if(!is.na(input$lfc_var) && is.na(input$pvalue_var) ){
          results_df = results_df[abs(results_df$logFC) >= input$lfc_var,]
          data_df_int = data_df_int[,rownames(results_df)]
        }else if(is.na(input$lfc_var) && !is.na(input$pvalue_var) ){
          results_df = results_df[results_df$adj.P.Val <= input$pvalue_var,]
          data_df_int = data_df_int[,rownames(results_df)]
        }else if(!is.na(input$lfc_var) && !is.na(input$pvalue_var) ){
          results_df = results_df[(abs(results_df$logFC) >= input$lfc_var)&&
                                    (results_df$adj.P.Val <= input$pvalue_var),]
          data_df_int = data_df_int[,rownames(results_df)]
        }else {
          
        }
        
        gplots::heatmap.2(
          as.matrix(t(data_df_int)),
          col = RColorBrewer::brewer.pal(11, "RdBu"),
          scale = "row",
          trace = "none",
          margins = c(8, 16),
          ColSideColors = as.character(as.numeric(data_df[, input$class_var]))
        )
        graphics::legend(
          "topright",
          legend = unique(data_df[, input$class_var]),
          col = unique(as.numeric(data_df[, input$class_var])),
          lty = 1,
          lwd = 5,
          cex = .7
        )
        
      })
      
      get_markers = function(marker_type) {
        if (marker_type == "Protoeases") {
          data_df = read.csv(
            "./functional_groups/proteases_ug.csv",
            head = TRUE,
            sep = ",",
            fileEncoding = "UTF-8-BOM"
          )
          return(data_df)
        } else if (marker_type == "Kinases") {
          data_df = read.csv(
            "./functional_groups/kinases.csv",
            head = TRUE,
            sep = ",",
            fileEncoding = "UTF-8-BOM"
          )
          return(data_df)
        } else if (marker_type == "Transcription Factors") {
          data_df = read.csv(
            "./functional_groups/tfs.csv",
            head = TRUE,
            sep = ",",
            fileEncoding = "UTF-8-BOM"
          )
          return(data_df)
        } else if (marker_type == "Hydrolases") {
          return(data.frame())
        } else if (marker_type == "Phosphatases") {
          return(data.frame())
        } 
      }
      
      output$limma_pca = shiny::renderPlot(res = 96, {
        shiny::validate(need(nrow(variables$data_table) != 0, 'No records to display'))
        
        data_df = get_dataset()
        data_df[, input$class_var] = gsub(" ", "", data_df[, input$class_var])
        
        classes = unique(data_df[input$class_var])
        
        if(is.null(variables$fit2C)){
          charColumns = data_df %>% select_if(is.character)
          data_df_int = data_df[,-which(names(data_df) %in%  colnames(charColumns))]
        } else {
          data_df_int = data_df[,rownames(variables$fit2C)]
        }
        
        data_df[, input$class_var] = as.factor(data_df[, input$class_var])
        
        variables$functional_dataset = data_df
        
        markers = get_markers(input$limma_pca_protein_group_var)
        
        if(!is.null(markers)){
          
          if (length(intersect(colnames(data_df_int), markers$Gene)) > 0) {
            data_df_int = data_df_int %>% select(any_of(c(markers$Gene)))
          } else if (length(intersect(colnames(data_df_int), markers$UniprotID)) >
                     0) {
            data_df_int = data_df_int %>% select(any_of(c(markers$UniprotID)))
          } else if (length(intersect(colnames(data_df_int), markers$Uniprot_Gene)) >
                     0) {
            data_df_int = data_df_int %>% select(any_of(c(markers$Uniprot_Gene)))
          } else {
            data_df_int = data.frame()
          }
          
          output$limma_pca_protein_group_num = shiny::renderText(
            paste(
              "Number of proteins identified as",
              input$limma_pca_protein_group_var,
              ncol(data_df_int),
              sep = " "
            )
          )
        }
        
        pca_type = NULL
        pca_frame = F
        
        if (input$limma_pca_frame == 1) {
          pca_type = "norm"
          pca_frame = T
        } else if (input$limma_pca_frame == 2) {
          pca_type = NULL
          pca_frame = T
        } else {
          pca_frame = F
        }
        
        shiny::validate(need(
          nrow(data_df_int) > 1 &&
            ncol(data_df_int) > 1,
          'No records to display'
        ))
        ggplot2::autoplot(
          prcomp((data_df_int), center = TRUE),
          data_df,
          colour = input$class_var,
          label = input$limma_pca_label,
          frame = pca_frame,
          frame.type = pca_type,
          main = if(input$limma_pca_protein_group_var == "None") "PCA" else
            paste("PCA of", input$limma_pca_protein_group_var, sep =" ")
        )
      })
      
      output$downloadResults = shiny::downloadHandler(
        filename = "limma_results.csv",
        content = function(file) {
          write.csv(variables$data_table, file, row.names = FALSE)
        }
      )
      
      output$downloadPlots = shiny::downloadHandler(
        filename = "limma_top50_plots.png",
        content = function(file) {
          if (input$plotDownregulated || input$plotUpregulated) {
            bp = filteredBoxplots()
            png(file, width = 800, height = 100 * bp$n_genes)
            print(bp$plots)
          } else {
            png(file,
                width = 800,
                height = 100 * nrow(variables$data_table))
            print(limma_boxplots_1())
          }
          dev.off()
        }
      )
    })}