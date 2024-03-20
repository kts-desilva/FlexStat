######################################################################################    
# Automated Combinatory Differential Expression
######################################################################################

source('helper_functions/multiclass_limma.R')

#Constants
not_sel = "Not Selected"

data_auto_limmaUI <- function(id) {

  ns = NS(id)
  
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        title = "Inputs",
        shinyjs::useShinyjs(),
        shiny::fileInput(ns("auto_limma_csv_input"), 
                  p(
                    span("Select CSV File to Import"),
                    span(icon("info-circle"), id = ns("icon4"), style = "color: #E95420")
                  ), accept = ".csv"),
        shinyBS::bsPopover(ns("icon4"), NULL, "Samples should be in rows, proteins should be on columns. A column with condition/class should be present. Sample input is shown in the left panel as sample data. Maximum file size is 64MB.", placement = "right"),
        htmltools::div(style = "margin-top: -50px"),
        fluidRow(
          column(
            width = 4,
            checkboxInput(ns("auto_use_sample_data"), "Use Sample Data", F)
          ),
          offset = 0,
          style = 'padding:0px;'
        ),
        shiny::selectInput(ns("auto_class_var"), "Class Variable", choices = c(not_sel)),
        htmltools::div(textOutput(ns("print_auto_class_var")), style="margin-top: -15px;margin-bottom: 10px; color:red;"),
        shiny::numericInput(ns("auto_lfc_var"), 
                     p(
                       span("Log fold-change variable"),
                       span(icon("info-circle"), id = ns("icon5"), style = "color: #E95420")
                     ), "Any"),
        shinyBS::bsPopover(ns("icon5"), NULL, "The choice is arbitrary. However, lower thresholds might result in more false positives. Usual choices are between 1 and 2. Default is set to Any LFC (Empty).", placement = "right"),
        shiny::numericInput(ns("auto_pvalue_var"), 
                     p(
                       span("P-value variable"),
                       span(icon("info-circle"), id = ns("icon6"), style = "color: #E95420")
                     ), "Any"),
        shinyBS::bsPopover(ns("icon6"), NULL, "Referred to the adjusted p-value. Usual choice is 0.05. Default is set to any p-value (Empty).", placement = "right"),
        shiny::selectInput(
          ns("auto_padjust_method_var"),
          p(
            span("Adjust P-values for Multiple Comparisons"),
            span(icon("info-circle"), id = ns("icon14"), style = "color: #E95420")
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
        shinyBS::bsPopover(ns("icon14"), NULL, "Adjusted p-value is (highly) recommended. Raw p-value cutoffs yield many false positives and give a general trend, not any statistical significance.", placement = "right"),
        br(),
        shiny::actionButton(ns("run_auto_limma_button"), "Perform Auto Limma", icon = icon("play"), style="background-color: #E95420")
      ),
      mainPanel(
        shiny::uiOutput(ns("conditional_contents_template2")),
        shiny::uiOutput(ns("contents_conditional_h52")),
        shiny::uiOutput(ns("contents_auto_limma")),
        shiny::verbatimTextOutput(ns("auto_limma_comb_list")),
        shinycssloaders::withSpinner(DTOutput(ns("auto_limma_table")), color = "#FF4500")
      )
    )
  )}


data_auto_limma_server <- function(id, variables ) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # Constants
      # MAX FILE SIZE FOR UPLOAD
      options(shiny.maxRequestSize = 64 * 1024 ^ 2)
      
      # Reactive values
      show_template2 = shiny::reactiveVal(TRUE)
      auto_limma_ready = shiny::reactiveVal(FALSE)
      
      ns = NS(id)
      
      output$conditional_contents_template2 = shiny::renderUI({
        if (show_template2()) {
          tagList(h3(strong("Sample Data")),
                  tableOutput(ns("contents_template2")))
        } else{
          NULL
        }
      })
      
      output$contents_template2 = shiny::renderTable({
        read.csv("data/sample_data.csv", row.names = 1)
      })
      
      output$contents_conditional_h52 = shiny::renderUI({
        if (!show_template2() & !auto_limma_ready()) {
          htmltools::tagList(h3(strong("Original Data")),
                  withSpinner(tableOutput(ns("contents2")), color = "#FF4500"))
        } else{
          NULL
        }
      })
      
      output$contents2 = shiny::renderTable({
        req(input$auto_limma_csv_input)
        df = read.csv(
          input$auto_limma_csv_input$datapath,
          head = TRUE,
          sep = ",",
          row.names = 1
        )
        return(head(df))
      }, rownames = TRUE)
      
      output$contents_auto_limma = shiny::renderUI({
        if (auto_limma_ready()) {
          htmltools::tagList(h3(strong("Auto limma Results")),
                  fluidRow(
                    column(
                      width = 4,
                      downloadButton(ns("downloadAutoTable"), "Download Top 50")
                    ),
                    column(
                      width = 4,
                      downloadButton(ns("downloadAutoTableAll"), "Download All ")
                    )
                  ),
                  br(),
                  br())
        } else{
          NULL
        }
      })
      
      get_auto_dataset = function() {
        if (!input$auto_use_sample_data) {
          req(input$auto_limma_csv_input)
          data_df = read.csv(input$auto_limma_csv_input$datapath,
                             head = TRUE,
                             sep = ",", row.names = 1)
        } else{
          data_df = read.csv("data/sample_data2.csv", row.names = 1)
        }
        
        return(data_df)
      }
      
      auto_limma_data_input = shiny::reactive({
        req(input$auto_limma_csv_input)
        fread(input$auto_limma_csv_input$datapath)
      })
      
      shiny::observeEvent(auto_limma_data_input(), {
        show_template2(FALSE)
        char_column_df = data.frame(auto_limma_data_input(), row.names=1) %>%
          select_if(is.character)
        choices = c(not_sel, names(char_column_df))
        shiny::updateSelectInput(inputId = "auto_class_var", choices = choices)
      })
      
      shiny::observeEvent(input$auto_use_sample_data, {
        reset(id = "auto_limma_csv_input")
        reset(id = "contents2")
        show_template2(TRUE)
        
        data_df = read.csv("data/sample_data2.csv", row.names = 1)
        char_column_df = data_df %>%
          select_if(is.character)
        choices = c(not_sel, names(char_column_df))
        shiny::updateSelectInput(inputId = "auto_class_var", choices = choices)
      })
      
      output$print_auto_class_var = shiny::renderText({
        if (input$auto_class_var != "Not Selected") {
          if (!input$auto_use_sample_data) {
            req(input$auto_limma_csv_input)
            data_df = read.csv(input$auto_limma_csv_input$datapath,
                               head = TRUE,
                               sep = ",")
          } else {
            data_df = read.csv("data/sample_data.csv", row.names = 1)
          }
          
          n = length(table(data_df[input$auto_class_var]))
          count = 0
          for(i in (1: n)){
            if(table(data_df[input$auto_class_var])[i]>1){
              count = count+1
            }
          }
          validate(
            need(count!=n,"")
          )
          shinyjs::disable("run_auto_limma_button")
          return("Chosen variable is not applicable for automated limma analysis")
        }
      })
      
      auto_class_var = shiny::eventReactive(input$run_auto_limma_button, input$auto_class_var)
      auto_lfc_var = shiny::eventReactive(input$run_auto_limma_button, input$auto_lfc_var)
      auto_pvalue_var = shiny::eventReactive(input$run_auto_limma_button, input$auto_pvalue_var)
      auto_padjust_method_var = shiny::eventReactive(input$run_auto_limma_button,
                                              input$auto_padjust_method_var)
      
      shiny::observe({
        if(input$auto_class_var!=not_sel){
          enable("run_auto_limma_button")
        } else {
          disable("run_auto_limma_button")
        }
      })
      
      output$auto_limma_table = DT::renderDT(limma_table2())
      
      perform_limma_2 =
        function(data_df, class_var, lfc_var, pvalue_var, class_group1, class_group2, method = "none",
                 fdr = "Benjamini-Hochberg") {
          
          if (!"Class1" %in% data_df[, class_var]) {
            class_of_interest_var = 'Class1'
            classes = c('Class1', 'Class2')
            contrast_var = "Class2"
            contrast_string = "Class1-Class2"
            contrast_other_classes_var = F
            
            for (c in class_group1) {
              data_df[data_df == c] = 'Class1'
            }
            
            for (c in class_group2) {
              data_df[data_df == c] = 'Class2'
            }
          } else {
            class_of_interest_var = 'Cls1'
            classes = c('Cls1', 'Cls2')
            contrast_var = "Cls2"
            contrast_string = "Cls1-Cls2"
            contrast_other_classes_var = F
            
            for (c in class_group1) {
              data_df[data_df == c] = 'Cls1'
            }
            
            for (c in class_group2) {
              data_df[data_df == c] = 'Cls2'
            }
          }
          
          if (is.na(lfc_var) && is.na(pvalue_var)) {
            if (method == "all") {
              results = perform_limma_top_all(
                data_df,
                class_of_interest_var,
                contrast_var,
                contrast_string,
                contrast_other_classes_var,
                class_var,
                classes,
                fdr
              )
              results = results$topProteins
            } else {
              results = perform_limma_top10(
                data_df,
                class_of_interest_var,
                contrast_var,
                contrast_string,
                contrast_other_classes_var,
                class_var,
                classes,
                fdr
              )
            }
          } else {
            if (method == "all") {
              results = perform_limma_top_specific_all(
                data_df,
                class_of_interest_var,
                contrast_var,
                contrast_string,
                contrast_other_classes_var,
                class_var,
                classes,
                lfc_var,
                pvalue_var,
                fdr
              )
            } else {
              results = perform_limma_top10_specific(
                data_df,
                class_of_interest_var,
                contrast_var,
                contrast_string,
                contrast_other_classes_var,
                class_var,
                classes,
                lfc_var,
                pvalue_var,
                fdr
              )
            }
            
          }
          data.table(results)
          
        }
      
      limma_table2 = shiny::eventReactive(input$run_auto_limma_button, {
        if (input$auto_use_sample_data) {
          show_template2(FALSE)
        }
        start = Sys.time()
        
        data_df = get_auto_dataset()
        dd = unique(data_df[, auto_class_var()])
        
        res = rapply(partitions::listParts(length(dd)), function(v) dd[v], how = "replace")
        res2 = list()
        index = 1
        for (j in (1:length(res))) {
          if (length(res[[j]]) == 2) {
            res2[[index]] = res[[j]]
            index = index + 1
          }
        }
        auto_limma_table = data.frame(
          Combination = character(0),
          Gene = character(0),
          logFC = numeric(0),
          AveExpr = numeric(0),
          t = numeric(0),
          P.Value = numeric(0),
          adj.P.Val = numeric(0),
          B = numeric(0)
        )
        
        comb_list = c()
        
        ###########################################  
        # Pairwise comparisons
        ###########################################  
        pcomb = combn(dd, 2)
        for (i in 1:ncol(pcomb)) {
          group1 = pcomb[1, i]
          group2 = pcomb[2, i]
          
          data_table_2 = perform_limma_2(
            get_auto_dataset(),
            auto_class_var(),
            input$auto_lfc_var,
            input$auto_pvalue_var,
            group1,
            group2,
            input$auto_padjust_method_var
          )
          
          data_table_2$Combination = toString(paste(group1, group2, sep = " vs "))
          
          if ((nrow(data_table_2) > 0) &&
              (!(unique(data_table_2$Combination) %in% comb_list))) {
            comb_list = append(comb_list, unique(data_table_2$Combination))
          }
          
          if (nrow(data_table_2) > 0) {
            auto_limma_table = rbind(auto_limma_table, data_table_2)
          }
        }
        
        for (i in (1:length(res2))) {
          group1 = res2[[i]][["1"]]
          group2 = res2[[i]][["2"]]
          
          group1_string = paste(group1, collapse = "+")
          group2_string = paste(group2, collapse = "+")
          
          data_table_2 = perform_limma_2(
            get_auto_dataset(),
            auto_class_var(),
            input$auto_lfc_var,
            input$auto_pvalue_var,
            group1,
            group2
          )
          
          data_table_2$Combination = toString(paste(group1_string, group2_string, sep = " vs "))
          
          if ((nrow(data_table_2) > 0) &&
              !(unique(data_table_2$Combination) %in% comb_list)) {
            comb_list = append(comb_list, unique(data_table_2$Combination))
          }
          if (nrow(data_table_2) > 0) {
            auto_limma_table = rbind(auto_limma_table, data_table_2)
          }
        }
        
        output$auto_limma_comb_list = shiny::renderText({
          return(paste(comb_list, collapse = "\n"))
        })
        
        auto_limma_table = auto_limma_table %>% dplyr::select(Combination, everything())
        
        dtable = DT::datatable(auto_limma_table, rownames = FALSE, options = list(rowsGroup = list(0)))
        dtable = dtable %>% formatRound(columns = which(sapply(auto_limma_table, is.numeric)), digits = 4)
        
        # Folder containing dataTables.rowsGroup.js                   
        path = getwd() 
        dep = htmltools::htmlDependency("RowsGroup", "2.0.0",
                                        path, script = "dataTables.rowsGroup.js")
        dtable$dependencies = c(dtable$dependencies, list(dep))
        
        variables$auto_data_table = auto_limma_table %>%  mutate_if(is.numeric, round, digits=4)
        
        print('auto limma elapsed time')
        print(Sys.time() - start)
        auto_limma_ready(TRUE)
        return(dtable)
      })
      
      generate_letter_combinations = function(iterations){
        results = character(iterations)
        
        for (i in 1:iterations) {
          letter = ""
          n = i
          
          while (n > 0) {
            remainder = (n - 1) %% 26
            letter = paste0(LETTERS[remainder + 1], letter)
            n = (n - 1) %/% 26
          }
          
          results[i] = letter
        }
        
        return(results)
      }
      
      output$downloadAutoTable = shiny::downloadHandler(
        filename = "limma_auto_results_top50.xlsx",
        
        content = function(file) {
          df = variables$auto_data_table
          combs_names = unique(df$Combination)
          comb_index_names = generate_letter_combinations(length(combs_names))
          combs_index = data.frame(index = comb_index_names, combination = combs_names)
          
          OUT = openxlsx::createWorkbook()
          
          # Write index
          openxlsx::addWorksheet(OUT, "Index")
          openxlsx::writeData(OUT, sheet = "Index", x = combs_index)
          i = 1
          for (c in comb_index_names) {
            openxlsx::addWorksheet(OUT, c)
            openxlsx::writeData(OUT, sheet = c, x = df[df$Combination == combs_names[i], ])
            i = i + 1
          }
          openxlsx::saveWorkbook(OUT, file)
        }
      )
      
      output$downloadAutoTableAll = shiny::downloadHandler(
        filename = "limma_auto_results_all.xlsx",
        content = function(file) {
          shinybusy::show_modal_spinner(text = HTML("The results are being generated.<br> Please wait..."))
          on.exit(remove_modal_spinner())
          
          df = variables$auto_data_table
          
          data_df = get_auto_dataset()
          dd = unique(data_df[, auto_class_var()])
          res = rapply(listParts(length(dd)), function(v)
            dd[v], how = "replace")
          res2 = list()
          index = 1
          for (j in (1:length(res))) {
            if (length(res[[j]]) == 2) {
              res2[[index]] = res[[j]]
              index = index + 1
            }
          }
          auto_limma_table = data.frame(
            Combination = character(0),
            Gene = character(0),
            logFC = numeric(0),
            AveExpr = numeric(0),
            t = numeric(0),
            P.Value = numeric(0),
            adj.P.Val = numeric(0),
            B = numeric(0)
          )
          
          comb_list = c()
          
          ###########################################  
          # Pairwise comparisons
          ###########################################  
          pcomb = combn(dd, 2)
          for (i in 1:ncol(pcomb)) {
            group1 = pcomb[1, i]
            group2 = pcomb[2, i]
            
            data_table_2 = perform_limma_2(
              get_auto_dataset(),
              auto_class_var(),
              input$auto_lfc_var,
              input$auto_pvalue_var,
              group1,
              group2,
              method = "all"
            )
            
            data_table_2$Combination = toString(paste(group1, group2, sep = " vs "))
            
            if ((nrow(data_table_2) > 0)  &&
                !(unique(data_table_2$Combination) %in% comb_list)) {
              comb_list = append(comb_list, unique(data_table_2$Combination))
            }
            
            if ((nrow(data_table_2) > 0)) {
              auto_limma_table = rbind(auto_limma_table, data_table_2)
            }
            
          }
          
          for (i in (1:length(res2))) {
            group1 = res2[[i]][["1"]]
            group2 = res2[[i]][["2"]]
            
            group1_string = paste(group1, collapse = "+")
            group2_string = paste(group2, collapse = "+")
            
            data_table_2 = perform_limma_2(
              get_auto_dataset(),
              auto_class_var(),
              input$auto_lfc_var,
              input$auto_pvalue_var,
              group1,
              group2,
              method = "all"
            )
            
            data_table_2$Combination = toString(paste(group1_string, group2_string, sep = " vs "))
            
            if ((nrow(data_table_2) > 0)  &&
                !(unique(data_table_2$Combination) %in% comb_list)) {
              comb_list = append(comb_list, unique(data_table_2$Combination))
            }
            
            if ((nrow(data_table_2) > 0)) {
              auto_limma_table = rbind(auto_limma_table, data_table_2)
            }
            
          }
          
          auto_limma_table = auto_limma_table %>% dplyr::select(Combination, everything())
          
          df = auto_limma_table
          
          combs_names = unique(df$Combination)
          comb_index_names = generate_letter_combinations(length(combs_names))
          combs_index = data.frame(index = comb_index_names, combination = combs_names)
          
          OUT = openxlsx::createWorkbook()
          
          #Write index
          openxlsx::addWorksheet(OUT, "Index")
          openxlsx::writeData(OUT, sheet = "Index", x = combs_index)
          i = 1
          for (c in comb_index_names) {
            length(c)
            openxlsx::addWorksheet(OUT, c)
            openxlsx::writeData(OUT, sheet = c, x = df[df$Combination == combs_names[i], ])
            i = i + 1
          }
          openxlsx::saveWorkbook(OUT, file)
        }
      )
    }
    )}