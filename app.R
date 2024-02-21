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

# library(BiocManager)
# options(repos = BiocManager::repositories())

not_sel <- "Not Selected"

about_page <- tabPanel(
  title = "Help",
  titlePanel("Help Page"),
  navlistPanel(
    "Tutorials",
    tabPanel("Data Preprocessing with Sample Data- Part 1", tags$iframe(style = "height:800px; width:100%", src = "manual00.pdf")),
    tabPanel("Data Preprocessing with Experiment Data- Part 2", tags$iframe(style = "height:800px; width:100%", src = "manual01.pdf")),
    tabPanel("Differential Expression Analysis with Sample Data - Part 1", tags$iframe(style = "height:800px; width:100%", src = "manual1.pdf")),
    tabPanel("Differential Expression Analysis with Experiment Data - Part 2", tags$iframe(style = "height:800px; width:100%", src = "manual2.pdf")),
    tabPanel("Automated Differential Expression Analysis", tags$iframe(style = "height:800px; width:100%", src = "manual3.pdf"),
    tabPanel("Consensus Clustering", tags$iframe(style = "height:800px; width:100%", src = "manual4.pdf"))),
    "Datasets",
    tabPanel("Example datasets",
             tags$ul(
               tags$li(tags$b("UPS1 spiked-in yeast data"),
               tags$a(href="https://doi.org/10.1016/j.dib.2015.11.063", style="color:blue", "Ramus, Claire, et al. 'Spiked proteomic standard dataset for testing label-free quantitative software and statistical methods.' Data in brief 6 (2016): 286-294.", target="_blank")),
               tags$a(href="dataset1_DEP.csv", tags$u("Download for DEP"), download=NA, target="_blank"), 
               HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),
               tags$a(href="dataset1_cc.csv", tags$u("Download for Clustering"), download=NA, target="_blank"),
               
               tags$li(tags$b("Ubiquitin interactor affinity enriched data"),
               tags$a(href="https://doi.org/10.1038/nprot.2017.147", style="color:blue", "Zhang, Xiaofei, et al. 'Proteome-wide identification of ubiquitin interactions using UbIA-MS.' Nature protocols 13.3 (2018): 530-550.", target="_blank")),
               tags$a(href="dataset2_DEP.csv", tags$u("Download for DEP"), download=NA, target="_blank"), 
               HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),
               tags$a(href="dataset2_cc.csv", tags$u("Download for Clustering"), download=NA, target="_blank"),
               
               tags$li(tags$b("Breast Cancer cellline data"),
               tags$a(href="https://doi.org/10.1016/j.isci.2020.100868", style="color:blue","Kosok, Max, et al. 'Comprehensive proteomic characterization reveals subclass-specific molecular aberrations within triple-negative breast cancer.' Iscience 23.2 (2020).", target="_blank")),
               tags$a(href="dataset3_DEP.csv", tags$u("Download for DEP"), download=NA, target="_blank"), 
               HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),
               tags$a(href="dataset3_cc.csv", tags$u("Download for Clustering"), download=NA, target="_blank"),
               
               tags$li(tags$b("Clear cell renal carinoma data"),
               tags$a(href="https://doi.org/10.1038/nm.3807", style="color:blue", "Guo, Tiannan, et al. 'Rapid mass spectrometric conversion of tissue biopsy samples into permanent quantitative digital proteome maps.' Nature medicine 21.4 (2015): 407-413", target="_blank")),
               tags$a(href="dataset4_DEP.csv", tags$u("Download for DEP"), download=NA, target="_blank"), 
               HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),
               tags$a(href="dataset4_cc.csv", tags$u("Download for Clustering"), download=NA, target="_blank"),
               
               tags$li(tags$b("Ovarian cancer tissue data "),
               tags$a(href="https://doi.org/10.1016/j.cell.2016.05.069", style="color:blue", "Zhang, Hui, et al. 'Integrated proteogenomic characterization of human high-grade serous ovarian cancer.' Cell 166.3 (2016): 755-765.", target="_blank")),
               tags$a(href="dataset5_DEP.csv", tags$u("Download for DEP"), download=NA, target="_blank"), 
               HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),
               tags$a(href="dataset5_cc.csv", tags$u("Download for Clustering"), download=NA, target="_blank")
               
             )),
    "Libraries",
    tabPanel("Used libraries and resources","Please cite following publications (if you have used them):",
             tags$ul(
               tags$li(tags$b("Data Preprocessing: ", style="color:orange"),tags$ul(
                 tags$li(tags$b("preprocessCore"),": Ritchie, Matthew E., et al. 'limma powers differential expression analyses for RNA-sequencing and microarray studies.' Nucleic acids research 43.7 (2015): e47-e47."), 
                 tags$li(tags$b("dplyr"),": Wickham, Hadley, et al. 'Package dplyr.' A Grammar of Data Manipulation. R package version 8 (2019)."))), 
               
               tags$li(tags$b("Differential expression analysis: ", style="color:orange"),tags$ul(
                 tags$li(tags$b("Limma"),": Ritchie, Matthew E., et al. 'limma powers differential expression analyses for RNA-sequencing and microarray studies.' Nucleic acids research 43.7 (2015): e47-e47."), 
                 tags$li(tags$b("dplyr"),": Wickham, Hadley, et al. 'Package dplyr.' A Grammar of Data Manipulation. R package version 8 (2019)."))), 
               
               tags$li(tags$b("Automated Differential expression analysis: ", style="color:orange"),tags$ul(
                 tags$li(tags$b("Limma"),": Ritchie, Matthew E., et al. 'limma powers differential expression analyses for RNA-sequencing and microarray studies.' Nucleic acids research 43.7 (2015): e47-e47."), 
                 tags$li(tags$b("partitions"),": Hankin, Robin KS. 'Additive integer partitions in R.' Journal of Statistical Software 16 (2006): 1-3."), 
                 tags$li(tags$b("utils"),": R Core Team. R: A Language and Environment for Statistical Computing. 2021, https://www.r-project.org/."))), 
    
               tags$li(tags$b("Consensus Clustering: ", style="color:orange"),tags$ul(
                 tags$li(tags$b("ConsensusClusterPlus"),": Wilkerson, Matthew D., and D. Neil Hayes. 'ConsensusClusterPlus: a class discovery tool with confidence assessments and item tracking.' Bioinformatics 26.12 (2010): 1572-1573."))), 
                
               tags$li(tags$b("Visualization: ", style="color:orange"),tags$ul(
                 tags$li(tags$b("ggplot2"),": Wickham, Hadley. Ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016, https://ggplot2.tidyverse.org."), 
                 tags$li(tags$b("ggpubr"),": Kassambara, Alboukadel. Ggpubr: ggplot2 Based Publication Ready Plots. 2020, https://CRAN.R-project.org/package=ggpubr."), 
                 tags$li(tags$b("plotly"),": Sievert, Carson. Interactive Web-Based Data Visualization with R, Plotly, and Shiny. Chapman and Hall/CRC, 2020, https://plotly-r.com."),
                 tags$li(tags$b("manhattanly"),": Bhatnagar, Sahir. Manhattanly: Interactive Q-Q and Manhattan Plots Using Plotly.Js. 2021, https://CRAN.R-project.org/package=manhattanly."),
                 tags$li(tags$b("gplots"),": Warnes, Gregory R., et al. Gplots: Various R Programming Tools for Plotting Data. 2022, https://CRAN.R-project.org/package=gplots."),
                 tags$li(tags$b("EnhancedVolcano"),": Blighe, Kevin, et al. EnhancedVolcano: Publication-Ready Volcano Plots with Enhanced Colouring and Labeling. 2021, https://github.com/kevinblighe/EnhancedVolcano."),
                 tags$li(tags$b("RColorBrewer"),": Neuwirth, Erich. RColorBrewer: ColorBrewer Palettes. 2022, https://CRAN.R-project.org/package=RColorBrewer."),
                 )), 
               
               tags$li(tags$b("Result/output generation: ", style="color:orange"),tags$ul(
                 tags$li(tags$b("data.table"),": Dowle, Matt, and Arun Srinivasan. Data.Table: Extension of `data.Frame`. 2021, https://CRAN.R-project.org/package=data.table."), 
                 tags$li(tags$b("DT"),": Xie, Yihui, et al. DT: A Wrapper of the JavaScript Library DataTables. 2023, https://CRAN.R-project.org/package=DT."), 
                 tags$li(tags$b("grid"),": R Core Team. R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing, 2021, https://www.R-project.org/."),
                 tags$li(tags$b("gridExtra"),": Auguie, Baptiste. gridExtra: Miscellaneous Functions for Grid Graphics. 2017, https://CRAN.R-project.org/package=gridExtra."),
                 tags$li(tags$b("openxlsx"),": Schauberger, Philipp, and Alexander Walker. Openxlsx: Read, Write and Edit Xlsx Files. 2023, https://CRAN.R-project.org/package=openxlsx."),
                 tags$li(tags$b("zip"),": Csardi, Gabor, et al. Zip: Cross-Platform 'zip' Compression. 2022, https://CRAN.R-project.org/package=zip.")
               )),
               
               tags$li(tags$b("App deployment: ", style="color:orange"),tags$ul(
                 tags$li(tags$b("shiny"),": Chang, Winston, et al. Shiny: Web Application Framework for R. 2022, https://CRAN.R-project.org/package=shiny."), 
                 tags$li(tags$b("shinythemes"),": Chang, Winston. Shinythemes: Themes for Shiny. 2021, https://CRAN.R-project.org/package=shinythemes."), 
                 tags$li(tags$b("shinyjs"),": Attali, Dean. Shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. 2021, https://CRAN.R-project.org/package=shinyjs."),
                 tags$li(tags$b("shinyBS"),": Bailey, Eric. shinyBS: Twitter Bootstrap Components for Shiny. 2022, https://CRAN.R-project.org/package=shinyBS."),
                 tags$li(tags$b("shinycssloaders"),": Sali, Andras, and Dean Attali. Shinycssloaders: Add Loading Animations to a shiny Output While It is Recalculating. 2020, https://CRAN.R-project.org/package=shinycssloaders."),
                 tags$li(tags$b("magick"),": Ooms, Jeroen. Magick: Advanced Graphics and Image-Processing in R. 2021, https://CRAN.R-project.org/package=magick."),
                 tags$li(tags$b("htmltools"),": Cheng, Joe, et al. Htmltools: Tools for HTML. 2022, https://CRAN.R-project.org/package=htmltools."),
                 tags$li(tags$b("bsplus"),": Lyttle, Ian. Bsplus: Adds Functionality to the R Markdown + Shiny Bootstrap Framework. 2022, https://CRAN.R-project.org/package=bsplus.")
               ))
             )),
    "Contact info",
    tabPanel("Contact us",
             "For any feedback or question regarding FlexStat, please contact the Translational Biomedical Proteomics Lab",
             tags$br(),
             tags$li("Adj Assoc Prof. Jayantha Gunarathne (PI): ",tags$a(href="mailto:jayanthag@imcb.a-star.edu.sg", "jayanthag@imcb.a-star.edu.sg")),
             tags$li("Senuri De Silva (PhD Student): ",tags$a(href="mailto:Senuri_De_Silva_from.tp@imcb.a-star.edu.sg", "Senuri_De_Silva_from.tp@imcb.a-star.edu.sg"))
             ),
    tabPanel("Feedback",
             tags$iframe(style = "height:800px; width:100%", src = "https://docs.google.com/forms/d/e/1FAIpQLSdL7XpIKcnTJGAxhNGhht30mDdD1n0cOM966yGKkw6R3fTWPw/viewform?embedded=true",
                         width="700", height="520", frameborder="0", marginheight="0", marginwidth="0")
    )
    
             
  ),
  tags$footer("Created with R Shiny, ","2024 January")
  
)

main_page <- tabPanel(
  title = "Comparative Statistics",
  titlePanel("Comparative Statistics"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
      selectInput("num_var_1", "Numerical Variable d1", choices = c(not_sel)),
      selectInput("num_var_2", "Numerical Variable 2", choices = c(not_sel)),
      selectInput("fact_var", "Factor Variable", choices = c(not_sel)),
      br(),
      actionButton("run_button", "Run Analysis", icon = icon("play"))
    ),
    mainPanel(tabsetPanel(
      tabPanel(title = "Plot",
               plotOutput("plot_1")),
      tabPanel(
        title = "Statistics",
        fluidRow(
          column(width = 4, strong(textOutput("num_var_1_title"))),
          column(width = 4, strong(textOutput("num_var_2_title"))),
          column(width = 4, strong(textOutput("fact_var_title")))
        ),
        fluidRow(
          column(width = 4, tableOutput("num_var_1_summary_table")),
          column(width = 4, tableOutput("num_var_2_summary_table")),
          column(width = 4, tableOutput("fact_var_summary_table"))
        ),
        fluidRow(column(width = 12, strong(
          "Combined Statistics"
        ))),
        fluidRow(column(
          width = 12, tableOutput("combined_summary_table")
        ))
        
      )
    ))
  )
)

preprocessing_page <- tabPanel(
  title = "Data Preperation",
  titlePanel("Data Preprocessing"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      shinyjs::useShinyjs(),
      fileInput("preproc_data_csv_input", 
                p(
                  span("Select CSV File to Import"),
                  span(icon("info-circle"), id = "icon0", style = "color: #E95420")
                ), accept = ".csv"),
      bsPopover("icon0", NULL, "Samples should be in rows, proteins should be on columns. A column with condition/class should be present. Sample input is shown in the left panel as sample data. Maximum file size is 64MB.", placement = "right"),
      div(style = "margin-top: -50px"),
      fluidRow(
        column(width = 4, checkboxInput("preproc_show_head_var", "Show head", T)),
        column(width = 4, textOutput("")),
        column(width = 4, checkboxInput("preproc_use_sample_data", 
                                        p(
                                          span("Use Sample Data"),
                                          span(icon("info-circle"), id = "icon15", style = "color: #E95420")
                                        ), F),
               bsPopover("icon15", NULL, "Use sample data to execute the analysis.", placement = "right"),),
        offset = 0,
        style = 'padding:0px;'
      ),
      fluidRow(
        column(
          width = 4,
          checkboxInput("preproc_show_transpose_var", "Transpose data", F)
        ),
        column(width = 4, checkboxInput("preproc_show_log2_var", "Log2 Transform", F)),
        column(width = 4, checkboxInput("preproc_show_log10_var", "Log10 Transform", F)),
        offset = 0,
        style = 'padding:0px;'
      ),
      shinyjs::useShinyjs(),
      selectizeInput(
        'preproc_remove_cols_var',
        'Select columns to remove',
        choices = c(not_sel),
        options = list(maxOptions = 5),
        multiple = TRUE
      ),
      selectInput("preproc_class_var", "Class Variable", choices = c(not_sel)),
      selectInput("preproc_batch_var", "Experimental Batch Variable", choices = c(not_sel)),
      radioButtons(
        "preproc_normalization_method_var", "Select Data Normalization Method",
        choices = list(
          "Median Normalization" = 1,
          "Quantile Normalization" = 2,
          "Internal Reference Normalization" = 3,
          "Variance Stabilization Normalization" = 4
        ), selected = 1
      ),
      conditionalPanel(condition = "input.preproc_normalization_method_var==3",
                       selectizeInput(
                         'preproc_ir_var',
                         'Select internal references',
                         choices = c(not_sel),
                         options = list(maxOptions = 5),
                         multiple = TRUE
                       )),
      sliderInput("preproc_missing_value_threshold_var", "Missing value threshold",
                  min = 0,max = 1,value = 0.5),
      textOutput('preproc_retain_protein'),
      radioButtons(
        "preproc_imputation_method_var", "Select Data Imputation Method",
        choices = list(
          "Random draw from a normal distribution" = 1, 
          "K-nearest neighbour" = 2,
          "MissForest" = 3
        ), selected = 1
      ),
      actionButton("run_preproc_button", "Preprocess Data", icon = icon("play"), 
                   style="background-color: #E95420"),
      hidden(div(id='preproc_message', textOutput("preproc_text"), style="color:green"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Data",
          br(),
          uiOutput("conditional_contents_template4"),
          uiOutput("contents_conditional_h6"),
          withSpinner(tableOutput("preproc_contents"), color = "#FF4500"),
          conditionalPanel(condition = "input.preproc_show_transpose_var || input.preproc_show_log2_var || input.preproc_show_log10_var",
                           h3(strong("Transformed Data"),
                              downloadButton("downloadTransformedMatrix", "Download Transformed Matrix"))),
          withSpinner(tableOutput("preproc_contents_ready"), color = "#FF4500")
        ),
        tabPanel(
          title = "Preprocessed Data",
          br(),
          h3(strong("Normalized Data")),
          withSpinner(DTOutput("preproc_contents2"), color = "#FF4500"),
          h3(strong("Imputed Data")),
          withSpinner(DTOutput("preproc_contents3"), color = "#FF4500")
        ),
        tabPanel(
          title = "Normalization Quality Control Plots",
          br(),
          h3(strong("Data Normalization")),
          textOutput(strong("preproc_normalization_colors")),
          withSpinner(plotOutput("preproc_boxplot2"), color = "#FF4500"),
          withSpinner(plotOutput("preproc_density2"), color = "#FF4500"),
          withSpinner(plotOutput("preproc_pca2"), color = "#FF4500")
        ),
        tabPanel(
          title = "Imputation Quality Control Plots",
          br(),
          h3(strong("Data Imputation")),
          textOutput(strong("preproc_imputation_colors")),
          withSpinner(plotOutput("preproc_boxplot"), color = "#FF4500"),
          withSpinner(plotOutput("preproc_density"), color = "#FF4500"),
          withSpinner(plotOutput("preproc_pca"), color = "#FF4500")
        )
      )
    )
  )
)


limma_page <- tabPanel(
  title = "Differential Expression",
  titlePanel("Limma Analysis"),
  value = "limma",
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      shinyjs::useShinyjs(),
      fileInput("limma_csv_input", 
                p(
                  span("Select CSV File to Import"),
                  span(icon("info-circle"), id = "icon0", style = "color: #E95420")
                ), accept = ".csv"),
      bsPopover("icon0", NULL, "Samples should be in rows, proteins should be on columns. A column with condition/class should be present. Sample input is shown in the left panel as sample data. Maximum file size is 64MB.", placement = "right"),
      div(style = "margin-top: -50px"),
      fluidRow(
        column(width = 4, checkboxInput("show_head_var", "Show head", T)),
        column(width = 4, textOutput("")),
        column(width = 4, checkboxInput("use_sample_data", 
                                        p(
                                          span("Use Sample Data"),
                                          span(icon("info-circle"), id = "icon01", style = "color: #E95420")
                                        ), F),
               bsPopover("icon01", NULL, "Use sample data to execute the analysis.", placement = "right"),),
        offset = 0,
        style = 'padding:0px;'
      ),
      shinyjs::useShinyjs(),
      selectizeInput(
        'remove_cols_var',
        'Select columns to remove',
        choices = NULL,
        options = list(maxOptions = 5),
        multiple = TRUE
      ),
      selectInput("class_var", "Class Variable", choices = c(not_sel)),
      div(textOutput("print_class_var"), style="margin-top: -15px;margin-bottom: 10px; color:red;"),
      selectInput(
        "class_of_interest_var",
        "Class of Interest",
        choices = c(not_sel)
      ),
      selectInput("contrast_var", "Contrast variable", choices = c(not_sel)),
      checkboxInput("contrast_other_classes_var",  p(
        span("Contrast other classes"),
        span(icon("info-circle"), id = "icon1", style = "color: #E95420")
      ), F),
      bsPopover("icon1", NULL, "Treat other classes expect the Class of Interest as controls", placement = "right"),
      # textInput("contrast_var", "Contrast variable","control", placeholder ="Enter contrast value: control"),
      numericInput("lfc_var", p(
        span("Log fold-change variable"),
        span(icon("info-circle"), id = "icon2", style = "color: #E95420")
      ), "Any"), 
      bsPopover("icon2", NULL, "The choice is arbitrary. However, lower thresholds might result in more false positives. Usual choices are between 1 and 2. Default is set to Any LFC (Empty).", placement = "right"),
      numericInput("pvalue_var", p(
        span("P-value variable"),
        span(icon("info-circle"), id = "icon3", style = "color: #E95420")
      ), "Any", max = 1), 
      bsPopover("icon3", NULL, "Referred to the adjusted p-value. Usual choice is 0.05. Default is set to any p-value (Empty).", placement = "right"),
      selectInput(
        "padjust_method_var",
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
      bsPopover("icon13", NULL, "Adjusted p-value is (highly) recommended. Raw p-value cutoffs yield many false positives and give a general trend, not any statistical significance.", placement = "right"),
      actionButton("run_limma_button", "Perform Limma", icon = icon("play"), style="background-color: #E95420"),
      hidden(div(id='limma_message', textOutput("limma_text"), style="color:green"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Data",
          br(),
          uiOutput("conditional_contents_template"),
          uiOutput("contents_conditional_h5"),
          withSpinner(tableOutput("contents"), color = "#FF4500"),

        ),
        
        tabPanel(title = "Results",
                 br(),
                 withSpinner(DTOutput("limma_table"), color = "#FF4500")),
        tabPanel(
          title = "Top 50",
          br(),
          downloadButton("downloadPlots", "Download Plots"),
          checkboxInput("plotUpregulated", "Upregulated", value = FALSE),
          checkboxInput("plotDownregulated", "Downregulated", value = FALSE),
          br(),
          withSpinner(plotOutput("limma_boxplot"), color = "#FF4500")
        ),
        tabPanel(title = "Volcano Plot",
                 withSpinner(
                   plotlyOutput("limma_volcanoplot", height = '700'), color = "#FF4500"
                 )),
        tabPanel(
          title = "Annotated Volcano Plot",
          uiOutput("volcano_cutoffs"),
          withSpinner(plotOutput("limma_ant_volcanoplot", height = '700'), color =
                        "#FF4500")
        ),
        tabPanel(title = "Heatmap",
                 withSpinner(
                   plotOutput("limma_heatmap", height = '700'), color = "#FF4500"
                 )),
        tabPanel(
          title = "PCA",
          fluidRow(
            column(
              width = 4,
              selectInput(
                "limma_pca_protein_group_var",
                "Protein Functional Group",
                choices = c(
                  'Kinases',
                  'Protoeases',
                  'Transcription Factors',
                  'Hydrolases',
                  'Phosphatases'
                )
              )
            ),
            column(width = 2, checkboxInput("limma_pca_label", "Show label", T)),
            column(
              width = 4,
              radioButtons(
                "limma_pca_frame",
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
            width = 4, textOutput("limma_pca_protein_group_num")
          )),
          withSpinner(plotOutput("limma_pca", height = '700'), color = "#FF4500")
          
        ),
        tabPanel(
          title = "Functional Annotation Analysis",
          h3("Perform functional annotation analysis on DEP proteins."),
          withSpinner(uiOutput("func_annot_analysis"), color ="#FF4500")
        )
      )
    )
  )
)

auto_limma_page <- tabPanel(
  title = "Automated Combinatory Differential Expression",
  titlePanel("Automated Combinatory Differential Expression Analysis"),
  value = "autolimma",
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      shinyjs::useShinyjs(),
      fileInput("auto_limma_csv_input", 
                p(
                  span("Select CSV File to Import"),
                  span(icon("info-circle"), id = "icon4", style = "color: #E95420")
                ), accept = ".csv"),
      bsPopover("icon4", NULL, "Samples should be in rows, proteins should be on columns. A column with condition/class should be present. Sample input is shown in the left panel as sample data. Maximum file size is 64MB.", placement = "right"),
      div(style = "margin-top: -50px"),
      fluidRow(
        column(
          width = 4,
          checkboxInput("auto_use_sample_data", "Use Sample Data", F)
        ),
        offset = 0,
        style = 'padding:0px;'
      ),
      selectInput("auto_class_var", "Class Variable", choices = c(not_sel)),
      div(textOutput("print_auto_class_var"), style="margin-top: -15px;margin-bottom: 10px; color:red;"),
      numericInput("auto_lfc_var", 
                   p(
                     span("Log fold-change variable"),
                     span(icon("info-circle"), id = "icon5", style = "color: #E95420")
                   ), "Any"),
      bsPopover("icon5", NULL, "The choice is arbitrary. However, lower thresholds might result in more false positives. Usual choices are between 1 and 2. Default is set to Any LFC (Empty).", placement = "right"),
      numericInput("auto_pvalue_var", 
                   p(
                     span("P-value variable"),
                     span(icon("info-circle"), id = "icon6", style = "color: #E95420")
                   ), "Any"),
      bsPopover("icon6", NULL, "Referred to the adjusted p-value. Usual choice is 0.05. Default is set to any p-value (Empty).", placement = "right"),
      selectInput(
        "auto_padjust_method_var",
        p(
          span("Adjust P-values for Multiple Comparisons"),
          span(icon("info-circle"), id = "icon14", style = "color: #E95420")
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
      bsPopover("icon14", NULL, "Adjusted p-value is (highly) recommended. Raw p-value cutoffs yield many false positives and give a general trend, not any statistical significance.", placement = "right"),
      br(),
      actionButton("run_auto_limma_button", "Perform Auto Limma", icon = icon("play"), style="background-color: #E95420")
    ),
    mainPanel(
      uiOutput("conditional_contents_template2"),
      uiOutput("contents_conditional_h52"),
      uiOutput("contents_auto_limma"),
      verbatimTextOutput("auto_limma_comb_list"),
      withSpinner(DTOutput("auto_limma_table"), color = "#FF4500")
    )
  )
)

consensus_clustering_page <- tabPanel(
  title = "Consensus Clustering",
  titlePanel("Perform Consensus Clustering"),
  value = "consclustering",
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      shinyjs::useShinyjs(),
      fileInput("cluster_csv_input", 
                p(
                  span("Select CSV File"),
                  span(icon("info-circle"), id = "icon7", style = "color: #E95420")
                ),accept = ".csv"),
      bsPopover("icon7", NULL, "Samples should be in rows, proteins should be on columns. Maximum file size is 64MB.", placement = "right"),
      div(style = "margin-top: -50px"),
      fluidRow(
        column(
          width = 4,
          checkboxInput("cluster_use_sample_data", "Use Sample Data", F)
        ),
        offset = 0,
        style = 'padding:0px;'
      ),
      uiOutput("cluster_slider"),
      bsPopover("icon8", NULL, "The maximum number of clusters. The choice is arbitrary based on the research question at hand.", placement = "right"),
      selectInput(
        "cluster_alg_var",
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
      bsPopover("icon9", NULL, "The type of clustering algorithm to use.", placement = "right"),
      selectInput(
        "cluster_distance_var",
        p(
          span("Distance Measurement"),
          span(icon("info-circle"), id = "icon10", style = "color: #E95420")
        ),
        choices = c('pearson', 'spearman','kendall')
      ),
      bsPopover("icon10", NULL, "This option describes the distance metric to be used.", placement = "right"),
      selectInput(
        "cluster_inner_linkage_var",
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
      bsPopover("icon11", NULL, "This option species the linkage method to use initerative agglomerative hierarchical clustering. Not applicable to other cluster algorithms.", placement = "right"),
      selectInput(
        "cluster_final_linkage_var",
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
      bsPopover("icon12", NULL, "This option species the linkage method to use in the final agglomerative hierarchical clustering.", placement = "right"),
      br(),
      actionButton(
        "run_consclustering_button",
        "Perform Clustering",
        icon = icon("play"), style="background-color: #E95420"
      )
    ),
    mainPanel(
      uiOutput("conditional_contents_template3"),
      uiOutput('cluster_images')
    )
  )
)

# pipeline_page <- tabPanel(
#   title = "Analytics Pipeline",
#   titlePanel("Automated Analytics Pipeline"),
#   sidebarLayout(
#     sidebarPanel(
#       title = "Inputs",
#       fileInput("pipeline_data_csv_input", "Data File to Import", accept = ".csv"),
#       fileInput("pipeline_ant_csv_input", "Annotation File to Import", accept = ".csv"),
#       selectInput("pipeline_batch_var", "Batch Variable", choices = c(not_sel)),
#       selectizeInput('pipeline_technical_factors_var', 'Technical Factors', choices = c(not_sel), multiple = TRUE),
#       actionButton("run_pipeline_button", "Perform Analysis", icon = icon("play"))
#     ),
#     mainPanel(
#       tabPanel(
#         title = "Data",
#         tableOutput("pipeline_data"),
#         tableOutput("pipeline_ant")
#       ),
#       tabPanel(
#         title = "Data",
#         tableOutput("pipeline_ant")
#       )
#     )
#   )
# )

pipeline_page <- tabPanel(
  title = "Analytics Pipeline",
  titlePanel("Automated Analytics Pipeline"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      fileInput("pipeline_data_csv_input", "Data File to Import", accept = ".csv"),
      fileInput("pipeline_ant_csv_input", "Annotation File to Import", accept = ".csv"),
      selectInput(
        "pipeline_biospecimen_id_col_var",
        "Specimen ID",
        choices = c(not_sel)
      ),
      selectInput(
        "pipeline_sample_name_col_var",
        "Sample Name",
        choices = c(not_sel)
      ),
      selectizeInput(
        'pipeline_technical_factors_var',
        'Technical Factors',
        choices = c(not_sel),
        multiple = TRUE
      ),
      selectizeInput(
        'pipeline_biological_factors_var',
        'Biological Factors',
        choices = c(not_sel),
        multiple = TRUE
      ),
      actionButton("run_pipeline_button", "Perform Analysis", icon = icon("play"), style="background-color: #E95420")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Data",
          fluidRow(column(width = 12, h2("Expression Data"))),
          fluidRow(column(width = 12, tableOutput("pipeline_data")),),
          fluidRow(column(width = 12, h2("Sample Annotation"))),
          fluidRow(column(width = 12, tableOutput("pipeline_ant")))
        ),
        tabPanel(
          title = "Missing values",
          fluidRow(column(width = 4, h2("Protein-wise")),
                   column(width = 8, h3(
                     textOutput('pipeline_total_protein')
                   )),),
          fluidRow(column(
            width = 8, withSpinner(plotOutput("pipeline_missing_protein"), color = "#FF4500")
          ),
          column(
            width = 4,
            tableOutput("pipeline_missing_protein_summary_table")
          )),
          fluidRow(
            column(5, sliderInput(
              "protein_slider",
              h3("Check how many proteins retain?"),
              min = 0,
              max = 100,
              value = 50
            )),
            column(3, h3(textOutput(
              'pipeline_retain_protein'
            ))),
            column(
              3,
              actionButton(
                "set_missing_valued_data_button",
                "Use this threshold",
                icon = icon("play")
              )
            )
          ),
          fluidRow(column(width = 4, h2("Sample-wise")),
                   column(width = 8, h3(
                     textOutput('pipeline_total_sample')
                   )),),
          fluidRow(column(
            width = 8, withSpinner(plotOutput("pipeline_missing_sample"), color = "#FF4500")
          ),
          column(
            width = 4,
            tableOutput("pipeline_missing_sample_summary_table")
          )),
          fluidRow(column(
            5, sliderInput(
              "sample_slider",
              h3("Check how many samples retain?"),
              min = 0,
              max = 100,
              value = 50
            )
          ),
          column(3, h3(
            textOutput('pipeline_retain_sample')
          )))
        ),
        tabPanel(
          title = "Initial Inspection",
          fluidRow(column(width = 4, h3("Sample Replicates"))),
          fluidRow(
            column(width = 4, verbatimTextOutput("pipeline_sample_replicates")),
            column(
              width = 4,
              style = "margin-top: 50px;",
              actionButton(
                "perform_unique_replicates_button",
                "Make unique",
                icon = icon("play")
              )
            ),
            column(
              4,
              downloadButton("downloadNonReplicatedTable", "Download Results")
            ),
            column(
              4,
              style = "margin-top: 50px;",
              downloadButton("downloadNonReplicatedAnnotation", "Download Annotations")
            ),
          ),
          fluidRow(column(
            width = 4, verbatimTextOutput("pipeline_unique_replicates")
          ),),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_init_protein_corr_plot"), color =
                                      "#FF4500")
          )),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_protein_mean_plot"), color =
                                      "#FF4500")
          )),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_protein_mean_box_plot"), color =
                                      "#FF4500")
          )),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_protein_mean_pca_plot"), color =
                                      "#FF4500")
          ))
        ),
        tabPanel(
          title = "Data Normalization",
          fluidRow(
            column(
              4,
              checkboxGroupInput(
                "pipeline_norm_method",
                h3("Select Normalization Methods"),
                choices = list(
                  "Median Normalization" = 1,
                  "Quantile Normalization" = 2
                ),
                selected = 1,
                width = '100%'
              )
            ),
            column(
              4,
              style = "margin-top: 70px;",
              actionButton(
                "perform_normalization_data_button",
                "Perform Normalization",
                icon = icon("play")
              )
            ),
            column(
              4,
              style = "margin-top: 70px;",
              downloadButton("downloadNormTable", "Download Results")
            )
          ),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_protein_norm_mean_plot"), color =
                                      "#FF4500")
          )),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_protein_norm_mean_box_plot"), color =
                                      "#FF4500")
          )),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_protein_norm_mean_pca_plot"), color =
                                      "#FF4500")
          ))
        ),
        tabPanel(
          title = "Imputation",
          fluidRow(
            column(
              4,
              checkboxGroupInput(
                "pipeline_impu_method",
                h3("Select Data Imputation Methods"),
                choices = list("KNN", "MissForest", "ADMIN",
                               "Birnn", "SpectroFM", "RegImpute"),
                selected = "KNN",
                width = '100%'
              )
            ),
            column(
              4,
              style = "margin-top: 150px;",
              actionButton(
                "perform_imputation_data_button",
                "Perform Imputation",
                icon = icon("play")
              )
            ),
            column(
              4,
              style = "margin-top: 150px;",
              align = "right",
              downloadButton("downloadImpuTable", "Download Results")
            )
          ),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_protein_impu_plot"), color =
                                      "#FF4500")
          )),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_protein_impu_box_plot"), color =
                                      "#FF4500")
          )),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_protein_impu_pca_plot"), color =
                                      "#FF4500")
          )),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_imput_protein_corr_plot"), color =
                                      "#FF4500")
          ))
        ),
        tabPanel(
          title = "Batch Effects",
          # fluidRow(
          #   column(width = 4, h3("Sample Replicates"))
          # ),
          # fluidRow(
          #   column(width = 4, verbatimTextOutput("pipeline_sample_replicates")),
          #   column(width = 4, style = "margin-top: 50px;", actionButton("perform_unique_replicates_button", "Make unique", icon = icon("play"))),
          #   column(4, downloadButton("downloadBatchCorrTable", "Download Results"))
          # ),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_protein_pcva_plot"), color =
                                      "#FF4500")
          )),
          fluidRow(
            # column(4, checkboxGroupInput("pipeline_impu_method", h3("Select Data Imputation Methods"),
            #                              choices = list("KNN", "MissForest", "ADMIN",
            #                                             "Birnn", "SpectroFM", "RegImpute"), selected = "KNN",width = '100%')),
            column(
              4,
              selectInput(
                "pipeline_batch_var",
                "Batch effect variable",
                choices = c(not_sel)
              )
            ),
            column(
              4,
              selectInput(
                "batch_corr_method_var",
                "Batch correction Method",
                choices = c("ComBat", "MedianCentering", "MeanCentering")
              ),
            ),
            column(
              4,
              actionButton(
                "perform_batchcorr_data_button",
                "Perform Batch Correction",
                icon = icon("play")
              )
            )
          ),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_protein_batchcorr_box_plot"), color =
                                      "#FF4500")
          )),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_protein_batchcorr_pca_plot"), color =
                                      "#FF4500")
          )),
          fluidRow(column(
            width = 12, withSpinner(plotOutput("pipeline_protein_batchcorr_pcva_plot"), color =
                                      "#FF4500")
          ))
        ),
        tabPanel(
          title = "Correlation",
          fluidRow(column(
            width = 12, withSpinner(
              plotOutput("pipeline_protein_corr_plot", height = "700"),
              color = "#FF4500"
            )
          )),
          fluidRow(column(
            width = 12, withSpinner(
              plotOutput("pipeline_protein_overall_corr_plot", height = "700"),
              color = "#FF4500"
            )
          )),
          fluidRow(column(
            width = 12, withSpinner(
              plotOutput("pipeline_protein_corr_dist_plot", height = "700"),
              color = "#FF4500"
            )
          )),
        ),
        tabPanel(
          title = "Differential Expression",
          fluidRow(
            column(
              4,
              selectInput("pipeline_class_var", "Class Variable", choices = c(not_sel))
            ),
            column(
              4,
              selectInput(
                "pipeline_class_of_interest_var",
                "Class of Interest",
                choices = c(not_sel)
              )
            ),
            column(
              4,
              textInput(
                "pipeline_contrast_var",
                "Contrast variable",
                "control",
                placeholder = "Enter contrast value: control"
              )
            ),
          ),
          fluidRow(
            column(
              4,
              textInput("pipeline_lfc_var", "Log fold-change variable", "Any")
            ),
            column(4, textInput(
              "pipeline_pvalue_var", "P-value variable", "Any"
            )),
            column(
              4,
              actionButton(
                "pipeline_perform_limma_data_button",
                "Perform Limma",
                icon = icon("play")
              )
            )
          ),
          fluidRow(column(
            8, withSpinner(tableOutput("pipeline_limma_table"), color = "#FF4500")
          ),
          column(
            4,
            downloadButton("downloadPipeLimmaTable", "Download Results")
          ), style = "padding-top:20px"),
        ),
        tabPanel(
          title = " Automated Differential Expression",
          fluidRow(
            column(
              4,
              selectInput(
                "pipeline_auto_class_var",
                "Class Variable",
                choices = c(not_sel)
              )
            ),
            column(
              4,
              textInput("pipeline_auto_lfc_var", "Log fold-change variable", "Any")
            ),
            column(
              4,
              textInput("pipeline_auto_pvalue_var", "P-value variable", "Any")
            ),
          ),
          fluidRow(column(
            4,
            actionButton(
              "pipeline_perform_auto_limma_data_button",
              "Perform Limma",
              icon = icon("play")
            )
          ),),
          fluidRow(column(
            8, withSpinner(DTOutput("pipeline_auto_limma_table"), color = "#FF4500")
          ),
          column(
            4,
            downloadButton("downloadPipeAutoLimmaTable", "Download Results")
          )),
        ),
        tabPanel(title = "Clustering",
                 fluidRow(column(
                   width = 12, withSpinner(plotOutput("pipeline_protein_dendogram"), color =
                                             "#FF4500")
                 )),
                 fluidRow(column(
                   width = 12, withSpinner(plotOutput("pipeline_protein_heatmap"), color =
                                             "#FF4500")
                 )),),
      )
    )
  )
)

draw_plot_1 <- function(data_input,
                        num_var_1,
                        num_var_2,
                        fact_var) {
  if (fact_var != not_sel) {
    data_input[, (fact_var) := as.factor(data_input[, get(fact_var)])]
  }
  if (num_var_1 != not_sel &
      num_var_2 != not_sel & fact_var != not_sel) {
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2, color = fact_var)) +
      geom_point()
  }
  else if (num_var_1 != not_sel &
           num_var_2 != not_sel & fact_var == not_sel) {
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2)) +
      geom_point()
  }
  else if (num_var_1 != not_sel &
           num_var_2 == not_sel & fact_var != not_sel) {
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_1)) +
      geom_violin()
  }
  else if (num_var_1 == not_sel &
           num_var_2 != not_sel & fact_var != not_sel) {
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_2)) +
      geom_violin()
  }
  else if (num_var_1 != not_sel &
           num_var_2 == not_sel & fact_var == not_sel) {
    ggplot(data = data_input,
           aes_string(x = num_var_1)) +
      geom_histogram()
  }
  else if (num_var_1 == not_sel &
           num_var_2 != not_sel & fact_var == not_sel) {
    ggplot(data = data_input,
           aes_string(x = num_var_2)) +
      geom_histogram()
  }
  else if (num_var_1 == not_sel &
           num_var_2 == not_sel & fact_var != not_sel) {
    ggplot(data = data_input,
           aes_string(x = fact_var)) +
      geom_bar()
  }
}
create_num_var_table <- function(data_input, num_var) {
  if (num_var != not_sel) {
    col <- data_input[, get(num_var)]
    if (length(col) > 5000)
      col_norm <- sample(col, 5000)
    else
      col_norm <- col
    norm_test <- shapiro.test(col_norm)
    statistic <-
      c(
        "mean",
        "median",
        "5th percentile",
        "95th percentile",
        "Shapiro statistic",
        "Shapiro p-value"
      )
    value <- c(
      round(mean(col), 2),
      round(median(col), 2),
      round(quantile(col, 0.05), 2),
      round(quantile(col, 0.95), 2),
      norm_test$statistic,
      norm_test$p.value
    )
    data.table(statistic, value)
  }
}

create_fact_var_table <- function(data_input, fact_var) {
  if (fact_var != not_sel) {
    freq_tbl <- data_input[, .N, by = get(fact_var)]
    freq_tbl <- setnames(freq_tbl, c("factor_value", "count"))
    freq_tbl
  }
}

create_combined_table <-
  function(data_input,
           num_var_1,
           num_var_2,
           fact_var) {
    if (fact_var != not_sel) {
      if (num_var_1 != not_sel & num_var_2 != not_sel) {
        res_tbl <-
          data_input[, .(correlation = cor(get(num_var_1), get(num_var_2))), by = fact_var]
      }
      else if (num_var_1 != not_sel & num_var_2 == not_sel) {
        res_tbl <-
          data_input[, .(mean = mean(get(num_var_1))), by = fact_var]
      }
      else if (num_var_1 == not_sel & num_var_2 != not_sel) {
        res_tbl <-
          data_input[, .(mean = mean(get(num_var_2))), by = fact_var]
      }
    }
    else if (num_var_1 != not_sel & num_var_2 != not_sel) {
      res_tbl <- data.table(statistic = c("correlation"),
                            value = c(cor(data_input[, get(num_var_1)],
                                          data_input[, get(num_var_2)])))
    }
    return(res_tbl)
  }

create_pca_plots = function(data, ant, parameters, color_list) {
  pcas = list()
  pca = NULL
  for (para in parameters) {
    pca = plot_PCA(
      data,
      ant,
      color_by = para,
      plot_title = para,
      color_scheme = color_list[[para]]
    )
    #pcas = append(pcas, pca)
    pcas[[para]] = pca
  }
  nrows = if (length(parameters) %% 2 == 0)
    (length(parameters) / 2)
  else
    ((length(parameters) / 2) + 1)
  plots = ggarrange(plotlist = pcas,
                    ncol = 2,
                    nrow = nrows)
  return(plots)
}

create_mean_plot = function(df, ant, batch, color_list, title) {
  return (
    plot_sample_mean(
      df,
      ant,
      order_col = 'ID',
      batch_col = batch,
      color_by_batch = TRUE,
      ylimits = c(19, 36),
      color_scheme = color_list[[batch]]
    ) + ggtitle(title)
  )
}

create_boxplot = function(df, ant, batch, color_list, title) {
  long_df = matrix_to_long(df)
  return(
    plot_boxplot(
      long_df,
      ant,
      batch_col = batch,
      color_scheme = color_list[[batch]]
    ) + ggtitle(title)
  )
}

create_consensus_clust_plots = function(data, ant, parameters, color_list) {
  clusts = list()
  pca = NULL
  for (para in parameters) {
    pca = plot_PCA(
      data,
      ant,
      color_by = para,
      plot_title = para,
      color_scheme = color_list[[para]]
    )
    #pcas = append(pcas, pca)
    pcas[[para]] = pca
  }
  
  plots = ggarrange(plotlist = clusts,
                    ncol = 1,
                    nrow = length(clusts))
  return(plots)
}


ui <- navbarPage(
  title = "FlexStat 1.0",
  theme = shinytheme('united'),
  id = "inTabset",
  # main_page,
  preprocessing_page,
  limma_page,
  auto_limma_page,
  consensus_clustering_page,
  # pipeline_page,
  about_page,
)

server <- function(input, output, session) {
  #MAX FILE SIZE FOR UPLOAD
  #options(shiny.maxRequestSize=10*1024^2)
  options(shiny.maxRequestSize = 64 * 1024 ^ 2)
  show_template <- reactiveVal(TRUE)
  show_template2 <- reactiveVal(TRUE)
  auto_limma_ready <- reactiveVal(FALSE)
  show_template3 <- reactiveVal(TRUE)
  preproc_show_template <- reactiveVal(TRUE)
  
  data_input <- reactive({
    req(input$csv_input)
    fread(input$csv_input$datapath)
  })
  
  output$conditional_contents_template <- renderUI({
    if (show_template()) {
      tagList(h3(strong("Sample Data")),
              tableOutput("contents_template"))
    } else{
      NULL
    }
  })
  
  output$conditional_contents_template2 <- renderUI({
    if (show_template2()) {
      tagList(h3(strong("Sample Data")),
              tableOutput("contents_template2"))
    } else{
      NULL
    }
  })
  output$conditional_contents_template3 <- renderUI({
    if (show_template3()) {
      tagList(h3(strong("Sample Data")),
              tableOutput("contents_template3"))
    } else{
      NULL
    }
  })
  
  output$conditional_contents_template4 <- renderUI({
    if (preproc_show_template()) {
      tagList(h3(strong("Sample Data")),
              tableOutput("contents_template4"))
    } else{
      NULL
    }
  })
  
  output$contents_conditional_h5 <- renderUI({
    if (!show_template()) {
      h3(strong("Original Data"))
    } else{
      NULL
    }
  })
  
  output$contents_conditional_h6 <- renderUI({
    if (!preproc_show_template()) {
      h3(strong("Original Data"))
    } else{
      NULL
    }
  })
  
  output$contents_conditional_h52 <- renderUI({
    if (!show_template2() & !auto_limma_ready()) {
      tagList(h3(strong("Original Data")),
              withSpinner(tableOutput("contents2"), color = "#FF4500"))
    } else{
      NULL
    }
  })
  
  output$contents_auto_limma <- renderUI({
    if (auto_limma_ready()) {
      tagList(h3(strong("Auto limma Results")),
              fluidRow(
                column(
                  width = 4,
                  downloadButton("downloadAutoTable", "Download Top 50")
                ),
                column(
                  width = 4,
                  downloadButton("downloadAutoTableAll", "Download All ")
                )
              ),
              br(),
              br())
    } else{
      NULL
    }
  })
  
  observeEvent(data_input(), {
    choices <- c(not_sel, names(data_input()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
    
  })
  
  observeEvent(input$contrast_other_classes_var, {
    if (input$contrast_other_classes_var == T) {
      shinyjs::disable('contrast_var')
    } else {
      shinyjs::enable('contrast_var')
    }
  }, ignoreNULL = T)
  
  num_var_1 <- eventReactive(input$run_button, input$num_var_1)
  num_var_2 <- eventReactive(input$run_button, input$num_var_2)
  fact_var <- eventReactive(input$run_button, input$fact_var)
  
  # plot
  
  plot_1 <- eventReactive(input$run_button, {
    draw_plot_1(data_input(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$plot_1 <- renderPlot(plot_1())
  
  # 1-d summary tables
  
  output$num_var_1_title <-
    renderText(paste("Num Var 1:", num_var_1()))
  
  num_var_1_summary_table <- eventReactive(input$run_button, {
    create_num_var_table(data_input(), num_var_1())
  })
  
  output$num_var_1_summary_table <-
    renderTable(num_var_1_summary_table(), colnames = FALSE)
  
  output$num_var_2_title <-
    renderText(paste("Num Var 2:", num_var_2()))
  
  num_var_2_summary_table <- eventReactive(input$run_button, {
    create_num_var_table(data_input(), num_var_2())
  })
  
  output$num_var_2_summary_table <-
    renderTable(num_var_2_summary_table(), colnames = FALSE)
  
  output$fact_var_title <-
    renderText(paste("Factor Var:", fact_var()))
  
  fact_var_summary_table <- eventReactive(input$run_button, {
    create_fact_var_table(data_input(), fact_var())
  })
  
  output$fact_var_summary_table <-
    renderTable(fact_var_summary_table(), colnames = FALSE)
  
  # multi-d summary table
  
  combined_summary_table <- eventReactive(input$run_button, {
    create_combined_table(data_input(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$combined_summary_table <- renderTable(combined_summary_table())
  output$contents_template <- renderTable({
    read.csv("sample_data.csv", row.names = 1)
  })
  output$contents_template2 <- renderTable({
    read.csv("sample_data.csv", row.names = 1)
  })
  output$pipeline_data <- renderTable({
    return(head(read.csv("pipeline_data.csv")))
  })
  output$contents_template3 <- renderTable({
    read.csv("consensus_data.csv")
  })
  output$contents_template4 <- renderTable({
    read.csv("sample_preproc_data.csv", row.names = 1)
  })
  
  # limma table
  
  limma_data_input <- reactive({
    req(input$limma_csv_input)
    fread(input$limma_csv_input$datapath)
  })
  
  observeEvent(limma_data_input(), {
    char_column_df = data.frame(limma_data_input(), row.names=1)  %>%
      select_if(is.character)
    ##print(char_column_df)
    show_template(FALSE)
    
    choices <- c(not_sel, names(char_column_df))
    updateSelectInput(inputId = "class_var", choices = choices)
    
    choices <- c(not_sel, names(char_column_df))
    updateSelectInput(inputId = "class_of_interest_var", choices = choices)
    
    choices2 <- c(sub(" ", "_", names(limma_data_input())))
    updateSelectizeInput(session, inputId = "remove_cols_var", choices = choices2,
                         server = TRUE)
  })
  
  preproc_data_input <- reactive({
    req(input$preproc_data_csv_input)
    fread(input$preproc_data_csv_input$datapath)
  })
  
  observeEvent(preproc_data_input(), {
    char_column_df = data.frame(preproc_data_input(), row.names=1)  %>%
      select_if(is.character)
    ##print(char_column_df)
    preproc_show_template(FALSE)
    
    choices <- c(not_sel, names(char_column_df))
    updateSelectInput(inputId = "preproc_class_var", choices = choices)
    
    choices <- c(not_sel, names(char_column_df))
    updateSelectInput(inputId = "preproc_batch_var", choices = choices)
    
    choices2 <- c(sub(" ", "_", names(char_column_df)))
    updateSelectizeInput(session, inputId = "preproc_remove_cols_var", choices = choices2,
                         server = TRUE)
    
    updateSelectInput(inputId = "preproc_ir_var", choices = (preproc_data_input()[,1]))
  })
  
  observeEvent(input$preproc_use_sample_data, {
    if (input$preproc_use_sample_data) {
      reset(id = "preproc_data_csv_input")
      reset(id = "preproc_contents")
      preproc_show_template(TRUE)
      variables$functional_dataset = NULL
      
      data_df = read.csv("sample_preproc_data.csv", row.names = 1)
      char_column_df = data_df %>% select_if(is.character)

      choices <- c(not_sel, names(char_column_df))
      updateSelectInput(inputId = "preproc_class_var", choices = choices)
      
      choices <- c(not_sel, names(char_column_df))
      updateSelectInput(inputId = "preproc_batch_var", choices = choices)
      
      choices2 <- c(sub(" ", "_", names(char_column_df)))
      updateSelectInput(inputId = "preproc_remove_cols_var", choices = choices2)
      
      updateSelectInput(inputId = "preproc_ir_var", choices = rownames(data_df))
    } else {
      variables$functional_dataset = NULL
    }
    
  })
  
  observeEvent(input$preproc_use_sample_data, {
    if (input$use_sample_data) {
      shinyjs::disable("preproc_show_transpose_var")
    } else {
      shinyjs::enable("preproc_show_transpose_var")
    }
  })
  
  observeEvent(input$preproc_show_log2_var, {
    if (input$preproc_show_log2_var) {
      shinyjs::disable("preproc_show_log10_var")
    } else{
      shinyjs::enable("preproc_show_log10_var")
    }
  })
  
  observeEvent(input$preproc_show_log10_var, {
    if (input$preproc_show_log10_var) {
      shinyjs::disable("preproc_show_log2_var")
    } else{
      shinyjs::enable("preproc_show_log2_var")
    }
  })
  
  observeEvent(input$use_sample_data, {
    if (input$use_sample_data) {
      reset(id = "limma_csv_input")
      reset(id = "contents")
      show_template(TRUE)
      variables$functional_dataset = NULL
      
      data_df = read.csv("sample_data.csv", row.names = 1)
      char_column_df = data_df %>% select_if(is.character)

      choices <- c(not_sel, names(char_column_df))
      updateSelectInput(inputId = "class_var", choices = choices)
      
      choices <- c(not_sel, names(char_column_df))
      updateSelectInput(inputId = "class_of_interest_var", choices = choices)
      
      choices2 <- c(sub(" ", "_", names(char_column_df)))
      updateSelectInput(inputId = "remove_cols_var", choices = choices2)
    } else {
      variables$functional_dataset = NULL
    }
    
  })
  
  # observeEvent(input$use_sample_data, {
  #   if (input$use_sample_data) {
  #     shinyjs::disable("show_transpose_var")
  #   } else {
  #     shinyjs::enable("show_transpose_var")
  #   }
  # })
  # 
  # observeEvent(input$show_log2_var, {
  #   if (input$show_log2_var) {
  #     shinyjs::disable("show_log10_var")
  #   } else{
  #     shinyjs::enable("show_log10_var")
  #   }
  # })
  # 
  # observeEvent(input$show_log10_var, {
  #   if (input$show_log10_var) {
  #     shinyjs::disable("show_log2_var")
  #   } else{
  #     shinyjs::enable("show_log2_var")
  #   }
  # })
  
  output$contents <- renderTable({
    if (!show_template()) {
      req(input$limma_csv_input)
      
      df = read.csv(
        input$limma_csv_input$datapath,
        head = TRUE,
        sep = ",",
        row.names = 1
      )
      
      if (input$show_head_var) {
        return(head(df))
      } else {
        return(df)
      }
    }
    
  }, rownames = TRUE)
  
  output$contents2 <- renderTable({
    req(input$auto_limma_csv_input)
    df = read.csv(
      input$auto_limma_csv_input$datapath,
      head = TRUE,
      sep = ",",
      row.names = 1
    )
    return(head(df))
  }, rownames = TRUE)
  
  output$preproc_contents <- renderTable({
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
  
  output$contents_ready <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    # if (input$show_transpose_var ||
    #     input$show_log2_var || input$show_log10_var) {
    if (!input$use_sample_data) {
      req(input$limma_csv_input)
      data_df = read.csv(
        input$limma_csv_input$datapath,
        head = TRUE,
        sep = ",",
        row.names = 1
      )
    } else{
      data_df = read.csv("sample_data.csv", row.names = 1)
    }
    
    if (!is.null(input$remove_cols_var) &
        !("Not Selected" %in% input$remove_cols_var)) {
      data_df = data_df[,-which(names(data_df) %in% input$remove_cols_var)]
    }
    # if (input$show_transpose_var) {
    #   data_df = as.data.frame(t(data_df))
    #   variables$functional_dataset = data_df
    # }
    # if (input$show_log2_var) {
    #   data_df = as.data.frame(mutate_if(data_df, is.numeric, log2))
    #   variables$functional_dataset = data_df
    # }
    # if (input$show_log10_var) {
    #   data_df = as.data.frame(mutate_if(data_df, is.numeric, log10))
    #   variables$functional_dataset = data_df
    # }
    
    if (input$show_head_var) {
      return(head(data_df))
    }
    else {
      return(data_df)
    }
    
    
  }, rownames = TRUE)
  
  output$preproc_contents_ready <- renderTable({
    if (input$preproc_show_transpose_var ||
        input$preproc_show_log2_var || input$preproc_show_log10_var) {
      if (!input$preproc_use_sample_data) {
        req(input$preproc_data_csv_input)
        data_df = read.csv(
          input$preproc_data_csv_input$datapath,
          head = TRUE,
          sep = ",",
          row.names = 1
        )
      } else{
        data_df = read.csv("sample_preproc_data.csv", row.names = 1)
      }
      
      if (!is.null(input$preproc_remove_cols_var) &
          !("Not Selected" %in% input$preproc_remove_cols_var)) {
        data_df = data_df[,-which(names(data_df) %in% input$preproc_remove_cols_var)]
      }
      if (input$preproc_show_transpose_var) {
        data_df = as.data.frame(t(data_df))
        variables$functional_dataset = data_df
      }
      if (input$preproc_show_log2_var) {
        data_df = as.data.frame(mutate_if(data_df, is.numeric, log2))
        variables$functional_dataset = data_df
      }
      if (input$preproc_show_log10_var) {
        data_df = as.data.frame(mutate_if(data_df, is.numeric, log10))
        variables$functional_dataset = data_df
      }
      return(data_df)
    }
    
  }, rownames = TRUE)
  
  output$downloadTransformedMatrix <- downloadHandler(
    filename = "transformed_data.csv",
    content = function(file) {
      write.csv(variables$functional_dataset, file)
    }
  )
  
  preproc_class_var = eventReactive(input$run_preproc_button, input$preproc_class_var)
  preproc_batch_var = eventReactive(input$run_preproc_button, input$preproc_batch_var)
  preproc_missing_value_threshold_var = eventReactive(input$run_preproc_button, input$preproc_missing_value_threshold_var)
  preproc_imputation_method_var = eventReactive(input$run_preproc_button, input$preproc_imputation_method_var)
  preproc_normalization_method_var = eventReactive(input$run_preproc_button, input$preproc_normalization_method_var)
  preproc_ir_var = eventReactive(input$run_preproc_button, input$preproc_ir_var)
  
  observe(
    {
      if(input$preproc_class_var!=not_sel && input$preproc_batch_var!=not_sel)
      {
        enable("run_preproc_button")
      }
      else
      {
        disable("run_preproc_button")
      }
    })
  
  observeEvent(input$run_preproc_button, {
    toggle('preproc_message')
    output$preproc_text <- renderText({"Check the Preprocessed Data Tab"})
  })
  
  observeEvent(input$run_limma_button, {
    toggle('limma_message')
    output$limma_text <- renderText({"Check the Results Tab"})
  })
  
  output$preproc_contents3 = renderDT(server = FALSE, {
    data <- preproc_table_3()
    if (!is.null(data$x)) {
      data
    } else {
      datatable(data.frame(Test = "No Data to show"))
    }
  })
  
  preproc_table_3 <- eventReactive(variables$normalized_dataset, {
    df = variables$functional_dataset
    
    if(!is.null(df)){
      num_column_df = names(df %>% select_if(is.numeric))
      char_columns = df %>% select_if(is.character)
      
      df = df[,colnames(df) %in% num_column_df]
      
      df = as.data.frame(t(df))
      
      # df = df[which(rowMeans(is.na(df)) < input$preproc_missing_value_threshold_var), ]
      
      if(input$preproc_imputation_method_var %in% c("K-nearest neighbour",
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
          method = input$preproc_imputation_method_var,
          out = "Ensemble"
        )
        imputed_df = as.data.frame(imputed_df$Ensemble)
      } else {
        imputed_df = as.data.frame(impute_normal(df, width = 0.3, downshift = 1.8, seed = 100))
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
      
      boxplot(t(num_column_df2), col = as.character(as.numeric(as.factor(char_columns2[,input$preproc_class_var]))), 
              notch = F, main = "Before Imputation",
              xlab = 'Sample', ylab = 'log2 of Intensity')
      
      boxplot(t(num_column_df), col = as.character(as.numeric(as.factor(char_columns[,input$preproc_class_var]))), 
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
      plotDensities(t(num_column_df2), col = as.character(as.numeric(as.factor(char_columns2[,input$preproc_class_var]))), 
                    main = "Before Imputation - Density Plot", legend=F)
      plotDensities(t(num_column_df), col = as.character(as.numeric(as.factor(char_columns[,input$preproc_class_var]))), 
                    main = "After Imputation - Density Plot", legend=F)
      #sub = paste("Colored by ", input$preproc_class_var)
    }
    
  })
  
  output$preproc_normalization_colors = renderText({
    paste("Colored by ", input$preproc_class_var)
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
      plotMDS(t(num_column_df2), labels= NULL, pch= 15,col = as.character(as.numeric(as.factor(char_columns2[,input$preproc_class_var]))), main = "Before Imputation - Principal Component Analysis")
      plotMDS(t(num_column_df), labels= NULL, pch= 15,col = as.character(as.numeric(as.factor(char_columns[,input$preproc_class_var]))), main = "After Imputation - Principal Component Analysis")
    }
    
  })
  
  output$preproc_contents2 = renderDT(server = FALSE, {
    data <- preproc_table_2()
    if (!is.null(data$x)) {
      data
    } else {
      datatable(data.frame(Test = "No Data to show"))
    }
  })
  
  preproc_table_2 <- eventReactive(input$run_preproc_button, {
    # df = variables$functional_dataset
    
    if(is.null(variables$functional_dataset)){ 
      if (!input$preproc_use_sample_data) {
        req(input$preproc_data_csv_input)
        df = read.csv(
          input$preproc_data_csv_input$datapath,
          head = TRUE,
          sep = ",",
          row.names = 1
        )
      } else{
        df = read.csv("sample_preproc_data.csv", row.names = 1)
      }
    } else{
      df = variables$functional_dataset
    }
    
    variables$original_dataset = df
    
    # validate(
    #   need(!is.null(df), 'No data exists, please upload data or use sample data')
    # )
    
    if(!is.null(df)){
      
      num_column_df = names(df %>% select_if(is.numeric))
      char_columns = df %>% select_if(is.character)
      
      df_ori = df
      
      df = df[,colnames(df) %in% num_column_df]
      df = as.data.frame(t(df))
      
      df = df[which(rowMeans(is.na(df)) < input$preproc_missing_value_threshold_var), ]
      
      if(input$preproc_normalization_method_var == 1){
        normalized_df = normalizeMedianValues(df)
        normalized_df = as.data.frame(t(normalized_df))
      } else if(input$preproc_normalization_method_var == 2){
        normalized_df = normalize.quantiles(as.matrix(df),copy=TRUE)
        normalized_df = as.data.frame(normalized_df)
        colnames(normalized_df) = colnames(df)
        rownames(normalized_df) = rownames(df)
        normalized_df = as.data.frame(t(normalized_df))
      } else if(input$preproc_normalization_method_var == 3){
        if(!is.null(input$preproc_batch_var)){
          colSums = list()
          data_sl = data.frame()
          i = 1
          for (u in unique(char_columns[,input$preproc_batch_var])){
            d = df[,rownames(char_columns[,input$preproc_batch_var==u])]
            d = d[,!colnames(d) %in% c(input$preproc_batch_var)]
            colSums[i] = as.data.frame(colSums(d, na.rm = T))
            i = i+1
          }
          
          target = mean(unlist(colSums))
          
          for (u in unique(char_columns[,input$preproc_batch_var])){
            d = df[,char_columns[,input$preproc_batch_var]==u]
            d = d[,!colnames(d) %in% c(input$preproc_batch_var)]
            norm_facs = target / colSums(d, na.rm = T)
            exp1_sl <- sweep(d, 2, norm_facs, FUN = "*")
            exp1_sl = as.data.frame(exp1_sl)
            
            exp1_sl = as.data.frame(t(exp1_sl))
            exp1_sl[,input$preproc_batch_var] = u
            
            # if(nrow(data_sl) == 0){
            #   data_sl = exp1_sl
            # } else {
            #   data_sl = cbind(data_sl, exp1_sl)
            # }
            data_sl = rbind(data_sl, exp1_sl)
          }
          
          target2 <- colMeans(data_sl[input$preproc_ir_var,!(names(data_sl) %in% c(input$preproc_batch_var))])
          data_sl_tmm = data.frame()
          
          j = 1
          for (k in input$preproc_ir_var){
            u = unique(char_columns[,input$preproc_batch_var])[j]
            d = data_sl[data_sl[,input$preproc_batch_var]==u,]
            d = d[,!colnames(d) %in% c(input$preproc_batch_var)]
            
            norm_facs2 = target2 / d[k,]
            norm_facs2 = as.numeric(as.vector(norm_facs2[1,]))
            exp1_sl <- sweep(d, 2, norm_facs2, FUN = "*")
            
            # if(nrow(data_sl_tmm) == 0){
            #   data_sl_tmm = as.data.frame(exp1_sl)
            # } else {
            #   data_sl_tmm = cbind(data_sl_tmm, as.data.frame(exp1_sl))
            # }
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
  
  output$preproc_boxplot2 = renderPlot({
    validate(need(!is.null(variables$normalized_dataset), 'Boxplot: No data exists, please upload data or use sample data'))
    preproc_boxplot_2()
  })
  
  preproc_boxplot_2 = eventReactive(input$run_preproc_button, {
    df = variables$normalized_dataset
    
    if(!is.null(df)){
      df = df[order(df[,input$preproc_class_var]),]
      
      num_column_df = df %>% select_if(is.numeric)
      char_columns = df %>% select_if(is.character)
      
      ori_df = variables$original_dataset
      ori_df = ori_df[order(ori_df[,input$preproc_class_var]),]
      
      num_column_df2 = ori_df %>% select_if(is.numeric)
      char_columns2 = ori_df %>% select_if(is.character)
      
      par(mfrow=c(1,2))
      
      boxplot(t(num_column_df2), col = as.character(as.numeric(as.factor(char_columns2[,input$preproc_class_var]))), 
              notch = F, main = "Before Normalization",
              xlab = 'Sample', ylab = 'log2 of Intensity')
      
      boxplot(t(num_column_df), col = as.character(as.numeric(as.factor(char_columns[,input$preproc_class_var]))), 
              notch = F, main = "After Normalization",
              xlab = 'Sample', ylab = 'log2 of Intensity')
    }
    
  })
  
  output$preproc_density2 = renderPlot({
    validate(need(!is.null(variables$normalized_dataset), 'Density Plot: No data exists, please upload data or use sample data'))
    preproc_density_2()
  })
  
  preproc_density_2 = eventReactive(input$run_preproc_button, {
    df = variables$normalized_dataset
    
    if(!is.null(df)){
      df = df[order(df[,input$preproc_class_var]),]
      
      num_column_df = df %>% select_if(is.numeric)
      char_columns = df %>% select_if(is.character)
      
      ori_df = variables$original_dataset
      ori_df = ori_df[order(ori_df[,input$preproc_class_var]),]
      
      num_column_df2 = ori_df %>% select_if(is.numeric)
      char_columns2 = ori_df %>% select_if(is.character)
      
      par(mfrow=c(1,2))
      plotDensities(t(num_column_df2), col = as.character(as.numeric(as.factor(char_columns2[,input$preproc_class_var]))), 
                    main = "Before Normalization - Density Plot", legend=F)
      plotDensities(t(num_column_df), col = as.character(as.numeric(as.factor(char_columns[,input$preproc_class_var]))), 
                    main = "After Normalization - Density Plot", legend=F)
      # par(mfrow=c(1,1))
      # text(0.5,0.5, paste("Colored by ", input$preproc_class_var),cex=6)
    }
    
  })
  
  output$preproc_imputation_colors = renderText({
    paste("Colored by ", input$preproc_class_var)
  })
  
  output$preproc_pca2 = renderPlot({
    validate(need(!is.null(variables$normalized_dataset), 'PCA Plot: No data exists, please upload data or use sample data'))
    preproc_pca_2()
  })
  
  preproc_pca_2 = eventReactive(input$run_preproc_button, {
    df = variables$normalized_dataset
    
    if(!is.null(df)){
      df = df[order(df[,input$preproc_class_var]),]
      
      num_column_df = df %>% select_if(is.numeric)
      char_columns = df %>% select_if(is.character)
      
      ori_df = variables$original_dataset
      ori_df = ori_df[order(ori_df[,input$preproc_class_var]),]
      
      num_column_df2 = ori_df %>% select_if(is.numeric)
      char_columns2 = ori_df %>% select_if(is.character)
      
      par(mfrow=c(1,2))
      plotMDS(t(num_column_df2), labels= NULL,pch= 15, col = as.character(as.numeric(as.factor(char_columns2[,input$preproc_class_var]))), main = "Before Normalization - Principal Component Analysis")
      plotMDS(t(num_column_df), labels= NULL,pch= 15, col = as.character(as.numeric(as.factor(char_columns[,input$preproc_class_var]))), main = "After Normalization - Principal Component Analysis")
    }
    
  })
  
  
  
  limma_data_input <- reactive({
    req(input$limma_csv_input)
    fread(input$limma_csv_input$datapath)
  })
  
  observeEvent(input$class_var, {
    if (input$class_var != "Not Selected") {
      if (!input$use_sample_data) {
        req(input$limma_csv_input)
        data_df = read.csv(input$limma_csv_input$datapath,
                           head = TRUE,
                           sep = ",")
      } else {
        data_df = read.csv("sample_data.csv", row.names = 1)
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
        updateSelectInput(session,
                          inputId = "class_of_interest_var",
                          choices = unique(data_df[input$class_var]))
        updateSelectInput(session,
                          inputId = "contrast_var",
                          choices = unique(data_df[input$class_var]))
      }
      
    }
  })
  
  output$print_class_var = renderText({
    if (input$class_var != "Not Selected") {
      if (!input$use_sample_data) {
        req(input$limma_csv_input)
        data_df = read.csv(input$limma_csv_input$datapath,
                           head = TRUE,
                           sep = ",")
      } else {
        data_df = read.csv("sample_data.csv", row.names = 1)
      }
      
      n = length(table(data_df[input$class_var]))
      count = 0
      for(i in (1: n)){
        if(table(data_df[input$class_var])[i]>1){
          count = count+1
        }
      }
      validate(
        need(count!=n,"")
      )
      shinyjs::disable('class_of_interest_var')
      shinyjs::disable('contrast_var')
      return("Chosen variable is not applicable for limma analysis")
    }
  })
  
  output$print_auto_class_var = renderText({
    if (input$auto_class_var != "Not Selected") {
      if (!input$auto_use_sample_data) {
        req(input$auto_limma_csv_input)
        data_df = read.csv(input$auto_limma_csv_input$datapath,
                           head = TRUE,
                           sep = ",")
      } else {
        data_df = read.csv("sample_data.csv", row.names = 1)
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
      disable("run_auto_limma_button")
      return("Chosen variable is not applicable for automated limma analysis")
    }
  })
  
  class_var = eventReactive(input$run_limma_button, input$class_var)
  contrast_var = eventReactive(input$run_limma_button, input$contrast_var)
  lfc_var = eventReactive(input$run_limma_button, input$lfc_var)
  pvalue_var = eventReactive(input$run_limma_button, input$pvalue_var)
  contrast_other_classes_var = eventReactive(input$run_limma_button, input$contrast_other_classes_var)
  class_of_interest_var = eventReactive(input$run_limma_button, input$class_of_interest_var)
  padjust_method_var = eventReactive(input$run_limma_button, input$padjust_method_var)
  remove_cols_var = eventReactive(input$run_limma_button,input$remove_cols_var)
  
  sort_results_by_var = eventReactive(input$run_limma_button, input$sort_results_by_var)
  
  #req(input$limma_csv_input)
  #data_df = read.csv(input$limma_csv_input$datapath, head=TRUE, sep=",")
  
  observe(
    {
      if(input$class_var!=not_sel&&input$contrast_var!=not_sel&&input$class_of_interest_var!=not_sel)
      {
        enable("run_limma_button")
      }
      else
      {
        disable("run_limma_button")
      }
    })
  
  get_dataset = function() {
    if (!input$use_sample_data) {
      req(input$limma_csv_input)
      data_df = read.csv(
        input$limma_csv_input$datapath,
        head = TRUE,
        sep = ",",
        row.names = 1
      )
      
      if (!is.null(input$remove_cols_var)) {
        data_df = data_df[,-which(names(data_df) %in% input$remove_cols_var)]
      }
      # if (input$show_transpose_var) {
      #   data_df = as.data.frame(t(data_df))
      # }
    } else{
      data_df = read.csv("sample_data.csv", row.names = 1)
    }
    
    return(data_df)
  }
  
  get_auto_dataset = function() {
    if (!input$auto_use_sample_data) {
      req(input$auto_limma_csv_input)
      data_df = read.csv(input$auto_limma_csv_input$datapath,
                         head = TRUE,
                         sep = ",", row.names = 1)
    } else{
      data_df = read.csv("sample_data2.csv", row.names = 1)
    }
    
    return(data_df)
  }
  
  perform_limma_1 <- function(class_var,
                              contrast_var,
                              lfc_var,
                              pvalue_var,
                              contrast_other_classes_var,
                              class_of_interest_var,
                              padjust_method_var = "Benjamini-Hochberg") {
    # req(input$limma_csv_input)
    # data_df = read.csv(input$limma_csv_input$datapath, head=TRUE, sep=",")
    if (!is.null(variables$functional_dataset)) {
      data_df = variables$functional_dataset
    } else {
      data_df = get_dataset()
    }
    
    #data_df[,input$class_var] = as.factor(data_df[,input$class_var])
    data_df[, input$class_var] = gsub(" ", "", data_df[, input$class_var])
    classes = unique(data_df[input$class_var])
    classes = classes[[input$class_var]]
    
    class_of_interest_var = gsub(" ", "", class_of_interest_var)
    contrast_var = gsub(" ", "", contrast_var)
    
    source('multiclass_limma.R')
    if (contrast_other_classes_var &
        class_of_interest_var == contrast_var) {
      # contrast_var_tmp =  classes[!classes %in% contrast_var][1]
      # contrast_string = paste(class_of_interest_var, paste("-", contrast_var_tmp))
      # contrast_var = classes[!classes %in% contrast_var][1]
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
    
    # print(results)
    data.table(results)
    
    #datatable(results, rownames = FALSE, options = list(rowsGroup = list(0)))
    
  }
  
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
    normalized_dataset = NULL
  )
  
  limma_table_1 <- eventReactive(input$run_limma_button, {
    start <- Sys.time()
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
    # dtt = datatable(data_table, rownames = FALSE, options = list(rowsGroup = list(0)))
    dtt = datatable(
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
  
  output$download = downloadHandler(
    filename = "limma_filtered_protein_matrix.csv",
    content = function(file) {
      filtered = variables$data_table[input$limma_table_rows_current, 'Gene']
      df = get_dataset()
      char_column_df = df %>% select_if(is.character)
      df = df[, c(names(char_column_df), filtered$Gene)]
      write.csv(df, file)
    }
  )
  
  # limma_table_1 <- eventReactive(input$sortLimmaResults,{
  #   if(!is.null(variables$data_table)){
  #     df = variables$data_table
  #     sort_var = sort_results_by_var()
  #     df = df[order(sort_var),]
  #     return(df)
  #   }
  # })
  
  #Consensus Clustering
  cluster_csv_input <- reactive({
    req(input$cluster_csv_input)
    fread(input$cluster_csv_input$datapath)
  })
  
  observeEvent(cluster_csv_input(), {

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
        "consensus_data.csv",
        head = TRUE,
        sep = ",",
        row.names = 1
      )
    }
    return(data_df)
  }
  
  observeEvent(input$cluster_use_sample_data, {
    if (input$cluster_use_sample_data) {
      reset(id = "cluster_csv_input")
      show_template3(TRUE)
    }
  })
  
  cluster_k_var = eventReactive(input$run_consclustering_button, input$cluster_k_var)
  cluster_alg_var = eventReactive(input$run_consclustering_button, input$cluster_alg_var)
  cluster_distance_var = eventReactive(input$run_consclustering_button,
                                       input$cluster_distance_var)
  cluster_inner_linkage_var = eventReactive(input$run_consclustering_button,
                                            input$cluster_inner_linkage_var)
  cluster_final_linkage_var = eventReactive(input$run_consclustering_button,
                                            input$cluster_final_linkage_var)
  
  # cons_clust <- eventReactive(input$run_limma_button,{
  #   source('consensus_clustering.R')
  #   perform_consensus_clustering(get_cluster_dataset(), dataname,k=cluster_k_var,clusterAlg=cluster_alg_var,
  #                                distance=cluster_distance_var,
  #     innerLinkage=cluster_inner_linkage_var,finalLinkage=cluster_final_linkage_var)
  #   return()
  # })
  
  output$cluster_slider = renderUI({
    sliderInput(
      "cluster_k_var",
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
  
  observe(
    {
      if(xor(input$cluster_use_sample_data,!is.null(input$cluster_csv_input$datapath)))
      {
        enable("run_consclustering_button")
      }
      else
      {
        disable("run_consclustering_button")
      }
    })
  
  observeEvent(input$run_consclustering_button, {
    show_template3(FALSE)
    start <- Sys.time()
    print('Started...')
    source('consensus_clustering.R')
    data = get_cluster_dataset()
    data[is.na(data)] = 0
    ##print(input$cluster_csv_input$name)
    clust_alg = cluster_alg_var()
    ca = c("hc", "pam", "km")
    names(ca) = c(
      "Hierachical Clustering",
      "Partition Around Medoids Clustering",
      "K-means Clustering"
    )
    
    path = perform_consensus_clustering_png(
      as.matrix(data),
      gsub("\\..*", "", input$cluster_csv_input$name),
      k = cluster_k_var(),
      clusterAlg = ca[[clust_alg]],
      distance = cluster_distance_var(),
      innerLinkage = cluster_inner_linkage_var(),
      finalLinkage = cluster_final_linkage_var()
    )
    # output$cluster_path = path
    # output$cluster_path = renderText({
    #   paste("Output files are in", path, sep = " ")
    # })
    variables$cluster_path = path
    #print(path)
    
    output$cluster_images = renderUI({
      all_images = list.files(variables$cluster_path,
                              pattern = "*png",
                              full.names = TRUE)
      withSpinner(tagList(
        downloadButton("clusterDownload", label = "Download Results"),
        br(),
        br(),
        # withSpinner(
        #   verbatimTextOutput("cluster_path"),
        #   color = "#FF4500",
        #   type = 4
        # ),
        renderImage(list(
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
                                          
  output$clusterDownload = downloadHandler(
    filename = function() {
      paste0("ConsensusClusterResults_", Sys.Date(), ".zip")
    },
    content = function(file) {
      files = list.files(variables$cluster_path, recursive = TRUE)
      files = paste(variables$cluster_path, files, sep = "/")
      return (zip(file ,files ,mode="cherry-pick"))
    },
    contentType = "application/zip"
  )
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
  
  limma_boxplots_1 = eventReactive(input$run_limma_button, {
    validate(need(!is.null(variables$data_table) && nrow(variables$data_table) != 0, 'No records to display'))
    bp = NULL
    if (!is.null(variables$data_table) && (nrow(variables$data_table) != 0)) {
      start <- Sys.time()
      top_genes_df = head(variables$data_table, 50)
      top_genes = as.vector(top_genes_df$Gene)
      bp = generate_box_plots(top_genes)
      print(Sys.time() - start)
    } else {
      img <- rasterGrob(png::readPNG("norecords.png"))
      bp = grid.arrange(ggplot(), img, heights = c(1, 1))
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
  
  observeEvent(input$plotUpregulated, {
    if (xor(input$plotUpregulated, input$plotDownregulated)) {
      bp = filteredBoxplots()
      output$limma_boxplot = renderPlot(bp$plots, height = 100 * bp$n_genes)
    } else {
      output$limma_boxplot = renderPlot(limma_boxplots_1(), height = 100 * min(nrow(variables$data_table), 50))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$plotDownregulated, {
    if (xor(input$plotDownregulated, input$plotUpregulated)) {
      bp = filteredBoxplots()
      output$limma_boxplot = renderPlot(bp$plots, height = 100 * bp$n_genes)
    } else {
      # output$limma_boxplot = renderPlot(limma_boxplots_1(), height= 5000)
      output$limma_boxplot = renderPlot(limma_boxplots_1(), height = 100 *
                                          min(nrow(variables$data_table), 50))
      
    }
  }, ignoreInit = TRUE)
  
  #output$limma_table = renderTable(limma_table_1())
  output$limma_table = renderDT(server = FALSE, {
    data <- limma_table_1()
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
  
  # output$limma_boxplot = renderPlot(limma_boxplots_1(), height= 5000)
  output$limma_boxplot = renderPlot({
    validate(need(!is.null(variables$data_table), 'No records to display'))
    limma_boxplots_1()
    }, height = boxplotHeight)
  
  output$limma_volcanoplot = renderPlotly({
    validate(need(nrow(variables$data_table) != 0, 'No records to display'))
    
    #print(sum(is.na(variables$data_table)))
    #variables$data_table = variables$data_table[!is.na(variables$data_table)]
    #print(sum(is.na(variables$data_table)))
    #start <- Sys.time()
    results6 = variables$data_table
    results6$FC = 2 ^ results6$logFC
    results6$P = 2 ^ results6$P.Value
    
    effectsize = if (!is.na(input$lfc_var))
      c(-1 * as.numeric(input$lfc_var), as.numeric(input$lfc_var))
    else
      c(-0.5, 0.5)
    effectivep = as.numeric(input$pvalue_var)
    #p = "P.Value",effect_size = "logFC"
    results6_vol = volcanor(
      results6,
      p = "P.Value",
      effect_size = "logFC",
      annotation1 = "FC",
      annotation2 = "P",
      snp = "Gene"
    )
    volcanoly(
      results6_vol,
      col = c("#808080"),
      genomewideline = -log10(effectivep),
      highlight_color = "red",
      effect_size_line = effectsize
    ) %>% layout(hoverlabel = list(bgcolor = "white"))
    #print( Sys.time() - start )
    
  })
  
  
  output$volcano_cutoffs = renderUI({
    fit2C = variables$fit2C
    fluidRow(column(
      width = 4,
      sliderInput(
        "pvalue_cutoff",
        "p-value cutoff (-log10):",
        min = if (!is.null(fit2C)) - round(log10(max(fit2C$p.value, na.rm = T)),2) else 0,
        max = if (!is.null(fit2C)) - round(log10(min(fit2C$p.value, na.rm = T)),2) else 1,
        value = if (!is.null(fit2C)) ((round(min(abs(fit2C$p.value), na.rm = T), 2) + round(min(abs(fit2C$p.value), na.rm = T), 2))/2) else 0.5
      )
    ),
    column(
      width = 4,
      sliderInput(
        "lfc_cutoff",
        "log fold change cutoff:",
        min = if (!is.null(fit2C))
          round(min(abs(fit2C$coef), na.rm = T), 2)
        else
          0 ,
        max = if (!is.null(fit2C))
          round(max(abs(fit2C$coef), na.rm = T), 2)
        else
          1,
        value = if (!is.null(fit2C)) ((round(min(abs(fit2C$coef), na.rm = T), 2) + round(min(abs(fit2C$coef), na.rm = T), 2))/2) else 0.5
      )
    ))
  })
  
  output$func_annot_analysis = renderUI({
    results_df = variables$results
    
    if(!is.na(input$lfc_var) && is.na(input$pvalue_var) ){
      results_df = results_df[abs(results_df$logFC) >= input$lfc_var,]
    }else if(is.na(input$lfc_var) && !is.na(input$pvalue_var) ){
      results_df = results_df[results_df$adj.P.Val <= input$pvalue_var,]
    }else if(!is.na(input$lfc_var) && !is.na(input$pvalue_var) ){
      results_df = results_df[(abs(results_df$logFC) >= input$lfc_var)&&
                                (results_df$adj.P.Val <= input$pvalue_var),]
    }else {
      results_df = results_df[results_df$adj.P.Val <0.05,]
    }
    
    
    
    url = paste("https://david.ncifcrf.gov/api.jsp?type=UNIPROT_ACCESSION&ids=",
                paste(rownames(results_df), collapse=","),"&tool=summary")
    
    # clipr::write_clip(rownames(results_df), allow_non_interactive = TRUE)  
    
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
  
  output$limma_ant_volcanoplot = renderPlot(res = 96, {
    validate(need(nrow(variables$data_table) != 0, 'No records to display'))
    source('multiclass_limma.R')
    fit2C = variables$fit2C
    
    data = get_dataset()
    # rownames(data) = data[,1]
    # data = data[,-1]
    char_columns = colnames(data %>% select_if(is.character))
    data <- t(data)
    results_df <- data.frame (
      log2FoldChange  = c(fit2C$coef),
      pvalue = c(fit2C$p.value),
      delabel = rownames(data)[!rownames(data) %in% c(char_columns)]
    )
    rownames(results_df) <-
      rownames(data)[!rownames(data) %in% c(char_columns)]
    results_df$diffexpressed <- "NO"
    # if log2Foldchange > 0.6 and pvalue < 0.05, set as "UP"
    results_df$diffexpressed[results_df$log2FoldChange > input$lfc_cutoff &
                               results_df$pvalue < 10 ** (-1 * input$pvalue_cutoff)] <- "UP"
    # if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"
    results_df$diffexpressed[results_df$log2FoldChange < (-1 * input$lfc_cutoff) &
                               results_df$pvalue < 10 ** (-1 * input$pvalue_cutoff)] <- "DOWN"
    
    EnhancedVolcano(
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
  
  output$limma_heatmap = renderPlot(res = 96, {
    data_df = get_dataset()
    #data_df[,input$class_var] = as.factor(data_df[,input$class_var])
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
    
    heatmap.2(
      as.matrix(t(data_df_int)),
      col = brewer.pal(11, "RdBu"),
      scale = "row",
      trace = "none",
      margins = c(8, 16),
      ColSideColors = as.character(as.numeric(data_df[, input$class_var]))
    )
    legend(
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
    }
  }
  
  output$limma_pca = renderPlot(res = 96, {
    validate(need(nrow(variables$data_table) != 0, 'No records to display'))
    
    data_df = get_dataset()
    #data_df[,input$class_var] = as.factor(data_df[,input$class_var])
    data_df[, input$class_var] = gsub(" ", "", data_df[, input$class_var])
    
    classes = unique(data_df[input$class_var])
    
    charColumns = data_df %>% select_if(is.character)
    data_df_int = data_df[,-which(names(data_df) %in%  colnames(charColumns))]
    data_df[, input$class_var] = as.factor(data_df[, input$class_var])
    
    variables$functional_dataset = data_df
    
    markers = get_markers(input$limma_pca_protein_group_var)
    
    output$limma_pca_protein_group_num = renderText(
      paste(
        "Number of proteins identified as",
        input$limma_pca_protein_group_var,
        nrow(markers),
        sep = " "
      )
    )
    
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
    
    print(dim(data_df_int))
    print(length(data_df[, input$class_var]))
    
    pca_type = NULL
    pca_frame = F
    #convex, t, norm or euclid
    
    if (input$limma_pca_frame == 1) {
      pca_type = "norm"
      pca_frame = T
    } else if (input$limma_pca_frame == 2) {
      pca_type = NULL
      pca_frame = T
    } else {
      pca_frame = F
    }
    
    validate(need(
      nrow(data_df_int) > 1 &&
        ncol(data_df_int) > 1,
      'No records to display'
    ))
    autoplot(
      prcomp((data_df_int), center = TRUE),
      data_df,
      colour = input$class_var,
      label = input$limma_pca_label,
      frame = pca_frame,
      frame.type = pca_type,
      main = paste("PCA of", input$limma_pca_protein_group_var, sep =
                     " ")
    )
    
  })
  
  #functional subset limma
  # limma_table_1 <- eventReactive(input$limma_perform_sub_button,{
  #   data_table = perform_limma_1(class_var(), input$contrast_var, input$lfc_var,
  #                                input$pvalue_var, input$contrast_other_classes_var,
  #                                class_of_interest_var(),
  #                                padjust_method_var())
  #   data_table = data_table[complete.cases(data_table), ]
  #   variables$data_table = data_table
  #   dtt = datatable(data_table, rownames = FALSE, options = list(rowsGroup = list(0)))
  #   return(dtt)
  # })
  
  output$downloadResults = downloadHandler(
    filename = "limma_results.csv",
    content = function(file) {
      write.csv(variables$data_table, file, row.names = FALSE)
    }
  )
  
  output$downloadPlots <- downloadHandler(
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
  
  #Auto limma
  auto_limma_data_input <- reactive({
    req(input$auto_limma_csv_input)
    fread(input$auto_limma_csv_input$datapath)
  })
  
  observeEvent(auto_limma_data_input(), {
    show_template2(FALSE)
    char_column_df = data.frame(auto_limma_data_input(), row.names=1) %>%
      select_if(is.character)
    choices <- c(not_sel, names(char_column_df))
    updateSelectInput(inputId = "auto_class_var", choices = choices)
  })
  
  observeEvent(input$auto_use_sample_data, {
    reset(id = "auto_limma_csv_input")
    reset(id = "contents2")
    show_template2(TRUE)
    
    data_df = read.csv("sample_data2.csv", row.names = 1)
    char_column_df = data_df %>%
      select_if(is.character)
    choices <- c(not_sel, names(char_column_df))
    updateSelectInput(inputId = "auto_class_var", choices = choices)
  })
  
  auto_class_var = eventReactive(input$run_auto_limma_button, input$auto_class_var)
  auto_lfc_var = eventReactive(input$run_auto_limma_button, input$auto_lfc_var)
  auto_pvalue_var = eventReactive(input$run_auto_limma_button, input$auto_pvalue_var)
  auto_padjust_method_var = eventReactive(input$run_auto_limma_button,
                                          input$auto_padjust_method_var)
  
  observe(
    {
      if(input$auto_class_var!=not_sel)
      {
        enable("run_auto_limma_button")
      }
      else
      {
        disable("run_auto_limma_button")
      }
    })
  
  output$auto_limma_table = renderDT(limma_table2())
  
  perform_limma_2 <-
    function(data_df,
             class_var,
             lfc_var,
             pvalue_var,
             class_group1,
             class_group2,
             method = "none",
             fdr = "Benjamini-Hochberg") {
      #data_df = get_auto_dataset()
      
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
      
      source('multiclass_limma.R')
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
      ##print(results)
      data.table(results)
      
    }
  
  limma_table2 = eventReactive(input$run_auto_limma_button, {
    if (input$auto_use_sample_data) {
      show_template2(FALSE)
    }
    start <- Sys.time()
    ##print(input$auto_pvalue_var)
    #dd=c('class1','class2','class3','class4')
    data_df = get_auto_dataset()
    dd = unique(data_df[, auto_class_var()])
    ##print(dd)
    res = rapply(listParts(length(dd)), function(v) dd[v], how = "replace")
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
    
    #Pairwise comparisons
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
      # #print(group1_string)
      # #print(group2_string)
      
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

    output$auto_limma_comb_list = renderText({
      return(paste(comb_list, collapse = "\n"))
    })
    
    auto_limma_table = auto_limma_table %>% dplyr::select(Combination, everything())
    
    #merge cells of column 1)) %>% formatRound(
    dtable <- datatable(auto_limma_table, rownames = FALSE, options = list(rowsGroup = list(0)))
    dtable = dtable %>% formatRound(columns = which(sapply(auto_limma_table, is.numeric)), digits = 4)
    # folder containing dataTables.rowsGroup.js                   
    path <- getwd() 
    dep <- htmltools::htmlDependency("RowsGroup", "2.0.0",
                                     path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    #dtable
    variables$auto_data_table = auto_limma_table %>%  mutate_if(is.numeric, round, digits=4)
    print('auto limma time')
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
    
    output$downloadAutoTable = downloadHandler(
      filename = "limma_auto_results_top50.xlsx",
      # content = function(file) {
      #   write.csv(variables$auto_data_table, file, row.names = FALSE)
      # }
      content = function(file) {
        df = variables$auto_data_table
        ##print(df)
        ##print(colnames(df))
        combs_names = unique(df$Combination)
        # comb_index_names = LETTERS[1:length(combs_names)]
        comb_index_names = generate_letter_combinations(length(combs_names))
        combs_index = data.frame(index = comb_index_names, combination = combs_names)
        
        OUT = createWorkbook()
        
        #Write index
        addWorksheet(OUT, "Index")
        writeData(OUT, sheet = "Index", x = combs_index)
        i = 1
        for (c in comb_index_names) {
          #print(c)
          length(c)
          addWorksheet(OUT, c)
          writeData(OUT, sheet = c, x = df[df$Combination == combs_names[i], ])
          i = i + 1
        }
        #write.xlsx(list_of_datasets, file, rowNames=FALSE)
        #write.csv(variables$auto_data_table, file, row.names = FALSE)
        saveWorkbook(OUT, file)
      }
    )
    
    output$downloadAutoTableAll = downloadHandler(
      filename = "limma_auto_results_all.xlsx",
      content = function(file) {
        show_modal_spinner(text = HTML("The results are being generated.<br> Please wait..."))
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
        
        #Pairwise comparisons
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
        # comb_index_names = LETTERS[1:length(combs_names)]
        comb_index_names = generate_letter_combinations(length(combs_names))
        combs_index = data.frame(index = comb_index_names, combination = combs_names)
        
        OUT = createWorkbook()
        
        #Write index
        addWorksheet(OUT, "Index")
        writeData(OUT, sheet = "Index", x = combs_index)
        i = 1
        for (c in comb_index_names) {
          #print(c)
          length(c)
          addWorksheet(OUT, c)
          writeData(OUT, sheet = c, x = df[df$Combination == combs_names[i], ])
          i = i + 1
        }
        #write.xlsx(list_of_datasets, file, rowNames=FALSE)
        #write.csv(variables$auto_data_table, file, row.names = FALSE)
        saveWorkbook(OUT, file)
      }
    )
    
    #Pipeline
    pipeline_data_input <- reactive({
      req(input$pipeline_data_csv_input)
      fread(input$pipeline_data_csv_input$datapath)
    })
    
    pipeline_ant_input <- reactive({
      req(input$pipeline_ant_csv_input)
      fread(input$pipeline_ant_csv_input$datapath)
    })
    
    observeEvent(pipeline_ant_input(), {
      choices <- c(not_sel, sub(" ", "_", names(pipeline_ant_input())))
      updateSelectInput(inputId = "pipeline_batch_var", choices = choices)
    })
    
    observeEvent(pipeline_ant_input(), {
      choices <- c(not_sel, sub(" ", "_", names(pipeline_ant_input())))
      updateSelectInput(inputId = "pipeline_biospecimen_id_col_var", choices = choices)
    })
    
    observeEvent(pipeline_ant_input(), {
      choices <- c(not_sel, sub(" ", "_", names(pipeline_ant_input())))
      updateSelectInput(inputId = "pipeline_sample_name_col_var", choices = choices)
    })
    
    output$pipeline_data <- renderTable({
      if (!is.null(input$pipeline_data_csv_input)) {
        req(input$pipeline_data_csv_input)
        df = read.csv(
          input$pipeline_data_csv_input$datapath,
          head = TRUE,
          sep = ",",
          check.names = FALSE
        )
        return(head(df))
      } else {
        return(head(
          read.csv("pipeline_data.csv", fileEncoding = "UTF-8-BOM")
        ))
      }
    })
    
    output$pipeline_ant <- renderTable({
      if (!is.null(input$pipeline_ant_csv_input)) {
        req(input$pipeline_ant_csv_input)
        df = read.csv(
          input$pipeline_ant_csv_input$datapath,
          head = TRUE,
          sep = ",",
          check.names = FALSE,
          fileEncoding = "UTF-8-BOM"
        )
        return(head(df))
      } else {
        return(head(
          read.csv("pipeline_sample_annotation.csv", fileEncoding = "UTF-8-BOM")
        ))
      }
    })
    
    observeEvent(pipeline_ant_input(), {
      choices <- c(not_sel, sub(" ", "_", names(pipeline_ant_input())))
      updateSelectInput(inputId = "pipeline_technical_factors_var", choices = choices)
    })
    
    observeEvent(pipeline_ant_input(), {
      choices <- c(not_sel, sub(" ", "_", names(pipeline_ant_input())))
      updateSelectInput(inputId = "pipeline_biological_factors_var", choices = choices)
    })
    
    pipeline_biospecimen_id_col_var = eventReactive(input$run_pipeline_button,
                                                    input$pipeline_biospecimen_id_col_var)
    pipeline_batch_var = eventReactive(input$run_pipeline_button, input$pipeline_batch_var)
    pipeline_biological_factors_var = eventReactive(input$run_pipeline_button,
                                                    input$pipeline_biological_factors_var)
    pipeline_technical_factors_var = eventReactive(input$run_pipeline_button,
                                                   input$pipeline_technical_factors_var)
    pipeline_sample_name_col_var = eventReactive(input$run_pipeline_button,
                                                 input$pipeline_sample_name_col_var)
    
    get_pipeline_dataset = function() {
      req(input$pipeline_data_csv_input)
      data_df = read.csv(
        input$pipeline_data_csv_input$datapath,
        head = TRUE,
        sep = ",",
        check.names = FALSE
      )
      variables$pipeline_data = data_df
      return(data_df)
    }
    
    get_annotation_dataset = function() {
      req(input$pipeline_ant_csv_input)
      data_df = read.csv(
        input$pipeline_ant_csv_input$datapath,
        head = TRUE,
        sep = ",",
        check.names = FALSE,
        fileEncoding = "UTF-8-BOM"
      )
      colnames(data_df) = sub(" ", "_", colnames(data_df))
      data_df$ID = seq(1, nrow(data_df))
      if (is.null(variables$pipeline_ant)) {
        variables$pipeline_ant = data_df
      }
      return(data_df)
    }
    
    output$pipeline_missing_protein = renderPlot({
      df = get_pipeline_dataset()
      return(
        hist(rowSums(is.na(df)), xlab = "Missing count",
             main = "Protein-wise Missing value Distribution")
      )
    })
    
    output$pipeline_missing_sample = renderPlot({
      df = get_pipeline_dataset()
      return(
        hist(colSums(is.na(df)), xlab = "Missing count",
             main = "Sample-wise Missing value Distribution")
      )
    })
    
    output$pipeline_total_protein = renderText({
      df = get_pipeline_dataset()
      paste(nrow(df), ' proteins total')
    })
    
    output$pipeline_total_sample = renderText({
      df = get_annotation_dataset()
      paste(nrow(df), ' samples total')
    })
    
    create_protein_missing_table <- function() {
      df = get_pipeline_dataset()
      col = rowSums(is.na(df))
      if (length(col) > 5000)
        col_norm = sample(col, 5000)
      else
        col_norm = col
      norm_test = shapiro.test(col_norm)
      statistic = c("min",
                    "max",
                    "mean",
                    "median",
                    "5th percentile",
                    "95th percentile")
      value = c(
        round(min(col), 2),
        round(max(col), 2),
        round(mean(col), 2),
        round(median(col), 2),
        round(quantile(col, 0.05), 2),
        round(quantile(col, 0.95), 2)
      )
      data.table(statistic, value)
    }
    
    output$pipeline_missing_sample_summary_table = renderTable(create_sample_missing_table())
    
    create_sample_missing_table <- function() {
      df = get_pipeline_dataset()
      col = colSums(is.na(df))
      if (length(col) > 5000)
        col_norm = sample(col, 5000)
      else
        col_norm = col
      norm_test = shapiro.test(col_norm)
      statistic = c("min",
                    "max",
                    "mean",
                    "median",
                    "5th percentile",
                    "95th percentile")
      value = c(
        round(min(col), 2),
        round(max(col), 2),
        round(mean(col), 2),
        round(median(col), 2),
        round(quantile(col, 0.05), 2),
        round(quantile(col, 0.95), 2)
      )
      data.table(statistic, value)
    }
    
    output$pipeline_missing_protein_summary_table = renderTable(create_protein_missing_table())
    
    output$pipeline_retain_protein = renderText({
      df = get_pipeline_dataset()
      mod_df = df[(rowSums(is.na(df)) / ncol(df)) <= (input$protein_slider /
                                                        100) , ]
      return(paste(nrow(mod_df), ' proteins'))
    })
    
    output$pipeline_retain_sample = renderText({
      df = get_pipeline_dataset()
      mod_df = df[, (colSums(is.na(df)) / nrow(df)) <= (input$sample_slider /
                                                          100)]
      samples = ncol(mod_df) - 1
      if (length(samples) == 0) {
        samples = 0
      }
      return(paste(samples, ' samples'))
    })
    
    observeEvent(input$set_missing_valued_data_button, {
      df = get_pipeline_dataset()
      mod_df = df[(rowSums(is.na(df)) / ncol(df)) <= (input$protein_slider /
                                                        100) , ]
      rownames(mod_df) = mod_df$Index
      mod_df = subset(mod_df, select = -c(Index))
      variables$pipeline_data = mod_df
    })
    
    output$pipeline_protein_mean_plot = renderPlot({
      df = variables$pipeline_data
      ant = variables$pipeline_ant
      # ant[,pipeline_batch_var()] = as.factor(ant[,pipeline_batch_var()])
      ant[, pipeline_technical_factors_var()[1]] = as.factor(ant[, pipeline_technical_factors_var()[1]])
      #print(unique(ant[,pipeline_batch_var()]))
      color_list = sample_annotation_to_colors(
        ant,
        factor_columns = c(
          pipeline_biological_factors_var(),
          pipeline_technical_factors_var()
        ),
        numeric_columns = c('ID')
      )
      
      df2 = as.matrix(df)
      variables$pipeline_data_matrix = df2
      variables$pipeline_ant = ant
      variables$color_list = color_list
      
      return(
        plot_sample_mean(
          df2,
          ant,
          order_col = 'ID',
          batch_col = pipeline_technical_factors_var()[1],
          color_by_batch = TRUE,
          ylimits = c(23, 26),
          color_scheme = color_list[[pipeline_technical_factors_var()[1]]]
        )
      )
    })
    
    output$pipeline_protein_mean_box_plot = renderPlot({
      df = variables$pipeline_data_matrix
      ant = variables$pipeline_ant
      color_list = variables$color_list
      long_data =  matrix_to_long(df)
      variables$pipeline_long_data = long_data
      
      return(
        plot_boxplot(
          long_data,
          ant,
          batch_col = pipeline_technical_factors_var()[1],
          color_scheme = color_list[[pipeline_technical_factors_var()[1]]]
        )
      )
    })
    
    output$pipeline_protein_mean_pca_plot = renderPlot(
      create_pca_plots(
        as.matrix(variables$pipeline_data),
        variables$pipeline_ant,
        c(
          pipeline_biological_factors_var(),
          pipeline_technical_factors_var()
        ),
        variables$color_list
      ),
      height = 1000
    )
    
    #Normalization
    
    pipeline_norm_method = eventReactive(input$perform_normalization_data_button,
                                         input$pipeline_norm_method)
    
    perform_normalization = eventReactive(input$perform_normalization_data_button, {
      df = variables$pipeline_data_matrix
      norm_df = NULL
      if (1 %in% pipeline_norm_method() &&
          2 %in% pipeline_norm_method()) {
        norm_df = normalize_data_dm(df, normalize_func = 'medianCentering')
        norm_df = normalize_data_dm(norm_df, normalize_func = 'quantile')
      } else if (1 %in% pipeline_norm_method()) {
        norm_df = normalize_data_dm(df, normalize_func = 'medianCentering')
      } else {
        norm_df = normalize_data_dm(df, normalize_func = 'quantile')
      }
      
      variables$pipeline_data_norm = norm_df
      return(
        plot_sample_mean(
          norm_df,
          variables$pipeline_ant,
          order_col = 'ID',
          batch_col = pipeline_technical_factors_var()[1],
          color_by_batch = TRUE,
          ylimits = c(19, 36),
          color_scheme = variables$color_list[[pipeline_technical_factors_var()[1]]]
        ) +
          ggtitle("Normalization")
      )
    })
    
    output$pipeline_protein_norm_mean_plot = renderPlot(perform_normalization())
    
    perform_normalization_boxplot = eventReactive(input$perform_normalization_data_button, {
      long_mat = matrix_to_long(variables$pipeline_data_norm)
      variables$pipeline_data_norm_long = long_mat
      
      return(
        plot_boxplot(
          long_mat,
          variables$pipeline_ant,
          batch_col = pipeline_technical_factors_var()[1],
          color_scheme = variables$color_list[[pipeline_technical_factors_var()[1]]]
        ) +
          ggtitle("Normalization")
      )
    })
    output$pipeline_protein_norm_mean_box_plot = renderPlot(perform_normalization_boxplot())
    
    output$pipeline_protein_norm_mean_pca_plot = renderPlot(
      create_pca_plots(
        variables$pipeline_data_norm,
        variables$pipeline_ant,
        c(
          pipeline_biological_factors_var(),
          pipeline_technical_factors_var()
        ),
        variables$color_list
      ),
      height = 1000
    )
    
    output$downloadNormTable = downloadHandler(
      filename = "normalized_results.csv",
      content = function(file) {
        write.csv(variables$pipeline_data_norm, file, row.names = FALSE)
      }
    )
    
    # Imputation
    
    pipeline_impu_method = eventReactive(input$perform_imputation_data_button,
                                         input$pipeline_impu_method)
    
    perform_imputation = eventReactive(input$perform_imputation_data_button, {
      df = variables$pipeline_data_norm
      
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
        method = pipeline_impu_method(),
        out = "Ensemble"
      )
      imputed_df = as.data.frame(imputed_df$Ensemble)
      
      variables$pipeline_data_imputed = imputed_df
      
      return(
        create_mean_plot(
          imputed_df,
          variables$pipeline_ant,
          pipeline_technical_factors_var()[1],
          variables$color_list,
          "Imputation"
        )
      )
    })
    
    output$pipeline_protein_impu_plot = renderPlot(perform_imputation())
    
    
    #create_boxplot(df, ant, batch,color_list, title)
    perform_boxplot_imputation = eventReactive(input$perform_imputation_data_button,
                                               {
                                                 return(
                                                   create_boxplot(
                                                     variables$pipeline_data_imputed,
                                                     variables$pipeline_ant,
                                                     pipeline_technical_factors_var()[1],
                                                     variables$color_list,
                                                     "Imputation"
                                                   )
                                                 )
                                               })
    
    output$pipeline_protein_impu_box_plot = renderPlot(perform_boxplot_imputation())
    
    perform_pca_imputation = eventReactive(input$perform_imputation_data_button,
                                           {
                                             return(
                                               create_pca_plots(
                                                 variables$pipeline_data_imputed,
                                                 variables$pipeline_ant,
                                                 c(
                                                   pipeline_biological_factors_var(),
                                                   pipeline_technical_factors_var()
                                                 ),
                                                 variables$color_list
                                               )
                                             )
                                           })
    output$pipeline_protein_impu_pca_plot = renderPlot(perform_pca_imputation(), height = 1000)
    
    output$downloadImpuTable = downloadHandler(
      filename = "imputed_results.csv",
      content = function(file) {
        write.csv(variables$pipeline_data_imputed, file, row.names = FALSE)
      }
    )
    
    # Replicates
    
    output$pipeline_sample_replicates = renderText({
      rep_str = NULL
      if (is.null(rep_str)) {
        df = variables$pipeline_ant
        replicates = df[duplicated(df[, pipeline_biospecimen_id_col_var()]), ]
        variables$replicates = replicates$FullRunName
        rep_str = paste(replicates$FullRunName, collapse = "\n")
      } else{
        rep_str = paste(variables$replicates, collapse = "\n")
      }
      
      return(rep_str)
    })
    
    # observeEvent(input$perform_unique_replicates_button, {
    #   df = variables$pipeline_ant
    #   init_colnames = df[,pipeline_biospecimen_id_col_var()]
    #   init_colnames = make.unique(init_colnames)
    #   df[, pipeline_biospecimen_id_col_var()] = init_colnames
    #   variables$pipeline_ant = df
    # })
    
    output$pipeline_unique_replicates = eventReactive(input$perform_unique_replicates_button, {
      df = variables$pipeline_ant
      init_colnames = df[, pipeline_biospecimen_id_col_var()]
      init_colnames = make.unique(init_colnames)
      df[, pipeline_biospecimen_id_col_var()] = init_colnames
      
      ordered = df[order(df[, pipeline_biospecimen_id_col_var()]), ]
      # cols = colnames(variables$pipeline_data_imputed)
      cols = colnames(variables$pipeline_data)
      samples = cols[order(cols)]
      
      rownames(df) = df[, pipeline_biospecimen_id_col_var()]
      variables$pipeline_ant = df
      # variables$pipeline_data_imputed = variables$pipeline_data_imputed[,cols]
      variables$pipeline_data = variables$pipeline_data[, cols]
      
      reps = c(variables$replicates, init_colnames[str_detect(init_colnames, "\\.")])
      variables$replicated = reps
      rep_str = paste(reps, collapse = "\n")
      output$pipeline_sample_replicates = renderText({
        paste(variables$replicates,
              collapse = "\n")
      })
      return(rep_str)
    })
    
    output$downloadNonReplicatedTable = downloadHandler(
      filename = "non_replicated_protdata.csv",
      content = function(file) {
        write.csv(variables$pipeline_data, file, row.names = FALSE)
      }
    )
    
    output$downloadNonReplicatedAnnotation = downloadHandler(
      filename = "non_replicated_sampleannotation.csv",
      content = function(file) {
        write.csv(variables$pipeline_ant, file, row.names = FALSE)
      }
    )
    
    output$pipeline_protein_pcva_plot = renderPlot({
      if (!is.null(variables$replicates) &&
          input$perform_unique_replicates_button) {
        df_nrown = variables$pipeline_data_imputed
        row.names(df_nrown) = NULL
        ant_nrown = variables$pipeline_ant
        row.names(ant_nrown) = NULL
        
        pvca_res = prepare_PVCA_df(
          df_nrown,
          ant_nrown,
          technical_factors = pipeline_technical_factors_var(),
          biological_factors = pipeline_biological_factors_var()
        )
        lbs = pvca_res[pvca_res$weights > 0.2, ]$label
        lbs = lbs[lbs != 'resid']
        lbs = sapply(lbs, function(x) {
          gsub("*:.*", "", x)
        })
        lbs = as.vector(lbs)
        
        #pipeline_batch_vars = lbs
        updateSelectInput(inputId = "pipeline_batch_var", choices = lbs)
        
        # plot_PVCA(df_nrown, ant_nrown,
        #           technical_factors = pipeline_technical_factors_var(),
        #           biological_factors = pipeline_biological_factors_var())
        plot_PVCA.df(pvca_res = pvca_res)
      }
    })
    
    batch_corr_method_var = eventReactive(input$perform_batchcorr_data_button,
                                          input$batch_corr_method_var)
    
    perform_batchcorr = eventReactive(input$perform_batchcorr_data_button, {
      df = variables$pipeline_data_imputed
      df_long = matrix_to_long(df)
      
      color_list = sample_annotation_to_colors(
        variables$pipeline_ant,
        factor_columns = c(
          pipeline_batch_var(),
          pipeline_biological_factors_var(),
          pipeline_technical_factors_var()
        ),
        numeric_columns = c('ID')
      )
      
      variables$color_list = color_list
      
      athres = 5
      pcthres = 1.20
      bc = correct_batch_effects_df(
        df_long =  df_long,
        sample_annotation = variables$pipeline_ant,
        discrete_func = batch_corr_method_var(),
        batch_col = pipeline_batch_var(),
        abs_threshold = athres,
        pct_threshold = pcthres
      )
      bc_mat = long_to_matrix(bc)
      
      variables$pipeline_data_batchcorrected = bc_mat
      
      # output$pipeline_protein_batchcorr_pca_plot = renderPlot(create_pca_plots(bc_mat,
      #                                                               variables$pipeline_ant,
      #                                                               c(pipeline_biological_factors_var(),pipeline_technical_factors_var()),
      #                                                               variables$color_list))
      #
      # output$pipeline_protein_batchcorr_pcva_plot = renderPlot(plot_PVCA(bc_mat, variables$pipeline_ant,
      #                                                                    technical_factors = pipeline_technical_factors_var(),
      #                                                                    biological_factors = pipeline_biological_factors_var()))
      
      return(
        create_mean_plot(
          bc_mat,
          variables$pipeline_ant,
          pipeline_batch_var(),
          variables$color_list,
          "Batch Correction"
        )
      )
    })
    
    output$pipeline_protein_batchcorr_box_plot = renderPlot(perform_batchcorr())
    
    perform_batchcorr_pca = eventReactive(input$perform_batchcorr_data_button, {
      return(
        create_pca_plots(
          variables$pipeline_data_batchcorrected,
          variables$pipeline_ant,
          c(
            pipeline_biological_factors_var(),
            pipeline_technical_factors_var()
          ),
          variables$color_list
        )
      )
    })
    output$pipeline_protein_batchcorr_pca_plot = renderPlot(perform_batchcorr_pca())
    
    perform_batchcorr_pcva = eventReactive(input$perform_batchcorr_data_button, {
      df_nrown = variables$pipeline_data_batchcorrected
      row.names(df_nrown) = NULL
      ant_nrown = variables$pipeline_ant
      row.names(ant_nrown) = NULL
      return(
        plot_PVCA(
          df_nrown,
          ant_nrown,
          technical_factors = pipeline_technical_factors_var(),
          biological_factors = pipeline_biological_factors_var()
        )
      )
    })
    output$pipeline_protein_batchcorr_pcva_plot = renderPlot(perform_batchcorr_pcva())
    
    output$downloadBatchCorrTable = downloadHandler(
      filename = "batch_corr_results.csv",
      content = function(file) {
        write.csv(variables$pipeline_data_batchcorrected, file, row.names = FALSE)
      }
    )
    
    # Correlation
    
    output$pipeline_protein_corr_plot = renderPlot({
      if (!is.null(variables$pipeline_data_batchcorrected)) {
        plot_sample_corr_heatmap(
          variables$pipeline_data_batchcorrected,
          samples_to_plot = variables$replicated,
          sample_annotation = variables$pipeline_ant[variables$replicated, ],
          factors_to_plot = c(
            pipeline_batch_var(),
            pipeline_biospecimen_id_col_var()
          ),
          plot_title = 'Correlation between Replicated Samples',
          color_list = variables$color_list,
          cluster_rows = FALSE,
          cluster_cols = FALSE,
          fontsize = 8,
          annotation_names_col = TRUE,
          annotation_legend = FALSE,
          show_colnames = T
        )
      }
    }, height = 700)
    
    output$pipeline_imput_protein_corr_plot = renderPlot({
      if (!is.null(variables$pipeline_data_imputed)) {
        plot_sample_corr_heatmap(
          variables$pipeline_data_imputed,
          samples_to_plot = variables$replicated,
          sample_annotation = variables$pipeline_ant[variables$replicated, ],
          factors_to_plot = c(
            pipeline_technical_factors_var()[1],
            pipeline_biospecimen_id_col_var()
          ),
          plot_title = 'Correlation between Replicated Samples After Imputation',
          color_list = variables$color_list,
          cluster_rows = FALSE,
          cluster_cols = FALSE,
          fontsize = 8,
          annotation_names_col = TRUE,
          annotation_legend = FALSE,
          show_colnames = T
        )
      }
    }, height = 700)
    
    output$pipeline_init_protein_corr_plot = renderPlot({
      if (!is.null(variables$replicated)) {
        tt = variables$pipeline_ant[variables$replicated, ]
        #dev.off()
        plot_sample_corr_heatmap(
          variables$pipeline_data,
          samples_to_plot = variables$replicated,
          sample_annotation = tt,
          factors_to_plot = c(
            pipeline_technical_factors_var()[1],
            pipeline_sample_name_col_var()
          ),
          plot_title = 'Correlation between Replicated Samples',
          color_list = variables$color_list,
          cluster_rows = FALSE,
          cluster_cols = FALSE,
          fontsize = 8,
          annotation_names_col = TRUE,
          annotation_legend = FALSE,
          show_colnames = T
        )
      }
    })
    
    output$pipeline_protein_overall_corr_plot = renderPlot({
      plot_sample_corr_heatmap(
        variables$pipeline_data_batchcorrected,
        sample_annotation = variables$pipeline_ant,
        factors_to_plot = c(pipeline_batch_var(), pipeline_biospecimen_id_col_var()),
        plot_title = 'Correlation between Replicated Samples',
        color_list = variables$color_list,
        cluster_rows = FALSE,
        cluster_cols = FALSE,
        fontsize = 8,
        annotation_names_col = TRUE,
        annotation_legend = FALSE,
        show_colnames = T
      )
    }, height = 700)
    
    output$pipeline_protein_corr_dist_plot = renderPlot({
      plot_sample_corr_distribution(
        variables$pipeline_data_batchcorrected,
        variables$pipeline_ant,
        repeated_samples = variables$replicated,
        batch_col = pipeline_batch_var(),
        biospecimen_id_col = pipeline_sample_name_col_var(),
        plot_title = 'Correlation of samples (raw)',
        plot_param = 'batch_replicate'
      )
    }, height = 700)
    
    # Diff expression
    
    observeEvent(pipeline_ant_input(), {
      choices <- c(not_sel, sub(" ", "_", names(pipeline_ant_input())))
      updateSelectInput(inputId = "pipeline_class_var", choices = choices)
    })
    
    observeEvent(input$pipeline_class_var, {
      if (input$pipeline_class_var != "Not Selected") {
        data_df = variables$pipeline_ant
        updateSelectInput(session,
                          inputId = "pipeline_class_of_interest_var",
                          choices = unique(data_df[input$pipeline_class_var]))
      }
    })
    
    pipeline_class_var = eventReactive(input$pipeline_perform_limma_data_button,
                                       input$pipeline_class_var)
    pipeline_contrast_var = eventReactive(input$pipeline_perform_limma_data_button,
                                          input$pipeline_contrast_var)
    pipeline_lfc_var = eventReactive(input$pipeline_perform_limma_data_button,
                                     input$pipeline_lfc_var)
    pipeline_pvalue_var = eventReactive(input$pipeline_perform_limma_data_button,
                                        input$pipeline_pvalue_var)
    pipeline_class_of_interest_var = eventReactive(
      input$pipeline_perform_limma_data_button,
      input$pipeline_class_of_interest_var
    )
    
    perform_limma_pipe <-
      function(data_df,
               class_var,
               contrast_var,
               lfc_var,
               pvalue_var,
               contrast_other_classes_var,
               class_of_interest_var) {
        classes = unique(data_df[class_var])
        classes = classes[[class_var]]
        
        source('multiclass_limma.R')
        contrast_string = paste(class_of_interest_var, paste("-", contrast_var))
        
        if (lfc_var == 'Any' && pvalue_var == 'Any') {
          results = perform_limma(
            data_df,
            class_of_interest_var,
            contrast_var,
            contrast_string,
            contrast_other_classes_var,
            class_var,
            classes
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
            pvalue_var
          )
        }
        variables$fit2C = results$fit2C
        results = results$topProteins
        
        data.table(results)
        data.table(results)
      }
    
    limma_table_pipe <-
      eventReactive(input$pipeline_perform_limma_data_button, {
        df = as.data.frame(variables$pipeline_data_batchcorrected)
        
        df = as.data.frame(t(df))
        df[, pipeline_class_var()] = variables$pipeline_ant[, pipeline_class_var()]
        data_table_1 = perform_limma_pipe(
          df,
          pipeline_class_var(),
          input$pipeline_contrast_var,
          input$pipeline_lfc_var,
          input$pipeline_pvalue_var,
          T,
          pipeline_class_of_interest_var()
        )
        variables$pipe_llimma_table = data_table_1
        #variables$pipe_diffexp = variables$pipeline_data_batchcorrected[data_table_1$P.Value<0.05,]
        return(data_table_1)
      })
    
    output$pipeline_limma_table = renderTable(limma_table_pipe())
    
    output$downloadPipeLimmaTable = downloadHandler(
      filename = "pipeline_limma_results.csv",
      content = function(file) {
        write.csv(variables$pipe_llimma_table, file, row.names = FALSE)
      }
    )
    
    
    #Automated Diff Expression
    
    observeEvent(pipeline_ant_input(), {
      choices <- c(not_sel, sub(" ", "_", names(pipeline_ant_input())))
      updateSelectInput(inputId = "pipeline_auto_class_var", choices = choices)
    })
    
    pipeline_auto_class_var = eventReactive(input$pipeline_perform_auto_limma_data_button,
                                            input$pipeline_auto_class_var)
    pipeline_auto_lfc_var = eventReactive(input$pipeline_perform_auto_limma_data_button,
                                          input$pipeline_auto_lfc_var)
    pipeline_auto_pvalue_var = eventReactive(input$pipeline_perform_auto_limma_data_button,
                                             input$pipeline_auto_pvalue_var)
    
    limma_auto_table_pipe = eventReactive(input$pipeline_perform_auto_limma_data_button, {
      data_df = as.data.frame(variables$pipeline_data_batchcorrected)
      data_df = as.data.frame(t(data_df))
      data_df[, pipeline_auto_class_var()] = variables$pipeline_ant[, pipeline_auto_class_var()]
      
      dd = unique(data_df[, pipeline_auto_class_var()])
      
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
      for (i in (1:length(res2))) {
        group1 = res2[[i]][["1"]]
        group2 = res2[[i]][["2"]]
        
        group1_string = paste(group1, collapse = "+")
        group2_string = paste(group2, collapse = "+")
        
        data_table_2 = perform_limma_2(
          data_df,
          pipeline_auto_class_var(),
          input$pipeline_auto_lfc_var,
          input$pipeline_auto_pvalue_var,
          group1,
          group2
        )
        
        data_table_2$Combination = toString(paste(group1_string, group2_string, sep = " vs "))
        auto_limma_table = rbind(auto_limma_table, data_table_2)
      }
      
      auto_limma_table = auto_limma_table %>% dplyr::select(Combination, everything())
      
      ## merge cells of column 1))
      dtable <- datatable(auto_limma_table,
                          rownames = FALSE,
                          options = list(rowsGroup = list(0)))
      # folder containing dataTables.rowsGroup.js
      path <- getwd()
      dep <-
        htmltools::htmlDependency("RowsGroup", "2.0.0",
                                  path, script = "dataTables.rowsGroup.js")
      dtable$dependencies <- c(dtable$dependencies, list(dep))
      #dtable
      variables$pipe_auto_data_table = auto_limma_table
      return(dtable)
    })
    
    output$pipeline_auto_limma_table = renderDT(limma_auto_table_pipe())
    
    output$downloadPipeAutoLimmaTable = downloadHandler(
      filename = "pipeline_auto_limma_results.csv.xlsx",
      content = function(file) {
        df = variables$pipe_auto_data_table
        combs = unique(df$Combination)
        OUT = createWorkbook()
        for (c in combs) {
          addWorksheet(OUT, c)
          writeData(OUT, sheet = c, x = df[df$Combination == c, ])
        }
        saveWorkbook(OUT, file)
      }
    )
    
    #Clustering
    perform_anova = function(merged_combat_df) {
      shortlisted = c()
      anova_stat = c()
      anova_stat_f = c()
      
      for (i in 1:(length(colnames(merged_combat_df)) - 1)) {
        val = colnames(merged_combat_df)
        # not assuming equal variances)
        r1 = oneway.test(merged_combat_df[, i] ~ Class,
                         data = merged_combat_df, var.equal = T)
        anova_stat = append(anova_stat, r1$p.value)
        anova_stat_f = append(anova_stat_f, r1$statistic)
        if (r1$p.value < 0.05) {
          shortlisted = append(shortlisted, colnames(merged_combat_df)[i])
        }
      }
      anova_stat_df = data.frame(row.names = colnames(merged_combat_df))
      anova_stat_df = head(anova_stat_df, -1)
      anova_stat_df$pvalue = anova_stat
      anova_stat_df$fstat = anova_stat_f
      
      selected_df = merged_combat_df[shortlisted, ]
      #selected_df5=as.data.frame(selected_df5)
      
      return(selected_df)
    }
    
    output$pipeline_protein_dendogram = renderPlot({
      df = as.data.frame(variables$pipeline_data_batchcorrected)
      df = as.data.frame(t(df))
      df$Class = variables$pipeline_ant[, pipeline_class_var()]
      diff = perform_anova(df)
      variables$pipe_diffexp = diff
      
      plot_hierarchical_clustering(
        diff,
        sample_annotation = variables$pipeline_ant,
        color_list = variables$color_list,
        factors_to_plot = c(pipeline_batch_var(), pipeline_biological_factors_var()),
        distance = 'euclidean',
        agglomeration = 'complete',
        label_samples = FALSE,
        plot_title = "Clustering"
      )
    })
    
    output$pipeline_protein_heatmap = renderPlot({
      plot_heatmap_diagnostic(
        variables$pipe_diffexp,
        variables$pipeline_ant,
        factors_to_plot = c(
          pipeline_batch_var(),
          pipeline_biological_factors_var(),
          pipeline_technical_factors_var()
        ),
        cluster_cols = TRUE,
        color_list = variables$color_list,
        scale = "row",
        show_rownames = FALSE,
        show_colnames = FALSE,
        plot_title = "Heatmap"
      )
    }, height = 1000)
    
    
}
#enableBookmarking("server")
shinyApp(ui = ui, server = server)

#runApp('~/Pipeline/app', display.mode = "showcase")