library(shinyjs)

######################################################################################  
# Help Page
###################################################################################### 

# Module UI function
informationUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
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
    
    
  )
}