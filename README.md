# FlexStat
FlexStat: Proteomics Data Analysis Tool

FlexStat is a user-friendly web-based interface designed to facilitate the extraction of DEP through combinatory analysis. This versatile tool accepts a protein expression matrix as input and systematically generates results for every conceivable combination of various experimental conditions/ disease types.
It offers a suite of robust statistical tools, including data statistics computation, data preprocessing, and analytics, tailored to protein subgroups such as proteases, kinases, and transcription factors, all in an automated fashion while generating publication-ready figures.

![Imgur](https://i.imgur.com/y56ebPF.png)

# Workflow
![Imgur](https://i.imgur.com/I1xJpYF.png)

# Prerequisites to run locally
Install the additional packages below
1. [jdcbioinfo](https://github.com/jdreyf/jdcbioinfo/)
```
remotes::install_github(repo="jdreyf/jdcbioinfo", build_opts = c("--no-resave-data", "--no-manual"))
```
2. [DREAMAI](https://github.com/WangLab-MSSM/DreamAI/tree/master)
```
require("remotes")
install_github("WangLab-MSSM/DreamAI/Code")
```
4. [EnhancedVolcano]()
```
  if (!requireNamespace('BiocManager', quietly = TRUE))
    install.packages('BiocManager')

  BiocManager::install('EnhancedVolcano')
```
5. ConsensusClusterPlus (https://bioconductor.org/packages/release/bioc/html/ConsensusClusterPlus.html)
   ```
   BiocManager::install("ConsensusClusterPlus")
   ```

# How to run the app
```
runApp()
```
