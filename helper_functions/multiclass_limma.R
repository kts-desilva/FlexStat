#! /usr/bin/Rscript
library(limma)
library(dplyr)
library(tidyverse)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(stringi)

fit2C = NULL

perform_limma = function (ourData,
                          class_type,
                          contrast_type,
                          contrast_string,
                          contrast_other_classes,
                          classvar,
                          class_types,
                          fdr = "Benjamini-Hochberg") {
  
  fdr_methods = c("BH","bonferroni","BY","holm", "none")
  names(fdr_methods) = c("Benjamini-Hochberg", "Bonferroni",  "Benjamini-Yekutieli", "Holm", "None" )
  fdr = fdr_methods[[fdr]]
  
  if (!contrast_other_classes) {
    ourData <-
      ourData[ourData[, classvar] %in% c(class_type, contrast_type), ]
  }
  if (contrast_other_classes) {
    contrast_string = stri_replace_last_fixed(contrast_string, contrast_type, 'control')
    contrast_type = "control"
  }
  
  #Replace the class names to support binary distinctions is only all classes should be contrasted against each other
  for (i in class_types) {
    if (contrast_other_classes && i != class_type) {
      if (class(ourData[, classvar]) == "factor") {
        ourData[, classvar] = as.character(ourData[, classvar])
      }
      ourData[ourData == i] = contrast_type
    }
  }
  
  ourData[, classvar] = factor(ourData[, classvar])
  
  design2 = model.matrix( ~ ourData[, classvar] - 1)
  
  design_cols = c(class_type, contrast_type)
  design_cols = design_cols[order(design_cols)]
  colnames(design2) = design_cols
  ourData = ourData[,-which(names(ourData) == classvar)]
  
  charColumns = ourData %>% select_if(is.character)
  if (ncol(charColumns) > 0) {
    ourData = ourData[,-which(names(ourData) %in%  colnames(charColumns))]
  }
  
  ourData = data.frame(apply(ourData, 2, function(x)
    as.numeric(as.character(x))))
  ourData = t(ourData)
  
  
  fit2 = lmFit(ourData, design2)
  contrast.matrix = makeContrasts(contrasts = contrast_string, levels = design2)
  
  fit2C = contrasts.fit(fit2, contrast.matrix)
  fit2C = eBayes(fit2C)
  topProteins = topTable(
    fit2C,
    number = Inf,
    sort.by = "logFC",
    adjust.method =  fdr
  )
  topProteins$Gene = rownames(topProteins)
  topProteins = topProteins %>% select(Gene, everything())
  
  return (list("topProteins" = topProteins, "fit2C" = fit2C))
  
}

getTop10 = function () {
  topProteins = topTable(fit2C, number = 10)
  return(topProteins)
}

perform_limma_top10 = function (ourData,
                                class_type,
                                contrast_type,
                                contrast_string,
                                contrast_other_classes,
                                classvar,
                                class_types,
                                fdr) {
  
  fdr_methods = c("BH","bonferroni","BY","holm", "none")
  names(fdr_methods) = c("Benjamini-Hochberg", "Bonferroni",  "Benjamini-Yekutieli", "Holm", "None" )
  fdr = fdr_methods[[fdr]]
  
  if (!contrast_other_classes) {
    ourData <-
      ourData[ourData[, classvar] %in% c(class_type, contrast_type), ]
  }
  
  if (contrast_other_classes) {
    contrast_string = stri_replace_last_fixed(contrast_string, contrast_type, 'control')
  }
  
  #Replace the class names to support binary distinctions is only all classes should be contrasted against each other
  for (i in class_types) {
    if (contrast_other_classes && i != class_type) {
      ourData[ourData == i] = contrast_type
    }
  }
  
  ourData[, classvar] = factor(ourData[, classvar])
  
  design2 = model.matrix( ~ ourData[, classvar] - 1)
  
  colnames(design2) = c(class_type, contrast_type)
  ourData = ourData[,-which(names(ourData) == classvar)]
  ourData = data.frame(apply(ourData, 2, function(x)
    as.numeric(as.character(x))))
  ourData = t(ourData)
  fit2 = lmFit(ourData, design2)
  contrast.matrix = makeContrasts(contrasts = contrast_string, levels = design2)
  
  fit2C = contrasts.fit(fit2, contrast.matrix)
  fit2C = eBayes(fit2C)
  topProteins = topTable(fit2C, sort.by = "logFC", number = 50, adjust.method = fdr)
  topProteins$Gene = rownames(topProteins)
  topProteins = topProteins %>% dplyr::select(Gene, everything())
  
  return (topProteins)
  
}

perform_limma_top_all = function (ourData,
                                  class_type,
                                  contrast_type,
                                  contrast_string,
                                  contrast_other_classes,
                                  classvar,
                                  class_types, fdr) {
  
  fdr_methods = c("BH","bonferroni","BY","holm", "none")
  names(fdr_methods) = c("Benjamini-Hochberg", "Bonferroni",  "Benjamini-Yekutieli", "Holm", "None" )
  fdr = fdr_methods[[fdr]]
  
  if (!contrast_other_classes) {
    ourData <-
      ourData[ourData[, classvar] %in% c(class_type, contrast_type), ]
  }
  
  # Replace the class names to support binary distinctions is only all classes
  # should be contrasted against each other
  for (i in class_types) {
    if (contrast_other_classes && i != class_type) {
      contrast_type = 'control'
      ourData[ourData == i] = contrast_type
    }
  }
  
  ourData[, classvar] = factor(ourData[, classvar])
  design2 = model.matrix( ~ ourData[, classvar] - 1)
  colnames(design2) = c(class_type, contrast_type)
  
  ourData = ourData[,-which(names(ourData) == classvar)]
  ourData = data.frame(apply(ourData, 2, function(x)
    as.numeric(as.character(x))))
  ourData = t(ourData)
  fit2 = lmFit(ourData, design2)
  contrast.matrix = makeContrasts(contrasts = contrast_string, levels = design2)
  
  fit2C = contrasts.fit(fit2, contrast.matrix)
  fit2C = eBayes(fit2C)
  topProteins = topTable(fit2C, sort.by = "logFC", number = Inf, adjust.method = fdr)
  topProteins$Gene = rownames(topProteins)
  topProteins = topProteins %>% dplyr::select(Gene, everything())
  
  return (list("topProteins" = topProteins, "fit2C" = fit2C))
  
}

perform_specific_limma = function (ourData,
                                   class_type,
                                   contrast_type,
                                   contrast_string,
                                   contrast_other_classes,
                                   classvar,
                                   class_types,
                                   lfc,
                                   pvalue,
                                   fdr) {
  
  fdr_methods = c("BH","bonferroni","BY","holm", "none")
  names(fdr_methods) = c("Benjamini-Hochberg", "Bonferroni",  "Benjamini-Yekutieli", "Holm", "None" )
  fdr = fdr_methods[[fdr]]
  
  if (!contrast_other_classes) {
    ourData <-
      ourData[ourData[, classvar] %in% c(class_type, contrast_type), ]
  }
  if (contrast_other_classes) {
    contrast_string = stri_replace_last_fixed(contrast_string, contrast_type, 'control')
  }
  
  #Replace the class names to support binary distinctions is only all classes should be contrasted against each other
  for (i in class_types) {
    if (contrast_other_classes && i != class_type) {
      ourData[ourData == i] = contrast_type
    }
  }
  
  ourData[, classvar] = factor(ourData[, classvar])
  
  design2 = model.matrix( ~ ourData[, classvar] - 1)
  
  colnames(design2) = c(class_type, contrast_type)
  ourData = ourData[,-which(names(ourData) == classvar)]
  ourData = data.frame(apply(ourData, 2, function(x)
    as.numeric(as.character(x))))
  ourData = t(ourData)
  fit2 = lmFit(ourData, design2)
  contrast.matrix = makeContrasts(contrasts = contrast_string, levels = design2)
  
  fit2C = contrasts.fit(fit2, contrast.matrix)
  fit2C = eBayes(fit2C)
  topProteins = NULL
  
  if (is.numeric(lfc) && is.numeric(pvalue)) {
    topProteins = topTable(
      fit2C,
      number = Inf,
      lfc = as.numeric(lfc),
      p.value = as.numeric(pvalue),
      adjust.method = fdr
    )
  } else if (is.numeric(lfc) && !is.numeric(pvalue)) {
      topProteins = topTable(
        fit2C,
        number = Inf,
        lfc = as.numeric(lfc),
        adjust.method = fdr
      )
  } else {
    topProteins = topTable(
      fit2C,
      number = Inf,
      p.value = as.numeric(pvalue),
      adjust.method = fdr
    )
  }
  topProteins$Gene = rownames(topProteins)
  topProteins = topProteins %>% select(Gene, everything())

  return (list("topProteins" = topProteins, "fit2C" = fit2C))
  
}

perform_limma_top10_specific = function (ourData,
                                         class_type,
                                         contrast_type,
                                         contrast_string,
                                         contrast_other_classes,
                                         classvar,
                                         class_types,
                                         lfc,
                                         pvalue,
                                         fdr) {
  
  fdr_methods = c("BH","bonferroni","BY","holm", "none")
  names(fdr_methods) = c("Benjamini-Hochberg", "Bonferroni",  "Benjamini-Yekutieli", "Holm", "None" )
  fdr = fdr_methods[[fdr]]
  
  if (!contrast_other_classes) {
    ourData <-
      ourData[ourData[, classvar] %in% c(class_type, contrast_type), ]
  }
  if (contrast_other_classes) {
    contrast_string = stri_replace_last_fixed(contrast_string, contrast_type, 'control')
  }
  #Replace the class names to support binary distinctions is only all classes should be contrasted against each other
  for (i in class_types) {
    if (contrast_other_classes && i != class_type) {
      ourData[ourData == i] = contrast_type
    }
  }
  
  ourData[, classvar] = factor(ourData[, classvar])
  
  design2 = model.matrix( ~ ourData[, classvar] - 1)
  
  colnames(design2) = c(class_type, contrast_type)
  ourData = ourData[,-which(names(ourData) == classvar)]
  ourData = data.frame(apply(ourData, 2, function(x)
    as.numeric(as.character(x))))
  ourData = t(ourData)
  fit2 = lmFit(ourData, design2)
  contrast.matrix = makeContrasts(contrasts = contrast_string, levels = design2)
  
  fit2C = contrasts.fit(fit2, contrast.matrix)
  fit2C = eBayes(fit2C)
  topProteins = NULL
  if (is.numeric(lfc) && is.numeric(pvalue)) {
    topProteins = topTable(
      fit2C,
      sort.by = "logFC",
      number = 50,
      lfc = as.numeric(lfc),
      p.value = as.numeric(pvalue),
      adjust.method = fdr
    )
  } else if (is.numeric(lfc) && !is.numeric(pvalue)) {
    topProteins = topTable(
      fit2C,
      sort.by = "logFC",
      number = 50,
      lfc = as.numeric(lfc),
      adjust.method = fdr
    )
  } else {
    topProteins = topTable(
      fit2C,
      sort.by = "logFC",
      number = 50,
      p.value = as.numeric(pvalue),
      adjust.method = fdr
    )
  }
  topProteins$Gene = rownames(topProteins)
  topProteins = topProteins %>% select(Gene, everything())
  
  return (topProteins)
  
}

perform_limma_top_specific_all = function (ourData,
                                           class_type,
                                           contrast_type,
                                           contrast_string,
                                           contrast_other_classes,
                                           classvar,
                                           class_types,
                                           lfc,
                                           pvalue,
                                           fdr) {
  
  fdr_methods = c("BH","bonferroni","BY","holm", "none")
  names(fdr_methods) = c("Benjamini-Hochberg", "Bonferroni",  "Benjamini-Yekutieli", "Holm", "None" )
  fdr = fdr_methods[[fdr]]
  
  if (!contrast_other_classes) {
    ourData <-
      ourData[ourData[, classvar] %in% c(class_type, contrast_type), ]
  }
  if (contrast_other_classes) {
    contrast_string = stri_replace_last_fixed(contrast_string, contrast_type, 'control')
  }
  #Replace the class names to support binary distinctions is only all classes should be contrasted against each other
  for (i in class_types) {
    if (contrast_other_classes && i != class_type) {
      ourData[ourData == i] = contrast_type
    }
  }
  
  ourData[, classvar] = factor(ourData[, classvar])
  
  design2 = model.matrix( ~ ourData[, classvar] - 1)
  
  colnames(design2) = c(class_type, contrast_type)
  ourData = ourData[,-which(names(ourData) == classvar)]
  ourData = data.frame(apply(ourData, 2, function(x)
    as.numeric(as.character(x))))
  ourData = t(ourData)
  fit2 = lmFit(ourData, design2)
  contrast.matrix = makeContrasts(contrasts = contrast_string, levels = design2)
  
  fit2C = contrasts.fit(fit2, contrast.matrix)
  fit2C = eBayes(fit2C)
  topProteins = NULL
  
  if (is.numeric(lfc) && is.numeric(pvalue)) {
    topProteins = topTable(
      fit2C,
      sort.by = "logFC",
      number = 50,
      lfc = as.numeric(lfc),
      p.value = as.numeric(pvalue),
      adjust.method = fdr
    )
  } else if (is.numeric(lfc) && !is.numeric(pvalue)) {
    topProteins = topTable(
      fit2C,
      sort.by = "logFC",
      number = 50,
      lfc = as.numeric(lfc),
      adjust.method = fdr
    )
  } else {
    topProteins = topTable(
      fit2C,
      sort.by = "logFC",
      number = 50,
      p.value = as.numeric(pvalue),
      adjust.method = fdr
    )
  }
  topProteins$Gene = rownames(topProteins)
  topProteins = topProteins %>% select(Gene, everything())
  
  return (topProteins)
  
}
