
if((suppressWarnings(suppressMessages(!require(openxlsx, warn.conflicts = FALSE))))){
  install.packages("openxlsx")
  suppressWarnings(suppressMessages(library(openxlsx, warn.conflicts = FALSE)))
}
if((suppressWarnings(suppressMessages(!require(data.table, warn.conflicts = FALSE))))){
  install.packages("data.table")
  suppressWarnings(suppressMessages(library(data.table, warn.conflicts = FALSE)))
}

if((suppressWarnings(suppressMessages(!require(pastecs, warn.conflicts = FALSE))))){
  install.packages("pastecs")
  suppressWarnings(suppressMessages(library(pastecs, warn.conflicts = FALSE)))
}

if((suppressWarnings(suppressMessages(!require(tidyverse, warn.conflicts = FALSE))))){
  install.packages("tidyverse")
  (suppressWarnings(suppressMessages(library(tidyverse, warn.conflicts = FALSE))))
}

if((suppressWarnings(suppressMessages(!require(doParallel, warn.conflicts = FALSE))))){
  install.packages("doParallel")
  (suppressWarnings(suppressMessages(library(doParallel, warn.conflicts = FALSE))))
}


if(!(suppressWarnings(suppressMessages(!require(fs, warn.conflicts = FALSE))))){
  install.packages("fs")
  (suppressWarnings(suppressMessages(library(fs, warn.conflicts = FALSE))))
}


if((suppressWarnings(suppressMessages(!require(caret, warn.conflicts = FALSE))))){
  install.packages("caret")
  (suppressWarnings(suppressMessages(library(caret, warn.conflicts = FALSE))))
}


if((suppressWarnings(suppressMessages(!require(caretEnsemble, warn.conflicts = FALSE))))){
  install.packages("caretEnsemble")
  (suppressWarnings(suppressMessages(library(caretEnsemble, warn.conflicts = FALSE))))
}


if((suppressWarnings(suppressMessages(!require(stats, warn.conflicts = FALSE))))){
  install.packages("stats")
  (suppressWarnings(suppressMessages(library(stats, warn.conflicts = FALSE))))
}


if((suppressWarnings(suppressMessages(!require(lme4, warn.conflicts = FALSE))))){
  install.packages("lme4")
  (suppressWarnings(suppressMessages(library(lme4, warn.conflicts = FALSE))))
}


if((suppressWarnings(suppressMessages(!require(lucid, warn.conflicts = FALSE))))){
  install.packages("lucid")
  (suppressWarnings(suppressMessages(library(lucid, warn.conflicts = FALSE))))
}

if((suppressWarnings(suppressMessages(!require(magick, warn.conflicts = FALSE))))){
  install.packages("magick")
  (suppressWarnings(suppressMessages(library(magick, warn.conflicts = FALSE))))
}

if((suppressWarnings(suppressMessages(!require(devtools, warn.conflicts = FALSE))))){
  install.packages("devtools")
  (suppressWarnings(suppressMessages(library(devtools, warn.conflicts = FALSE))))
}

if((suppressWarnings(suppressMessages(!require(asreml, warn.conflicts = FALSE))))){
#  install_github("jbkey730/BreedStats/External Packages/asreml_4.1.0.160.zip", repos = NULL, type = "source")
#  (suppressWarnings(suppressMessages(library(asreml, warn.conflicts = FALSE))))
}

if((suppressWarnings(suppressMessages(!require(DiGGer, warn.conflicts = FALSE))))){
#  install_github("jbkey730/BreedStats/External Packages/DiGGer_1.0.5.zip",repos = NULL, type = "source")
#  (suppressWarnings(suppressMessages(library(DiGGer, warn.conflicts = FALSE))))
}







