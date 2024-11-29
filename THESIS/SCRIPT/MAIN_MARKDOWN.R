require(rmarkdown)
require(bookdown)
require(knitr)
require(kableExtra)
require(pander)
require(dplyr)
require(ggplot2)

### set working directory

setwd("")

### load functions

source("SCRIPT/MAIN_ANALYSIS.R")

### render

render(input = "MAIN.Rmd", # file Rmd name
       output_format = pdf_document(toc = F, number_sections = T), # document options
       output_file = "Master_Thesis_Ventruti.pdf", # output name
       output_dir = "OUTPUT" # output directory
)

