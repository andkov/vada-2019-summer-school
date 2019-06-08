# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md")
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./manipulation/stitched-output/0-greeter.md", )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  
base::source("./scripts/common-functions.R")
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(dplyr) # disable when temp lines are removed
library(ggplot2)
library(ggpubr)
library(readxl)
# ---- declare-globals ---------------------------------------------------------
path_folder <- "./data-unshared/raw/"
path_input_files <- list.files(path_folder,".xlsx")

# ---- load-data ---------------------------------------------------------------
files_provided  <- list.files(path = path_folder,full.names = T)
csv_input_paths <- grep(".csv$", files_provided, value = T) %>% print()
# data transfer object
dto <- list()
for(i in seq_along(csv_input_paths) ){
  # i <- 1 # for testing
  dto[["full_name"]][i] <- csv_input_paths[i]
  dto[["base_name"]][i] <- csv_input_paths[i] %>% basename()
  # spell out the regex to govern substitutions
  regex_pattern <- "^(.+)_(\\d{2})(\\d{2})(\\d{4}).csv$"
  i_string <- basename(csv_input_paths[i])
                                          # capturing group
  dto[["variable_set"]][i] <- gsub(regex_pattern, "\\1", i_string  )
  (                i_day   <- gsub(regex_pattern, "\\2", i_string) )
  (                i_month <- gsub(regex_pattern, "\\3", i_string) )
  (                i_year  <- gsub(regex_pattern, "\\4", i_string) )
  dto[["date"]][i] <- paste(i_year,i_month,i_day, sep="-") # to make ISO compliant
  dto[["variable_set"]][i] <- gsub("_variables$","", dto[["variable_set"]][i]) %>% tolower()
}
dto %>% lapply(glimpse)
# populate dto
dto[["data"]] <- list() 
dto[["meta"]] <- list() 
for(i in seq_along(dto$variable_set)){
  dto[["data"]][[i]]        <- readr::read_csv(file = dto$full_name[i])
  names(dto[["data"]][[i]]) <- dto[["data"]][[i]] %>% names() %>% tolower()
  dto[["meta"]][[i]]        <- names_labels(dto[["data"]][[i]])
}
names(dto[["data"]]) <- dto[["variable_set"]]
names(dto[["meta"]]) <- dto[["variable_set"]]

# ---- tweak-data --------------------------------------------------
# ---- inspect-data ----------------------------------------------

dto %>% lapply(class)
dto %>% lapply(names)

# ---- basic-table ----------------------------

# ---- basic-pivot -----------------------------

# ---- save-to-disk ----------------------------

dto %>% pryr::object_size()
dto %>% saveRDS("./data-unshared/derived/0-greeted.rds")
# ---- publish ---------------------------------

path_report_1 <- "./analysis/0-greeter/0-greeter-table.Rmd"
path_report_2 <- "./analysis/0-greeter/0-greeter-pivot.Rmd"
path_report_3 <- "./analysis/0-greeter/0-greeter.Rmd"

# allReports <- c(path_report_1,path_report_2, path_report_3)
# allReports <- c(path_report_3)
allReports <- c(path_report_1)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}





