#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
library(magrittr)          # enables piping : %>% 
library(dplyr)             # data wrangling
library(knitr)             # tables
library(ggplot2)           # graphs
requireNamespace("readr")  # for data input
requireNamespace("tidyr")  # for data manipulation
requireNamespace("testit") # for asserting conditions meet expected patterns
requireNamespace("car")    # for its `recode()` function

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines external functions needed in this report. 
# Ideally, no real operations are performed in these scripts.
base::source("./scripts/common-functions.R") # used in multiple reports
base::source("./scripts/graphing/graph-presets.R") # fonts, colors, themes 

# ---- declare-globals ---------------------------------------------------------
path_file_input <- "./data-unshared/derived/0-greeted.rds"
html_flip <- FALSE


# to select target variables from each source 
# 1 - Clinical Outcomes at year 1
var_clinical_outcomes_1y <- c(
  "atopy_1y"           # (Bin)  Atopy at 1y (by SPT - reaction to any allergen)
  ,"wz_recurrent_1y"    # (Bin)  Recurrent wheeze at 1y: 2+ episodes (ZL 06-2015)	Binary
  ,"atopy_1y"           # (Bin)  Atopy at 1y (by SPT - reaction to any allergen)	Binary
  ,"atopy_food_1y"      # (Bin)  Sensitization to food allergen at 1y by SPT	Binary
  ,"atopy_nonfood_1y"   # (Bin)  Sensitization to non-food allergen at 1y by SPT	Binary
  ,"wtclass_1y"         # (Cat3) Weight Class at 1y (based on WFL z-score)	Categorical
  ,"ow_1y"              # (Bin)  Overweight at 1y (WFL z-score > 97%ile)	Binary
  ,"owrisk_1y"          # (Bin)  At risk for overweight at 1y (WFL z-score > 85%ile)	Binary
  ,"rapid_bmigrowth_12m"# (Bin)  >1 SD change in z-score from birth(BMI) to 12m(BMI)	Binary
  ,"rapid_wflgrowth_12m"# (Bin)  >1 SD change in z-score from birth(WFA) to 12m(WFL)	Binary
  ,"rapid_wgt_0to12m"   # (Bin)  Change in WHO zwgt >0.67	Binary
  ,"dzwgt_0to12m"       # (Num)  Weight gain velocity from 0 to 12m (change in WHO weight z-score)
) 
# 2 - Covariates and Risk variables
var_covariates_risk <- c(
  "cat_pn"         # (Bin) Dog as  pet  in your home in the last 12 months
  ,"dog_pn"        # (Bin) Cat as  pet in your home in the last 12 months
  ,"ga_wks"        # (Int) Gestational Age in weeks
  ,"ga_wks_cat"    # (Cat) Gestational Age categories: 1)34-36, 2)37-38, 3)39-40, 4=41+
  ,"sex"	         # (Bin) Infant sex (0=Female, 1=Male) - from CHILD profile
  ,"older_sibs3"	 # (Cat) Older siblings: 0, 1, 2+
  ,"lact_season"   # (Cat) Season of Milk Collection: (W):Dec-Feb, (Spring):Mar-May, (Summer):Jun-Aug, (Fall):Sept-Nov
  ,"mom_wtclass3"	 # (Cat) Moms weight class From mom_wtclass_best
  ,"prudent"	     # (Num) Prudent Diet
)
# 3 - Extra
var_extra <- c(
  "alcohol"          # (Num) alcohol (g)
  ,"caffeine"        # (Num) caffeine (mg)
)
# 4 - Feeding
var_feeding <- c(
  "bf_duration_imp"  # (Int) Breastfeeding Duration in Months (implied)
  ,"bf_12m"          # (Bin) Breastfeeding at 12 Months (final)
  ,"diet_sampling"	 # (Cat) Infant Diet at milk sample collection - BFS: Breast, Formula, Solids (CAPITAL=YES, small=no)
  ,"ebf_duration"	   # (Int) Duration of Exclusive Breastfeeding (Months)
  
)
#5 - Milk Components
var_milk_components <- c(
  "hmo_total"	       # (Num) Sum of all  HMOs
  ,"hmo_diversity"	 # (Num) HMO Diversity
)

# ---- rmd-specific ----------------------------
# define custom functions used in this script

# function that prepares data for plot `1`
prep_data_1 <- function(
  d
  ,dim_horizontal
  ,dim_vertical  
  ,dim_fill
){
  # to declare values for testing and development (within this function)
  # d <- ds
  # dim_horizontal = "hmo_total"
  # dim_vertical   = "wtg_velocity"
  # dim_fill      = "atopy_1y"
  # palette_custom = c("TRUE"="#1B9E77", "FALSE"="#D95F02")
  
  # data prep step
  d1 <- d %>% 
    tidyr::drop_na(.dots = dim_fill) # to avoid clutter in labels
  custom_label_title = paste0(
    "(",dim_fill,") across (", dim_horizontal,") and (",dim_vertical,")" 
  )
  custom_label_x     = toupper(dim_horizontal)
  custom_label_y     = toupper(dim_vertical)
  custom_label_fill = toupper(dim_fill)
  
  # to store objects for passing to the `make_plot` function
  l_support <- list()
  l_support[["data"]] <- d1
  l_support[["dimension"]] <- c(
    "horizontal" = dim_horizontal
    ,"vertical"  = dim_vertical
    ,"fill"      = dim_fill
  )
  l_support[["label"]] <- c(
    "x"      = custom_label_x
    ,"y"      = custom_label_y
    ,"fill"   = custom_label_fill
    ,"title"  = custom_label_title
  )
  
  lapply(l_support, class) # view contents
  # the make_plot funtion will rely on the structure and values in l_support
  
  return(l_support)
}
# how to use
# l_support <- ds %>% 
#   prep_data_1(
#     dim_horizontal  = "hmo_total"
#     ,dim_vertical   = "wtg_velocity"
#     ,dim_fill      = "atopy_1y"
#   )
# l_support %>% print()

# now we can pass this curated object `l_support` to graphing function

make_plot_1 <- function(
  l_support
  ,palette_custom = c("TRUE"="red", "FALSE"=NA) 
){
  # plotting prep step
  # palette_custom = c("TRUE"="#1B9E77", "FALSE"="#D95F02") 
  
  # plotting step
  g_out <- l_support[["data"]] %>%
    ggplot(
      aes_string(
        x      = l_support[["dimension"]]["horizontal"] # continuous  # hmo_total
        ,y     = l_support[["dimension"]]["vertical"]   # continuous  # wtg_velocity
        ,fill = l_support[["dimension"]]["fill"]      # categorical # atopy_1y
      )
    ) +
    geom_point(shape = 21, alpha = .4, size= 3 )+
    scale_fill_manual(values = palette_custom)+
    theme_minimal()+
    labs(
      title  = l_support[["label"]]["title"]    
      ,x     = l_support[["label"]]["x"]    
      ,y     = l_support[["label"]]["y"]
      ,fill = l_support[["label"]]["fill"]
    )
  # g_out # for viewing while developing
  l_support[["graph"]]   <- g_out
  return(l_support)  
}
# how to use
# to add a new element in `l_support`` that would contain the plot
# l_support <- ds %>% 
#   prep_data_1(
#     dim_horizontal  = "hmo_total"
#     ,dim_vertical   = "wtg_velocity"
#     ,dim_fill      = "atopy_1y"
#   ) %>% 
#   make_plot_1()
# l_support$graph %>% print() # note that printing is a separate step

print_plot_1 <- function(
  l_support
  ,path_output_folder
  ,prefex             = NA      # string token to mark the version
  ,graph_name         = "auto"  # replace with custom name 
  ,...
){
  # path_output_folder = "./analysis/prep-plot-print/demo-1/"
  # prefex            = "attempt1"
  
  file_name = paste0("("
                     ,l_support$dimension["color"]
                     ,")-across-("
                     , l_support$dimension["horizontal"]
                     ,")-and-("
                     ,l_support$dimension["vertical"]
                     ,")" 
  )
  if( !graph_name == "auto" ){ # to trigger auto, must be `auto`
    (file_name <- graph_name) # replace with a custom string 
  }
  # add a label to distinguish a particular graph (first element in the file name)
  if( !is.na(prefex) ){ # inserts a PREFEX before the graph name
    (path_save_plot <- paste0(path_output_folder, prefex,"-",file_name) )
  }else{
    ( path_save_plot <- paste0(path_output_folder, file_name) )
  }
  # if folder does not exist yet, create it
  if( !dir.exists(path_output_folder) ){
    dir.create(path_output_folder)
  }
  # print the graphical object using jpeg device
  path_printed_plot <- paste0(path_save_plot, ".jpg")
  jpeg(
    filename = path_printed_plot
    ,...
  )
  l_support$graph %>% print() # reach into the custom object we made for plotting
  dev.off() # close the device
  l_support[["path_plot"]] <- path_printed_plot # save the path where plot was printed
  return(l_support)
}
# how to use
# l_support <- ds %>% 
#   prep_data_1(
#     dim_horizontal  = "hmo_total"
#     ,dim_vertical   = "wtg_velocity"
#     ,dim_fill      = "atopy_1y"
#   ) %>% 
#   make_plot_1() %>% 
#   print_plot_1(
#     path_output_folder = "./analysis/prep-plot-print/demo-1-print/"
#     ,prefex            = "1600-900"
#     # ,graph_name        = "take1" # `auto` by default
#     # options added through `...` into the jpeg() function   
#     ,width   = 1600
#     ,height  = 900
#     ,units   = "px" # "px" - pixels,  "in" - inches , "cm" - centimiters
#     ,quality = 100  # percent
#     ,res     = 200  # resolution, dots per inch
#     # ,res     = 100 
#   )
# this function will create the `l_support` object AND print the graph to disk
# ---- load-data ---------------------------------------------------------------
# see ./data-unshared/contents.md for origin of the data
dto <- readRDS(path_file_input)
dto %>% pryr::object_size(); dto %>% class(); dto %>% names()

# ---- tweak-data ------------------------------

# assemble a focal ds of the project (we dont' want to many variables distracting us)
ls_ds <- list(
  dto$data$clinical_outcomes_1y %>% dplyr::select(c("ids",var_clinical_outcomes_1y))
  ,dto$data$covariates_risk     %>% dplyr::select(c("ids",var_covariates_risk))
  ,dto$data$extra               %>% dplyr::select(c("ids",var_extra))
  ,dto$data$feeding             %>% dplyr::select(c("ids",var_feeding))
  ,dto$data$milk_components     %>% dplyr::select(c("ids",var_milk_components))
)

# combine list of ds into a single ds
ds <- base::Reduce(dplyr::full_join, ls_ds)
# to creat a new varaible
ds <- ds %>% 
  dplyr::mutate(
    pets_home_12m = ifelse(cat_pn==1 & dog_pn==1, "both",
                           ifelse(
                             cat_pn==1 & dog_pn==0, "cat",
                             ifelse(
                               cat_pn==0 & dog_pn==1, "dog",
                               ifelse(cat_pn==0 & dog_pn==0, "none",NA))))
    ) %>%
  # to bring variables in the appropriate format
  dplyr::mutate(
   atopy_1y              = as.logical(atopy_1y)
   ,wz_recurrent_1y      = as.logical(wz_recurrent_1y)                
   ,atopy_1y             = as.logical(atopy_1y)                
   ,atopy_food_1y        = as.logical(atopy_food_1y)                
   ,atopy_nonfood_1y     = as.logical(atopy_nonfood_1y)                
   ,ow_1y                = as.logical(ow_1y)                
   ,owrisk_1y            = as.logical(owrisk_1y)                
   ,rapid_bmigrowth_12m  = as.logical(rapid_bmigrowth_12m)                
   ,rapid_wflgrowth_12m  = as.logical(rapid_wflgrowth_12m)                
   ,rapid_wgt_0to12m     = as.logical(rapid_wgt_0to12m)                
  
   ,older_sibs3          = as.character(older_sibs3)
   ,wtg_velocity         = dzwgt_0to12m # because hard to spell
) 
# to update the definition of the group set:
var_covariates_risk <- c(var_covariates_risk,"pets_home_12m")

# for a quick reference for the variables in focus (and their sources)
focal_variables <- list(
  "outcome"     = var_clinical_outcomes_1y
  ,"covariate"  = var_covariates_risk
  ,"extra"      = var_extra
  ,"feeding"    = var_feeding
  ,"milk"       = var_milk_components
)

# to view the new categorical variable
ds %>% group_by(pets_home_12m) %>% dplyr::count()

# ---- inspect-data-1 ----------------------------
# to view basic info for each set of focal variables
# for(variable_set_i in names(focal_variables)){
#   cat("\n ### - ", variable_set_i, " - ###\n\n")
#   ds %>% 
#     dplyr::select_(.dots = focal_variables[[variable_set_i]]) %>% 
#     explore::describe() %>% 
#     print()
# }

# ---- inspect-data-2 ----------------------------
# ds %>% explore::explore_all(ncol = 5 )
# ---- inspect-data-3 ----------------------------
# to view the outcomes against ONLY continuous variables
# ds %>%
#   dplyr::select_(.dots = c(
#     "atopy_1y"         # outcome # Atopy at 1y (by SPT - reaction to any allergen)
#     ,"wtg_velocity"    # outcome # Weight gain velocity from 0 to 12m (change in WHO weight z-score)
#     ,"ga_wks"          # covar   # Gestational Age in weeks
#     ,"prudent"	       # covar   # Prudent Diet
#     ,"bf_duration_imp" # feed    # Breastfeeding Duration in Months (implied)
#     ,"ebf_duration"	   # feed    # Duration of Exclusive Breastfeeding (Months)
#     ,"hmo_total"	     # milk    # Sum of all  HMOs
#     ,"hmo_diversity"	 # milk    # HMO Diversity
#   )) %>%
#   GGally::ggpairs(mapping = aes(fill = atopy_1y))+theme_minimal()

# ---- sketch-the-graph -------------------
# Let us sketch a graph that would map three dimensions:
# (x)     - horizontal - continuous
# (y)     - vertical   - continuous
# (color) - color      - discrete

# ----- example-1-minimal --------------------------------
# print `focal_variables` to refernce the spelling of variables
focal_variables %>% print()
# how to use
l_support <- ds %>% 
  prep_data_1(
    dim_horizontal  = "hmo_total"     # must be continuous/integer
    ,dim_vertical   = "wtg_velocity"  # must be continuous/integer
    ,dim_fill       = "atopy_1y"      # must be categorical
  ) %>% 
  make_plot_1()
# to display the plot in the 
l_support$graph 

# ----- example-2-extended ------------------------
# to remind yourself what variables available:
focal_variables %>% print()

# after trying out various configuration:
l_support <- ds %>% 
  prep_data_1(
    # dim_horizontal = "prudent"	       # (Num) Covariate - Prudent Diet
    # dim_horizontal = "ga_wks"          # (Int) Covariate - Gestational Age in weeks
    # dim_horizontal = "hmo_total"	     # (Num) Extra     - Sum of all  HMOs
    # dim_horizontal = "hmo_diversity"	 # (Num) Extra     - HMO Diversity
    dim_horizontal = "hmo_total"	     # (Num) Milk      - Sum of all  HMOs
    # dim_horizontal = "hmo_diversity"	 # (Num) Milk      - HMO Diversity
    # dim_horizontal = "bf_duration_imp" # (Int) Feeding   - Breastfeeding Duration in Months (implied)
    # dim_horizontal = "ebf_duration"	   # (Int) Feeding   - Duration of Exclusive Breastfeeding (Months)
    
    # ,dim_vertical = "prudent"	         # (Num) Covariate - Prudent Diet
    # ,dim_vertical = "ga_wks"           # (Int) Covariate - Gestational Age in weeks
    # ,dim_vertical = "hmo_total"	       # (Num) Milk      - Sum of all  HMOs
    ,dim_vertical = "hmo_diversity"	   # (Num) Milk      - HMO Diversity
    # ,dim_vertical = "bf_duration_imp"  # (Int) Feeding   - Breastfeeding Duration in Months (implied)
    # ,dim_vertical = "ebf_duration"	   # (Int) Feeding   - Duration of Exclusive Breastfeeding (Months)
    
    # ,dim_fill = "atopy_1y"             # (Bin)  Atopy at 1y (by SPT - reaction to any allergen)	Binary
     # ,dim_fill = "wz_recurrent_1y"      # (Bin)  Recurrent wheeze at 1y: 2+ episodes (ZL 06-2015)	Binary
    # ,dim_fill = "wtclass_1y"           # (Cat3) Weight Class at 1y (based on WFL z-score)	Categorical
    # ,dim_fill = "ow_1y"                # (Bin)  Overweight at 1y (WFL z-score > 97%ile)	Binary
    # ,dim_fill = "owrisk_1y"            # (Bin)  At risk for overweight at 1y (WFL z-score > 85%ile)	Binary
    ,dim_fill = "rapid_bmigrowth_12m"  # (Bin)  >1 SD change in z-score from birth(BMI) to 12m(BMI)	Binary
    # ,dim_fill = "rapid_wflgrowth_12m"  # (Bin)  >1 SD change in z-score from birth(WFA) to 12m(WFL)	Binary
    # ,dim_fill = "rapid_wgt_0to12m"     # (Bin)  Change in WHO zwgt >0.67	Binary
  ) %>% 
  make_plot_1(
    # ,palette_custom = c("1-Normal" = "white", "2-At Risk" = "#1AFF1A", "3-Overweight" = "#4B0092") # for `wtclass_1y`
    # palette from https://davidmathlogic.com/colorblind/#%231AFF1A-%234B0092
  ) 
# to print the graph
l_support$graph

# to print graph with modifications
l_support$graph + 
  # facet_grid( pets_home_12m ~ older_sibs3)
  facet_grid( pets_home_12m ~ lact_season)
  # facet_grid( ga_wks_cat ~ diet_sampling)

# Candidates for faceting:
# ga_wks_cat     # (Cat) Gestational Age categories: 1)34-36, 2)37-38, 3)39-40, 4=41+
# older_sibs3    # (Cat) Older siblings: 0, 1, 2+
# lact_season    # (Cat) Season of Milk Collection: (W):Dec-Feb, (Spring):Mar-May, (Summer):Jun-Aug, (Fall):Sept-Nov
# diet_sampling  # (Cat) Infant Diet at milk sample collection - BFS: Breast, Formula, Solids (CAPITAL=YES, small=no)


# ---- print-to-disk ---------------------------------------
# to print to disk
l_support %>% 
  print_plot_1(
    path_output_folder = "./analysis/prep-plot-print/demo/"
    ,prefex            = "1600-900"
    # ,graph_name        = "take1" # `auto` by default
# options added through `...` into the jpeg() function   
    ,width   = 1600
    ,height  = 900
    ,units   = "px" # "px" - pixels,  "in" - inches , "cm" - centimiters
    ,quality = 100  # percent
    ,res     = 200  # resolution, dots per inch
    # ,res     = 100 
  )
# ---- dimensions ----------------------
# canvas size guide ( portrait orientation )

# Size           Width x Height (mm) Width x Height (in)  Aspect Ratio
# Half Letter      140 x 216           5.5 x  8.5          1: 1.55
# Letter           216 x 279           8.5 x 11.0          1: 1.29
# Legal            216 x 356           8.5 x 14.0          1: 1.65
# Junior Legal     127 x 203           5.0 x  8.0          1: 1.60
# Ledger/Tabloid   279 x 432          11.0 x 17.0          1: 1.55


# ---- publish ---------------------------------------
# This chunk will publish the summative report
path_publish_report_1 <- "./analysis/prep-plot-print/prep-plot-print-00-applications.Rmd"
# path_publish_report_2 <- "./reports/*/report_2.Rmd"
allReports <- c(
  path_publish_report_1

) # add more reports, in necessary
pathFilesToBuild <- c(allReports) # line up report(s) to render
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build (render) the reports
for( pathFile in pathFilesToBuild ) {
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document"
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" 
                    ),
                    clean=TRUE)
}




# ---- save-to-disk --------------------- 
# save the created and groomed dataset for further use by subsequent reports
readr::write_csv(ds, path_save)
ds %>% saveRDS( gsub(".csv$",".rds",path_save) )

# phase 0 - build the plot
# phase 1 - build the function
# phase 2 - isolate the prep step
# phase 3 - develop the print step
# phase 4 - serialize application
# phase 6 - place onto the canvas
  
# potential color picks
# ,palette_custom = c("TRUE"="red", "FALSE"="white") # deep purple
# ,palette_custom = c("TRUE"="#4B0092", "FALSE"="white") # deep purple
# ,palette_custom = c("TRUE"="#0C7BDC", "FALSE"="#FFC20A") # blue, yellow
# ,palette_custom = c("TRUE"="#D35FB7", "FALSE"="#FEFE62") # dar pink, light yellow
# ,palette_custom = c("TRUE"="#40B0A6", "FALSE"="#E1BE6A") # light brown, teal
# ,palette_custom = c("TRUE"="#4B0092", "FALSE"="#1AFF1A") # deep purple, acid green

