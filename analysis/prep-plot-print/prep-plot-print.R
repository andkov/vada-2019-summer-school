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

# ---- rmd-specific ----------------------------

# ---- load-data ---------------------------------------------------------------
# see ./data-unshared/contents.md for origin of the data
dto <- readRDS(path_file_input)
dto %>% pryr::object_size(); dto %>% class(); dto %>% names()

# ---- tweak-data ------------------------------

# to select target variables from each source 
# 1 - Clinical Outcomes at year 1
var_clinical_outcomes_1y <- c(
   "atopy_1y"             # Atopy at 1y (by SPT - reaction to any allergen)
  ,"wz_recurrent_1y"     # Recurrent wheeze at 1y: 2+ episodes (ZL 06-2015)	Binary
  ,"atopy_1y"            # Atopy at 1y (by SPT - reaction to any allergen)	Binary
  ,"atopy_food_1y"       # Sensitization to food allergen at 1y by SPT	Binary
  ,"atopy_nonfood_1y"    # Sensitization to non-food allergen at 1y by SPT	Binary
  ,"wtclass_1y"          # Weight Class at 1y (based on WFL z-score)	Categorical
  ,"ow_1y"               # Overweight at 1y (WFL z-score > 97%ile)	Binary
  ,"owrisk_1y"           # At risk for overweight at 1y (WFL z-score > 85%ile)	Binary
  ,"rapid_bmigrowth_12m" # >1 SD change in z-score from birth(BMI) to 12m(BMI)	Binary
  ,"rapid_wflgrowth_12m" # >1 SD change in z-score from birth(WFA) to 12m(WFL)	Binary
  ,"rapid_wgt_0to12m"    # Change in WHO zwgt >0.67	Binary
  ,"dzwgt_0to12m"        # Weight gain velocity from 0 to 12m (change in WHO weight z-score)
) 
# 2 - Covariates and Risk variables
var_covariates_risk <- c(
  "cat_pn"         # Dog as  pet  in your home in the last 12 months
  ,"dog_pn"        # Cat as  pet in your home in the last 12 months
  ,"ga_wks"        # Gestational Age in weeks
  ,"ga_wks_cat"    # Gestational Age categories: 1)34-36, 2)37-38, 3)39-40, 4=41+
  ,"sex"	         # Infant sex (0=Female, 1=Male) - from CHILD profile
  ,"older_sibs3"	 # Older siblings: 0, 1, 2+
  ,"lact_season"   # Season of Milk Collection: (1)Winter:Dec-Feb, (2)Spring:Mar-May, (3)Summer:Jun-Aug, (4)Fall:Sept-Nov
  ,"mom_wtclass3"	 # Moms weight class From mom_wtclass_best
  ,"prudent"	     # Prudent Diet
)
# 3 - Extra
var_extra <- c(
  "alcohol"          # alcohol (g)
  ,"caffeine"        # caffeine (mg)
)
# 4 - Feeding
var_feeding <- c(
  "bf_duration_imp"  # Breastfeeding Duration in Months (implied)
  ,"bf_12m"          # Breastfeeding at 12 Months (final)
  ,"diet_sampling"	 # Infant Diet at milk sample collection - BFS: Breast, Formula, Solids (CAPITAL=YES, small=no)
  ,"ebf_duration"	    # Duration of Exclusive Breastfeeding (Months)
  
)
#5 - Milk Components
var_milk_components <- c(
  "hmo_total"	       # Sum of all  HMOs
  ,"hmo_diversity"	 # HMO Diversity
)

# assemble a focal ds of the project (we dont' want to many variables distracting us)
ls_ds <- list(
  dto$data$clinical_outcomes_1y %>% dplyr::select(c("ids",var_clinical_outcomes_1y))
  ,dto$data$covariates_risk     %>% dplyr::select(c("ids",var_covariates_risk))
  ,dto$data$extra               %>% dplyr::select(c("ids",var_extra))
  ,dto$data$feeding             %>% dplyr::select(c("ids",var_feeding))
  ,dto$data$milk_components     %>% dplyr::select(c("ids",var_milk_components))
)
lapply(ls_ds, glimpse)
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

# to have a quick reference for the variables in focus (and their sources)
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
# ds %>% explore::explore_all(ncol = 5 )
ds %>% explore::describe()
ds %>% dplyr::glimpse()
# ---- inspect-data-2 ----------------------------
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


# ----- temp-explore ---------------------
ds %>% dplyr::select_(.dots = var_clinical_outcomes_1y) %>% 
  GGally::ggpairs(mapping = aes(fill = atopy_1y))+theme_minimal()

# 2
outcomes_covariates <- c(
  var_clinical_outcomes_1y
  ,setdiff(var_covariates_risk,c("cat_pn","dog_pn")) 
  ,"pets_home_12m"
)
ds %>% 
  dplyr::select_(.dots = outcomes_covariates) %>% 
  GGally::ggpairs(mapping = aes(fill = atopy_1y))+theme_minimal()

# 3
milk_feeding_extra <- c(
  var_milk_components, var_feeding, var_extra, "atopy_1y"
)
ds %>% 
  dplyr::select_(.dots = milk_feeding_extra) %>% 
  GGally::ggpairs(mapping = aes(fill = atopy_1y))+theme_minimal()

ds1 <- ds %>% 
  dplyr::select(
    hmo_diversity   
    ,prudent
    ,bf_duration_imp
    ,dzwgt_0to12m
    ,hmo_total
    ,atopy_1y   
    ,pets_home_12m   
    ,older_sibs3 
  ) %>% 
  GGally::ggpairs(mapping = aes(fill = atopy_1y))+theme_minimal()

# ---- inspect-data-3 ----------------------------
table(ds1$atopy_1y, ds1$pets_home_12m)

# ---- declare-components ----------------------
table(ds$older_sibs3,ds$pets_home_12m )
table(ds$lact_season,ds$older_sibs3 )
table(ds$ga_wks,ds$older_sibs3 )
table(ds$ga_wks_cat,ds$older_sibs3 )
# ---- inspect-data-2 --------------------------

# ---- inspect-data-3 --------------------------

# ---- phase-1-graph -----------------------
# now let us find ways to look at/in/with data
# the above analysis helps us to conceptualize available variables as:
ds %>% dplyr::glimpse()

#  I - DATA space

## MEASURE - hmo_diversity  ( 0 - 2,500) 
## MEASURE - hmo_total      ( 0 - 10 )
## DESIGN  - atopy_1y       ( yes, no)
## DESIGN  - pets_home_12m  ( none, dogs, cats, both )
## DESIGN  - older_sibs3    ( 0, 1, 2+ )


#  II - VISUALIZATION space 

## INTERNAL - horizontal - X - MEASURE - (hmo_diversity) 
## INTERNAL - vertical   - Y - MEASURE - (hmo_totatl)  
## INTERNAL - color      - Z - DESIGN  - (atopy_1y)
## EXTERNAL - horizontal - K - DESIGN  - (pets_home_12m)
## EXTERNAL - vertial    - P - DESIGN  - (older_sibs3)


# ---- phase-1-graph-1 --------------------------
# let us sketch the most basic graph in 3 internal dimensions
# notice that we isolate a single value on all the rest dimensions

# to add a few bells and whistles that aid quick evaluation:

# ---- phase-1-graph-2 --------------------------
# let us introduce external dimensions through faceting
# we will keep one of the external dimension constant and facet_wrap on the other


# ---- phase-1-graph-3 --------------------------
# now let facet_grid on both demension

# ---- phase-2-make_plot --------------------------
# suppose, we have settled on the graphical form `...` (immediately above), repeat below


# ---- phase-1-graph -------------------
# Let us sketch a graph that would map three dimension:
# (x) continuous, (y) continuous, (color) discrete

#1
g1 <- ds %>%
  tidyr::drop_na(.dots = c("older_sibs3")) %>%
  ggplot(
    aes(
      x      = hmo_total
      ,y     = wtg_velocity
      ,color = atopy_1y 
    )
  ) +
  geom_point()+
  facet_grid( pets_home_12m ~ older_sibs3 )+
  # facet_grid( pets_home_12m ~ older_sibs3 )+
  theme_minimal()
g1

# ---- phase-2-make_plot-1 --------------------------
# now let us re-express this plot as a custom function
make_plot_1_basic <- function(
  d
){
  # d <- ds # for testing and development
  g_out <- d %>%
     ggplot(
      aes(
        x      = hmo_total
        ,y     = wtg_velocity
        ,color = atopy_1y 
      )
    ) +
    geom_point()+
    theme_minimal()

  return(g_out)  
}
# how to use:
g <- ds %>% 
  # tidyr::drop_na(.dots = c("older_sibs3")) %>%
  # dplyr::filter(older_sibs3 == 0) %>%
  # dplyr::filter(pets_home_12m == "none") %>% 
  # notice that we keep some operations on the data outside of the function definition
  make_plot_1_basic()
g
# g +  facet_grid( pets_home_12m ~ older_sibs3 )

# ---- phase-2-make_plot-2 --------------------------
# We need our function to offer us a convenient way to:
# 1. Control what variables are mapped onto the three dimensions we use
# 2. Control how color is mapped onto the level of the categorical variable 

# if we were to pack everything into a single function we would get something like:
make_plot_1_packed <- function(
  d
  ,dim_horizontal
  ,dim_vertical  
  ,dim_color
  ,palette_custom = c("TRUE"="#1B9E77", "FALSE"="#D95F02")
){
  # to declare values for testing and development (within this function)
  # d <- ds 
  # dim_horizontal = "hmo_total"
  # dim_vertical   = "wtg_velocity"
  # dim_color      = "atopy_1y"
  # palette_custom = c("TRUE"="#1B9E77", "FALSE"="#D95F02")
  
  # data prep step
  d1 <- d %>% 
    tidyr::drop_na(.dots = dim_color) # to avoid clutter in labels
  custom_label_title = paste0(
    dim_horizontal," (spread by) ", dim_vertical, " (colored by) ", dim_color
  )
  custom_label_x     = toupper(dim_horizontal)
  custom_label_y     = toupper(dim_vertical)
  custom_label_color = toupper(dim_color)
  
  # plotting prep step
  # palette_custom = c("TRUE"="#1B9E77", "FALSE"="#D95F02") 

  # plotting step
  g_out <- d1 %>%
    ggplot(
      aes_string(
        x      = dim_horizontal # continuous  # hmo_total
        ,y     = dim_vertical   # continuous  # wtg_velocity
        ,color = dim_color      # categorical # atopy_1y 
      )
    ) +
    geom_point()+
    scale_color_manual(values = palette_custom)+
    theme_minimal()+
    labs(
      title  = custom_label_title
      ,x     = custom_label_x
      ,y     = custom_label_y
      ,color = custom_label_color 
    )
    # g_out # for viewing while developing
  return(g_out)  
}
# how to use
g <- ds %>% 
  make_plot_1_packed(
    dim_horizontal  = "hmo_total"
    ,dim_vertical   = "wtg_velocity"
    ,dim_color      = "atopy_1y"
  )# or:
# g <- ds %>% make_plot_1_packed("hmo_total","wtg_velocity","wz_recurrent_1y")

# applications: 
g <- ds %>% 
  make_plot_1_packed(
    dim_horizontal  = "hmo_total"
    # dim_horizontal  = "prudent"
    ,dim_vertical   = "wtg_velocity"
    ,dim_color      = "atopy_1y"
    # ,dim_color      = "wz_recurrent_1y"
    # ,dim_color      = "rapid_wgt_0to12m"
    # ,dim_color      = "wtclass_1y"
    # ,dim_color      = "wtg_velocity" # will produce error
    #http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
    # ,palette_custom = c("1-Normal" = "#1b9e77", "2-At Risk" = "#d95f02", "3-Overweight" = "#7570b3")
  ) 
g
# g +  facet_grid( pets_home_12m ~ older_sibs3 )


# as you notice, the function got bulkier due to operations needed
# to construct a reference vector for factor levels 
# these and other operations are typically best sourced out 
# to the `prep_data` function

# ---- phase-3-prep_data-1 ------------------------------

# let us construct a new `prep_data` function that would
# isolate the preparatory operations from the `make_plot` function
prep_data_plot_1 <- function(
  d
  ,dim_horizontal
  ,dim_vertical  
  ,dim_color
){
  # to declare values for testing and development (within this function)
  # d <- ds
  # dim_horizontal = "hmo_total"
  # dim_vertical   = "wtg_velocity"
  # dim_color      = "atopy_1y"
  # palette_custom = c("TRUE"="#1B9E77", "FALSE"="#D95F02")

  # data prep step
  d1 <- d %>% 
    tidyr::drop_na(.dots = dim_color) # to avoid clutter in labels
  custom_label_title = paste0(
    dim_horizontal," (spread by) ", dim_vertical, " (colored by) ", dim_color
  )
  custom_label_x     = toupper(dim_horizontal)
  custom_label_y     = toupper(dim_vertical)
  custom_label_color = toupper(dim_color)
  
  # to store objects for passing to the `make_plot` function
  l_support <- list()
  l_support[["data"]] <- d1
  l_support[["dimension"]] <- c(
    "horizontal" = dim_horizontal
    ,"vertical"  = dim_vertical
    ,"color"     = dim_color
  )
  l_support[["label"]] <- c(
     "x"      = custom_label_x
    ,"y"      = custom_label_y
    ,"color"  = custom_label_color
    ,"title"  = custom_label_title
  )
  
  lapply(l_support, class) # view contents
  # the make_plot funtion will rely on the structure and values in l_support

  return(l_support)
}
# how to use
l_support <- ds %>% 
  prep_data_plot_1(
    dim_horizontal  = "hmo_total"
    ,dim_vertical   = "wtg_velocity"
    ,dim_color      = "atopy_1y"
  )
l_support %>% print()

# now we can pass this curated object `l_support` to graphing function
# note that we need to adjust the function to accomodate a new input object
make_plot_1 <- function(
  l_support
  ,palette_custom = c("TRUE"="#1B9E77", "FALSE"="#D95F02") 
){
  # plotting prep step
  # palette_custom = c("TRUE"="#1B9E77", "FALSE"="#D95F02") 

    # plotting step
  g_out <- l_support[["data"]] %>%
    ggplot(
      aes_string(
        x      = l_support[["dimension"]]["horizontal"] # continuous  # hmo_total
        ,y     = l_support[["dimension"]]["vertical"]   # continuous  # wtg_velocity
        ,color = l_support[["dimension"]]["color"]      # categorical # atopy_1y 
      )
    ) +
    geom_point()+
    scale_color_manual(values = palette_custom)+
    theme_minimal()+
    labs(
      title  = l_support[["label"]]["title"]    
      ,x     = l_support[["label"]]["x"]    
      ,y     = l_support[["label"]]["y"]
      ,color = l_support[["label"]]["color"]
    )
  # g_out # for viewing while developing
  l_support[["graph"]]   <- g_out
  return(l_support)  
}
# how to use
l_support <- ds %>% 
  prep_data_plot_1(
    dim_horizontal  = "hmo_total"
    ,dim_vertical   = "wtg_velocity"
    ,dim_color      = "atopy_1y"
  ) %>% 
  make_plot_1()
l_support$graph %>% print()

# applications:
l_support <- ds %>% 
  prep_data_plot_1(
    dim_horizontal  = "hmo_total"
    ,dim_vertical   = "wtg_velocity"
    # ,dim_color      = "atopy_1y"
    # ,dim_color      = "rapid_wgt_0to12m"
    ,dim_color      = "wtclass_1y"
    # ,dim_color      = "wtg_velocity" # will produce error
  ) %>% 
  make_plot_1(
    #http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
    palette_custom = c("1-Normal" = "#1b9e77", "2-At Risk" = "#d95f02", "3-Overweight" = "#7570b3")
  )
l_support$graph %>% print()

# ---- phase-4-print_plot ---------------------------------

print_plot_1 <- function(
  l_support
  ,path_output_folder
  ,prefex     = NA
  ,graph_name = "auto"
  ,...
){
  if( graph_name == "auto" ){
    graph_name <- paste0(
      # should be replaced with features appropriate for analysis
      l_support$measure
      ,"-("
      ,l_support$set$sex %>% paste0(collapse = "-")
      ,")-("
      ,l_support$set$area %>% paste0(collapse = "-")
      ,")-("
      ,l_support$set$age_group %>% paste0(collapse = "-")
      ,")"
      ,collapse = "-"
    )
  }else{
    graph_name <- paste0(l_support$measure,"-", graph_name)
  }
  # add a label to distinguish a particular graph (first element in the file name)
  if( !is.na(prefex) ){ # inserts a PREFEX before the graph name
    (path_save_plot <- paste0(path_output_folder, prefex,"-",graph_name) )
  }else{
    ( path_save_plot <- paste0(path_output_folder, graph_name) )
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
  l_support$graph %>% print() # reach into the custom object we made for graphing
  dev.off() # close the device
  l_support[["path_plot"]] <- path_printed_plot
  return(l_support)
}
# how to use
l_support <- ds1 %>% 
  prep_data_plot_1(
    set_sex        = c("Females", "Males") 
    # set_sex        = c("Females", "Males", "Both sexes") 
    ,set_area      = c("Canada", "Alberta", "British Columbia") 
    ,set_age_group = c("1-19", "20-34", "80+","1+")
  ) %>% 
  make_plot_1(
    measure = "rate"
  ) %>% 
  print_plot_1(
    path_output_folder = "./analysis/scenario-3/prints/demo-1/"
    # ,prefex            = "attempt1"
    # ,graph_name        = "take1" # `auto` by default
# options added through `...` into the jpeg() function   
    ,width   = 1700
    ,height  = 500
    ,units   = "px"
    ,quality = 100
    ,res     = 200
  )
# ---- phase-4-print_plot-1 ---------------------------------
# notice that if I print the GRAPH by reaching into the `l_support` object
# if will be displayed according to the `fig.width`, `fig.height`, and `out.width`
# parameters specified in the chuck options (in the .Rmd file)
l_support$graph %>% print()

# ---- phase-4-print_plot-2 ---------------------------------
# if, however, we reach into the disk, we will recover the image generated
# with the dimensions and specs defined in the `print_plot` function
l_support$path_plot %>% jpeg::readJPEG() %>% grid::grid.raster()



# ---- dimensions ----------------------
# canvas size guide ( portrait orientation )

# Size           Width x Height (mm) Width x Height (in)  Aspect Ratio
# Half Letter      140 x 216           5.5 x  8.5          1: 1.55
# Letter           216 x 279           8.5 x 11.0          1: 1.29
# Legal            216 x 356           8.5 x 14.0          1: 1.65
# Junior Legal     127 x 203           5.0 x  8.0          1: 1.60
# Ledger/Tabloid   279 x 432          11.0 x 17.0          1: 1.55


# ---- phase-5-serialize ---------------------------------
# it often makes sense to genrate a series of plot to be explored manually

# GRAPH SERIES 1
path_target <- "./analysis/scenario-3/prints/series_1/"
provinces_to_pair     <- c("British Columbia", "Alberta", "Quebec")
age_groups_to_display <- c("1-19", "20-34", "35-49", "50-64","65-79","80+","+1")
# for each selected province create a comparison with Canada
ls_plot_series <- list()
for(province_i in provinces_to_pair){
  ls_plot_series[[province_i]]  <- ds1 %>%
    prep_data_plot_1(
      set_sex        = c("Females", "Males")
      ,set_area      = c("Canada", province_i)
      ,set_age_group = age_groups_to_display
    ) %>%
    make_plot_1(
      measure = "rate"
    ) %>%
    print_plot_1(
      path_output_folder = path_target
      # options added through `...` into the jpeg() function
      ,width   = 1700
      ,height  = 500
      ,units   = "px"
      ,quality = 100
      ,res     = 200
    )
}
saveRDS(ls_plot_series, paste0(path_target,"ls_plots.rds") )

# GRAPH SERIES 2
path_target <- "./analysis/scenario-3/prints/series_2/"
provinces_to_pair    <- c("British Columbia", "Alberta", "Quebec")
age_groups_to_display <- c( "+1")
# for each selected province create a comparison with Canada
ls_plot_series <- list()
for(province_i in provinces_to_pair){
  ls_plot_series[[province_i]]  <- ds1 %>%
    prep_data_plot_1(
      set_sex        = c("Males","Females")
      ,set_area      = c("Canada", province_i)
      ,set_age_group = c("20-34", "+1")
    ) %>%
    make_plot_1(
      measure = "rate"
    ) %>%
    print_plot_1(
      path_output_folder = path_target
      # options added through `...` into the jpeg() function
      ,width   = 900
      ,height  = 500
      ,units   = "px"
      ,quality = 100
      ,res     = 200
    )
}
saveRDS(ls_plot_series, paste0(path_target,"ls_plots.rds") )


# ---- phase-6-place_plot ---------------------------------
l_support <- readRDS("./analysis/scenario-3/prints/series_1/ls_plots.rds")
l_support[["British Columbia"]]$path_plot
l_support[["British Columbia"]]$path_plot %>% jpeg::readJPEG() %>% grid::grid.raster()

# ---- publish ---------------------------------------
# This chunk will publish the summative report
path_publish_report_1 <- "./analysis/scenario-3/scenario-3-ccdss.Rmd"
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
# phase 2 - isolate prep step
# phase 3 - isolate print step
# phase 4 - Serialize
# phase 6 - place into the canvas
  


