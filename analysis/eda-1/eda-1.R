# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md", figure)
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
library(magrittr) # pipes
library(dplyr)    # disable when temp lines are removed
library(ggplot2)  # graphs
library(ggpubr)   # documents
  
# ---- declare-globals ---------------------------------------------------------
path_file_input <- "./data-unshared/derived/0-greeted.rds"
html_flip <- FALSE
# ---- load-data ---------------------------------------------------------------
dto      <- readRDS(path_file_input)
dto %>% pryr::object_size(); dto %>% class(); dto %>% names()

# assign aliases for this report
# ----- custom-functions --------------------------------------

# ---- tweak-data ---------------------------------------------------------------

# to select target variables from each source 
# 1 - Clinical Outcomes at year 1
var_clinical_outcomes_1y <- c(
  "atopy_1y"       # Atopy at 1y (by SPT - reaction to any allergen)
  ,"wtclass_1y"    # Weight Class at 1y (based on WFL z-score)
  ,"dzwgt_0to12m"  # Weight gain velocity from 0 to 12m (change in WHO weight z-score)
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

)
#5 - Milk Components
var_milk_components <- c(
 "hmo_total"	       # Sum of all  HMOs
 ,"hmo_diversity"	   # HMO Diversity
 ,"hmo_evenness"	   # HMO Evenness
)

ls_ds <- list(
  dto$data$clinical_outcomes_1y %>% dplyr::select(c("ids",var_clinical_outcomes_1y))
  ,dto$data$covariates_risk     %>% dplyr::select(c("ids",var_covariates_risk))
  ,dto$data$extra               %>% dplyr::select(c("ids",var_extra))
  ,dto$data$feeding             %>% dplyr::select(c("ids",var_feeding))
  ,dto$data$milk_components     %>% dplyr::select(c("ids",var_milk_components))
  
)
lapply(ls_ds, class)
# 
# dto$data$clinical_outcomes_1y %>% explore::explore_all(ncol = 5)                  
# dto$data$covariates_risk %>% ncol()
# dto$data$covariates_risk      %>%  dplyr::select(1:30) %>% explore::explore_all(ncol = 6)
# dto$data$covariates_risk      %>%  dplyr::select(31:56) %>% explore::explore_all(ncol = 6)
# # dto$data$extra                %>% explore::explore_all(ncol = 5)                  
# dto$data$feeding              %>%  dplyr::select(1:30) %>% explore::explore_all(ncol = 6)            
# dto$data$feeding              %>%  dplyr::select(31:56) %>% explore::explore_all(ncol = 6)            
# dto$data$milk_components      %>% dplyr::select(1:20) %>% explore::explore_all(ncol = 5)                     
# dto$data$milk_components      %>% dplyr::select(21:40) %>% explore::explore_all(ncol = 5)                     
# dto$data$milk_components      %>% dplyr::select(41:60) %>% explore::explore_all(ncol = 5)                     
# dto$data$milk_components      %>% dplyr::select(61:78) %>% explore::explore_all(ncol = 5)                     
#                   
# ncol(dto$data$milk_components)

dto$data$covariates_risk %>% 
  dplyr::select(dplyr::matches("^PNDIET|^Prudent|^Western")) %>% 
  explore::explore_all(ncol = 4)

dto$data$covariates_risk %>% 
  dplyr::select(dplyr::matches("^PNDIET|^Prudent|^Western")) %>% 
  GGally::ggpairs()

# combine list of ds into a single ds
ds <- base::Reduce(dplyr::full_join, ls_ds)
# ds %>% explore::explore( ) # shiny app to view data and distributions
ds %>% explore::explore_all(ncol = 5 ) # fraph with distribution of each variable
ds %>% explore::describe()


ds %>% GGally::ggpairs()






# ----- basic-questions -------------------------------------------------

#How many counties and zipcodes were engaged by the program?
#How many distincts training types?
#How many individuals received training?
ds %>% 
  dplyr::group_by(region) %>% 
  dplyr::summarize(
    n_counties             =  dplyr::n_distinct(county)
    ,n_zipcodes            =  dplyr::n_distinct(zipcode)
    ,n_training_types      =  dplyr::n_distinct(type_training)
    ,total_persons_trained = sum(na.omit(n_trained))
  )

ds %>% 
  dplyr::group_by(audience, region) %>% 
  dplyr::summarize(
    n_counties             =  dplyr::n_distinct(county)
    ,n_zipcodes            =  dplyr::n_distinct(zipcode)
    ,n_training_types      =  dplyr::n_distinct(type_training)
    ,total_persons_trained = sum(na.omit(n_trained))
    )

dt1 <- ds %>% 
  dplyr::group_by(region, county) %>% 
  dplyr::summarize(
    n_zipcodes             =  dplyr::n_distinct(zipcode)
    ,n_training_types      =  dplyr::n_distinct(type_training)
    ,total_persons_trained = sum(na.omit(n_trained))
    ) 

dt1 %>%  neat()

g1 <- dt1 %>% 
  tidyr::gather(
    "measure"
    ,"count"
    , c("n_zipcodes","n_training_types","total_persons_trained")
  ) %>% 
  ggplot(aes(x=county, y = count, fill = measure ))+
  geom_bar(stat = "identity")+
  coord_flip()+
  # facet_grid(region ~ measure )+
  facet_grid(region ~ measure,scales = "free")+
  theme_minimal()+
  theme( legend.position = "none")
g1


#########################


dt2 <- ds %>% 
  dplyr::group_by(region, county, audience) %>% 
  dplyr::summarize(
    n_zipcodes             =  dplyr::n_distinct(zipcode)
    ,n_training_types      =  dplyr::n_distinct(type_training)
    ,total_persons_trained = sum(na.omit(n_trained))
  ) 

dt2 %>%  neat()

# BOTH audiences
g2 <- dt2 %>% 
  tidyr::gather(
    "measure"
    ,"count"
    , c("n_zipcodes","n_training_types","total_persons_trained")
  ) %>% 
  dplyr::mutate(
    measure = factor(measure, levels = c(
      "n_zipcodes"
      ,"n_training_types"
      ,"total_persons_trained"))
    ,audience = factor(audience, levels = c(
      "community"
      ,"professionals"
      ,NA
    ))
  ) %>%    
  ggplot(aes(x=county, y = count, color = audience, fill = audience ))+
  scale_fill_manual(values = c("professionals" = "red", "community" = NA))+
  scale_color_manual(values = c("professionals" = NA, "community" = "black"))+
  geom_bar(stat = "identity", position = "identity", alpha = .5)+
  coord_flip()+
  # facet_grid(region ~ measure )+
  facet_grid(region ~ measure, scales = "free")+
  theme_minimal()
g2

# SINGLE audience
g3 <- dt2 %>% 
  dplyr::filter(audience == "professionals") %>%
  # dplyr::filter(audience == "community") %>%
  tidyr::gather(
    "measure"
    ,"count"
    , c("n_zipcodes","n_training_types","total_persons_trained")
  ) %>% 
  dplyr::mutate(
    measure = factor(measure, levels = c(
      "n_zipcodes"
      ,"n_training_types"
      ,"total_persons_trained")) 
  ) %>%  
  # dplyr::arrange(county) %>% 
  ggplot(aes(x=county, y = count, fill = measure, pattern = region ))+
  # ggplot(aes(x=county, y = count ))+
  geom_bar(stat = "identity", alpha = .5)+
  geom_text( aes(label = count), vjust = 0.2)+
  coord_flip()+
  facet_grid(region ~ measure, scales = "free")+
  theme_minimal()+
  theme(
    legend.position = "none"
  )
g3


# Experimental: want to make bars the same width
dt4 <- dt2 %>% 
  # dplyr::filter(audience == "professionals") %>%
  dplyr::filter(audience == "community") %>%
  tidyr::gather(
    "measure"
    ,"count"
    , c("n_zipcodes","n_training_types","total_persons_trained")
  ) %>% 
  dplyr::mutate(
    measure = factor(measure, levels = c(
      "n_zipcodes"
      ,"n_training_types"
      ,"total_persons_trained"))
    ,county_region = paste0(county,"-",toupper(region))
    ,region_county = paste0(toupper(region),"-",county)
  ) 

cr_levels <- dt4 %>% 
  # dplyr::arrange(county, region) %>% 
  dplyr::arrange(region, county) %>% 
  dplyr::ungroup() %>% 
  dplyr::distinct(county_region ) %>% 
  as.list() %>% unlist() %>% as.character()

g4 <- dt4 %>% 
  dplyr::mutate(
    county_region = factor(county_region,levels = cr_levels)
    ,county_region = factor(county_region, levels = rev(levels(county_region)))
  ) %>% 
  ggplot(aes(x=county_region, y = count, fill = measure ))+
  # ggplot(aes(x=region_county, y = count, fill = measure ))+
  geom_bar(stat = "identity", alpha = .5)+
  geom_text( aes(label = count), vjust = 0.2)+
  coord_flip()+
  facet_grid(. ~ measure, scales = "free")+
  theme_minimal()+
  theme(
    legend.position = "none"
    ,axis.text.y = element_text(hjust = 1)
    ,axis.title.y = element_blank()
  )
g4
# ---- basic-graph -------------------------------------------------------

g1 <- ds %>% 
  ggplot2::ggplot(aes(x = date, y = ))

# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------

# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/gls-activity/gls-activity-1.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



