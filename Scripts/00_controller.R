# Load in required packages
library(rmarkdown)
library(tidyverse)

#Determine which scripts should be run
process_data = F #Runs data analysis 
make_report = F #Runs project summary
knit_manuscript = F #Compiles manuscript draft

############################
### Read in the RAW data ###
############################

if(process_data == T){
  source(file = "Scripts/01_data_processing.R")
}

##################################
### Read in the PROCESSED data ###
##################################

bharb_temps = read.csv(file = "Output/Output_data/boston_harbor_temps.csv")
trait_data = readr::read_csv(list.files(path = "Raw_data/trait_data/", 
                                        pattern = "*.csv", 
                                        full.names = TRUE),
                             show_col_types = FALSE) %>% 
  drop_na(species) %>% 
  mutate(species = fct_reorder(species, ctmax, mean, .desc = T),
         collection_datetime = as_datetime(collection_datetime),
         experiment_datetime = as_datetime(experiment_datetime)) 

if(make_report == T){
  render(input = "Output/Reports/report.Rmd", #Input the path to your .Rmd file here
         #output_file = "report", #Name your file here if you want it to have a different name; leave off the .html, .md, etc. - it will add the correct one automatically
         output_format = "all")
}

##################################
### Read in the PROCESSED data ###
##################################

if(knit_manuscript == T){
  render(input = "Manuscript/manuscript_name.Rmd", #Input the path to your .Rmd file here
         output_file = paste("dev_draft_", Sys.Date(), sep = ""), #Name your file here; as it is, this line will create reports named with the date
         #NOTE: Any file with the dev_ prefix in the Drafts directory will be ignored. Remove "dev_" if you want to include draft files in the GitHub repo
         output_dir = "Output/Drafts/", #Set the path to the desired output directory here
         output_format = "all",
         clean = T)
}
