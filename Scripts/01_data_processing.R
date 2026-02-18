#### Tidying up the Boston Harbor time series data  ####
# Define the path to the directory containing your CSV files
files_path <- "Raw_data/hadISST_temp_data/" 

# List all CSV files in the specified directory
csv_files <- list.files(path = files_path, pattern = "*.csv", full.names = TRUE)

# Read and combine all CSV files into a single data frame
# map_dfr from purrr (part of tidyverse) is used to read each file and bind rows
df_combined <- csv_files %>% 
  map_dfr(read_csv)

df_combined %>% 
  janitor::clean_names() %>% 
  arrange(number_year, month_2) %>% 
  mutate(month_num = row_number(), 
         date = lubridate::as_date(paste(number_year, month_2, sep = "-"), format = "%Y-%m")) %>% 
  select(month_num, date, "mean_temp" = monthly_mean_mm) %>%
  write.csv(., file = "Output/Output_data/boston_harbor_temps.csv", row.names = F)

# Optionally, save the combined data to a new CSV file
# write_csv(df_combined, "path/to/save/combined_data.csv")


# Identifies all files for the respiration experiments 
resp_files = dir(path = "Raw_data/respiration_data_UTF8/")

tpc_rates = tibble()

for(i in resp_files){
  
  test_temp = parse_number(str_split_fixed(i, pattern = "_", n = 5)[4])
  test_date = paste0(str_split_fixed(i, pattern = "_", n = 5)[c(1,2,3)], collapse = "-")

  
  file_path <- paste("Raw_data/respiration_data_UTF8/", i, sep = "") # Replace with the actual path to your file
  lines <- readLines(file_path, skipNul = T)
  treatment_line <- grep("TREATMENTS", lines)
  calib_line <- grep("CALIBRATION", lines)
  start_line = grep("Date", lines)[2]
  
  ## Pull treatment IDs (in between "TREATMENTS" and "CALIBRATION")
  treatment_codes = read.csv(file_path,
                             sep = "\t",
                             skip = treatment_line, 
                             nrows = calib_line - treatment_line - 2) %>% 
    select("treatment_id" = ID, "treatment" = Name) %>% 
    mutate(treatment_id = if_else(treatment_id == "X", NA, treatment_id), 
           treatment_id = as.numeric(treatment_id)) %>% 
    filter(treatment != "")
  
  ## Pull well treatments and individual numbers (Three lines: wells, organisms, treatment ID)
  ind_numbers = read.csv(file_path,
                         sep = "\t",
                         skip = calib_line, 
                         nrows = 7, 
                         na.strings = "X") %>% 
    filter(Wells %in% c("Organisms")) %>% 
    select(Wells:D6) %>% 
    pivot_longer(cols = c(A1:D6), 
                 values_to = "individuals", 
                 names_to = "well_id") %>% 
    select(-Wells)
  
  treatment_ids = read.csv(file_path,
                           sep = "\t",
                           skip = calib_line, 
                           nrows = 7, 
                           na.strings = "X") %>% 
    filter(Wells %in% c("Treatment ID")) %>% 
    select(Wells:D6) %>% 
    pivot_longer(cols = c(A1:D6), 
                 values_to = "treatment_id", 
                 names_to = "well_id") %>% 
    select(-Wells) %>% 
    filter(!is.na(treatment_id)) %>% 
    left_join(treatment_codes) %>%  
    left_join(ind_numbers)
  
  oxy_data = read.csv(file_path,
                      sep = "\t",
                      skip = start_line-1) %>% 
    janitor::clean_names() %>%  
    separate_wider_delim(cols = relative_time_hh_mm_ss, 
                         delim = ":",
                         names = c("hour", "minute", "second")) %>% 
    mutate(timepoint = as.numeric(minute) + 60*as.numeric(hour) + as.numeric(second)/60) %>% 
    select(timepoint, ends_with("_oxygen")) %>% 
    pivot_longer(cols = (a1_oxygen:d6_oxygen), 
                 names_to = c("well_id", NA), 
                 values_to = "perc_oxy_sat", 
                 names_sep = "_oxygen") %>% 
    mutate(well_id = toupper(well_id))
  
  
  all_rates = data.frame()
  
  for(j in 1:dim(treatment_ids)[1]){
    
    select_well = treatment_ids$well_id[j]
    treatment = treatment_ids$treatment[j]
    ind_num = treatment_ids$individuals[j]
    
    well_data = oxy_data %>% 
      filter(well_id == select_well) %>% 
      select(timepoint, perc_oxy_sat)
    
    auto_rate = suppressMessages(auto_rate(well_data, plot = F, width = 0.2))$summary[1,] %>% 
      select(rate, rsq)
    
    rates = data.frame(
      well_id = select_well, 
      treatment = treatment,
      ind_num = ind_num,
      rate = auto_rate$rate, 
      rsq = auto_rate$rsq, 
      temp = test_temp
    )
    
    all_rates = bind_rows(all_rates, rates)
    
  }
  
  blanks = all_rates %>% 
    filter(treatment == "Blank") %>% 
    group_by(temp) %>% 
    summarise(blank_resp = mean(rate))
  
  resp_rates = all_rates %>% 
    filter(treatment != "Blank") %>% 
    mutate(rate = ((rate / ind_num) * -1) + blanks$blank_resp,
           date = test_date)
  
  tpc_rates = bind_rows(tpc_rates, resp_rates)
}

#tpc_rates = read.csv(file = "Output/Output_data/resp_data.csv")

write.csv(tpc_rates, "Output/Output_data/resp_data.csv", row.names = F)


