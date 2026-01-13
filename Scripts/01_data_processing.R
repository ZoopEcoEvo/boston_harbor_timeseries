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


