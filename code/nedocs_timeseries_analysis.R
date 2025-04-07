#/////////////////////////////////////
# title: "NEDOCS Time Series Analysis"
# author: "Druthi Palle"
#/////////////////////////////////////

# Load packages and functions
source("get_packages_used.R")
source("analysis_functions.R")
source("plotting_functions.R")

# Make output folders if they don't exist
output_dir = "../produced_data/hourly_nedocs"
fig_dir = "../figures"
if(!dir.exists(output_dir)) dir.create(output_dir)
if(!dir.exists(fig_dir)) dir.create(fig_dir)

# Command line arguments
command_line = FALSE
if (command_line) {
  args            = commandArgs(TRUE)
  hosp_names      = as.character(strsplit(args[1], ",")[[1]])
  rerun           = as.logical(args[2])
  ic(hosp_names)
} else {
  hosp_names = c("1", "2", "3", "4", "5", "6")
  hosp_name  = hosp_names[1]
  rerun      = TRUE
}


#### Data Preprocessing #### 
# Read in data file
nedocs_input = "../input_data/sample_NEDOCS_data.csv"
nedocs_og_df = read_csv(nedocs_input)


NEDOCS_data <- nedocs_og_df %>%
  filter(RESOURCE %in% hosp_names) %>%
  mutate(DURATION_HOURS = as.numeric(DURATION_HOURS),
         NEDOCS_SAT_SCORE = as.numeric(NEDOCS_SAT_SCORE)) %>%
  select(-NEDOCS_VALUE_CAT, -COMMENTS)

#### Produce Hourly Data ####
# Compute hourly sequences for each resource and assign NEDOCS Sat Score

for (hosp_name in hosp_names) {
  ic(hosp_names)
  hosp_data <- NEDOCS_data %>%
    filter(RESOURCE == hosp_name)
  
  hourly_data_path = file.path(output_dir, paste0("hourly_nedocs_timeseries_", hosp_name, ".csv"))
  ic(hourly_data_path)
  hourly_plot_path = file.path(fig_dir, paste0("hourly_nedocs_", hosp_name, ".png"))
  ic(hourly_plot_path)
  
  # Generate hourly sequences
  if (!file.exists(hourly_data_path) | rerun==TRUE) {
    ic(paste("Create ", hourly_data_path))
    hourly_sequences <- generate_and_assign_scores(hosp_data)
    write.csv(hourly_sequences %>%
                mutate(across(everything(), as.character)), 
              hourly_data_path, 
              row.names = FALSE,
              fileEncoding = "UTF-8"
    )
    
    # Generate hourly NEDOCS plot
    hourly_plot <- hourly_NEDOCS_plot(hourly_sequences)
    ggsave(hourly_plot_path, hourly_plot, width = 25, height = 20, dpi = 300)
  } 
}

#### Produce Daily Data ####
output_dir = "../produced_data/"

daily_data_path <- file.path(output_dir, "daily_nedocs_timeseries.csv")
daily_plot_path = file.path(fig_dir, "daily_nedocs.png")

daily_data <- process_hourly_to_daily(input_dir = "../produced_data/hourly_nedocs/", output_file = daily_data_path)

daily_plot <- generate_nedocs_heatmap(daily_data)
ggsave(daily_plot_path, daily_plot, width = 35, height = 20, dpi = 300)
