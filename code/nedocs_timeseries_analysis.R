#/////////////////////////////////////
# title: "NEDOCS Time Series Analysis"
# author: "Druthi Palle"
# description: This script reads emergency department (ED) crowding data (NEDOCS),
# processes it into hourly and daily time series per hospital,
# saves processed data, and creates corresponding visualizations.
#/////////////////////////////////////

# Load packages and user-defined functions
source("get_packages_used.R")       # Loads necessary packages for analysis
source("analysis_functions.R")      # Custom functions for processing and analysis
source("plotting_functions.R")      # Custom functions for generating plots

# Make output folders if they don't exist
output_dir = "../produced_data/hourly_nedocs"
fig_dir = "../figures"

if(!dir.exists(output_dir)) dir.create(output_dir)
if(!dir.exists(fig_dir)) dir.create(fig_dir)

# Determine whether script is being run from command line
command_line = FALSE

# Handle arguments if script is run from the command line
if (command_line) {
  args       = commandArgs(TRUE)
  hosp_names = as.character(strsplit(args[1], ",")[[1]])  # Hospital IDs
  rerun      = as.logical(args[2])                        # Whether to rerun processing
  ic(hosp_names)
} else {
  hosp_names = c("1", "2", "3", "4", "5", "6")  # Default hospital IDs
  hosp_name  = hosp_names[1]                    # First hospital ID
  rerun      = TRUE                             # Force rerun of data processing
}

#### Data Preprocessing #### 
# Read in the raw NEDOCS dataset
nedocs_input = "../input_data/sample_NEDOCS_data.csv"
nedocs_og_df = read_csv(nedocs_input)

# Filter and clean dataset
NEDOCS_data <- nedocs_og_df %>%
  filter(RESOURCE %in% hosp_names) %>%                          # Keep only selected hospitals
  mutate(DURATION_HOURS = as.numeric(DURATION_HOURS),           # Convert to numeric
         NEDOCS_SAT_SCORE = as.numeric(NEDOCS_SAT_SCORE)) %>%
  select(-NEDOCS_VALUE_CAT, -COMMENTS)                          # Drop unnecessary columns

#### Produce Hourly Data ####

# Loop through each hospital and generate hourly NEDOCS data and plot
for (hosp_name in hosp_names) {
  ic(hosp_names)
  
  # Subset data for current hospital
  hosp_data <- NEDOCS_data %>%
    filter(RESOURCE == hosp_name)
  
  # Define file paths for saving processed data and plot
  hourly_data_path = file.path(output_dir, paste0("hourly_nedocs_timeseries_", hosp_name, ".csv"))
  ic(hourly_data_path)
  
  hourly_plot_path = file.path(fig_dir, paste0("hourly_nedocs_", hosp_name, ".png"))
  ic(hourly_plot_path)
  
  # Generate and save data/plot if rerun is TRUE or file doesn't exist
  if (!file.exists(hourly_data_path) | rerun==TRUE) {
    ic(paste("Create ", hourly_data_path))
    
    # Generate hourly sequences with scores assigned to each hour
    hourly_sequences <- generate_and_assign_scores(hosp_data)
    
    # Save hourly data
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
# Reset output directory to main produced_data folder
output_dir = "../produced_data/"

# Define file paths
daily_data_path <- file.path(output_dir, "daily_nedocs_timeseries.csv")
daily_plot_path = file.path(fig_dir, "daily_nedocs.png")

# Aggregate hourly data into daily timeseries
daily_data <- process_hourly_to_daily(input_dir = "../produced_data/hourly_nedocs/", output_file = daily_data_path)

# Generate and save daily heatmap plot
daily_plot <- generate_nedocs_heatmap(daily_data)
ggsave(daily_plot_path, daily_plot, width = 35, height = 20, dpi = 300)
