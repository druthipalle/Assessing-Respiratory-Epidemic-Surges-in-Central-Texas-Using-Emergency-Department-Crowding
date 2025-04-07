#/////////////////////////////////////
# title: "Load Required R Packages"
# author: "Druthi Palle"
#/////////////////////////////////////

# Check if 'pacman' package is installed; install it if missing
if (!requireNamespace("pacman", quietly = TRUE)) 
  install.packages("pacman")

# Load required libraries using 'pacman'
pacman::p_load(
  tidyverse,     # Data manipulation and visualization (includes dplyr, ggplot2, etc.)
  openxlsx,      # Read/write Excel files
  TTR,           # Technical Trading Rules, includes SMA() for smoothing time series
  lubridate,     # Date and time manipulation
  icecream,      # For debugging and print statements
  zoo,           # Rolling functions for time series analysis
  GGally         # Additional visualization tools including ggpairs()
)