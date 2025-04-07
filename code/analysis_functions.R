#/////////////////////////////////////
# title: "NEDOCS Analysis Functions"
# author: "Druthi Palle"
#/////////////////////////////////////

#//////////////////////////////////////////
#' Select and rename columns in NEDOCS data
#'
#' This function selects specific columns from a NEDOCS dataset and renames them according to a predefined list.
#'
#' @param NEDOCS_data A data frame containing NEDOCS data with columns to be selected and renamed.
#' 
#' @return A data frame with selected columns and new names.
#'
#' @examples
#' \dontrun{
#' NEDOCS_data <- select_and_rename_NEDOCS_columns(NEDOCS_data)
#' }
select_and_rename_NEDOCS_columns <- function(NEDOCS_data) {
  new_names <- c(
    "RESOURCE", "START_DATE", "END_DATE", "DURATION_HOURS", 
    "NEDOCS_SAT_SCORE", "NEDOCS_VALUE_CAT", "ED_BEDS_A", 
    "INPATIENT_BEDS_B", "ED_PATIENTS_C", 
    "CRITICAL_CARE_PATIENTS_D", "LONGEST_ED_ADMIT_E", 
    "ED_ADMITS_F", "LAST_DOOR_TO_BED_TIME_G", "COMMENTS"
  )
  
  NEDOCS_data <- NEDOCS_data %>%
    select(
      `Resource`, `Start Date`, `End Date`, `Duration (Hours)`, 
      `NEDOCS Sat Score`, `NEDOCS Value`, `ED Beds`, 
      `Inpatient Beds`, `ED Patients`, 
      `Critical Care Patients`, `Longest ED Admit`, 
      `ED Admits`, `Last Door-to-bed Time`, `Comments`
    ) %>%
    rename_with(~new_names, .cols = everything())
  
  return(NEDOCS_data)
} # end select_and_rename_NEDOCS_columns

#///////////////////////////////////////////
#' Generate hourly sequences for NEDOCS data
#'
#' This function rounds start and end dates to the nearest hour, calculates the minimum and maximum dates for each resource,
#' and generates an hourly sequence from the earliest to the latest date for each resource.
#'
#' @param NEDOCS_data A data frame containing NEDOCS data
#'
#' @return A data frame with hourly sequences for each resource. The last row is removed to account for one additional hour value.
#'
#' @examples
#' \dontrun{
#' hourly_sequences <- generate_hourly_sequences(NEDOCS_data)
#' }
generate_hourly_sequences <- function(NEDOCS_data) {
  hourly_sequences <- NEDOCS_data %>%
    mutate(
      START_DATE = floor_date(START_DATE, unit = "hour"),  # Round start dates down to the nearest hour
      END_DATE = floor_date(END_DATE, unit = "hour")       # Round end dates down to the nearest hour
    ) %>%
    group_by(RESOURCE) %>%
    summarize(
      min_date = floor_date(min(START_DATE), "day"),  # Calculate earliest start date for each resource
      max_date = ceiling_date(max(END_DATE), "day")   # Calculate latest end date for each resource
    ) %>%
    rowwise() %>%
    mutate(hour_seq = list(seq(min_date, max_date, by = "hour"))) %>%  # Generate a sequence of hours from min_date to max_date for each resource
    unnest(hour_seq) %>%
    select(RESOURCE, HOUR = hour_seq)
  
  # This might be better to remove days that don't have 24hrs
  hourly_sequences <- hourly_sequences[-nrow(hourly_sequences), ]  # Remove the last row due to one additional hour value
  
  return(hourly_sequences)
} # end generate_hourly_sequences

#' Assign NEDOCS Sat Score based on resource and hour
#'
#' This function assigns the NEDOCS Saturation Score to a resource for a specific hour by checking if the hour falls within the start and end times of the resource.
#' It handles cases where multiple intervals overlap by returning the score from the first matching interval.
#'
#' @param NEDOCS_data A data frame containing NEDOCS data with columns such as `RESOURCE`, `START_DATE`, `END_DATE`, and `NEDOCS_SAT_SCORE`.
#' @param resource The resource identifier (e.g., a hospital or department) for which the score is to be assigned.
#' @param hour A specific hour (POSIXct or datetime) to check against the start and end times in the data.
#'
#' @return The NEDOCS Saturation Score for the specified resource and hour. Returns NA if no score is available for that hour.
#'
#' @examples
#' \dontrun{
#' nedocs_score <- assign_nedocs_score(NEDOCS_data, "Hospital A", as.POSIXct("2024-10-07 12:00:00"))
#' }
assign_nedocs_score <- function(NEDOCS_data, resource, hour) {
  within_interval <- NEDOCS_data %>%
    filter(RESOURCE == resource, START_DATE <= hour, END_DATE >= hour)
  
  if (nrow(within_interval) == 1) {
    return(within_interval$NEDOCS_SAT_SCORE) 
  } else if (nrow(within_interval) > 1) {  # Overlapping intervals - return score from first row 
    return(within_interval$NEDOCS_SAT_SCORE[1])
  } else {  # No score available for that hour                       
    return(NA)
  }
} # end assign_nedocs_score

#' Generate hourly sequences and assign NEDOCS Sat Score
#'
#' This function generates hourly sequences for each resource, then assigns the NEDOCS Saturation Score for each resource and hour using the `assign_nedocs_score` function.
#'
#' @param NEDOCS_data A data frame containing NEDOCS data with columns such as `RESOURCE`, `START_DATE`, `END_DATE`, and `NEDOCS_SAT_SCORE`.
#'
#' @return A data frame with hourly sequences and the corresponding NEDOCS Saturation Score for each resource and hour.
#'
#' @examples
#' \dontrun{
#' hourly_sequences <- generate_and_assign_scores(NEDOCS_data)
#' }
generate_and_assign_scores <- function(NEDOCS_data) {
  
  # Generate hourly sequences
  hourly_sequences <- generate_hourly_sequences(NEDOCS_data)
  
  # Assign NEDOCS Sat Score using pmap
  hourly_sequences <- hourly_sequences %>%
    mutate(
      NEDOCS_SAT_SCORE = pmap_dbl(
        list(RESOURCE, HOUR),
        ~ assign_nedocs_score(NEDOCS_data, ..1, ..2)
      ),
      DATE = as.Date(HOUR),  # Extract Date part
      HOUR = hour(HOUR)      # Extract Hour part
    ) %>%
    rename(DATE = DATE, HOUR = HOUR) %>%
    arrange(RESOURCE, DATE, HOUR) %>%
    group_by(RESOURCE) %>%
    fill(NEDOCS_SAT_SCORE, .direction = "down") %>%  # Fill down for missing values
    fill(NEDOCS_SAT_SCORE, .direction = "up") %>%   # Fill up for missing values
    ungroup() %>%
    mutate(
      # Create NEDOCS Value and ED Status Columns 
      NEDOCS_VALUE = sapply(NEDOCS_SAT_SCORE, function(score) {
        if (is.na(score)) {
          return(NA)
        } else if (score >= 0 & score <= 50) {
          return("Green")
        } else if (score > 50 & score <= 100) {
          return("Yellow")
        } else if (score > 100 & score <= 140) {
          return("Orange")
        } else if (score > 140 & score <= 180) {
          return("Red")
        } else if (score > 180) {
          return("Black")
        }
      }),
      ED_STATUS = sapply(NEDOCS_VALUE, function(color) {
        color_to_status <- list(
          "Green" = "Normal",
          "Yellow" = "Busy",
          "Orange" = "Overcrowded",
          "Red" = "Severe Overcrowding",
          "Black" = "Disaster"
        )
        return(color_to_status[[color]])
      })
    ) %>%
    select(RESOURCE, DATE, HOUR, NEDOCS_SAT_SCORE, NEDOCS_VALUE, ED_STATUS)
  
  return(hourly_sequences)
} # end generate_and_assign_scores

#/////////////////////////////////////////// 
#' Generate daily sequences for NEDOCS data 
#' 
#' This function processes hourly NEDOCS data by summarizing the NEDOCS SAT score 
#' for each resource and date. It calculates the average NEDOCS SAT score per day 
#' for each resource across multiple hourly data files, and then combines all the 
#' daily summaries into a single data frame. The output is saved to a specified CSV file.
#' 
#' @param input_dir A character string specifying the path to the directory containing 
#'                  the hourly NEDOCS CSV files.
#' @param output_file A character string specifying the path to save the resulting daily 
#'                    NEDOCS data as a CSV file.
#' 
#' @return A data frame with the daily average NEDOCS SAT scores for each resource, 
#'         combined across all hourly data files.
#' 
#' @examples 
#' \dontrun{
#' process_hourly_to_daily(input_dir = "produced_data/hourly_nedocs/", 
#'                         output_file = "produced_data/daily_nedocs_timeseries.csv")
#' }
process_hourly_to_daily <- function(input_dir, output_file) {
  
  all_hourly_sequences <- list.files(path = input_dir, pattern = "hourly_nedocs_timeseries.*\\.csv$", full.names = TRUE)
  
  daily_sequences <- list()
  
  for (sequence in all_hourly_sequences) {
    hourly_sequence <- read_csv(sequence)
    
    daily_summary <- hourly_sequence %>%
      group_by(RESOURCE, DATE) %>%
      summarise(AVG_NEDOCS_SAT_SCORE = mean(NEDOCS_SAT_SCORE, na.rm = TRUE), .groups = 'drop')
    
    daily_sequences <- append(daily_sequences, list(daily_summary))
  }
  
  all_daily_sequences <- bind_rows(daily_sequences)
  
  # Function to assign NEDOCS value based on the score
  get_nedocs_value <- function(score) {
    if (is.na(score)) {
      return(NA)
    } else if (score >= 0 & score <= 50) {
      return("Green")
    } else if (score > 50 & score <= 100) {
      return("Yellow")
    } else if (score > 100 & score <= 140) {
      return("Orange")
    } else if (score > 140 & score <= 180) {
      return("Red")
    } else if (score > 180) {
      return("Black")
    }
  }
  
  # Mapping NEDOCS value to ED status
  color_to_status <- list(
    "Green" = "NORMAL",
    "Yellow" = "BUSY",
    "Orange" = "OVERCROWDED",
    "Red" = "SEVERE OVERCROWDING",
    "Black" = "DISASTER"
  )
  
  get_ed_status <- function(color) {
    return(color_to_status[[color]])
  }
  
  all_daily_sequences <- all_daily_sequences %>%
    mutate(
      NEDOCS_VALUE = sapply(AVG_NEDOCS_SAT_SCORE, get_nedocs_value),
      ED_STATUS = sapply(NEDOCS_VALUE, get_ed_status)
    )
  
  write.csv(all_daily_sequences, output_file, row.names = FALSE)
  
  return(all_daily_sequences)
} # end process_hourly_to_daily

#/////////////////////////////////////////////////////////////////////////////////////////////////
#' Detects anomalies in the hospital bed data by analyzing changes in `ED_BEDS_A` and 
#' `INPATIENT_BEDS_B` columns and replaces anomalies with the mean of the neighboring values.
#' 
#' @param nedocs_df Data frame containing `START_DATE`, `RESOURCE`, `ED_BEDS_A`, and 
#'                  `INPATIENT_BEDS_B`. If `NULL`, reads from a default CSV path.
#' @param ed_thresh Numeric threshold for detecting anomalies in `ED_BEDS_A` (default is 10), e.g. 1 order magnitude difference.
#' @param ip_thresh Numeric threshold for detecting anomalies in `INPATIENT_BEDS_B` (default is 100), e.g. 2 order magnitude difference.
#' @return A data frame with anomalies detected and cleaned columns (`ED_BEDS_A_clean` 
#'         and `INPATIENT_BEDS_B_clean`).
#' @examples
#' detect_replace_anomalies(nedocs_df, ed_thresh = 10, ip_thresh = 100)
detect_replace_anomalies <- function(
    nedocs_df = NULL, 
    ed_thresh = 10, 
    ip_thresh = 100
) {
  
  if (is.null(nedocs_df)) {
    nedocs_df <- read_csv("input_data/sample_NEDOCS_data.csv")
  }
  
  # Detect anomalies in each hospital bed sizes reported
  anomalies <- nedocs_df %>%
    arrange(RESOURCE, START_DATE) %>%
    group_by(RESOURCE) %>%
    mutate(
      # Calculate forward and backward ratios
      ED_BEDS_A_ratio_prev = ED_BEDS_A / lag(ED_BEDS_A),
      ED_BEDS_A_ratio_next = lead(ED_BEDS_A) / ED_BEDS_A,
      INPATIENT_BEDS_B_ratio_prev = INPATIENT_BEDS_B / lag(INPATIENT_BEDS_B),
      INPATIENT_BEDS_B_ratio_next = lead(INPATIENT_BEDS_B) / INPATIENT_BEDS_B,
      
      # Label anomalies for INPATIENT_BEDS_B
      INPATIENT_BEDS_B_anomaly = ifelse(
        (INPATIENT_BEDS_B_ratio_prev >= ip_thresh | INPATIENT_BEDS_B_ratio_prev <= (1 / ip_thresh)) &
          (INPATIENT_BEDS_B_ratio_next >= ip_thresh | INPATIENT_BEDS_B_ratio_next <= (1 / ip_thresh)),
        TRUE, FALSE
      ),
      # Label anomalies for ED_BEDS_A
      ED_BEDS_A_anomaly = ifelse(
        (ED_BEDS_A_ratio_prev >= ed_thresh | ED_BEDS_A_ratio_prev <= (1 / ed_thresh)) &
          (ED_BEDS_A_ratio_next >= ed_thresh | ED_BEDS_A_ratio_next <= (1 / ed_thresh)),
        TRUE, FALSE
      )
    ) %>%
    ungroup() %>%
    mutate(
      ED_BEDS_A_clean = ifelse(
        ED_BEDS_A_anomaly == TRUE, 
        mean(c(lag(ED_BEDS_A), lead(ED_BEDS_A)), na.rm = TRUE), 
        ED_BEDS_A
      ),
      INPATIENT_BEDS_B_clean = ifelse(
        INPATIENT_BEDS_B_anomaly == TRUE, 
        mean(c(lag(INPATIENT_BEDS_B), lead(INPATIENT_BEDS_B)), na.rm = TRUE), 
        INPATIENT_BEDS_B
      )
    )
  
  anomaly_plot_ed <- 
    ggplot(anomalies %>%
             filter(!is.na(ED_BEDS_A_anomaly)),
           aes(x = START_DATE, y = ED_BEDS_A_anomaly)) +
    geom_point(alpha = 0.3) +
    facet_wrap(~RESOURCE) +
    theme_bw()
  ggsave(
    paste0("figures/ed_report_anomaly_thresh", ed_thresh, ".png"),
    anomaly_plot_ed,
    width = 11, height = 8, units = "in", dpi = 1200, bg = "white"
  )
  
  anomaly_plot_ip <- ggplot(anomalies %>%
                              filter(!is.na(INPATIENT_BEDS_B_anomaly)),
                            aes(x = START_DATE, y = INPATIENT_BEDS_B_anomaly)) +
    geom_point(alpha = 0.3) +
    facet_wrap(~RESOURCE) +
    theme_bw()
  ggsave(
    paste0("figures/ip_report_anomaly_thresh", ip_thresh, ".png"),
    anomaly_plot_ip,
    width = 11, height = 8, units = "in", dpi = 1200, bg = "white"
  )
  
  return(anomalies)
} # end detect_replace_anomalies

#/////////////////////////////////////////////////////////////////////////////////////////////////
#' Extracts rows around each detected anomaly in the `anomalies` data frame.
#' 
#' @param anomalies A data frame containing anomaly detection results with `ED_BEDS_A_anomaly` 
#'                  and `INPATIENT_BEDS_B_anomaly` columns indicating anomalies. Dataframe produced 
#'                  by the function `detect_replace_anomalies`
#' @param rows_before_after Integer specifying the number of rows to include before and after 
#'                          each anomaly. Default is 2.
#' @return A data frame containing rows around each anomaly with relevant columns (`START_DATE`, 
#'         `RESOURCE`, and columns starting with `INPATIENT_BEDS` and `ED_BEDS_A`).
#' @examples
#' get_rows_around_anomaly(anomalies, rows_before_after = 2)
get_rows_around_anomaly <- function(anomalies, rows_before_after = 2) {
  
  # Get the row numbers of the anomalies
  anomaly_indices <- which(anomalies$ED_BEDS_A_anomaly | anomalies$INPATIENT_BEDS_B_anomaly)
  
  # Collect indices for rows around each anomaly
  all_indices <- unique(unlist(lapply(anomaly_indices, function(x) {
    seq(max(1, x - rows_before_after), min(nrow(anomalies), x + rows_before_after))
  })))
  
  # Slice the data frame using the collected indices
  context_rows <- anomalies %>%
    slice(all_indices) %>%
    dplyr::select(START_DATE, RESOURCE, starts_with("INPATIENT_BEDS"), starts_with("ED_BEDS_A"))
  
  return(context_rows)
} # end get_rows_around_anomaly