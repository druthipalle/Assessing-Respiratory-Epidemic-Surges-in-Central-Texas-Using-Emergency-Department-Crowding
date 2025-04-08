#/////////////////////////////////////
# title: "Correlate NEDOCS and HHS Data"
# author: "Druthi Palle"
# description: 
#   This script calculates rolling correlations between NEDOCS scores and 
#   HHS-reported hospitalizations (COVID+FLU) across Texas hospitals. 
#   The analysis focuses on how emergency department crowding (as measured by 
#   NEDOCS) aligns with respiratory infection surges, stratified by hospital size.
#   A 7-week centered rolling window is used to compute correlations over time.
#/////////////////////////////////////

#### Load libraries ####
source("get_packages_used.R")

#### Load Data ####
hhs_input = "../input_data/CATRAC_hhs_hospitals.csv"

if(!file.exists(hhs_input)){
  
  # Load county-level info for TSA-O (Central Texas region) with FIPS codes
  CATRAC_county_df = read_csv("input_data/CATRAC_counties.csv") %>%
    mutate(FIPS = as.character(FIPS))
  
  # Load raw HHS hospital-level data
  hhs_data = read_csv("../big_input_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility_20241007.csv") %>% # 104506 rows
    filter(state=="TX") %>%
    mutate(
      fips_code = as.character(fips_code),         # Standardize FIPS format
      collection_week = as.Date(collection_week),  # Convert to Date type
      ccn = as.factor(ccn)                         # Treat hospital CCNs as categorical
      ) %>%
    filter(fips_code %in% CATRAC_county_df$FIPS)   # Keep only hospitals in the CATRAC region
  
  # Replace placeholder -999999.0 values (possibly due to privacy suppression) with 3
  hhs_data[hhs_data==-999999.0] = 3                
  
  # Save cleaned and filtered data for future use
  write.csv(hhs_data, hhs_input, row.names = F)
  
} else {
  
  # Load the previously cleaned and saved HHS data
  hhs_data = read_csv(hhs_input, col_types = c(collection_week="D", fips_code="c", ccn="c"))
  
} # end if hhs data not cleaned

#### Load and Preprocess NEDOCS Data ####
nedocs_input = "../input_data/sample_NEDOCS_data.csv"

####  Create Time-Weighted Daily NEDOCS Metrics #### 
# This section computes time-weighted daily averages for various NEDOCS input variables
# across all hospitals. Weights are based on the number of hours a given observation spans.
# This provides a more accurate daily summary than a simple mean.

time_weighted_nedocs <- nedocs_og_df %>%
  arrange(RESOURCE, START_DATE) %>%             # Sort by hospital (RESOURCE) and start time
  mutate(START_DATE = as.Date(START_DATE)) %>%  # Ensure date is in Date format
  group_by(RESOURCE, START_DATE) %>%            # Group by hospital and day
  summarise(
    across(
      # Apply time-weighted averaging to each key NEDOCS metric
      .cols = c(
        NEDOCS_SAT_SCORE, ED_BEDS_A, INPATIENT_BEDS_B, ED_PATIENTS_C,
        CRITICAL_CARE_PATIENTS_D, LONGEST_ED_ADMIT_E, ED_ADMITS_F,
        LAST_DOOR_TO_BED_TIME_G, ED_BEDS_A_clean, INPATIENT_BEDS_B_clean
      ),
      .fns = ~ sum(.x * DURATION_HOURS, na.rm = TRUE) / sum(DURATION_HOURS, na.rm = TRUE),
      .names = "tw_{.col}"  # Prefix with 'tw_' to indicate time-weighted metric
    )
  ) %>%
  mutate(
    # Calculate proportion of critical care patients among ED patients
    CC_ED_Proportion = tw_CRITICAL_CARE_PATIENTS_D / tw_ED_PATIENTS_C,
    
    # Cap the proportion at 1 to account for data noise or small denominators
    CC_ED_Proportion_cap = ifelse(CC_ED_Proportion > 1, 1, CC_ED_Proportion)
  ) %>%
  ungroup()  # Remove grouping to finalize the cleaned dataset

#### Convert NEDOCS to Match HHS Weekly Data ####
# Get the last day of HHS data (collection_week is always Friday, so add 6 to include full week)
last_day_hhs_data = max(hhs_data$collection_week, na.rm = TRUE) + 6

# Filter and align daily NEDOCS with weekly HHS periods
nedocs_weekly <- time_weighted_nedocs %>%
  filter(START_DATE <= last_day_hhs_data) %>%  # Trim NEDOCS data to match HHS reporting period
  left_join(
    hhs_data %>%
      dplyr::select(collection_week) %>%
      distinct(),  # Get unique weekly HHS reporting start dates
    by = c("START_DATE" = "collection_week"), keep=T) %>%
  dplyr::select(collection_week, START_DATE,
                everything()
  ) %>%
  fill(collection_week, .direction = "down") %>%  # Fill down collection_week to assign week to daily rows
  drop_na(collection_week) %>%                    # Drop any rows not associated with an HHS week
  group_by(RESOURCE, collection_week) %>%         # Group by hospital and HHS week
  summarise(
    # Calculate weekly summary statistics (mean, median, min, max) for each time-weighted metric
    across(tw_NEDOCS_SAT_SCORE:tw_INPATIENT_BEDS_B_clean, mean,   .names = "{.col}_mean"),
    across(tw_NEDOCS_SAT_SCORE:tw_INPATIENT_BEDS_B_clean, median, .names = "{.col}_median"),
    across(tw_NEDOCS_SAT_SCORE:tw_INPATIENT_BEDS_B_clean, min,    .names = "{.col}_min"),
    across(tw_NEDOCS_SAT_SCORE:tw_INPATIENT_BEDS_B_clean, max,    .names = "{.col}_max")
  ) %>%
  ungroup() %>%
  mutate(
    # Estimate total number of staffed beds (ED + inpatient)
    max_staffed_beds = tw_ED_BEDS_A_clean_max + tw_INPATIENT_BEDS_B_clean_max,
    mean_staffed_beds = tw_ED_BEDS_A_clean_mean + tw_INPATIENT_BEDS_B_clean_mean
  ) # end filter

#### Classify Hospitals by Size Based on ED vs Inpatient Bed Difference ####  
hospital_size_df <- time_weighted_nedocs %>%
  group_by(RESOURCE) %>%
  summarise(
    mean_ed_beds     = mean(tw_ED_BEDS_A_clean, na.rm = TRUE),
    mean_inpt_beds   = mean(tw_INPATIENT_BEDS_B_clean, na.rm = TRUE),
    med_ed_beds      = median(tw_ED_BEDS_A_clean, na.rm = TRUE),
    med_inpt_beds    = median(tw_INPATIENT_BEDS_B_clean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Calculate median difference between inpatient and ED beds
    diff_median = med_inpt_beds - med_ed_beds,
    # Classify hospital by size based on median bed difference
    hosp_size = case_when(
      diff_median >= 100 ~ "LARGE",
      diff_median >= 20  ~ "MEDIUM",  # Implicitly excludes >=100
      diff_median <  20  ~ "SMALL",
      TRUE ~ NA_character_  # Catch-all fallback
    )
  ) %>%
  # Remove hospitals where difference is too small to be meaningful or unreliable
  filter(diff_median > 1)

#### Merge Size Labels Back Into Weekly NEDOCS Summary ####
nedocs_weekly_size = nedocs_weekly %>%
  left_join(hospital_size_df, by = "RESOURCE")

#### CORRELATE NEDOCS & HHS #### 

### Step 1: Clean & Interpolate HHS Weekly Data ###
hhs_data_na_approx <- hhs_data %>%
  group_by(ccn) %>%
  arrange(collection_week) %>%
  mutate(
    # Interpolate missing values for key HHS variables using linear approximation per hospital (rule = 2 extends ends)
    total_beds_7_day_avg = approx(collection_week, total_beds_7_day_avg, xout = collection_week, rule = 2, ties = mean)$y,
    
    total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg =
      approx(collection_week, total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg, xout = collection_week, rule = 2, ties = mean)$y,
    
    total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg =
      approx(collection_week, total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg, xout = collection_week, rule = 2, ties = mean)$y,
    
    staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg =
      approx(collection_week, staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg, xout = collection_week, rule = 2, ties = mean)$y,
    
    total_patients_hospitalized_confirmed_influenza_7_day_avg = 
      approx(collection_week, total_patients_hospitalized_confirmed_influenza_7_day_avg, xout = collection_week, rule = 2, ties = mean)$y,
    
    icu_patients_confirmed_influenza_7_day_avg = 
      approx(collection_week, icu_patients_confirmed_influenza_7_day_avg, xout = collection_week, rule = 2, ties = mean)$y
  ) %>%
  ungroup() %>%
  # Keep only relevant columns for correlation
  select(
    ccn, collection_week, total_beds_7_day_avg,
    total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg,
    total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg,
    staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg,
    total_patients_hospitalized_confirmed_influenza_7_day_avg,
    icu_patients_confirmed_influenza_7_day_avg
  ) %>%
  group_by(ccn, collection_week) %>%
  arrange(ccn, collection_week) %>%
  rowwise() %>%
  mutate(
    # Combine COVID & FLU inpatient/ICU/pediatric/adult totals into a single sum for visualization and comparison
    IP_ICU_ped_ad_COVFLU_7d_avg_sum = sum(c_across(c(
      total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg,
      total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg,
      staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg,
      total_patients_hospitalized_confirmed_influenza_7_day_avg,
      icu_patients_confirmed_influenza_7_day_avg
    )), na.rm = TRUE)
  ) %>%
  ungroup()

### Step 2: Classify Hospitals by Size Using HHS Bed Counts ###
hhs_hosp_size <- hhs_data_na_approx %>%
  group_by(ccn) %>%
  summarise(
    mean_total_beds = mean(total_beds_7_day_avg, na.rm = TRUE),
    med_total_beds  = median(total_beds_7_day_avg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Classify hospitals into SMALL, MEDIUM, or LARGE based on mean or median total beds
    hosp_size = case_when(
      mean_total_beds <= 25 | med_total_beds <= 25   ~ "SMALL",
      mean_total_beds <= 100 | med_total_beds <= 100 ~ "MEDIUM",
      mean_total_beds > 100  | med_total_beds > 100  ~ "LARGE",
      TRUE ~ NA_character_  # fallback
    )
  )

# Quick check of size distribution
table(hhs_hosp_size$hosp_size)
#   LARGE MEDIUM SMALL 
#     11     11     10 

### Step 3: Join HHS Size Classifications and Visualize Epidemic Curves ###
hhs_data_na_approx_size <- hhs_data_na_approx %>%
  left_join(hhs_hosp_size, by = "ccn")

# Plot epidemic curves for COVID+FLU by hospital size
ggplot(
  hhs_data_na_approx_size,
  aes(x = collection_week,
      y = IP_ICU_ped_ad_COVFLU_7d_avg_sum,
      group = ccn, color = hosp_size)) +
  geom_line() +
  facet_wrap(~hosp_size, ncol = 1, scales = "free_y") +
  theme_bw()

### Step 4: Collapse HHS to Weekly by Hospital Size ###
hhs_hosp_size_covflu_weekly <- hhs_data_na_approx_size %>%
  group_by(hosp_size, collection_week) %>%
  summarise(
    IP_ICU_ped_ad_COVFLU_size_mean = mean(IP_ICU_ped_ad_COVFLU_7d_avg_sum, na.rm = TRUE),
    IP_ICU_ped_ad_COVFLU_size_med  = median(IP_ICU_ped_ad_COVFLU_7d_avg_sum, na.rm = TRUE),
    .groups = "drop"
  )

# Plot median epidemic curves (less sensitive to outliers)
ggplot(
  hhs_hosp_size_covflu_weekly,
  aes(x = collection_week,
      y = IP_ICU_ped_ad_COVFLU_size_med,
      color = hosp_size)) +
  geom_line() +
  facet_wrap(~hosp_size, ncol = 1, scales = "free_y") +
  theme_bw()

### Step 5: Collapse NEDOCS to Weekly Averages by Size ###
nedocs_avg_by_week <- nedocs_weekly_size %>%
  group_by(collection_week, hosp_size) %>%
  summarise(
    avg_nedocs = mean(tw_NEDOCS_SAT_SCORE_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  drop_na()  # Remove weeks with missing NEDOCS data

#### ALIGN NEDOCS & HHS TIME INTERVALS #### 
# Join the weekly aggregated NEDOCS and HHS data by hospital size and collection week
# Ensures that only weeks present in both datasets are kept
nedocs_hhs_compare_df = hhs_hosp_size_covflu_weekly %>%
  left_join(nedocs_avg_by_week, by = c("collection_week", "hosp_size")) %>%
  drop_na(avg_nedocs)  # remove weeks missing NEDOCS data

# Plot HHS COVID+FLU Hospitalizations (by size)
flucov_weekly_plt = ggplot(nedocs_hhs_compare_df, 
                           aes(x = collection_week, 
                               y = IP_ICU_ped_ad_COVFLU_size_mean,
                               color = hosp_size)) +
  geom_line(linewidth = 1) +  # main line plot
  facet_wrap(~hosp_size, ncol = 3) +  # separate plots per hospital size
  labs(x = "Collection Week", 
       y = "Mean COVID+FLU Hospitalizations") +
  theme_bw(base_size = 15) +
  theme(axis.title.x = element_blank(),  # remove x-axis label (shared below)
        legend.position = "none")  # simplify plot by hiding legend

# Plot NEDOCS
nedocs_thresholds <- tibble::tibble(
  yintercept = c(20, 60, 100, 140, 180),
  label = c("Not busy", "Busy", "Extremely busy", "Overcrowded", "Disaster"),
  hosp_size = "SMALL"  # show labels only in SMALL facet
)

nedocs_weekly_plt <-
  ggplot(nedocs_hhs_compare_df, 
                           aes(x = collection_week, 
                               y = avg_nedocs,
                               color = hosp_size)) +
  geom_line(linewidth = 1) +
  
  # Add dashed horizontal reference lines at key NEDOCS score levels
  geom_hline(yintercept = c(20, 60, 100, 140, 180),
             linetype = "dashed", color = "gray60", linewidth = 0.4) +
  
  # Add threshold labels (only appears in SMALL facet)
  geom_text(data = nedocs_thresholds,
            aes(x = max(nedocs_hhs_compare_df$collection_week),
                y = yintercept,
                label = label),
            inherit.aes = FALSE,
            hjust = 1, vjust = -0.2,
            size = 4, color = "black") +
  
  facet_wrap(~hosp_size, ncol = 3) +
  labs(x = "Collection Week", 
       y = "Mean NEDOCS") +
  theme_bw(base_size = 15) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none"
  )

# Use 8-week sliding window to calculate correlations
rolling_corr_df <- nedocs_hhs_compare_df %>%
  arrange(hosp_size, collection_week) %>%
  group_by(hosp_size) %>%
  mutate(
    # Correlation at same week (lag 0)
    corr_lag0 = slider::slide_dbl(
      .x = cur_data(),
      .f = ~ {
        x <- .x$avg_nedocs
        y <- .x$IP_ICU_ped_ad_COVFLU_size_mean
        if (length(na.omit(x)) >= 3 && length(na.omit(y)) >= 3) {
          cor(x, y, use = "complete.obs")
        } else {
          NA_real_
        }
      },
      .before = 7, .after = 0,  # 8-week rolling window
      .complete = TRUE
    ),
    
    # Correlation with 2-week lead in NEDOCS (i.e., NEDOCS leads HHS)
    corr_lag2 = slider::slide_dbl(
      .x = cur_data(),
      .f = ~ {
        x <- head(.x$avg_nedocs, -2)
        y <- tail(.x$IP_ICU_ped_ad_COVFLU_size_mean, -2)
        if (length(na.omit(x)) >= 3 && length(na.omit(y)) >= 3) {
          cor(x, y, use = "complete.obs")
        } else {
          NA_real_
        }
      },
      .before = 7, .after = 0,
      .complete = TRUE
    )
  ) %>%
  ungroup()

# Reshape to long format for plotting
rolling_corr_long <- rolling_corr_df %>%
  pivot_longer(
    cols = starts_with("corr_lag"),
    names_to = "lag_type",
    values_to = "rolling_corr"
  )

# Plot Rolling Correlations
roll_corr_plot <-
  ggplot(rolling_corr_long,
         aes(x = collection_week, y = rolling_corr, 
             color=hosp_size, linetype = lag_type)) +
  geom_line(alpha=1) +
  facet_wrap(~ hosp_size, ncol = 3) +
  labs(
    x = "Collection Week",
    y = "8wk Rolling Correlation",
    linetype = "Lag"#,
    #color = "Lag"
  ) +
  theme_bw(base_size = 15)+
  theme(legend.position = "none"
  ) +
  guides(color = "none")

# Save the combined plot
ggsave("../figures/nedocs_flu_covid_correlation.png", 
       cowplot::plot_grid(
         flucov_weekly_plt,       # Top: COVID+FLU hospitalizations
         nedocs_weekly_plt,       # Middle: NEDOCS
         roll_corr_plot,          # Bottom: rolling correlation
         nrow = 3), 
       width = 12, height = 8, dpi = 1200)