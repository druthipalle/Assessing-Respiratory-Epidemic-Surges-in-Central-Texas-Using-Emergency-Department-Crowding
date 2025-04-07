#/////////////////////////////////////
# title: "Correlate NEDOCS and HHS Data"
# author: "Druthi Palle"
#/////////////////////////////////////

#### Load libraries ####
source("get_packages_used.R")

#### Load Data ####
hhs_input = "../input_data/CATRAC_hhs_hospitals.csv"
if(!file.exists(hhs_input)){
  CATRAC_county_df = read_csv("input_data/CATRAC_counties.csv") %>% # TSA O county names + fips
    mutate(FIPS = as.character(FIPS))
  
  hhs_data = read_csv("../big_input_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility_20241007.csv") %>% # 104506 rows
    filter(state=="TX") %>% # 92347
    mutate(fips_code = as.character(fips_code),
           collection_week = as.Date(collection_week),
           ccn = as.factor(ccn)) %>%
    filter(fips_code %in% CATRAC_county_df$FIPS) # 6452
  hhs_data[hhs_data==-999999.0] = 3 # data anonymized because too few people?
  # length(unique(hhs_data$ccn)) # 32 hospitals, so a little less than 34 in NEDOCS
  
  write.csv(hhs_data, hhs_input, row.names = F)
}else{
  hhs_data = read_csv(hhs_input, col_types = c(collection_week="D", fips_code="c", ccn="c"))
} # end if hhs data not cleaned

# Load original data, but fix hospitals not anonymized & anomalies in bed counts
#  Replace with the true score in 24hrs rather than just start date
#    nedocs_hourly = read_csv("produced_data/hourly_nedocs_timeseries.csv")
nedocs_input = "../input_data/sample_NEDOCS_data.csv"

#/////////////////////////////////////////////
# weight metrics by time per day
time_weighted_nedocs <- nedocs_og_df %>%
  arrange(RESOURCE, START_DATE) %>%
  mutate(START_DATE = as.Date(START_DATE)) %>%
  group_by(RESOURCE, START_DATE) %>%
  summarise(
    across(
      .cols = c(NEDOCS_SAT_SCORE, ED_BEDS_A, INPATIENT_BEDS_B, ED_PATIENTS_C, CRITICAL_CARE_PATIENTS_D,
                LONGEST_ED_ADMIT_E, ED_ADMITS_F, LAST_DOOR_TO_BED_TIME_G,
                ED_BEDS_A_clean, INPATIENT_BEDS_B_clean),
      .fns = ~ sum(.x * DURATION_HOURS, na.rm = TRUE) / sum(DURATION_HOURS, na.rm = TRUE),
      .names = "tw_{.col}"
    )
  ) %>%
  mutate(
    CC_ED_Proportion = tw_CRITICAL_CARE_PATIENTS_D/tw_ED_PATIENTS_C,
    CC_ED_Proportion_cap = ifelse(CC_ED_Proportion>1, 1, CC_ED_Proportion)
  ) %>%
  ungroup()

#///////////////////////////////////////////
#### Convert NEDOCS to match HHS Weekly ####
last_day_hhs_data = max(hhs_data$collection_week, na.rm = T)+6
nedocs_weekly <- time_weighted_nedocs %>%
  filter(START_DATE<=last_day_hhs_data) %>%
  left_join(
    hhs_data %>%
      dplyr::select(collection_week) %>%
      distinct(),
    by = c("START_DATE" = "collection_week"), keep=T) %>%
  dplyr::select(collection_week, START_DATE,
                everything()
  ) %>%
  # collection_week is the start of the reporting period (Friday)
  fill(collection_week, .direction = "down") %>%
  drop_na(collection_week) %>%
  group_by(RESOURCE, collection_week) %>%
  summarise(
    across(tw_NEDOCS_SAT_SCORE:tw_INPATIENT_BEDS_B_clean, mean,   .names = "{.col}_mean"),
    across(tw_NEDOCS_SAT_SCORE:tw_INPATIENT_BEDS_B_clean, median, .names = "{.col}_median"),
    across(tw_NEDOCS_SAT_SCORE:tw_INPATIENT_BEDS_B_clean, min,    .names = "{.col}_min"),
    across(tw_NEDOCS_SAT_SCORE:tw_INPATIENT_BEDS_B_clean, max,    .names = "{.col}_max")
  ) %>%
  ungroup() %>%
  mutate(max_staffed_beds = tw_ED_BEDS_A_clean_max + tw_INPATIENT_BEDS_B_clean_max,
         mean_staffed_beds = tw_ED_BEDS_A_clean_mean + tw_INPATIENT_BEDS_B_clean_mean
  ) %>%
  filter(!(RESOURCE %in% c("0018c00002S8O0ZAAV", # Almost all 0 for ED Admits & NEDOCS score
                           "0018c00002S8TgUAAV"  # only 1 point in time period of comparison
  ))) # end filter

# Get cluster NEDOCS hospitals by size difference of ED and IP
hospital_size_df <- time_weighted_nedocs %>%
  group_by(RESOURCE) %>%
  summarise(
    mean_ed_beds = mean(tw_ED_BEDS_A_clean, na.rm = TRUE),
    mean_inpt_beds = mean(tw_INPATIENT_BEDS_B_clean, na.rm = TRUE),
    med_ed_beds = median(tw_ED_BEDS_A_clean, na.rm = TRUE),
    med_inpt_beds = median(tw_INPATIENT_BEDS_B_clean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    diff_median = med_inpt_beds - med_ed_beds,
    hosp_size = case_when(
      diff_median >= 100 ~ "LARGE",
      diff_median >= 20  ~ "MEDIUM",  # no need to check `< 100`, already implied
      diff_median < 20   ~ "SMALL",
      TRUE ~ NA_character_  # fallback if something goes wrong
    )
  ) %>%
  # removes "0018c00002S8O0aAAF", "0018c00002S8O1LAAV"
  filter(diff_median > 1)

# Join size estimate to NEDOCS weekly
nedocs_weekly_size = nedocs_weekly %>%
  left_join(hospital_size_df, by="RESOURCE")



############ CORR NEDOCS & HHS ###########################
# HHS DATA
hhs_data_na_approx <- hhs_data %>%
  group_by(ccn) %>%
  arrange(collection_week) %>%
  mutate(
    total_beds_7_day_avg = 
      approx(collection_week, total_beds_7_day_avg, xout = collection_week, rule = 2, ties = mean)$y,
    
    total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg = 
      approx(collection_week, total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg, xout = collection_week, rule = 2, ties = mean)$y,
    
    total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg = 
      approx(collection_week, total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg, xout = collection_week, rule = 2, ties = mean)$y,
    
    staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg = 
      approx(collection_week, staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg, xout = collection_week, rule = 2, ties = mean)$y,
    
    total_patients_hospitalized_confirmed_influenza_7_day_coverage = 
      approx(collection_week, total_patients_hospitalized_confirmed_influenza_7_day_coverage, xout = collection_week, rule = 2, ties = mean)$y,
    
    icu_patients_confirmed_influenza_7_day_coverage = 
      approx(collection_week, icu_patients_confirmed_influenza_7_day_coverage, xout = collection_week, rule = 2, ties = mean)$y
  ) %>%
  ungroup() %>%
  select(ccn, collection_week, total_beds_7_day_avg,
         total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg,
         total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg,
         staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg,
         total_patients_hospitalized_confirmed_influenza_7_day_coverage,
         icu_patients_confirmed_influenza_7_day_coverage) %>%
  group_by(ccn,collection_week) %>%
  arrange( ccn,collection_week) %>%
  rowwise() %>%
  mutate(
    IP_ICU_ped_ad_COVFLU_7d_avg_sum = sum(c_across(c(
      total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg,
      total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg,
      staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg,
      # )), na.rm = TRUE),
      # 
      # IP_ICU_ped_ad_FLU_7d_avg_sum = sum(c_across(c(
      total_patients_hospitalized_confirmed_influenza_7_day_coverage,
      icu_patients_confirmed_influenza_7_day_coverage
    )), na.rm = TRUE)
  ) %>%
  ungroup()

hhs_hosp_size = hhs_data_na_approx %>%
  group_by(ccn) %>%
  summarise(
    mean_total_beds = mean(total_beds_7_day_avg, na.rm = TRUE),
    med_total_beds = median(total_beds_7_day_avg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(hosp_size =
           case_when(mean_total_beds <=25  | med_total_beds <=25   ~ "SMALL",
                     mean_total_beds <=100 | med_total_beds <=100  ~ "MEDIUM",
                     mean_total_beds > 100 | med_total_beds > 100  ~ "LARGE",
                     TRUE ~ NA_character_  # fallback if something goes wrong
           )
  )
# Pretty similar sizing to the NEDOCS data for comparison, but not exact
table(hhs_hosp_size$hosp_size)
# LARGE MEDIUM  SMALL 
#    11     11     10 

# Full DF with approx hosp size and the summed COV + FLU columns
hhs_data_na_approx_size = hhs_data_na_approx %>%
  left_join(hhs_hosp_size, by="ccn")

# Actually looks like epidemic curves
# FLU alone looked like almost nothing, so we should merge FLU as it's negligable contribution
ggplot(
  hhs_data_na_approx_size,
  aes(x=collection_week, 
      y=IP_ICU_ped_ad_COVFLU_7d_avg_sum,
      group=ccn, color=hosp_size))+
  geom_line()+
  facet_wrap(~hosp_size, ncol=1, scales="free_y")+
  theme_bw()

hhs_hosp_size_covflu_weekly <- hhs_data_na_approx_size %>%
  group_by(hosp_size, collection_week) %>%
  summarise(
    IP_ICU_ped_ad_COVFLU_size_mean = mean(IP_ICU_ped_ad_COVFLU_7d_avg_sum, na.rm = TRUE),
    IP_ICU_ped_ad_COVFLU_size_med  = median(IP_ICU_ped_ad_COVFLU_7d_avg_sum, na.rm = TRUE),
    .groups = "drop"
  )

# Median shows peaks a little better, and in general is less susceptibel to outliers 
# The Hospial Size classification isn't perfectly aligned to our NEDCOCS sizing, but close
ggplot(
  hhs_hosp_size_covflu_weekly,
  aes(x=collection_week, 
      y=IP_ICU_ped_ad_COVFLU_size_med,
      color=hosp_size))+
  geom_line()+
  facet_wrap(~hosp_size, ncol=1, scales="free_y")+
  theme_bw()

# NEDOCS DATA
nedocs_avg_by_week <- nedocs_weekly_size %>%
  group_by(collection_week, hosp_size) %>% 
  summarise(
    avg_nedocs = mean(tw_NEDOCS_SAT_SCORE_mean, na.rm = TRUE),
    .groups = "drop") %>%
  drop_na()

#//////////////////////////////////////////////////////////////////////////
# ALIGN NEDOCS & HHS TIME INTERVALS
nedocs_hhs_compare_df = hhs_hosp_size_covflu_weekly %>%
  left_join(nedocs_avg_by_week, by=c("collection_week", "hosp_size")) %>%
  # Weeks in HHS not in NEDOCS will be NA for the NEDOCS value
  drop_na(avg_nedocs)

# PLOT HHS DATA
flucov_weekly_plt = ggplot(nedocs_hhs_compare_df, 
                           aes(x = collection_week, 
                               y = IP_ICU_ped_ad_COVFLU_size_mean,
                               color=hosp_size)) +
  geom_line(linewidth = 1) +
  facet_wrap(~hosp_size, ncol=3) + # , scales="free"
  labs(x = "Collection Week", 
       y = "Mean COVID+FLU Hospitalizations") +
  theme_bw(base_size = 15)+
  theme(axis.title.x = element_blank(),
        legend.position = "none")

# PLOT NEDOCS
nedocs_thresholds <- tibble::tibble(
  yintercept = c(20, 60, 100, 140, 180),
  label = c("Not busy", "Busy", "Extremely busy", "Overcrowded", "Disaster"),
  hosp_size = "SMALL"  # only want labels on this facet
)

nedocs_weekly_plt = 
  ggplot(nedocs_hhs_compare_df, 
         aes(x = collection_week, 
             y = avg_nedocs,
             color = hosp_size)) +
  geom_line(linewidth = 1) +
  
  # Add horizontal lines for all facets
  geom_hline(yintercept = c(20, 60, 100, 140, 180),
             linetype = "dashed", color = "gray60", linewidth = 0.4) +
  
  # Add labels only to SMALL hospital facet
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

# CORRELATION
rolling_corr_df <- nedocs_hhs_compare_df %>%
  arrange(hosp_size, collection_week) %>%
  group_by(hosp_size) %>%
  mutate(
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
      .before = 7, .after = 0,
      .complete = TRUE
    ),
    # corr_lag1 = slider::slide_dbl(
    #   .x = cur_data(),
    #   .f = ~ {
    #     x <- head(.x$avg_nedocs, -1)
    #     y <- tail(.x$IP_ICU_ped_ad_COVFLU_size_mean, -1)
    #     if (length(na.omit(x)) >= 3 && length(na.omit(y)) >= 3) {
    #       cor(x, y, use = "complete.obs")
    #     } else {
    #       NA_real_
    #     }
    #   },
    #   .before = 7, .after = 0,
    #   .complete = TRUE
    # ),
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

rolling_corr_long <- rolling_corr_df %>%
  pivot_longer(
    cols = starts_with("corr_lag"),
    names_to = "lag_type",
    values_to = "rolling_corr"
  )

roll_corr_plot =
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

ggsave("../figures/nedocs_flu_covid_correlation.png", 
       cowplot::plot_grid(flucov_weekly_plt, 
                          nedocs_weekly_plt, 
                          roll_corr_plot, 
                          nrow=3), 
       width = 12, height = 8, dpi = 1200)