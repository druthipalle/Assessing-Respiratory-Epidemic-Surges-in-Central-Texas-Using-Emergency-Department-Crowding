#/////////////////////////////////////
# title: "NEDOCS Plotting Functions"
# author: "Druthi Palle"
#/////////////////////////////////////

#//////////////////////////////////////////
#' Plot Hourly NEDOCS Data
#'
#' This function processes hourly NEDOCS score data and generates a ggplot object
#' for visualizing trends over time for a specific hospital.
#'
#' @param hourly_sequences A data frame containing hourly NEDOCS data for a specific hospital.
#'
#' @return A ggplot object showing the hourly NEDOCS scores and their trends (daily, weekly, and monthly SMAs).
#'
#' @examples
#' \dontrun{
#' plot <- hourly_NEDOCS_plot(hourly_sequences)
#' }
hourly_NEDOCS_plot <- function(hourly_sequences) {  
  hourly_sequences_for_plotting <- hourly_sequences %>%
    arrange(DATE, HOUR) %>%
    mutate(
      daily_SMA = SMA(NEDOCS_SAT_SCORE, n = 24),    # 24-hour SMA for daily trend
      weekly_SMA = SMA(NEDOCS_SAT_SCORE, n = 168),  # 168-hour SMA for weekly trend
      monthly_SMA = SMA(NEDOCS_SAT_SCORE, n = 720), # 720-hour SMA for monthly trend
      date_hour = as.POSIXct(paste(DATE, HOUR), format="%Y-%m-%d %H")
    )
  
  color_mapping <- c(
    "HOURLY" = "lightblue",
    "DAILY" = "orange",
    "WEEKLY" = "black",
    "MONTHLY" = "red"
  )
  
  plot <- ggplot(data = hourly_sequences_for_plotting, aes(x = date_hour)) + 
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(
      x = "Date",
      y = "NEDOCS Score",
      color = "Legend"
    ) +
    geom_line(aes(y = NEDOCS_SAT_SCORE, color = "HOURLY"), linewidth = 1) + 
    geom_line(aes(y = daily_SMA, color = "DAILY"), linewidth = 1) + 
    geom_line(aes(y = weekly_SMA, color = "WEEKLY"), linewidth = 1) + 
    geom_line(aes(y = monthly_SMA, color = "MONTHLY"), linewidth = 1) + 
    scale_color_manual(
      values = color_mapping,
      breaks = c("HOURLY", "DAILY", "WEEKLY", "MONTHLY")
    ) +
    theme_classic()
  
  return(plot)
} # end hourly_NEDOCS_plot

#' Generate a heatmap of daily NEDOCS data
#' 
#' This function generates a heatmap of daily NEDOCS scores for each hospital resource,
#' where the fill color represents the NEDOCS SAT score. The heatmap is saved as a PNG file.
#' 
#' @param daily_data A data frame containing daily NEDOCS data with columns:
#'                           `Date`, `RESOURCE`,`AVG_NEDOCS_SAT_SCORE`, `NEDOCS_VALUE`, `ED_STATUS`
#' @param output_file A character string specifying the path to save the heatmap as a PNG file.
#' 
#' @return A heatmap plot saved as a PNG file.
#' 
#' @examples 
#' \dontrun{
#' generate_nedocs_heatmap(daily_data)
#' }
generate_nedocs_heatmap <- function(daily_data) {
  
  heatmap <- ggplot(daily_data, aes(x = `DATE`, y = `RESOURCE`, fill = `AVG_NEDOCS_SAT_SCORE`)) +
    geom_tile() +
    scale_fill_gradientn(
      colors = c("#fcde9c", "#faa476", "#f0746e", "#e34f6f", "#dc3977", "#b9257a", "#7c1d6f"),
      breaks = c(0, 50, 100, 140, 180),
      labels = c(
        "0-50 (normal)",
        "51-100 (busy)",
        "101-140 (overcrowded)",
        "141-180 (severe)",
        "181+ (disaster)"
      ), 
      limits = c(0, 200),
      oob = scales::squish
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    scale_y_discrete() +
    labs(
      title = "Daily Average NEDOCS Scores",
      x = "Date",
      y = "Resource",
      fill = "NEDOCS Score"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 15, face = "bold"),
      axis.text.y = element_text(size = 15, face = "bold"),
      plot.title = element_text(face = "bold", size = 25),
      axis.title.x = element_text(size = 20, face = "bold"), 
      axis.title.y = element_text(size = 20, face = "bold"), 
      legend.position = "right",
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 20),
      legend.key.size = unit(1.5, "cm"),
      plot.margin = margin(t = 20, r = 30, b = 20, l = 30)
    )
  
  return(heatmap)
}