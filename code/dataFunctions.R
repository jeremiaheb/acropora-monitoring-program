# Load necessary libraries for data manipulation and visualization.
# If you don't have them installed, run: install.packages(c("dplyr", "tidyr", "ggplot2", "forcats", "lubridate", "readxl"))
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(lubridate)
library(readxl)

# ==============================================================================
# Function Definitions
# ==============================================================================

# Custom function to calculate standard error
std.error <- function(x) sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))

#' Wrangled raw coral colony data into analysis-ready metrics.
#'
#' @param raw_data A data frame containing the raw tagged colony measurements.
#' @return A data frame with new calculated metrics (Structure, LAI, Bins, etc.).
wrangle_coral_data <- function(raw_data) {
  raw_data %>%
    mutate(
      # Convert units from cm to m
      Length = Length / 100,
      Width = Width / 100,
      Height = Height / 100,
      
      # new metrics
      Structure = Length * Width * Height,
      SurfaceArea = (Structure / 3)^2,
      
      # Convert percentage to decimal
      Per_Live = Per_Live / 100,
      
      # Calculate LAI
      LAI = SurfaceArea * Per_Live,
      
      Bin = case_when(
        Structure == 0                         ~ "Bin_0",
        Structure > 0 & Structure <= 0.001     ~ "Bin_1",
        Structure > 0.001 & Structure <= 0.125 ~ "Bin_2",
        Structure > 0.125 & Structure <= 1     ~ "Bin_3",
        Structure > 1                          ~ "Bin_4",
        TRUE                                   ~ NA_character_
      ),
      
      DATE = ymd(DATE)
    ) %>%
    # Group by Site and Timepoint to find the most common date for each group
    group_by(Site_Name, Timepoint) %>%
    mutate(
      top_date = as.Date(names(which.max(table(DATE))))
    ) %>%
    ungroup() %>%
    filter(!is.na(Bin))
}


#' Summarizes coral metrics by site and timepoint.
#'
#' @param metrics_data The wrangled data frame from wrangle_coral_data().
#' @return A summarized data frame with mean, sd, se, and n_plots for LAI and Percent Live.
summarize_site_metrics <- function(metrics_data) {
  metrics_data %>%
    # group_by(Site_Name, Timepoint, top_date, Plot_ID) %>%
    # summarise(
    #   plot_lai_mean = mean(LAI, na.rm = TRUE),
    #   plot_perc_live_mean = mean(Per_Live, na.rm = TRUE),
    #   .groups = 'drop'
    # ) %>%
    group_by(Site_Name, Timepoint, top_date) %>%
    summarise(
      LAI_mean = mean(LAI, na.rm = TRUE),
      Perc_Live_mean = mean(Per_Live, na.rm = TRUE),
      n_plots = n(),
      LAI_sd = sd(LAI, na.rm = TRUE),
      LAI_se = std.error(LAI),
      Perc_Live_sd = sd(Per_Live, na.rm = TRUE),
      Perc_Live_se = std.error(Per_Live),
      .groups = 'drop'
    )
}

#' Calculates the proportional abundance of each size bin.
#'
#' @param metrics_data The wrangled data frame from wrangle_coral_data().
#' @return A data frame in long format with proportions for each bin per group.
calculate_binned_proportions <- function(metrics_data) {
  metrics_data %>%
    count(Site_Name, Timepoint, top_date, Bin) %>%
    complete(nesting(Site_Name, Timepoint, top_date), Bin, fill = list(n = 0)) %>%
    group_by(Site_Name, Timepoint, top_date) %>%
    mutate(Prop = n / sum(n)) %>%
    ungroup()
}

#' Creates a line plot of a metric over time, faceted by site.
#'
#' @param summary_data The data frame from summarize_site_metrics().
#' @param y_var The column to plot on the y-axis (e.g., LAI_mean).
#' @param y_se The column for the standard error of the y_var.
#' @param title The main title for the plot.
#' @param y_lab The label for the y-axis.
#' @return A ggplot object.
plot_metric_over_time <- function(summary_data, y_var, y_se, title, y_lab) {
  ggplot(summary_data, aes(x = top_date, y = {{y_var}}, group = 1)) +
    geom_line(color = "gray50") +
    geom_errorbar(
      aes(ymin = {{y_var}} - {{y_se}}, ymax = {{y_var}} + {{y_se}}),
      width = 25,
      color = "darkgray"
    ) +
    geom_point(aes(color = Timepoint), size = 4, alpha = 0.8) +
    scale_x_date(date_breaks = "4 months", date_labels = "%b %Y") +
    scale_color_viridis_d() +
    facet_wrap(~ Site_Name, scales = "free") +
    labs(
      title = title,
      x = "Date",
      y = y_lab,
      color = "Timepoint"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}


#' Creates a stacked area plot of coral size distribution.
#'
#' @param proportions_data The data frame from calculate_binned_proportions().
#' @return A ggplot object.
plot_size_distribution <- function(proportions_data) {
  proportions_data %>%
    mutate(
      Bin = fct_recode(Bin,
                       "Dead"        = "Bin_0",
                       "Small"       = "Bin_1",
                       "Medium"      = "Bin_2",
                       "Large"       = "Bin_3",
                       "Extra Large" = "Bin_4"
      ),
      Bin = fct_relevel(Bin,
                        "Extra Large", "Large", "Medium", "Small", "Dead"
      )
    ) %>%
    ggplot(aes(x = top_date, y = Prop, fill = Bin, group = Bin)) +
    geom_area(position = "stack", alpha = 0.8, color = "white", linewidth = 0.2) +
    geom_point(position = "stack", size = 2, shape = 21, color = "white") +
    scale_fill_manual(
      name = "Colony Size",
      values = c(
        "Extra Large" = "#0072B2", "Large" = "#E69F00", "Medium" = "#009E73",
        "Small" = "#CC79A7", "Dead" = "#696969"
      )
    ) +
    facet_wrap(~ Site_Name) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "4 months") +
    labs(
      x = "Date",
      y = "Proportion of Tagged Colonies",
      title = "Size Class Distribution of Corals"
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

