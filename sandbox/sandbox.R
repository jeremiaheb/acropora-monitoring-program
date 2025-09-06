library(tidyverse)
library(readxl)


# Define custom function first
std.error <- function(x) sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))

# Assuming the data is in a subfolder named "data"
# If not, change the path to "TaggedColonies.xlsx"
TaggedColonies <- read_xlsx("data/TaggedColonies.xlsx")

Tagged_Metrics <- TaggedColonies %>%
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
    top_date = names(which.max(table(DATE))),
    top_date = as.Date(top_date)
  ) %>%
  ungroup()

# Consolidated summary calculation
site_summary <- Tagged_Metrics %>%
  group_by(Site_Name, Timepoint, top_date, Plot_ID) %>%
  summarise(
    plot_lai_mean = mean(LAI, na.rm = TRUE),
    plot_perc_live_mean = mean(Per_Live, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(Site_Name, Timepoint, top_date) %>%
  summarise(
    LAI_mean = mean(plot_lai_mean, na.rm = TRUE),
    Perc_Live_mean = mean(plot_perc_live_mean, na.rm = TRUE),
    n_plots = n(),
    LAI_sd = sd(plot_lai_mean, na.rm = TRUE),
    LAI_se = std.error(plot_lai_mean),
    Perc_Live_sd = sd(plot_perc_live_mean, na.rm = TRUE),
    Perc_Live_se = std.error(plot_perc_live_mean),
    .groups = 'drop'
  )

lai_plot <- ggplot(site_summary, aes(x = top_date, y = LAI_mean, group = 1)) +
  geom_line(color = "gray50") +  # Use a neutral color for the connecting line
  geom_errorbar(
    aes(ymin = LAI_mean - LAI_se, ymax = LAI_mean + LAI_se),
    width = 25,
    color = "darkgray"
  ) +
  geom_point(aes(color = Timepoint), size = 4, alpha = 0.8) + # Color points by Timepoint
  scale_x_date(
    date_breaks = "4 months",
    date_labels = "%b %Y" # Format 'Mon YYYY'
  ) +
  scale_color_viridis_d() +
  facet_wrap(~ Site_Name, scales = "free") +
  labs(
    title = "Mean Live Area Index (LAI) Over Time by Site",
    subtitle = "Error bars represent standard error of plot means",
    x = "Date",
    y = "Mean LAI (± SE)",
    color = "Timepoint"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

Perc_Live_plot <- ggplot(site_summary, aes(x = top_date, y = Perc_Live_mean, group = 1)) +
  geom_line(color = "gray50") +
  geom_errorbar(
    aes(ymin = Perc_Live_mean - Perc_Live_se, ymax = Perc_Live_mean + Perc_Live_se),
    width = 25,
    color = "darkgray"
  ) +
  geom_point(aes(color = Timepoint), size = 4, alpha = 0.8) +
  scale_x_date(
    date_breaks = "4 months",
    date_labels = "%b %Y"
  ) +
  scale_color_viridis_d() +
  facet_wrap(~ Site_Name, scales = "free") +
  labs(
    title = "Mean Percent Living Tissue Over Time by Site",
    subtitle = "Error bars represent standard error of plot means",
    x = "Date",
    y = "Mean %Living Tissue (± SE)",
    color = "Timepoint"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

binned_proportions <- Tagged_Metrics %>%
  # 1. Count the occurrences of each Bin within each group.
  count(Site_Name, Timepoint, top_date, Bin) %>%
  
  # 2. Make implicit missing values explicit. This is the key step.
  complete(nesting(Site_Name, Timepoint, top_date), Bin, fill = list(n = 0)) %>%
  
  # 3. Group by the observation event to calculate proportions.
  group_by(Site_Name, Timepoint, top_date) %>%
  
  # 4. Calculate the proportion for each Bin.
  mutate(Prop = n / sum(n)) %>%
  
  # 5. Ungroup for safety in subsequent steps.
  ungroup()

# This single pipeline prepares the factors and creates the plot
SizeBin_Fig <- binned_proportions %>%
  # Use mutate to relabel and reorder the 'Bin' factor for plotting
  mutate(
    Bin = fct_recode(Bin,
                     "Dead"        = "Bin_0",
                     "Small"       = "Bin_1",
                     "Medium"      = "Bin_2",
                     "Large"       = "Bin_3",
                     "Extra Large" = "Bin_4"
    ),
    # Relevel the factor to set the desired stacking order in the plot
    Bin = fct_relevel(Bin,
                      "Extra Large", "Large", "Medium", "Small", "Dead"
    )
  ) %>%
  # Now pipe the prepared data directly into ggplot
  ggplot(aes(x = top_date, y = Prop, fill = Bin, group = Bin)) +
  # Create the stacked area plot
  geom_area(position = "stack", alpha = 0.8, color = "white", linewidth = 0.2) +
  # Add points at each observation date, stacked in the same way as the areas
  geom_point(position = "stack", size = 2, shape = 21, color = "white") +
  # Use a named vector for scale_fill_manual to ensure colors are correctly assigned
  scale_fill_manual(
    name = "Colony Size",
    values = c(
      "Extra Large" = "#0072B2",
      "Large"       = "#E69F00",
      "Medium"      = "#009E73",
      "Small"       = "#CC79A7",
      "Dead"        = "#696969"
    )
  ) +
  facet_wrap(~ Site_Name) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "4 months"
  ) +
  labs(
    x = "Date",
    y = "Proportion of Tagged Colonies",
    title = "Proportional Size Class Distribution of Corals Over Time"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

