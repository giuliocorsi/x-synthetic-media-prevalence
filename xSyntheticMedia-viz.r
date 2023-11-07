#######################################################################################################################
   # Set up environment #
#######################################################################################################################

install.packages("reticulate")
install.packages('ggdist')
install.packages("readxl")
library(reticulate)
library(tidyverse)
library(ggplot2)
library(ggdist)
library(dplyr)
library(lubridate)
library(readxl)
library(scales)
library(gridExtra)
options(scipen = 999)

pd <- import("pandas")

#######################################################################################################################
   # Import Data from Python Pickle and Labelled Data from Csv #
#######################################################################################################################

community_notes <- as.list(pd$read_pickle("data/community-notes-filtered.pkl"))
annotated_data <- read.csv("data/annotated-data.csv")

#######################################################################################################################
   # Pre-Process Data to Format Numeric Columns and Compute Views/Followers Ratio #
#######################################################################################################################

convert_to_numeric <- function(x) {
  if (grepl("k", x)) {
    return(as.numeric(gsub("k", "", x)) * 1e3)
  } else if (grepl("m", x)) {
    return(as.numeric(gsub("m", "", x)) * 1e6)
  } else {
    return(as.numeric(x))
  }
}

columns_to_transform <- c("views", "reposts", "quotes", "likes","bookmarks","userFollowers")
for (col in columns_to_transform) {
  annotated_data[[col]] <- sapply(annotated_data[[col]], convert_to_numeric)
}

annotated_data$viewsFollowersRatio <- with(annotated_data, ifelse(userFollowers > 0, views / userFollowers, NA))
annotated_data <- annotated_data[!is.na(annotated_data$viewsFollowersRatio), ]

#######################################################################################################################
   # Set a Custom Theme for Visualizations #
#######################################################################################################################

custom_theme <- theme_bw(base_size = 16) +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_blank(), # Remove vertical lines
        panel.grid.major.y = element_line(color = "gray"), # Keep horizontal lines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.2),
        axis.line.y = element_line(color = "black", size = 0.2),
        axis.ticks = element_line(size = 1),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.caption = element_text(size = 15, face = "bold"),
        legend.position = "none",
        legend.box = "none", 
        plot.margin = margin(t = 30, r = 10, b = 30, l = 10, unit = "pt"))

#######################################################################################################################
   # Relative Frequency of Community Notes Mentioning Synthetic Media by Monthly #
#######################################################################################################################

plot_visual_disinformation_percentage <- function(community_notes) {

  preprocess_data <- function(df) {
    df$Month <- as.Date(df$date, format='%d/%m/%Y')
    monthly_counts <- df %>%
      group_by(Month = floor_date(Month, "month")) %>%
      summarise(n = n_distinct(tweetId), .groups = "drop") %>%  
      mutate(Month_Year = format(Month, "%m/%Y")) %>%
      filter(Month > as.Date("2022-11-01")) %>%
      filter(Month <= as.Date("2023-09-30"))
    return(monthly_counts)
  }

  monthly_total_notes <- preprocess_data(community_notes[['no_visual']])
  monthly_visual_disinformation <- preprocess_data(community_notes[['visual']])
  monthly_counts <- monthly_total_notes %>%
    full_join(monthly_visual_disinformation, by = c("Month", "Month_Year"), suffix = c("_total", "_visual")) %>%
    replace_na(list(n_total = 0, n_visual = 0)) %>%
    mutate(Percentage = (n_visual / n_total) * 100)

  percentage_plot <- ggplot(monthly_counts, aes(x = Month, y = Percentage)) +
    geom_bar(stat = "identity", fill = '#539794') +
    geom_smooth(se=FALSE,size=2,color="#D4AF37") +
    scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
    labs(title = "Monthly Percentage of Notes Mentioning AI-Generated Media",
         y = "Percentage", x = "Month/Year") +
    custom_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(percentage_plot)
}

percentage_plot = plot_visual_disinformation_percentage(community_notes)

#######################################################################################################################
   # Monthly Views Obtained by Synthetic Media #
#######################################################################################################################

plot_monthly_views <- function(annotated_data) {

  preprocess_data <- function(df) {
    df$Month <- as.Date(df$tweetDate, format='%d/%m/%Y')
    monthly_counts <- df %>%
      group_by(Month = floor_date(Month, "month")) %>%
      summarise(total_views = sum(views, na.rm = TRUE), .groups = "drop") %>%  
      filter(Month > as.Date("2022-11-01")) %>%
      filter(Month <= as.Date("2023-09-30"))
    return(monthly_counts)
  }

  monthly_counts <- preprocess_data(annotated_data)
  
  views_plot <- ggplot(monthly_counts, aes(x = Month, y = total_views)) +
    geom_bar(stat = "identity", fill = '#539794') +
    geom_smooth(se=FALSE, size=2, color="#D4AF37") +
    scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
    scale_y_continuous(labels = function(x) paste0(number(x / 1e6, accuracy = 1), "m")) +
    labs(title = "Monthly Views of Tweets with AI-Generated Media",
         y = "Total Views", x = "Month/Year") +
    custom_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(views_plot)
}

raw_views_plot = plot_monthly_views(annotated_data) 

#######################################################################################################################
   # Merge and Output Temporal Plots #
#######################################################################################################################

merged_temporal_plot = grid.arrange(percentage_plot, raw_views_plot, nrow = 2)
ggsave("figures/fig_1.png", plot = merged_temporal_plot, width = 11, height = 10, dpi = 500)

#######################################################################################################################
   # Stepwise CDF Plot #
#######################################################################################################################

weekly_cdf_plot <- function(annotated_data) {
  annotated_data <- annotated_data %>%
    mutate(
      tweetDate = as.Date(tweetDate, format="%d/%m/%Y"),
      Week = floor_date(tweetDate, unit="week")
    )
  
  weekly_data <- annotated_data %>%
    group_by(Week) %>%
    summarise(weekly_views = sum(views), .groups = "drop") %>%
    arrange(Week) %>%
    mutate(
      cumulative_views = cumsum(weekly_views),
      Week_Label = strftime(Week, format="%V/%Y"),
      cumulative_views_millions = cumulative_views / 1e6
    )
  
  cdf_plot <- ggplot(weekly_data, aes(x = Week, y = cumulative_views_millions)) +
    geom_step(color="#539794", size=2) +
    geom_vline(xintercept = as.Date("2023-03-13"), linetype="dotted", color="#D4AF37", size=2) +
    scale_x_date(
      date_breaks = "5 weeks", 
      labels = function(x) strftime(x, format="%V/%Y")  
    ) +
    scale_y_continuous(labels = scales::label_number(suffix = "m")) +
    labs(x = "Week", y = "Cumulative Views (Millions)", title = "CDF of Views of Tweets with AI-Generated Media") +
    custom_theme
  
  return(cdf_plot)
}

cdf_plot <- weekly_cdf_plot(annotated_data)
ggsave("figures/fig_2.png", plot = cdf_plot, width = 11, height = 10, dpi = 500)

#######################################################################################################################
   # Raincloud Plots by Features Combinations (log scale) - Define Main Function #
#######################################################################################################################

plot_raincloud <- function(data_frame, views_col, grouping_var, custom_title, color_values) {
  
  # Filter out NA values
  data_filtered <- na.omit(data_frame[, c(views_col, grouping_var)])
  
  # Convert categorical_col to factor if it's not
  data_filtered[[grouping_var]] <- as.factor(data_filtered[[grouping_var]])
  
  # Custom label function for axis
  label_format <- function(x) {
    ifelse(x >= 1e6, paste0(round(x / 1e6, 1), 'm'),
           ifelse(x >= 1e3, paste0(round(x / 1e3, 1), 'k'), x))
  }

  # Create the raincloud plot
  p <- ggplot(data_filtered, aes_string(x = views_col, y = grouping_var, fill = grouping_var, color = grouping_var)) +
    stat_halfeye(point_color = NA,
      .width = 0, height = 0.6,
      position = position_nudge(y = 0.3)
    ) +
    geom_boxplot(
      position = position_nudge(y = 0.2),
      width = 0.1, outlier.shape = NA,
      aes(fill=NULL)
    ) +
    geom_point(
      position = position_jitter(width = 0, height = 0.1, seed = 1)
    ) +
    coord_flip() +
    scale_color_manual(values = color_values) +
    scale_fill_manual(values = color_values) +
    scale_x_continuous(
      trans = 'log10', 
      labels = label_format, 
      limits = c(1e2, 1e8), 
      breaks = c(1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8)
    ) +
    labs(
      title = custom_title,
      x = "Views (Log Scale)", 
      y = "Category"
    ) +
    custom_theme +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)  # Center-align the title
    )

  # Save the plot
  return(p)
}

#######################################################################################################################
   # Raincloud Plots by Features Combinations (log scale) - Run and Save Plots for Each Feature Combination #
#######################################################################################################################

# Define the color mappings for each categorical variable
media_map <- c("IMAGE" = "#539794", "VIDEO" = "#D4AF37")
pol_map <- c("NON-POLITICAL" = "#539794", "POLITICAL" = "#D4AF37")
verified_map <- c("FALSE" = "#539794", "TRUE" = "#D4AF37")

# Create the plots for the categorial variables POLITICAL and MEDIA
political_raincloud = plot_raincloud(annotated_data, 'views', 'political', 'Tweet Views by Political Status', pol_map)
media_raincloud = plot_raincloud(annotated_data, 'views', 'media', 'Tweet Views by Media Type', media_map)
media_pol_rainclouds = grid.arrange(political_raincloud,media_raincloud,ncol=2)

ggsave("figures/fig_3.png", plot = media_pol_rainclouds, width = 14, height = 10, dpi = 500)


# Create the plots for the categorial variable VERIFIED with views and viewsFollowersRatio
verified_raincloud = plot_raincloud(annotated_data, 'views', 'verified', 'Tweet Views by Verified Status', verified_map)

verified_ratio_raincloud <- plot_raincloud(annotated_data, 'viewsFollowersRatio', 'verified', 'Tweet Views/Followers Ratio by Verified Status', verified_map) +
  scale_x_continuous(trans = 'log10', 
                     limits = c(0.01, 10000), 
                     breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                     labels = c("0.01", "0.1", "1", "10", "100", "1000", "10000")) +
    labs( x = "Views/Followers Ratio (Log Scale)")

verified_rainclouds = grid.arrange(verified_raincloud,verified_ratio_raincloud,ncol=2)
ggsave("figures/fig_4.png", plot = verified_rainclouds, width = 14, height = 10, dpi = 500)

#######################################################################################################################
   # Scraps #
#######################################################################################################################