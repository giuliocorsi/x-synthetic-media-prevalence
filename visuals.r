# Synthetic Media Prevalence on X - Visualization Script
# This script generates publication-quality figures for the analysis.

library(reticulate)
library(tidyverse)
library(ggplot2)
library(ggdist)
library(dplyr)
library(lubridate)
library(scales)
library(gridExtra)

options(scipen = 999)
pd <- import("pandas")

# Configuration
DATE_START <- as.Date("2022-11-01")
DATE_END <- as.Date("2023-09-30")
PRIMARY_COLOR <- "#539794"
ACCENT_COLOR <- "#D4AF37"

# =============================================================================
# Data Import and Preprocessing
# =============================================================================

community_notes <- as.list(pd$read_pickle("data/community-notes-filtered.pkl"))
annotated_data <- read.csv("data/annotated-data.csv")

convert_to_numeric <- function(x) {
  if (grepl("k", x, ignore.case = TRUE)) {
    as.numeric(gsub("k", "", x, ignore.case = TRUE)) * 1e3
  } else if (grepl("m", x, ignore.case = TRUE)) {
    as.numeric(gsub("m", "", x, ignore.case = TRUE)) * 1e6
  } else {
    as.numeric(x)
  }
}

numeric_columns <- c("views", "reposts", "quotes", "likes", "bookmarks", "userFollowers")
for (col in numeric_columns) {
  annotated_data[[col]] <- sapply(annotated_data[[col]], convert_to_numeric)
}

annotated_data <- annotated_data %>%
  mutate(viewsFollowersRatio = ifelse(userFollowers > 0, views / userFollowers, NA)) %>%
  filter(!is.na(viewsFollowersRatio))

# =============================================================================
# Custom Theme
# =============================================================================

custom_theme <- theme_bw(base_size = 16) +
  theme(
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.2),
    axis.line.y = element_line(color = "black", linewidth = 0.2),
    axis.ticks = element_line(linewidth = 1),
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.caption = element_text(size = 15, face = "bold"),
    legend.position = "none",
    legend.box = "none",
    plot.margin = margin(t = 30, r = 10, b = 30, l = 10, unit = "pt")
  )

# =============================================================================
# Helper Functions
# =============================================================================

aggregate_monthly_counts <- function(df, date_col, count_col = NULL) {
  df %>%
    mutate(Month = floor_date(as.Date(.data[[date_col]], format = "%d/%m/%Y"), "month")) %>%
    filter(Month > DATE_START, Month <= DATE_END) %>%
    {
      if (is.null(count_col)) {
        group_by(., Month) %>% summarise(n = n_distinct(tweetId), .groups = "drop")
      } else {
        group_by(., Month) %>% summarise(total = sum(.data[[count_col]], na.rm = TRUE), .groups = "drop")
      }
    }
}

format_axis_labels <- function(x) {
  ifelse(x >= 1e6, paste0(round(x / 1e6, 1), "m"),
    ifelse(x >= 1e3, paste0(round(x / 1e3, 1), "k"), x))
}

# =============================================================================
# Plot 1: Monthly Percentage of Community Notes Mentioning Synthetic Media
# =============================================================================

plot_notes_percentage <- function(community_notes) {
  total_counts <- aggregate_monthly_counts(community_notes[["no_visual"]], "date")
  visual_counts <- aggregate_monthly_counts(community_notes[["visual"]], "date")

  monthly_data <- total_counts %>%
    full_join(visual_counts, by = "Month", suffix = c("_total", "_visual")) %>%
    replace_na(list(n_total = 0, n_visual = 0)) %>%
    mutate(Percentage = (n_visual / n_total) * 100)

  ggplot(monthly_data, aes(x = Month, y = Percentage)) +
    geom_bar(stat = "identity", fill = PRIMARY_COLOR) +
    geom_smooth(se = FALSE, linewidth = 2, color = ACCENT_COLOR) +
    scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
    labs(
      title = "Monthly Percentage of Tweets with Notes Mentioning Synthetic Media",
      x = "Month/Year",
      y = "Percentage"
    ) +
    custom_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# =============================================================================
# Plot 2: Monthly Percentage of Tweets Containing Synthetic Media
# =============================================================================

plot_synthetic_percentage <- function(community_notes, annotated_data) {
  all_notes <- bind_rows(
    community_notes[["no_visual"]] %>%
      mutate(Month = floor_date(as.Date(date, format = "%d/%m/%Y"), "month")) %>%
      filter(Month > DATE_START, Month <= DATE_END) %>%
      count(Month),
    community_notes[["visual"]] %>%
      mutate(Month = floor_date(as.Date(date, format = "%d/%m/%Y"), "month")) %>%
      filter(Month > DATE_START, Month <= DATE_END) %>%
      count(Month)
  ) %>%
    group_by(Month) %>%
    summarise(total_n = sum(n, na.rm = TRUE), .groups = "drop")

  annotated_counts <- annotated_data %>%
    mutate(Month = floor_date(as.Date(tweetDate, format = "%d/%m/%Y"), "month")) %>%
    count(Month)

  plot_data <- all_notes %>%
    left_join(annotated_counts, by = "Month") %>%
    mutate(Percentage = (n / total_n) * 100)

  ggplot(plot_data, aes(x = Month, y = Percentage)) +
    geom_col(fill = PRIMARY_COLOR) +
    geom_smooth(se = FALSE, linewidth = 2, color = ACCENT_COLOR) +
    scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
    labs(
      title = "Monthly Percentage of Community-Noted Tweets Containing Synthetic Media",
      x = "Month/Year",
      y = "Percentage"
    ) +
    custom_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# =============================================================================
# Plot 3: Monthly Views of Synthetic Media Tweets
# =============================================================================

plot_monthly_views <- function(annotated_data) {
  monthly_data <- annotated_data %>%
    mutate(Month = floor_date(as.Date(tweetDate, format = "%d/%m/%Y"), "month")) %>%
    filter(Month > DATE_START, Month <= DATE_END) %>%
    group_by(Month) %>%
    summarise(total_views = sum(views, na.rm = TRUE), .groups = "drop")

  ggplot(monthly_data, aes(x = Month, y = total_views)) +
    geom_bar(stat = "identity", fill = PRIMARY_COLOR) +
    geom_smooth(se = FALSE, linewidth = 2, color = ACCENT_COLOR) +
    scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
    scale_y_continuous(labels = function(x) paste0(number(x / 1e6, accuracy = 1), "m")) +
    labs(
      title = "Monthly Views of Tweets with Synthetic Media",
      x = "Month/Year",
      y = "Total Views"
    ) +
    custom_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# =============================================================================
# Plot 4: Monthly Raw Frequency
# =============================================================================

plot_raw_frequency <- function(annotated_data) {
  monthly_data <- annotated_data %>%
    mutate(Month = floor_date(as.Date(tweetDate, format = "%d/%m/%Y"), "month")) %>%
    filter(Month > DATE_START, Month <= DATE_END) %>%
    group_by(Month) %>%
    summarise(unique_tweets = n_distinct(tweetId), .groups = "drop")

  ggplot(monthly_data, aes(x = Month, y = unique_tweets)) +
    geom_bar(stat = "identity", fill = PRIMARY_COLOR) +
    geom_smooth(se = FALSE, linewidth = 2, color = ACCENT_COLOR) +
    scale_x_date(labels = date_format("%m/%Y"), date_breaks = "1 month") +
    labs(
      title = "Monthly Number of Unique Tweets Containing Synthetic Media",
      x = "Month",
      y = "Unique Tweets"
    ) +
    custom_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# =============================================================================
# Plot 5: Cumulative Distribution Function
# =============================================================================

plot_weekly_cdf <- function(annotated_data) {
  weekly_data <- annotated_data %>%
    mutate(
      tweetDate = as.Date(tweetDate, format = "%d/%m/%Y"),
      Week = floor_date(tweetDate, unit = "week")
    ) %>%
    group_by(Week) %>%
    summarise(weekly_views = sum(views), .groups = "drop") %>%
    arrange(Week) %>%
    mutate(
      cumulative_views = cumsum(weekly_views),
      cumulative_views_millions = cumulative_views / 1e6
    )

  ggplot(weekly_data, aes(x = Week, y = cumulative_views_millions)) +
    geom_step(color = PRIMARY_COLOR, linewidth = 2) +
    geom_vline(xintercept = as.Date("2023-03-13"), linetype = "dotted", color = ACCENT_COLOR, linewidth = 2) +
    scale_x_date(date_breaks = "5 weeks", labels = function(x) strftime(x, format = "%V/%Y")) +
    scale_y_continuous(labels = label_number(suffix = "m")) +
    labs(
      title = "CDF of Views of Tweets with AI-Generated Media",
      x = "Week",
      y = "Cumulative Views (Millions)"
    ) +
    custom_theme
}

# =============================================================================
# Plot 6: Raincloud Plots
# =============================================================================

plot_raincloud <- function(data, views_col, grouping_var, title, color_map) {
  data_filtered <- data %>%
    select(all_of(c(views_col, grouping_var))) %>%
    na.omit() %>%
    mutate(across(all_of(grouping_var), as.factor))

  ggplot(data_filtered, aes(x = .data[[views_col]], y = .data[[grouping_var]],
                            fill = .data[[grouping_var]], color = .data[[grouping_var]])) +
    stat_halfeye(
      point_color = NA,
      .width = 0,
      height = 0.6,
      position = position_nudge(y = 0.3)
    ) +
    geom_boxplot(
      position = position_nudge(y = 0.2),
      width = 0.1,
      outlier.shape = NA,
      aes(fill = NULL)
    ) +
    geom_point(position = position_jitter(width = 0, height = 0.1, seed = 1)) +
    coord_flip() +
    scale_color_manual(values = color_map) +
    scale_fill_manual(values = color_map) +
    scale_x_continuous(
      trans = "log10",
      labels = format_axis_labels,
      limits = c(1e2, 1e8),
      breaks = c(1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8)
    ) +
    labs(
      title = title,
      x = "Views (Log Scale)",
      y = "Category"
    ) +
    custom_theme +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
}

# =============================================================================
# Generate and Save Figures
# =============================================================================

# Figure 1: Temporal trends
percentage_plot <- plot_synthetic_percentage(community_notes, annotated_data)
views_plot <- plot_monthly_views(annotated_data)
fig_1 <- grid.arrange(percentage_plot, views_plot, nrow = 2)
ggsave("figures/fig_1.png", plot = fig_1, width = 11, height = 9, dpi = 1500)

# Figure 2: CDF
cdf_plot <- plot_weekly_cdf(annotated_data)
ggsave("figures/fig_2.png", plot = cdf_plot, width = 14, height = 10, dpi = 500)

# Figure 3: Political and media type raincloud plots
media_colors <- c("IMAGE" = PRIMARY_COLOR, "VIDEO" = ACCENT_COLOR)
political_colors <- c("NON-POLITICAL" = PRIMARY_COLOR, "POLITICAL" = ACCENT_COLOR)

political_raincloud <- plot_raincloud(annotated_data, "views", "political",
                                      "Tweet Views by Political Status", political_colors)
media_raincloud <- plot_raincloud(annotated_data, "views", "media",
                                  "Tweet Views by Media Type", media_colors)
fig_3 <- grid.arrange(political_raincloud, media_raincloud, ncol = 2)
ggsave("figures/fig_3.png", plot = fig_3, width = 14, height = 10, dpi = 500)

# Figure 4: Verification status raincloud plots
verified_colors <- c("FALSE" = PRIMARY_COLOR, "TRUE" = ACCENT_COLOR)

verified_views <- plot_raincloud(annotated_data, "views", "verified",
                                 "Tweet Views by Verified Status", verified_colors)
verified_ratio <- plot_raincloud(annotated_data, "viewsFollowersRatio", "verified",
                                 "Tweet Views/Followers Ratio by Verified Status", verified_colors) +
  scale_x_continuous(
    trans = "log10",
    limits = c(0.01, 10000),
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
    labels = c("0.01", "0.1", "1", "10", "100", "1000", "10000")
  ) +
  labs(x = "Views/Followers Ratio (Log Scale)")

fig_4 <- grid.arrange(verified_views, verified_ratio, ncol = 2)
ggsave("figures/fig_4.png", plot = fig_4, width = 14, height = 10, dpi = 500)

# Additional plots (not saved to file)
notes_percentage_plot <- plot_notes_percentage(community_notes)
print(notes_percentage_plot)

raw_frequency_plot <- plot_raw_frequency(annotated_data)
print(raw_frequency_plot)
