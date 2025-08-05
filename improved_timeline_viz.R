# Improved Time-Based Visualization for Sentiment Analysis
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)

# Read all CSV files to get proper date range
csv_files <- list.files(pattern = "seattle_news_.*\\.csv", full.names = TRUE)
news_data <- map_dfr(csv_files, ~ read.csv(.x, stringsAsFactors = FALSE)) %>%
  mutate(publish_date = as.Date(publish_date)) %>%
  filter(!is.na(publish_date))

# Check actual date range
date_range <- range(news_data$publish_date, na.rm = TRUE)
cat("Date range:", as.character(date_range[1]), "to", as.character(date_range[2]), "\n")

# Read sentiment data
sentiment_scores <- read.csv("article_sentiments.csv", stringsAsFactors = FALSE)

# Create daily sentiment trends with better aggregation
daily_sentiment <- sentiment_scores %>%
  left_join(news_data %>% select(url, publish_date), by = "url") %>%
  filter(!is.na(publish_date)) %>%
  group_by(publish_date, source) %>%
  summarise(
    avg_sentiment = mean(avg_sentiment),
    articles = n(),
    .groups = "drop"
  ) %>%
  # Add 3-day rolling average for smoother trends
  arrange(source, publish_date) %>%
  group_by(source) %>%
  mutate(
    sentiment_smooth = zoo::rollmean(avg_sentiment, k = 3, fill = NA, align = "center")
  ) %>%
  ungroup()

# Create improved timeline visualization
timeline_plot <- ggplot(daily_sentiment, aes(x = publish_date)) +
  # Raw daily sentiment (lighter)
  geom_line(aes(y = avg_sentiment, color = source), alpha = 0.3, size = 0.5) +
  # Smoothed trend (darker)
  geom_line(aes(y = sentiment_smooth, color = source), size = 1.2) +
  # Add points for article volume
  geom_point(aes(y = avg_sentiment, size = articles, color = source), alpha = 0.6) +
  # Reference line at neutral sentiment
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  # Improved scales
  scale_x_date(
    date_labels = "%m/%d",
    date_breaks = "2 days",
    expand = expansion(mult = 0.02)
  ) +
  scale_size_continuous(
    range = c(1, 4),
    name = "Articles",
    breaks = c(1, 5, 10, 15),
    labels = c("1", "5", "10", "15+")
  ) +
  # Better color palette
  scale_color_viridis_d(name = "News Source") +
  # Improved labels and theme
  labs(
    title = "News Sentiment Over Time by Source",
    subtitle = "Thick lines show 3-day rolling average, thin lines show daily values",
    x = "Date",
    y = "Average Sentiment Score",
    caption = "Point size indicates number of articles per day"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60")
  )

print(timeline_plot)
ggsave("improved_timeline.png", timeline_plot, width = 12, height = 8, dpi = 300)

# Create faceted version for better individual source clarity
faceted_timeline <- ggplot(daily_sentiment, aes(x = publish_date, y = avg_sentiment)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(aes(size = articles), color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "red", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  facet_wrap(~source, scales = "free_y") +
  scale_x_date(date_labels = "%m/%d", date_breaks = "3 days") +
  scale_size_continuous(range = c(1, 3), name = "Articles") +
  labs(
    title = "Daily Sentiment Trends by News Source",
    subtitle = "Red line shows trend with confidence interval",
    x = "Date",
    y = "Average Sentiment Score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(faceted_timeline)
ggsave("faceted_timeline.png", faceted_timeline, width = 12, height = 8, dpi = 300)

# Create volume vs sentiment scatter plot over time
volume_sentiment <- daily_sentiment %>%
  mutate(week = floor_date(publish_date, "week")) %>%
  group_by(week, source) %>%
  summarise(
    total_articles = sum(articles),
    avg_sentiment = mean(avg_sentiment),
    .groups = "drop"
  )

volume_plot <- ggplot(volume_sentiment, aes(x = total_articles, y = avg_sentiment)) +
  geom_point(aes(color = source, size = as.numeric(week)), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "gray50", linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "solid", alpha = 0.3) +
  geom_vline(xintercept = median(volume_sentiment$total_articles), linetype = "solid", alpha = 0.3) +
  scale_color_viridis_d(name = "Source") +
  scale_size_continuous(name = "Week", guide = "none") +
  labs(
    title = "Article Volume vs Sentiment by Week",
    subtitle = "Larger points represent more recent weeks",
    x = "Total Articles per Week",
    y = "Average Sentiment Score",
    caption = "Lines show median volume (vertical) and neutral sentiment (horizontal)"
  ) +
  theme_minimal()

print(volume_plot)
ggsave("volume_sentiment.png", volume_plot, width = 10, height = 6, dpi = 300)

cat("\nImproved visualizations saved:\n")
cat("- improved_timeline.png: Multi-source timeline with smoothing\n")
cat("- faceted_timeline.png: Individual source trends with confidence intervals\n") 
cat("- volume_sentiment.png: Volume vs sentiment relationship\n")