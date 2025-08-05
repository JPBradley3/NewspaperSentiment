# Quick timeline fix
library(tidyverse)
library(ggplot2)

# Read sentiment data with dates
sentiment_data <- read.csv("article_sentiments.csv", stringsAsFactors = FALSE)

# Parse dates properly
sentiment_data <- sentiment_data %>%
  mutate(
    publish_date_clean = case_when(
      str_detect(publish_date, "^\\d{4}-\\d{2}-\\d{2}") ~ as.Date(publish_date),
      str_detect(publish_date, "^\\w{3}, \\d{2} \\w{3} 20\\d{2}") ~ as.Date(publish_date, format = "%a, %d %b %Y"),
      str_detect(publish_date, "^\\w{6,9} \\d{1,2}, 20\\d{2}") ~ as.Date(publish_date, format = "%B %d, %Y"),
      str_detect(publish_date, "^\\w{3} \\d{1,2}, 20\\d{2}") ~ as.Date(publish_date, format = "%b %d, %Y"),
      str_detect(publish_date, "^\\w{6,9} \\d{1,2} at") ~ as.Date(str_extract(publish_date, "^\\w{6,9} \\d{1,2}"), format = "%B %d"),
      TRUE ~ as.Date(NA)
    )
  ) %>%
  filter(!is.na(publish_date_clean))

# Create daily timeline
daily_sentiment <- sentiment_data %>%
  group_by(publish_date_clean, source) %>%
  summarise(avg_sentiment = mean(avg_sentiment), articles = n(), .groups = "drop")

# Simple timeline plot
timeline_plot <- ggplot(daily_sentiment, aes(x = publish_date_clean, y = avg_sentiment, color = source)) +
  geom_line(size = 1.2) +
  geom_point(aes(size = articles), alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_date(date_labels = "%m/%d") +
  labs(title = "Sentiment Timeline", x = "Date", y = "Sentiment") +
  theme_minimal()

print(timeline_plot)
ggsave("timeline_fixed.png", timeline_plot, width = 12, height = 6)

print(daily_sentiment)