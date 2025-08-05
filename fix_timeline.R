# Fix timeline visualization with proper date parsing
library(tidyverse)
library(ggplot2)
library(lubridate)

# Read data with proper date parsing
csv_files <- list.files(pattern = "seattle_news_.*\\.csv", full.names = TRUE)
news_data <- map_dfr(csv_files, ~ read.csv(.x, stringsAsFactors = FALSE)) %>%
  mutate(
    publish_date = case_when(
      str_detect(publish_date, "^\\d{4}-\\d{2}-\\d{2}") ~ as.Date(publish_date),
      str_detect(publish_date, "^\\w{3}, \\d{2} \\w{3} 20\\d{2}") ~ as.Date(publish_date, format = "%a, %d %b %Y"),
      str_detect(publish_date, "^\\w{6,9} \\d{2}, 20\\d{2}") ~ as.Date(publish_date, format = "%B %d, %Y"),
      TRUE ~ as.Date(publish_date)
    )
  ) %>%
  filter(!is.na(publish_date))

# Check date range
date_range <- range(news_data$publish_date, na.rm = TRUE)
cat("Actual date range:", as.character(date_range[1]), "to", as.character(date_range[2]), "\n")
cat("Total unique dates:", n_distinct(news_data$publish_date), "\n")

# Read sentiment data
sentiment_scores <- read.csv("article_sentiments.csv", stringsAsFactors = FALSE)

# Create timeline data
timeline_data <- sentiment_scores %>%
  left_join(news_data %>% select(url, publish_date, source), by = "url") %>%
  filter(!is.na(publish_date))

# Daily sentiment timeline
daily_sentiment <- timeline_data %>%
  group_by(publish_date, source) %>%
  summarise(
    avg_sentiment = mean(avg_sentiment),
    articles = n(),
    .groups = "drop"
  )

# Create clear timeline
timeline_plot <- ggplot(daily_sentiment, aes(x = publish_date, y = avg_sentiment, color = source)) +
  geom_line(size = 1.2) +
  geom_point(aes(size = articles), alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "1 day") +
  scale_size_continuous(range = c(2, 5), name = "Articles") +
  labs(
    title = "News Sentiment Over Time",
    x = "Date",
    y = "Average Sentiment Score",
    color = "Source"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(timeline_plot)
ggsave("fixed_timeline.png", timeline_plot, width = 12, height = 6)

print(daily_sentiment)