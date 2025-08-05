library(tidyverse)
library(ggplot2)

sentiment_data <- read.csv("article_sentiments.csv")

# Use the working date extraction
sentiment_data$date_clean <- case_when(
  str_detect(sentiment_data$publish_date, "2025-\\d{2}-\\d{2}") ~ as.Date(str_extract(sentiment_data$publish_date, "2025-\\d{2}-\\d{2}")),
  str_detect(sentiment_data$publish_date, "Aug 2025") ~ as.Date("2025-08-04"),
  str_detect(sentiment_data$publish_date, "August.*2025") ~ as.Date("2025-08-04"),
  TRUE ~ as.Date(NA)
)

# Create timeline with valid dates
timeline_data <- sentiment_data %>% 
  filter(!is.na(date_clean)) %>%
  group_by(source) %>%
  summarise(avg_sentiment = mean(avg_sentiment), articles = n(), .groups = "drop")

# Since all dates are same day, plot by source instead
ggplot(timeline_data, aes(x = reorder(source, avg_sentiment), y = avg_sentiment)) +
  geom_col(aes(fill = avg_sentiment > 0)) +
  coord_flip() +
  labs(title = "Sentiment by Source (Aug 4, 2025)", x = "Source", y = "Sentiment") +
  theme_minimal()

ggsave("source_sentiment.png", width = 10, height = 6)