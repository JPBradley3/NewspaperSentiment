library(tidyverse)
library(ggplot2)

# Read data
sentiment_data <- read.csv("article_sentiments.csv")

# Extract just the date part and convert
sentiment_data$date_clean <- as.Date(str_extract(sentiment_data$publish_date, "\\d{4}-\\d{2}-\\d{2}"))

# Remove rows with no valid dates
timeline_data <- sentiment_data %>% 
  filter(!is.na(date_clean)) %>%
  group_by(date_clean, source) %>%
  summarise(avg_sentiment = mean(avg_sentiment), .groups = "drop")

# Plot
ggplot(timeline_data, aes(x = date_clean, y = avg_sentiment, color = source)) +
  geom_line() + geom_point() +
  labs(title = "Timeline", x = "Date", y = "Sentiment")

print(timeline_data)