library(tidyverse)
library(ggplot2)

# Read data
sentiment_data <- read.csv("article_sentiments.csv")
news_data <- map_dfr(list.files(pattern = "seattle_news_.*\\.csv", full.names = TRUE), 
                     ~ read.csv(.x, stringsAsFactors = FALSE))

# Join sentiment with candidate keywords
timeline_data <- sentiment_data %>%
  left_join(news_data %>% select(url, matched_keywords), by = "url") %>%
  mutate(
    candidate = case_when(
      str_detect(tolower(matched_keywords), "harrell") ~ "Harrell",
      str_detect(tolower(matched_keywords), "wilson") ~ "Wilson", 
      str_detect(tolower(matched_keywords), "nelson") ~ "Nelson",
      str_detect(tolower(matched_keywords), "davison") ~ "Davison",
      TRUE ~ NA_character_
    ),
    article_order = row_number()
  ) %>%
  filter(!is.na(candidate))

# Plot sentiment per article over time (by article order)
ggplot(timeline_data, aes(x = article_order, y = avg_sentiment, color = candidate)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Candidate Sentiment Over Article Sequence", 
       x = "Article Order", y = "Sentiment", color = "Candidate") +
  theme_minimal()

ggsave("candidate_timeline.png", width = 12, height = 6)