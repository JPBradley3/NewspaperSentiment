# Seattle News Sentiment Analysis by Source
library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(scales)

# Read all CSV files and combine them
csv_files <- list.files(pattern = "seattle_news_.*\\.csv", full.names = TRUE)
if (length(csv_files) == 0) stop("No CSV files found")

news_data <- map_dfr(csv_files, ~ read.csv(.x, stringsAsFactors = FALSE)) %>%
  mutate(
    publish_date = case_when(
      str_detect(publish_date, "^\\d{4}-\\d{2}-\\d{2}") ~ as.Date(str_extract(publish_date, "^\\d{4}-\\d{2}-\\d{2}")),
      TRUE ~ as.Date(NA)
    )
  ) %>%
  filter(!is.na(title), !is.na(content), !is.na(source)) %>%
  distinct(title, source, .keep_all = TRUE)

# Clean and prepare text data
news_clean <- news_data %>%
  mutate(
    text = paste(title, content, sep = " "),
    text = gsub("[^[:alnum:][:space:]]", "", tolower(text))
  ) %>%
  filter(nchar(text) > 50)

# Get sentiment scores
sentiment_scores <- news_clean %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(source, url, title) %>%
  summarise(
    sentiment_score = sum(value),
    word_count = n(),
    avg_sentiment = sentiment_score / word_count,
    .groups = "drop"
  )

# Source sentiment
source_sentiment <- sentiment_scores %>%
  group_by(source) %>%
  summarise(articles = n(), avg_sentiment = mean(avg_sentiment), .groups = "drop")

print(source_sentiment)

# Create source sentiment plot
source_sentiment_plot <- ggplot(source_sentiment, aes(x = reorder(source, avg_sentiment), y = avg_sentiment)) +
  geom_col(aes(fill = avg_sentiment > 0)) +
  coord_flip() +
  scale_fill_manual(values = c("red", "blue"), guide = "none") +
  labs(title = "Average Sentiment by News Source", x = "News Source", y = "Average Sentiment Score") +
  theme_minimal()

print(source_sentiment_plot)
png("source_sentiment_plot.png", width = 1000, height = 600)
print(source_sentiment_plot)
dev.off()

# Define all candidates including 2025 mayoral candidates
candidates <- c("harrell", "wilson", "armstrong", "bliss", "mallahan", "molloy", "whelan", "willoughby", "savage",
                "davison", "juarez", "lewis", "morales", "mosqueda", "nelson", "pedersen", "sawant", "strauss", "herbold", "gonzalez", "oliver", "foster", "chan", "thomas-kennedy")

# Create candidate sentiment by source
candidate_sentiment <- sentiment_scores %>%
  left_join(news_data %>% select(url, matched_keywords), by = "url") %>%
  mutate(keywords_clean = tolower(gsub("[\\[\\]']", "", matched_keywords))) %>%
  filter(str_detect(keywords_clean, paste(candidates, collapse = "|")) & !is.na(keywords_clean)) %>%
  mutate(
    candidate = case_when(
      str_detect(keywords_clean, "harrell") ~ "Harrell",
      str_detect(keywords_clean, "wilson") ~ "Wilson",
      str_detect(keywords_clean, "armstrong") ~ "Armstrong",
      str_detect(keywords_clean, "bliss") ~ "Bliss",
      str_detect(keywords_clean, "mallahan") ~ "Mallahan",
      str_detect(keywords_clean, "molloy") ~ "Molloy",
      str_detect(keywords_clean, "whelan") ~ "Whelan",
      str_detect(keywords_clean, "willoughby") ~ "Willoughby",
      str_detect(keywords_clean, "savage") ~ "Savage",
      str_detect(keywords_clean, "davison") ~ "Davison",
      str_detect(keywords_clean, "nelson") ~ "Nelson",
      str_detect(keywords_clean, "sawant") ~ "Sawant",
      str_detect(keywords_clean, "gonzalez") ~ "Gonzalez",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(candidate)) %>%
  group_by(source, candidate) %>%
  summarise(articles = n(), avg_sentiment = mean(avg_sentiment), .groups = "drop")

# Create candidate heatmap
if (nrow(candidate_sentiment) > 0) {
  candidate_heatmap <- ggplot(candidate_sentiment, aes(x = candidate, y = source, fill = avg_sentiment)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
    labs(title = "Candidate Sentiment by News Source", x = "Candidate", y = "News Source", fill = "Avg Sentiment") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(candidate_heatmap)
  png("candidate_sentiment_heatmap.png", width = 1200, height = 600)
  print(candidate_heatmap)
  dev.off()
}

# Candidate timeline
candidate_timeline_data <- sentiment_scores %>%
  left_join(news_data %>% select(url, matched_keywords), by = "url") %>%
  mutate(
    candidate = case_when(
      str_detect(tolower(matched_keywords), "harrell") ~ "Harrell",
      str_detect(tolower(matched_keywords), "wilson") ~ "Wilson",
      str_detect(tolower(matched_keywords), "armstrong") ~ "Armstrong",
      str_detect(tolower(matched_keywords), "bliss") ~ "Bliss",
      str_detect(tolower(matched_keywords), "mallahan") ~ "Mallahan",
      str_detect(tolower(matched_keywords), "molloy") ~ "Molloy",
      str_detect(tolower(matched_keywords), "whelan") ~ "Whelan",
      str_detect(tolower(matched_keywords), "willoughby") ~ "Willoughby",
      str_detect(tolower(matched_keywords), "savage") ~ "Savage",
      TRUE ~ NA_character_
    ),
    article_order = row_number()
  ) %>%
  filter(!is.na(candidate))

if (nrow(candidate_timeline_data) > 0) {
  candidate_timeline <- ggplot(candidate_timeline_data, aes(x = article_order, y = avg_sentiment, color = candidate)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Candidate Sentiment Over Article Sequence", x = "Article Order", y = "Sentiment") +
    theme_minimal()
  
  print(candidate_timeline)
  
  # Force save using png method
  png("candidate_sentiment_timeline.png", width = 1200, height = 600)
  print(candidate_timeline)
  dev.off()
  cat("Timeline saved to candidate_sentiment_timeline.png\n")
  
  # Also save timeline data
  write.csv(candidate_timeline_data, "candidate_timeline_data.csv", row.names = FALSE)
  cat("Timeline data saved to candidate_timeline_data.csv\n")
}

# Thematic sentiment analysis
keyword_categories <- list(
  "Politics" = c("harrell", "wilson", "armstrong", "bliss", "mallahan", "molloy", "whelan", "willoughby", "savage", "nelson", "davison", "mayor", "election"),
  "Urban Issues" = c("housing", "homeless", "transportation", "budget"),
  "Public Safety" = c("police", "crime", "safety")
)

# Create keyword lookup
keyword_lookup <- tibble(
  category = rep(names(keyword_categories), lengths(keyword_categories)),
  keywords = unlist(keyword_categories)
)

# Thematic analysis
thematic_sentiment <- sentiment_scores %>%
  left_join(news_data %>% select(url, matched_keywords), by = "url") %>%
  mutate(
    keywords_clean = gsub("[\\[\\]']", "", matched_keywords),
    keywords_clean = gsub("\\$title\\$", "", keywords_clean)
  ) %>%
  separate_rows(keywords_clean, sep = ",\\s*") %>%
  filter(!is.na(keywords_clean), keywords_clean != "") %>%
  inner_join(keyword_lookup, by = c("keywords_clean" = "keywords")) %>%
  group_by(source, category) %>%
  summarise(
    articles = n_distinct(url),
    avg_sentiment = mean(avg_sentiment),
    .groups = "drop"
  )

if (nrow(thematic_sentiment) > 0) {
  thematic_plot <- ggplot(thematic_sentiment, aes(x = avg_sentiment, y = reorder(category, avg_sentiment), fill = ifelse(avg_sentiment > 0, "Positive", "Negative"))) +
    geom_col() +
    facet_wrap(~source, scales = "free") +
    scale_fill_manual(values = c("Negative" = "red", "Positive" = "blue"), guide = "none") +
    labs(title = "Sentiment by Theme and Source", x = "Average Sentiment", y = "Category") +
    theme_minimal()
  
  print(thematic_plot)
  ggsave("thematic_sentiment_plot.png", thematic_plot, width = 12, height = 8)
}

# Save all results
write.csv(source_sentiment, "sentiment_by_source.csv", row.names = FALSE)
write.csv(sentiment_scores, "article_sentiments.csv", row.names = FALSE)
if (exists("candidate_sentiment")) {
  write.csv(candidate_sentiment, "candidate_sentiment_by_source.csv", row.names = FALSE)
}
if (exists("thematic_sentiment")) {
  write.csv(thematic_sentiment, "sentiment_by_theme_and_source.csv", row.names = FALSE)
}

cat("Analysis complete - all files saved\n")