# Seattle Candidate Sentiment Analysis by News Source
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(tidytext)) install.packages("tidytext")
if (!require(textdata)) install.packages("textdata")
if (!require(ggplot2)) install.packages("ggplot2")

library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)

# Read latest CSV file
csv_files <- list.files(pattern = "seattle_news_.*\\.csv", full.names = TRUE)
latest_file <- csv_files[which.max(file.mtime(csv_files))]
cat("Reading:", latest_file, "\n")

news_data <- read.csv(latest_file, stringsAsFactors = FALSE)

# Define Seattle political candidates/officials
candidates <- c(
  "harrell", "bruce", "davison", "juarez", "lewis", "morales", 
  "mosqueda", "nelson", "pedersen", "sawant", "strauss", "herbold",
  "gonzalez", "oliver", "foster", "chan", "thomas-kennedy"
)

# Clean text data
news_clean <- news_data %>%
  mutate(
    text = paste(title, content, sep = " "),
    text = gsub("[^[:alnum:][:space:]]", "", text),
    text = tolower(text)
  ) %>%
  filter(nchar(text) > 50)

# Find articles mentioning each candidate
candidate_articles <- map_dfr(candidates, function(candidate) {
  news_clean %>%
    filter(grepl(candidate, text, ignore.case = TRUE)) %>%
    mutate(candidate = candidate)
})

# Extract sentences mentioning candidates
candidate_sentences <- candidate_articles %>%
  mutate(
    sentences = str_split(text, "\\. "),
    candidate_sentences = map2(sentences, candidate, function(sents, cand) {
      sents[grepl(cand, sents, ignore.case = TRUE)]
    })
  ) %>%
  unnest(candidate_sentences) %>%
  select(source, candidate, candidate_sentences, title, url)

# Tokenize and get sentiment for candidate mentions
candidate_sentiment <- candidate_sentences %>%
  unnest_tokens(word, candidate_sentences) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(source, candidate, title, url) %>%
  summarise(
    sentiment_score = sum(value),
    word_count = n(),
    avg_sentiment = sentiment_score / word_count,
    .groups = "drop"
  )

# Calculate average sentiment by candidate and source
candidate_source_summary <- candidate_sentiment %>%
  group_by(candidate, source) %>%
  summarise(
    articles = n(),
    avg_sentiment = mean(avg_sentiment),
    total_sentiment = sum(sentiment_score),
    .groups = "drop"
  ) %>%
  filter(articles >= 2) %>%  # Only candidates with 2+ mentions
  arrange(candidate, desc(avg_sentiment))

# Overall candidate sentiment across all sources
candidate_overall <- candidate_sentiment %>%
  group_by(candidate) %>%
  summarise(
    total_articles = n(),
    avg_sentiment = mean(avg_sentiment),
    sources = n_distinct(source),
    .groups = "drop"
  ) %>%
  filter(total_articles >= 3) %>%
  arrange(desc(avg_sentiment))

# Print results
cat("\n=== CANDIDATE SENTIMENT BY NEWS SOURCE ===\n")
print(candidate_source_summary)

cat("\n=== OVERALL CANDIDATE SENTIMENT ===\n")
print(candidate_overall)

# Visualization: Candidate sentiment by source
if(nrow(candidate_source_summary) > 0) {
  p1 <- ggplot(candidate_source_summary, aes(x = candidate, y = avg_sentiment, fill = source)) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(
      title = "Candidate Sentiment by News Source",
      x = "Candidate",
      y = "Average Sentiment Score",
      fill = "News Source"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p1)
}

# Heatmap of candidate-source sentiment
if(nrow(candidate_source_summary) > 0) {
  p2 <- ggplot(candidate_source_summary, aes(x = source, y = candidate, fill = avg_sentiment)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
    labs(
      title = "Candidate Sentiment Heatmap by Source",
      x = "News Source",
      y = "Candidate",
      fill = "Avg Sentiment"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p2)
}

# Save results
write.csv(candidate_source_summary, "candidate_sentiment_by_source.csv", row.names = FALSE)
write.csv(candidate_overall, "candidate_sentiment_overall.csv", row.names = FALSE)
write.csv(candidate_sentiment, "detailed_candidate_sentiment.csv", row.names = FALSE)

cat("\nResults saved to candidate_sentiment_by_source.csv, candidate_sentiment_overall.csv, and detailed_candidate_sentiment.csv\n")