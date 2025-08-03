# Seattle News Sentiment Analysis by Source
# Install required packages if not already installed
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(tidytext)) install.packages("tidytext")
if (!require(textdata)) install.packages("textdata")
if (!require(ggplot2)) install.packages("ggplot2")

library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)

# Read the latest CSV file (adjust filename as needed)
csv_files <- list.files(pattern = "seattle_news_.*\\.csv", full.names = TRUE)
latest_file <- csv_files[which.max(file.mtime(csv_files))]
cat("Reading:", latest_file, "\n")

# Load data
news_data <- read.csv(latest_file, stringsAsFactors = FALSE)

# Clean and prepare text data
news_clean <- news_data %>%
  mutate(
    text = paste(title, content, sep = " "),
    text = gsub("[^[:alnum:][:space:]]", "", text),
    text = tolower(text)
  ) %>%
  filter(nchar(text) > 50)

# Tokenize text
news_tokens <- news_clean %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(nchar(word) > 2)

# Get sentiment scores using AFINN lexicon
sentiment_scores <- news_tokens %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(source, url, title) %>%
  summarise(
    sentiment_score = sum(value),
    word_count = n(),
    avg_sentiment = sentiment_score / word_count,
    .groups = "drop"
  )

# Calculate sentiment by source
source_sentiment <- sentiment_scores %>%
  group_by(source) %>%
  summarise(
    articles = n(),
    avg_sentiment = mean(avg_sentiment),
    median_sentiment = median(avg_sentiment),
    total_sentiment = sum(sentiment_score),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_sentiment))

# Print results
cat("\n=== SENTIMENT ANALYSIS BY NEWS SOURCE ===\n")
print(source_sentiment)

# Create visualization
p1 <- ggplot(source_sentiment, aes(x = reorder(source, avg_sentiment), y = avg_sentiment)) +
  geom_col(aes(fill = avg_sentiment > 0)) +
  coord_flip() +
  scale_fill_manual(values = c("red", "blue"), guide = "none") +
  labs(
    title = "Average Sentiment by News Source",
    x = "News Source",
    y = "Average Sentiment Score",
    caption = paste("Based on", sum(source_sentiment$articles), "articles")
  ) +
  theme_minimal()

print(p1)

# Most positive and negative articles by source
extreme_articles <- sentiment_scores %>%
  group_by(source) %>%
  slice_max(avg_sentiment, n = 2) %>%
  bind_rows(
    sentiment_scores %>%
      group_by(source) %>%
      slice_min(avg_sentiment, n = 2)
  ) %>%
  arrange(source, desc(avg_sentiment))

cat("\n=== MOST POSITIVE/NEGATIVE ARTICLES BY SOURCE ===\n")
print(extreme_articles %>% select(source, title, avg_sentiment))

# Save results
write.csv(source_sentiment, "sentiment_by_source.csv", row.names = FALSE)
write.csv(sentiment_scores, "article_sentiments.csv", row.names = FALSE)

cat("\nResults saved to sentiment_by_source.csv and article_sentiments.csv\n")