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

# --- NEW SECTION: THEMATIC SENTIMENT ANALYSIS ---

# Define keyword categories based on the Python scraper
# Using more descriptive names for categories
keyword_categories <- list(
  "Politics & Governance" = c(
    "mayor", "mayoral", "city council", "councilmember", "councilwoman", "councilman",
    "seattle politics", "election", "candidate", "municipal", "governance",
    "harrell", "bruce", "davison", "juarez", "lewis", "morales", "mosqueda", "nelson",
    "pedersen", "sawant", "strauss", "herbold", "gonzalez", "oliver", "foster", "chan",
    "thomas-kennedy", "wilson",
    "bruce harrell", "ann davison", "debora juarez", "andrew lewis", "tammy morales",
    "teresa mosqueda", "sara nelson", "alex pedersen", "kshama sawant", "dan strauss",
    "lisa herbold", "lorena gonzalez", "ntikkela oliver", "nicole thomas-kennedy",
    "katie wilson"
  ),
  "Core Urban Issues" = c(
    "budget", "housing", "homeless", "homelessness", "transportation", "zoning",
    "public safety", "police", "spd", "infrastructure", "taxes"
  ),
  "Civic Mechanics" = c(
    "policy", "development", "parks", "fire department", "referendum",
    "ordinance", "legislation", "ballot measure"
  )
)

# Create a lookup table for keywords to categories
keyword_lookup <- tibble(category = names(keyword_categories), keywords = keyword_categories) %>%
  unnest(keywords)

# Parse the matched_keywords column and join with sentiment scores
# The matched_keywords column is a string like "['mayor', 'harrell (title)']"
thematic_sentiment <- sentiment_scores %>%
  # Join back to the original data to get the matched_keywords
  left_join(news_data %>% select(url, matched_keywords), by = "url") %>%
  # Clean and parse the keyword string
  mutate(
    # Remove brackets, quotes, and (title) markers
    keywords_clean = gsub("\\[|\\]|'|\\s*\\(title\\)", "", matched_keywords),
    # Split into a list of keywords, splitting by comma and optional space
    keywords_list = str_split(keywords_clean, ",\\s*")
  ) %>%
  # Create one row per keyword per article
  unnest(keywords_list) %>%
  filter(keywords_list != "") %>% # Remove empty strings that can result from parsing
  # Join with our category lookup table
  inner_join(keyword_lookup, by = c("keywords_list" = "keywords")) %>%
  # We can now calculate sentiment by theme
  # STRATIFY BY SOURCE: group by both source and category
  group_by(source, category) %>%
  summarise(
    articles = n_distinct(url),
    avg_sentiment = mean(avg_sentiment),
    .groups = "drop"
  ) %>%
  arrange(source, desc(avg_sentiment))

# Print and visualize thematic results
cat("\n=== SENTIMENT ANALYSIS BY THEME AND SOURCE ===\n")
print(thematic_sentiment)

if (nrow(thematic_sentiment) > 0) {
  # Create a faceted plot to show thematic sentiment stratified by newspaper
  p2 <- ggplot(thematic_sentiment, aes(x = avg_sentiment, y = reorder_within(category, avg_sentiment, source), fill = avg_sentiment > 0)) +
    geom_col() +
    facet_wrap(~source, scales = "free") + # Create a plot for each newspaper
    scale_y_reordered() +
    scale_fill_manual(values = c("red", "blue"), guide = "none") +
    labs(
      title = "Average Sentiment by Thematic Category, Stratified by News Source",
      y = "Thematic Category",
      x = "Average Sentiment Score",
      caption = "Each panel represents a different news source."
    ) +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold"))

  print(p2)
}

# Save thematic results
write.csv(thematic_sentiment, "sentiment_by_theme_and_source.csv", row.names = FALSE)
cat("\nResults saved to sentiment_by_theme_and_source.csv\n")