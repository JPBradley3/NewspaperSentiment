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

# Define Seattle political candidates/officials with full names and search terms
# This ensures we group data correctly (e.g., "bruce" and "harrell" map to "Bruce Harrell")
candidates_df <- tibble::tribble(
  ~full_name, ~search_terms,
  "Bruce Harrell",         c("harrell", "bruce harrell"),
  "Ann Davison",           c("davison", "ann davison"),
  "Debora Juarez",         c("juarez", "debora juarez"),
  "Andrew Lewis",          c("lewis", "andrew lewis"),
  "Tammy Morales",         c("morales", "tammy morales"),
  "Teresa Mosqueda",       c("mosqueda", "teresa mosqueda"),
  "Sara Nelson",           c("nelson", "sara nelson"),
  "Alex Pedersen",         c("pedersen", "alex pedersen"),
  "Kshama Sawant",         c("sawant", "kshama sawant"),
  "Dan Strauss",           c("strauss", "dan strauss"),
  "Lisa Herbold",          c("herbold", "lisa herbold"),
  "Lorena Gonzalez",       c("gonzalez", "lorena gonzalez"),
  "Nikkita Oliver",        c("oliver", "ntikkela oliver", "nikkita oliver"),
  "Nicole Thomas-Kennedy", c("thomas-kennedy", "nicole thomas-kennedy"),
  "Katie Wilson",          c("wilson", "katie wilson")
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
candidate_articles <- purrr::pmap_dfr(candidates_df, function(full_name, search_terms) {
  # Create a regex pattern to match any of the search terms as whole words
  search_pattern <- paste0("\\b(", paste(search_terms, collapse = "|"), ")\\b")
  
  news_clean %>%
    # Filter rows where 'text' contains any of the search terms
    filter(grepl(search_pattern, text, ignore.case = TRUE)) %>%
    # Assign the clean, full name to the matching articles
    mutate(candidate = full_name)
})

# Extract sentences mentioning candidates
candidate_sentences <- candidate_articles %>%
  # Re-join with the candidates_df to get the search_terms for each candidate
  left_join(candidates_df, by = c("candidate" = "full_name")) %>%
  mutate(
    sentences = str_split(text, "\\. "),
    candidate_sentences = map2(sentences, search_terms, function(sents, terms) {
      # Create a pattern for the specific terms for this candidate
      search_pattern <- paste0("\\b(", paste(terms, collapse = "|"), ")\\b")
      # Filter sentences that contain any of the search terms
      sents[grepl(search_pattern, sents, ignore.case = TRUE)]
    })
  ) %>%
  unnest(candidate_sentences) %>%
  select(source, candidate, candidate_sentences, title, url) # search_terms is no longer needed

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
  filter(articles >= 1) %>%  # Show candidates with at least 1 mention
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
  filter(total_articles >= 1) %>% # Show candidates with at least 1 mention overall
  arrange(desc(avg_sentiment))

# Print results
cat("\n=== CANDIDATE SENTIMENT BY NEWS SOURCE ===\n")
print(candidate_source_summary)

cat("\n=== OVERALL CANDIDATE SENTIMENT ===\n")
print(candidate_overall)

# --- NEW SECTION: Overall source sentiment on candidates ---
source_candidate_summary <- candidate_sentiment %>%
  group_by(source) %>%
  summarise(
    total_articles = n_distinct(url),
    avg_sentiment = mean(avg_sentiment),
    .groups = "drop"
  ) %>%
  filter(total_articles >= 2) %>% # Filter sources with few articles
  arrange(desc(avg_sentiment))

cat("\n=== OVERALL SOURCE SENTIMENT ON CANDIDATES ===\n")
print(source_candidate_summary)

# Visualization: Overall source sentiment on candidates
if(nrow(source_candidate_summary) > 0) {
  p3 <- ggplot(source_candidate_summary, aes(x = reorder(source, avg_sentiment), y = avg_sentiment, fill = avg_sentiment > 0)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("red", "blue"), guide = "none") +
    labs(
      title = "Overall News Source Sentiment in Candidate Coverage",
      x = "News Source",
      y = "Average Sentiment Score",
      caption = "Based on all articles mentioning any listed candidate."
    ) +
    theme_minimal()

  print(p3)
}

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
write.csv(source_candidate_summary, "source_sentiment_on_candidates.csv", row.names = FALSE)
write.csv(candidate_sentiment, "detailed_candidate_sentiment.csv", row.names = FALSE)

cat("\nResults saved to candidate_sentiment_by_source.csv, candidate_sentiment_overall.csv, source_sentiment_on_candidates.csv, and detailed_candidate_sentiment.csv\n")