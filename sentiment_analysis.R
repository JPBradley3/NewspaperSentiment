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

# Tidy the data
news_data <- news_data %>%
  # Remove rows with missing essential data
  filter(!is.na(title), !is.na(content), !is.na(source)) %>%
  # Clean whitespace and empty strings
  mutate(
    title = str_trim(title),
    content = str_trim(content),
    source = str_trim(source)
  ) %>%
  # Remove rows where title or content are empty after trimming
  filter(title != "", content != "") %>%
  # Remove duplicate articles based on title and source
  distinct(title, source, .keep_all = TRUE) %>%
  # Standardize source names to handle variations
  mutate(source = case_when(
    str_detect(tolower(source), "kuow") ~ "KUOW",
    TRUE ~ source
  )) %>%
  # Remove duplicates again after source standardization
  distinct(title, source, .keep_all = TRUE) %>%
  # Ensure URL column exists and handle missing URLs
  mutate(url = if_else(is.na(url) | url == "", paste0("missing_url_", row_number()), url))

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
source_sentiment_plot <- ggplot(source_sentiment, aes(x = reorder(source, avg_sentiment), y = avg_sentiment)) +
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

print(source_sentiment_plot)
tryCatch({
  ggsave("source_sentiment_plot.png", source_sentiment_plot, width = 10, height = 6)
  cat("Saved source_sentiment_plot.png\n")
}, error = function(e) {
  cat("Error saving plot:", e$message, "\n")
})

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

# --- CANDIDATE SENTIMENT HEATMAP ---

# Define candidate names
candidates <- c("harrell", "davison", "juarez", "lewis", "morales", "mosqueda", 
                "nelson", "pedersen", "sawant", "strauss", "herbold", "gonzalez", 
                "oliver", "foster", "chan", "thomas-kennedy", "wilson")

# Create candidate sentiment by source
candidate_sentiment <- sentiment_scores %>%
  left_join(news_data %>% select(url, matched_keywords), by = "url") %>%
  mutate(keywords_clean = tolower(gsub("\\[|\\]|'|\\s*\\(title\\)", "", matched_keywords))) %>%
  filter(str_detect(keywords_clean, paste(candidates, collapse = "|")) & !is.na(keywords_clean)) %>%
  mutate(
    candidate = case_when(
      str_detect(keywords_clean, "harrell") ~ "Harrell",
      str_detect(keywords_clean, "davison") ~ "Davison",
      str_detect(keywords_clean, "juarez") ~ "Juarez",
      str_detect(keywords_clean, "lewis") ~ "Lewis",
      str_detect(keywords_clean, "morales") ~ "Morales",
      str_detect(keywords_clean, "mosqueda") ~ "Mosqueda",
      str_detect(keywords_clean, "nelson") ~ "Nelson",
      str_detect(keywords_clean, "pedersen") ~ "Pedersen",
      str_detect(keywords_clean, "sawant") ~ "Sawant",
      str_detect(keywords_clean, "strauss") ~ "Strauss",
      str_detect(keywords_clean, "herbold") ~ "Herbold",
      str_detect(keywords_clean, "gonzalez") ~ "Gonzalez",
      str_detect(keywords_clean, "oliver") ~ "Oliver",
      str_detect(keywords_clean, "foster") ~ "Foster",
      str_detect(keywords_clean, "chan") ~ "Chan",
      str_detect(keywords_clean, "thomas-kennedy") ~ "Thomas-Kennedy",
      str_detect(keywords_clean, "wilson") ~ "Wilson"
    )
  ) %>%
  filter(!is.na(candidate)) %>%
  group_by(source, candidate) %>%
  summarise(
    articles = n(),
    avg_sentiment = mean(avg_sentiment),
    .groups = "drop"
  )

# Create heatmap
candidate_heatmap <- ggplot(candidate_sentiment, aes(x = candidate, y = source, fill = avg_sentiment)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(
    title = "Candidate Sentiment by News Source",
    x = "Candidate",
    y = "News Source",
    fill = "Avg Sentiment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(candidate_heatmap)
tryCatch({
  ggsave("candidate_sentiment_heatmap.png", candidate_heatmap, width = 12, height = 6)
  cat("Saved candidate_sentiment_heatmap.png\n")
}, error = function(e) {
  cat("Error saving candidate heatmap:", e$message, "\n")
})

# Save candidate sentiment data
write.csv(candidate_sentiment, "candidate_sentiment_by_source.csv", row.names = FALSE)
cat("Candidate sentiment data saved to candidate_sentiment_by_source.csv\n")

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
  thematic_sentiment_plot <- ggplot(thematic_sentiment, aes(x = avg_sentiment, y = reorder_within(category, avg_sentiment, source), fill = avg_sentiment > 0)) +
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

  print(thematic_sentiment_plot)
  tryCatch({
    ggsave("thematic_sentiment_plot.png", thematic_sentiment_plot, width = 12, height = 8)
    cat("Saved thematic_sentiment_plot.png\n")
  }, error = function(e) {
    cat("Error saving thematic plot:", e$message, "\n")
  })
}

# Save thematic results
write.csv(thematic_sentiment, "sentiment_by_theme_and_source.csv", row.names = FALSE)
cat("\nResults saved to sentiment_by_theme_and_source.csv\n")