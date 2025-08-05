# Seattle News Sentiment Analysis by Source
# Install required packages if not already installed
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(tidytext)) install.packages("tidytext")
if (!require(textdata)) install.packages("textdata")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(scales)) install.packages("scales")

library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(scales)

# Read all CSV files and combine them
csv_files <- list.files(pattern = "seattle_news_.*\\.csv", full.names = TRUE)

# Check if files exist
if (length(csv_files) == 0) {
  stop("No CSV files found matching pattern 'seattle_news_*.csv'")
}

cat("Reading", length(csv_files), "CSV files\n")

# Load and combine all data with robust date cleaning
news_data <- map_dfr(csv_files, ~ read.csv(.x, stringsAsFactors = FALSE)) %>%
  mutate(
    # Clean and standardize publish_date
    publish_date = case_when(
      is.na(publish_date) | publish_date == "" ~ as.Date(NA),
      str_detect(publish_date, "^\\d{4}-\\d{2}-\\d{2}$") ~ as.Date(publish_date),
      str_detect(publish_date, "^\\d{4}-\\d{2}-\\d{2}T") ~ as.Date(str_extract(publish_date, "^\\d{4}-\\d{2}-\\d{2}")),
      str_detect(publish_date, "^\\w{3}, \\d{2} \\w{3} 20\\d{2}") ~ as.Date(publish_date, format = "%a, %d %b %Y"),
      str_detect(publish_date, "^\\w{6,9} \\d{1,2}, 20\\d{2}") ~ as.Date(publish_date, format = "%B %d, %Y"),
      str_detect(publish_date, "^\\d{1,2}/\\d{1,2}/20\\d{2}") ~ as.Date(publish_date, format = "%m/%d/%Y"),
      TRUE ~ tryCatch(as.Date(publish_date), error = function(e) as.Date(NA))
    )
  ) %>%
  # Filter out rows with invalid dates that are clearly wrong
  filter(is.na(publish_date) | (publish_date >= as.Date("2020-01-01") & publish_date <= Sys.Date() + 30))

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
  mutate(keywords_clean = tolower(gsub("\
$$
|\
$$|'|\\s*\$title\$", "", matched_keywords))) %>%
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

# Create dot plot for better comparison
candidate_dotplot <- ggplot(candidate_sentiment, aes(x = avg_sentiment, y = candidate)) +
  geom_point(aes(color = source, size = articles), alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Candidate Sentiment by News Source",
    x = "Average Sentiment Score",
    y = "Candidate",
    color = "News Source",
    size = "Articles"
  ) +
  theme_minimal()

print(candidate_dotplot)
tryCatch({
  ggsave("candidate_sentiment_dotplot.png", candidate_dotplot, width = 12, height = 8)
  cat("Saved candidate_sentiment_dotplot.png\n")
}, error = function(e) {
  cat("Error saving dot plot:", e$message, "\n")
})

# Create candidate sentiment timeline by article sequence
candidate_timeline_data <- sentiment_scores %>%
  left_join(news_data %>% select(url, matched_keywords), by = "url") %>%
  mutate(
    candidate = case_when(
      str_detect(tolower(matched_keywords), "harrell") ~ "Harrell",
      str_detect(tolower(matched_keywords), "wilson") ~ "Wilson", 
      str_detect(tolower(matched_keywords), "nelson") ~ "Nelson",
      str_detect(tolower(matched_keywords), "davison") ~ "Davison",
      str_detect(tolower(matched_keywords), "sawant") ~ "Sawant",
      str_detect(tolower(matched_keywords), "gonzalez") ~ "Gonzalez",
      TRUE ~ NA_character_
    ),
    article_order = row_number()
  ) %>%
  filter(!is.na(candidate))

if (nrow(candidate_timeline_data) > 0) {
  # Plot sentiment per article over sequence
  candidate_timeline <- ggplot(candidate_timeline_data, aes(x = article_order, y = avg_sentiment, color = candidate)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Candidate Sentiment Over Article Sequence", 
      x = "Article Order", 
      y = "Sentiment Score", 
      color = "Candidate"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(candidate_timeline)
  
  tryCatch({
    ggsave("candidate_sentiment_timeline.png", candidate_timeline, width = 12, height = 6)
    cat("Saved candidate_sentiment_timeline.png\n")
  }, error = function(e) {
    cat("Error saving timeline:", e$message, "\n")
  })
  
  # Save timeline data
  write.csv(candidate_timeline_data, "candidate_sentiment_timeline.csv", row.names = FALSE)
  cat("Timeline data saved to candidate_sentiment_timeline.csv\n")
} else {
  cat("No candidate timeline data available\n")
}

# Create daily sentiment timeline for all sources
daily_sentiment <- sentiment_with_dates %>%
  filter(!is.na(publish_date)) %>%
  group_by(publish_date, source) %>%
  summarise(
    avg_sentiment = mean(avg_sentiment),
    articles = n(),
    .groups = "drop"
  )

if (nrow(daily_sentiment) > 0) {
  daily_timeline <- ggplot(daily_sentiment, aes(x = publish_date, y = avg_sentiment, color = source)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(aes(size = articles), alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_x_date(date_labels = "%m/%d", date_breaks = "1 day") +
    scale_size_continuous(range = c(2, 5), name = "Articles") +
    labs(
      title = "Daily Sentiment Timeline by Source",
      x = "Date",
      y = "Average Sentiment Score",
      color = "News Source"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  print(daily_timeline)
  ggsave("daily_sentiment_timeline.png", daily_timeline, width = 12, height = 6)
  cat("Saved daily_sentiment_timeline.png\n")
}

# Save results with publish_date included
sentiment_with_dates <- sentiment_scores %>%
  left_join(news_data %>% select(url, publish_date, source), by = c("url", "source"))

write.csv(source_sentiment, "sentiment_by_source.csv", row.names = FALSE)
write.csv(sentiment_with_dates, "article_sentiments.csv", row.names = FALSE)

cat("\nResults saved to sentiment_by_source.csv and article_sentiments.csv\n")
cat("Date range in data:", as.character(range(news_data$publish_date, na.rm = TRUE)), "\n")

# Save candidate sentiment data
write.csv(candidate_sentiment, "candidate_sentiment_by_source.csv", row.names = FALSE)
cat("Candidate sentiment data saved to candidate_sentiment_by_source.csv\n")

cat("\n=== ANALYSIS COMPLETE ===\n")per article over sequence
  candidate_timeline <- ggplot(candidate_timeline_data, aes(x = article_order, y = avg_sentiment, color = candidate)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Candidate Sentiment Over Article Sequence", 
      x = "Article Order", 
      y = "Sentiment Score", 
      color = "Candidate"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(candidate_timeline)
  
  tryCatch({
    ggsave("candidate_sentiment_timeline.png", candidate_timeline, width = 12, height = 6)
    cat("Saved candidate_sentiment_timeline.png\n")
  }, error = function(e) {
    cat("Error saving timeline:", e$message, "\n")
  })
  
  # Save timeline data
  write.csv(candidate_timeline_data, "candidate_sentiment_timeline.csv", row.names = FALSE)
  cat("Timeline data saved to candidate_sentiment_timeline.csv\n")
} else {
  cat("No candidate timeline data available\n")
}te_diff > 365) {
    date_breaks <- "3 months"
    date_labels <- "%b %Y"
  } else if (date_diff > 90) {
    date_breaks <- "1 month"
    date_labels <- "%b %Y"
  } else {
    date_breaks <- "2 weeks"
    date_labels <- "%m/%d"
  }
  
  # Improved timeline chart
  candidate_timeline <- ggplot(candidate_time_sentiment, aes(x = week, y = avg_sentiment, color = candidate)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_point(aes(size = articles), alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_x_date(date_labels = date_labels, date_breaks = date_breaks) +
    scale_size_continuous(range = c(2, 5), name = "Articles") +
    labs(
      title = "Candidate Sentiment Over Time (Weekly Aggregation)",
      x = "Week",
      y = "Average Sentiment Score",
      color = "Candidate",
      caption = "Point size represents number of articles that week"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    guides(color = guide_legend(override.aes = list(size = 3)))
  
  print(candidate_timeline)
  
  # Also create a faceted version for better readability
  candidate_timeline_faceted <- ggplot(candidate_time_sentiment, aes(x = week, y = avg_sentiment)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_point(aes(size = articles), color = "steelblue", alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_x_date(date_labels = date_labels, date_breaks = date_breaks) +
    scale_size_continuous(range = c(2, 4), name = "Articles") +
    facet_wrap(~candidate, scales = "free_y") +
    labs(
      title = "Candidate Sentiment Over Time by Individual (Weekly Aggregation)",
      x = "Week",
      y = "Average Sentiment Score",
      caption = "Point size represents number of articles that week"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  print(candidate_timeline_faceted)
  
  tryCatch({
    ggsave("candidate_sentiment_timeline.png", candidate_timeline, width = 12, height = 6)
    ggsave("candidate_sentiment_timeline_faceted.png", candidate_timeline_faceted, width = 12, height = 8)
    cat("Saved candidate_sentiment_timeline.png and candidate_sentiment_timeline_faceted.png\n")
  }, error = function(e) {
    cat("Error saving timeline:", e$message, "\n")
  })
} else {
  cat("No time-series data available for candidates\n")
}

# Save candidate sentiment data
write.csv(candidate_sentiment, "candidate_sentiment_by_source.csv", row.names = FALSE)
cat("Candidate sentiment data saved to candidate_sentiment_by_source.csv\n")

# Save results with publish_date included
sentiment_with_dates <- sentiment_scores %>%
  left_join(news_data %>% select(url, publish_date, source), by = c("url", "source"))

write.csv(source_sentiment, "sentiment_by_source.csv", row.names = FALSE)
write.csv(sentiment_with_dates, "article_sentiments.csv", row.names = FALSE)

cat("\nResults saved to sentiment_by_source.csv and article_sentiments.csv\n")
cat("Date range in data:", as.character(range(news_data$publish_date, na.rm = TRUE)), "\n")

# Create daily sentiment timeline for all sources
daily_sentiment <- sentiment_with_dates %>%
  filter(!is.na(publish_date)) %>%
  group_by(publish_date, source) %>%
  summarise(
    avg_sentiment = mean(avg_sentiment),
    articles = n(),
    .groups = "drop"
  )

if (nrow(daily_sentiment) > 0) {
  daily_timeline <- ggplot(daily_sentiment, aes(x = publish_date, y = avg_sentiment, color = source)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(aes(size = articles), alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_x_date(date_labels = "%m/%d", date_breaks = "1 day") +
    scale_size_continuous(range = c(2, 5), name = "Articles") +
    labs(
      title = "Daily Sentiment Timeline by Source",
      x = "Date",
      y = "Average Sentiment Score",
      color = "News Source"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  print(daily_timeline)
  ggsave("daily_sentiment_timeline.png", daily_timeline, width = 12, height = 6)
  cat("Saved daily_sentiment_timeline.png\n")
}

# --- NEW SECTION: THEMATIC SENTIMENT ANALYSIS ---

# Define keyword categories based on the Python scraper
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
thematic_sentiment <- sentiment_scores %>%
  left_join(news_data %>% select(url, matched_keywords), by = "url") %>%
  mutate(
    keywords_clean = gsub("\
$$
|\
$$|'|\\s*\$title\$", "", matched_keywords),
    keywords_list = str_split(keywords_clean, ",\\s*")
  ) %>%
  unnest(keywords_list) %>%
  filter(keywords_list != "") %>%
  inner_join(keyword_lookup, by = c("keywords_list" = "keywords")) %>%
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
    facet_wrap(~source, scales = "free") +
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

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Generated files:\n")
cat("- source_sentiment_plot.png\n")
cat("- candidate_sentiment_heatmap.png\n")
cat("- candidate_sentiment_dotplot.png\n")
cat("- candidate_sentiment_timeline.png\n")
cat("- candidate_sentiment_timeline_faceted.png\n")
cat("- thematic_sentiment_plot.png\n")
cat("- sentiment_by_source.csv\n")
cat("- article_sentiments.csv\n")
cat("- candidate_sentiment_by_source.csv\n")
cat("- sentiment_by_theme_and_source.csv\n")