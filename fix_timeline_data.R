# Fix Timeline Data Issues
library(tidyverse)
library(lubridate)

# Read all CSV files to check actual date distribution
csv_files <- list.files(pattern = "seattle_news_.*\\.csv", full.names = TRUE)
cat("Found", length(csv_files), "CSV files\n")

# Examine file dates from filenames
file_dates <- str_extract(csv_files, "\\d{8}_\\d{6}") %>%
  ymd_hms() %>%
  as.Date()

cat("File creation dates range from", as.character(min(file_dates, na.rm = TRUE)), 
    "to", as.character(max(file_dates, na.rm = TRUE)), "\n")

# Load all data and examine publish_date distribution
all_data <- map_dfr(csv_files, function(file) {
  data <- read.csv(file, stringsAsFactors = FALSE)
  data$source_file <- basename(file)
  return(data)
}) %>%
  mutate(publish_date = as.Date(publish_date))

# Check publish_date distribution
date_summary <- all_data %>%
  filter(!is.na(publish_date)) %>%
  group_by(publish_date) %>%
  summarise(articles = n(), .groups = "drop") %>%
  arrange(publish_date)

cat("\nPublish date distribution:\n")
print(date_summary)

# Check if publish_date is being parsed correctly
cat("\nSample of raw publish_date values:\n")
print(head(unique(all_data$publish_date), 10))

# Create hourly timeline if we only have one day
if (nrow(date_summary) <= 2) {
  cat("\nLimited date range detected. Creating hourly analysis...\n")
  
  # Try to extract time information if available
  hourly_data <- all_data %>%
    mutate(
      # Try different datetime parsing approaches
      datetime = case_when(
        !is.na(as.POSIXct(publish_date, format = "%Y-%m-%d %H:%M:%S")) ~ 
          as.POSIXct(publish_date, format = "%Y-%m-%d %H:%M:%S"),
        !is.na(as.POSIXct(publish_date, format = "%Y-%m-%d")) ~ 
          as.POSIXct(paste(publish_date, "12:00:00"), format = "%Y-%m-%d %H:%M:%S"),
        TRUE ~ as.POSIXct(NA)
      ),
      hour = hour(datetime),
      date_hour = floor_date(datetime, "hour")
    ) %>%
    filter(!is.na(datetime))
  
  if (nrow(hourly_data) > 0) {
    # Read sentiment scores
    if (file.exists("article_sentiments.csv")) {
      sentiment_scores <- read.csv("article_sentiments.csv", stringsAsFactors = FALSE)
      
      # Create hourly sentiment timeline
      hourly_sentiment <- sentiment_scores %>%
        left_join(hourly_data %>% select(url, date_hour, source), by = "url") %>%
        filter(!is.na(date_hour)) %>%
        group_by(date_hour, source) %>%
        summarise(
          avg_sentiment = mean(avg_sentiment),
          articles = n(),
          .groups = "drop"
        )
      
      # Save hourly timeline data
      write.csv(hourly_sentiment, "hourly_sentiment_timeline.csv", row.names = FALSE)
      cat("Saved hourly_sentiment_timeline.csv\n")
      
      # Create hourly visualization
      library(ggplot2)
      
      hourly_plot <- ggplot(hourly_sentiment, aes(x = date_hour, y = avg_sentiment, color = source)) +
        geom_line(size = 1.2) +
        geom_point(aes(size = articles), alpha = 0.7) +
        geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
        scale_x_datetime(
          date_labels = "%H:%M",
          date_breaks = "2 hours"
        ) +
        scale_size_continuous(range = c(2, 5), name = "Articles") +
        labs(
          title = "Sentiment Timeline by Hour",
          subtitle = "Intraday sentiment patterns by news source",
          x = "Time of Day",
          y = "Average Sentiment Score",
          color = "News Source"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom"
        )
      
      print(hourly_plot)
      ggsave("hourly_sentiment_timeline.png", hourly_plot, width = 12, height = 6)
      cat("Saved hourly_sentiment_timeline.png\n")
    }
  }
}

# Alternative: Create timeline based on article order if dates are limited
if (nrow(date_summary) <= 2) {
  cat("\nCreating sequential article timeline...\n")
  
  if (file.exists("article_sentiments.csv")) {
    sentiment_scores <- read.csv("article_sentiments.csv", stringsAsFactors = FALSE)
    
    # Create sequential timeline
    sequential_timeline <- sentiment_scores %>%
      left_join(all_data %>% select(url, source, title), by = "url") %>%
      arrange(source, title) %>%
      group_by(source) %>%
      mutate(
        article_sequence = row_number(),
        # Create rolling average over 5 articles
        sentiment_smooth = zoo::rollmean(avg_sentiment, k = 5, fill = NA, align = "center")
      ) %>%
      ungroup()
    
    # Plot sequential sentiment
    sequential_plot <- ggplot(sequential_timeline, aes(x = article_sequence, y = avg_sentiment, color = source)) +
      geom_point(alpha = 0.4, size = 1) +
      geom_line(aes(y = sentiment_smooth), size = 1.2, na.rm = TRUE) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      facet_wrap(~source, scales = "free_x") +
      labs(
        title = "Sequential Article Sentiment Analysis",
        subtitle = "Thick line shows 5-article rolling average",
        x = "Article Sequence",
        y = "Sentiment Score",
        color = "News Source"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        strip.text = element_text(face = "bold")
      )
    
    print(sequential_plot)
    ggsave("sequential_sentiment.png", sequential_plot, width = 12, height = 8)
    cat("Saved sequential_sentiment.png\n")
  }
}

cat("\nTimeline data analysis complete!\n")