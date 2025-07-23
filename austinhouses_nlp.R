# Zan Merrill - ahm2452.  I "vibe-coded" this section w/ chatGPT
# the idea our group had was to use one-hot-encoding for keywords within the description
# various articles have been published about how descriptions can add or detract value based on keywords
# this is a way to empirically test that

# Load libraries
library(dplyr)
library(stringr)
library(tidytext)
library(tibble)

getwd()

# setwd("/Users/zan/Desktop/MSBA/summer/machine learning/class 6")

data <- read.csv('austinhouses.csv')

# This code snippet counts the top words in the description (not including stop words)

# Load built-in stop words from tidytext
data("stop_words")

# Define your custom ignore list
ignore_words <- c("austin", "home", "house", "texas","kitchen","bedrooms","2","3","master","living","family",
                  "house","built","bath","bathrooms","ft","floor","bedroom","sq","4","tx","mo"
                  )

# Combine built-in and custom stop words
all_stop_words <- bind_rows(
  stop_words,
  tibble(word = ignore_words)
)

# Tokenize, remove all stop words, count, and get top 25
top_words <- housing_data %>%
  mutate(description = str_to_lower(description)) %>%
  unnest_tokens(word, description) %>%
  anti_join(all_stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 25)

print(top_words)

  
word_lists <- list(
# https://www.zillow.com/learn/15-words-that-add-value/
  good = c(
    'luxurious', 'captivating', 'impeccable', 'stainless', 'basketball',
    'landscaped', 'granite', 'pergola', 'remodel', 'beautiful',
    'gentle', 'spotless', 'tile', 'upgraded', 'updated','greenbelt'
  ),
  
# https://www.zillow.com/learn/9-listing-words-that-could-cost-you/
# https://www.housebeautiful.com/design-inspiration/real-estate/a64957249/real-estate-listing-warning-signs/
  bad = c(
    'fixer', 'charming', 'motivated seller', 'cozy', 'tlc',
    'cosmetic', 'investment', 'investor', 'potential', 'bargain',
    'opportunity', 'nice', 'bones', 'sold as is'
  )
)

# This code snipped will count the words in the good and bad lists

# Function to count total matches for a list of keywords
count_keywords <- function(df, keyword_list, column_name = "description") {
  sapply(keyword_list, function(word) {
    pattern <- paste0("\\b", word)  # Match word at beginning of word boundary
    sum(str_count(df[[column_name]], regex(pattern, ignore_case = TRUE)))
  })
}

# Count good and bad word appearances (as substrings)
good_counts <- count_keywords(housing_data, word_lists$good)
bad_counts <- count_keywords(housing_data, word_lists$bad)

# Convert to data frames
good_df <- tibble(word = names(good_counts), count = good_counts)
bad_df  <- tibble(word = names(bad_counts), count = bad_counts)

print(good_df)
print(bad_df)

# This adds and deletes columns for one hot encoding for words that we're searching for.  
# only the words that we're currently looking for will have a column.  
# old data frames are overwritten when the code is re-run

# Step 1: Combine good/bad word lists into a flat vector
search_words <- c(good = word_lists$good, bad = word_lists$bad)
categories <- rep(c("good", "bad"), times = c(length(word_lists$good), length(word_lists$bad)))

# Step 2: Create clean, underscore-safe column names
colnames_safe <- paste0(categories, "_", gsub(" ", "_", search_words))
names(search_words) <- colnames_safe  # Assign safe names to the actual search words

# Step 3: Remove old one-hot encoded columns (those starting with good_ or bad_)
data_nlp <- housing_data %>%
  select(-matches("^(good|bad)_"))  # Remove previous one-hot columns

# Step 4: Add new one-hot encoded columns
for (colname in names(search_words)) {
  word <- search_words[[colname]]
  pattern <- regex(word, ignore_case = TRUE)
  data_nlp[[colname]] <- ifelse(str_detect(data_nlp$description, pattern), 1, 0)
}

# Step 5: View or work with your result
glimpse(data_nlp)

# option to save data_nlp as a separate csv
# write.csv(data_nlp, file = "data_nlp.csv", row.names = FALSE)
