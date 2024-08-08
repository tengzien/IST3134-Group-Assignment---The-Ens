# Install necessary packages 
install.packages("rstudioapi")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("data.table")
install.packages("syuzhet")
install.packages("ggplot2")

# Load necessary libraries
library(tm)
library(SnowballC)
library(wordcloud)
library(data.table)
library(syuzhet)
library(ggplot2)

# Get current path
current_path <- rstudioapi::getActiveDocumentContext()$path # location of R script
setwd(dirname(current_path))

# Define the file path
file_path <- "amazon_reviews_us_Books_v1_02.tsv"

# Load the data
reviews <- fread(file_path, sep = "\t", stringsAsFactors = FALSE)

# Define the number of observations to include
num_observations <- 10000

# Create a new dataframe with the first 100,000 observations
reviews_subset <- reviews[1:num_observations, ]

# Check the number of rows in the new dataframe
print(paste("Number of rows in the subset:", nrow(reviews_subset)))

# Extract the review text column from the subset
review_text_subset <- reviews_subset$review_body

# Create a text corpus for the subset
corpus_subset <- Corpus(VectorSource(review_text_subset))

custom_stopwords <- c(stopwords("en"), "book", "books", "read", "reading", "author", 
                      "amazon", "kindle", "novel", "story", "character", "characters",
                      "one", "two", "three", "four", "five", "first", "second", "third", 
                      "really", "also", "good", "great", "will", "make", "made", "much", 
                      "many", "can", "get", "got", "well", "like", "just", "even", "plot",
                      "time", "find", "found", "thought", "felt", "way", "lot", "love", 
                      "loved", "would", "could", "see", "say", "said", "go", "going", 
                      "think", "thought", "give", "gave", "review", "reviews", "free", 
                      "copy", "received", "arc", "ebook", "netgalley", "thank", "thanks", 
                      "received", "advanced", "reader", "recommend", "recommended", "was", 
                      "were", "had", "has", "have", "my", "this", "that", "there", "their", 
                      "them", "they", "what", "which", "when", "where", "who", "how", 
                      "from", "about", "been", "more", "other", "some", "any", "than", 
                      "then", "these", "those", "such", "do", "does", "did", "doing", 
                      "having", "out", "up", "down", "in", "on", "off", "over", "under", 
                      "again", "further", "here", "there", "why", "all", "each", "few", 
                      "most", "its", "and", "or", "an", "as", "is", "are", "it", "be", 
                      "with", "his", "her", "by", "for", "at", "but", "not", "he", "she", 
                      "you", "me", "we", "us", "our", "yours", "ours", "theirs", "his", 
                      "hers", "between", "before", "after", "above", "below", "to", "of", 
                      "a", "the", "if", "your", "it's", "I'm", "you're", "I've", "we're", 
                      "they're", "she's", "he's", "that's", "this's", "those's", "it'd", 
                      "we'd", "you'd", "it'll", "you'll", "they'll", "he'll", "she'll", 
                      "I'd", "you'd", "we'd", "they'd", "I", "s", "d", "ll", "t",
                      "and", "with", "can", "really", "just", "read", "one", "would", "didn't", 
                      "liked", "much", "could", "get", "go", "love", "lot", "think", "thought",
                      "more", "found", "good", "great", "will", "make", "made", "lot", "many",
                      "books", "even", "way", "find", "felt", "think", "thought", "said", 
                      "like", "time", "going", "novel", "character", "characters", "story", 
                      "plot", "see", "say", "lot", "did", "good", "well", "take", "people", 
                      "see", "seems", "lot", "want", "bit", "never", "liked", "felt", "still",
                      "makes", "though", "without", "use", "took", "does", "part", "way", 
                      "around", "always", "another", "actually", "different", "life", 
                      "become", "isn't", "sure", "highly", "anything", "became", "seemed", 
                      "became", "seems", "seem", "less", "came", "several", "else", "place", 
                      "come", "small", "things", "everything", "thoughts", "wasn't", "although", 
                      "especially", "anything", "nothing", "someone", "sometimes", "every", 
                      "each", "different", "way", "anything", "sure", "actually", "always",
                      "anyone", "became", "whether", "place", "things", "anyway", "becomes",
                      "especially", "someone", "getting", "towards", "further", "away", "without")

# Text cleaning
corpus_subset <- tm_map(corpus_subset, content_transformer(tolower))
corpus_subset <- tm_map(corpus_subset, removePunctuation)
corpus_subset <- tm_map(corpus_subset, removeNumbers)
corpus_subset <- tm_map(corpus_subset, removeWords, custom_stopwords)
corpus_subset <- tm_map(corpus_subset, stripWhitespace)

# Create a Document-Term Matrix for the subset
dtm_subset <- TermDocumentMatrix(corpus_subset)
m_subset <- as.matrix(dtm_subset)
word_freqs_subset <- sort(rowSums(m_subset), decreasing = TRUE)
df_subset <- data.frame(word = names(word_freqs_subset), freq = word_freqs_subset)

# Display the most frequent words in the subset
print(head(df_subset, 10))

# Generate the word cloud for the subset
set.seed(9999)
wordcloud(words = df_subset$word, freq = df_subset$freq, min.freq = 5,
          max.words = 100, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

# Perform sentiment analysis using the NRC lexicon
sentiment_scores <- get_nrc_sentiment(review_text_subset)

# Calculate the average sentiment scores
average_sentiment <- colMeans(sentiment_scores)

# Display the average sentiment scores
print("Average Sentiment Scores:")
print(average_sentiment)

# Convert average sentiment to a data frame for plotting
sentiment_df <- data.frame(
  sentiment = names(average_sentiment),
  score = average_sentiment
)

# Plot the sentiment scores
ggplot(sentiment_df, aes(x = reorder(sentiment, score), y = score, fill = sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Average Sentiment Scores",
       x = "Sentiment",
       y = "Average Score")


