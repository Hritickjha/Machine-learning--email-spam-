df <- read.csv("email_spam_test.csv")
sample_df <- df[sample(nrow(df), 5), ]
print(sample_df)
df_dim <- dim(df)
num_rows <- df_dim[1]
num_cols <- df_dim[2]
print(paste("Number of rows:", num_rows))
print(paste("Number of columns:", num_cols))
str(df)
df <- df[, !(names(df) %in% c("Email 24", "Email 18", "Email 15"))]
sample_df <- df[sample(nrow(df), 5), ]
print(sample_df)
names(df)[names(df) == "the"] <- "target"
names(df)[names(df) == "to"] <- "text"
sample_df <- df[sample(nrow(df), 5), ]
print(sample_df)
df$target <- factor(df$target)
print(levels(df$target))
df$target <- as.numeric(df$target)
print(unique(df$target))
df$target <- as.factor(df$target)
print(levels(df$target))
df$target <- as.numeric(df$target) - 1 
print(head(df))
missing_values <- colSums(is.na(df))
print(missing_values)
num_duplicates <- sum(duplicated(df))
print(num_duplicates)
df <- df[!duplicated(df), ]
print(df)
df <- df[!duplicated(df), ]
num_duplicates <- sum(duplicated(df))
df_dim <- dim(df)
num_rows <- df_dim[1]
num_cols <- df_dim[2]
print(num_duplicates)
print(paste("Number of rows:", num_rows))
print(paste("Number of columns:", num_cols))
print(head(df))
target_counts <- table(df$target)
pie(target_counts, labels = c('ham', 'spam'), 
    main = "Target Distribution", 
    col = c("skyblue", "salmon"),
    cex = 0.8,
    clockwise = TRUE,
    border = NA,
    init.angle = 90)
library(tokenizers)
df$num_characters <- nchar(df$text)
print(head(df))
library(tokenizers)
df$num_words <- sapply(df$text, function(x) length(tokenize_words(as.character(x))))
print(head(df))
df$num_sentences <- sapply(df$text, function(x) length(tokenize_sentences(as.character(x))))
print(head(df))
selected_columns <- c('num_characters', 'num_words', 'num_sentences')
summary_df <- summary(df[selected_columns])
print(summary_df)
filtered_df <- df[df$target == 0, c('num_characters', 'num_words', 'num_sentences')]
summary_df <- summary(filtered_df)
print(summary_df)
filtered_df <- df[df$target == 1, c('num_characters', 'num_words', 'num_sentences')]
summary_df <- summary(filtered_df)
print(summary_df)
filtered_df <- df[df$target == 0, 'num_characters']
hist(filtered_df, 
     main = "Histogram of num_characters (target = 0)", 
     xlab = "Number of Characters", 
     ylab = "Frequency",
     col = "blue")
filtered_df <- df[df$target == 1, 'num_characters']
hist(filtered_df, 
     main = "Histogram of num_characters (target = 0)", 
     xlab = "Number of Characters", 
     ylab = "Frequency",
     col = "green")
install.packages("car")
library(car)
df$target <- as.factor(df$target)
scatterplotMatrix(~ num_characters + num_words + num_sentences | target, data = df, 
                  main = "Pairplot of num_characters, num_words, and num_sentences",
                  diagonal = "histogram",
                  col = c("red", "green")) 
library(gplots)
correlation_matrix <- cor(df)
heatmap.2(correlation_matrix, 
          col = colorRampPalette(c("blue", "white", "red"))(100),
          symm = TRUE)
library(gplots)
correlation_matrix <- cor(df)
correlation_matrix[is.na(correlation_matrix) | !is.finite(correlation_matrix)] <- 0
heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        symm = TRUE)
numeric_df <- df[sapply(df, is.numeric)]
correlation_matrix <- cor(numeric_df)

transform_text <- function(text) {
  text <- tolower(text)
  return(text)
}
transform_text('Hi how Are you')
install.packages("tokenizers")
library(tokenizers)
transform_text <- function(text) {
  text <- tolower(text)
  text <- tokenize_words(text)
  return(text)
}
transform_text('Hi how Are you')
transform_text <- function(text) {
  text <- tolower(text)  
  text <- tokenize_words(text) 
  y <- c()
  for (i in text) {
    if (grepl("^[A-Za-z0-9]+$", i)) {
      y <- c(y, i)  
    }
  }
  return(y)  
}
transform_text("hi how are you  %% eg")

transform_text <- function(text) {
  text <- tolower(text)
  text <- tokenize_words(text)
  y <- c()
  for (i in text) {
    if (grepl("^[A-Za-z0-9]+$", i)) {
      y <- c(y, i) 
    }
  }
  return(y) 
}
transformed_text <- transform_text("hi how are you  %% eg")
text_2000 <- df$text[2000]
transformed_text_2000 <- transform_text(text_2000)
print(transformed_text_2000)
library(tokenizers)
library(stringr)

transform_text <- function(text) {
  text <- tolower(text) 
  text <- tokenize_words(text) 
  return(alphanumeric_tokens) 
}
transformed_text <- transform_text("hi how are you  %% eg")
print(transformed_text)

transform_text <- function(text) {
  text <- tolower(text) 
  text <- unlist(tokenize_words(text)) 
  y <- character(0)
  for (i in text) {
    if (grepl("^[A-Za-z0-9]+$", i)) {
      y <- c(y, i)  
    }
  }
  
  return(y)
}
transformed_text <- transform_text("hi how are you %% eg")
print(transformed_text)

library(tm)
install.packages("tm")
library(tm)
punctuation_marks <- c("!", "\"", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", ":", ";", "<", "=", ">", "?", "@", "[", "\\", "]", "^", "_", "`", "{", "|", "}", "~")
print(punctuation_marks)
transform_text <- function(text) {
  text <- tolower(text)
  text <- tokenize_words(text)
  text <- text[grep("^[A-Za-z0-9]+$", text, value = TRUE)]
  text <- text[!text %in% stopwords("en")]
  text <- text[!text %in% c(".", ",", ":", ";", "?", "!", "-", "'", "\"", "(", ")", "[", "]", "{", "}", "/", "*", "&", "%", "$", "@", "#", "^", "=", "+", "|", "<", ">")]
  
  return(text)
}
transformed_text <- transform_text('hi how are you Hritick?')
print(transformed_text)
# Install and load the SnowballC package
install.packages("SnowballC")
library(SnowballC)
stem_text <- function(text) {
  stemmed_text <- wordStem(text, language = "en")
  return(stemmed_text)
}

stemmed_word <- stem_text("dancing")
print(stemmed_word)
