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

############## part 2 #############
View(email_spam_test)

#### scaling #####
df <- read.csv("email_spam_test.csv")
df <- df[, !(names(df) %in% c("Email 24", "Email 18", "Email 15"))]
names(df)[names(df) == "the"] <- "target"
names(df)[names(df) == "to"] <- "text"
df$target <- as.factor(df$target)
df <- df[!duplicated(df), ]
numeric_cols <- sapply(df, is.numeric)
df[numeric_cols] <- scale(df[numeric_cols])
print(head(df))

####k-Mean ####
email_spam_test <- read.csv("email_spam_test.csv")
email_spam_test.s <- scale(email_spam_test[, -c(1, 2)]) 
if(any(is.na(email_spam_test.s)) | any(!is.finite(email_spam_test.s))) {
  email_spam_test.s[is.na(email_spam_test.s)] <- 0 
  email_spam_test.s[!is.finite(email_spam_test.s)] <- 0  
}
set.seed(123)
wss <- sapply(2:10, function(k) {
  kmeans(email_spam_test.s, centers=k, nstart=25)$tot.withinss
})
plot(2:10, wss, type="b", main="Elbow Method for Choosing k email_spam_test", xlab="Number of Clusters of email_spam_test", ylab="Total Within-Cluster Sum of Squares email_spam_test")
k <- 3
set.seed(123)
kmeans_result <- kmeans(email_spam_test.s, centers=k, nstart=25)
email_spam_test$cluster <- as.factor(kmeans_result$cluster)
table(email_spam_test$cluster)
if(ncol(email_spam_test.s) <= 3) {
  library(scatterplot3d)
  scatterplot3d(email_spam_test.s[,1:3], color = kmeans_result$cluster, pch = 19)
} else {
  print("Too many dimensions to plot.")
}
print(kmeans_result$centers)
print(table(email_spam_test$cluster))

##### factoextra #####
install.packages("factoextra")
library(ggplot2)
library(factoextra)
fviz_nbclust(email_spam_test.s,kmeans,method = "wss")
fviz_nbclust(email_spam_test.s,kmeans,method = "silhouette")

####Nb Clust ####
install.packages("NbClust")
library(NbClust)
email_spam_test <- read.csv("email_spam_test.csv")

email_spam_test <- email_spam_test[, !(names(email_spam_test) %in% c("Email 24", "Email 18", "Email 15"))]
names(email_spam_test)[names(email_spam_test) == "the"] <- "target"
names(email_spam_test)[names(email_spam_test) == "to"] <- "text"
email_spam_test$target <- as.factor(email_spam_test$target)
numeric_cols <- sapply(email_spam_test, is.numeric)


nb_results <- NbClust(data = email_spam_test[, -c(1, 2)],  
                      distance = "euclidean", 
                      min.nc = 2, 
                      max.nc = 15, 
                      method = "kmeans", 
                      index = "all", 
                      alphaBeale = 0.1)
fviz_nbclust(nb_results)
####set k=5 ####
k <- 5
set.seed(123)
kmeans_result <- kmeans(email_spam_test.s, centers = k, nstart = 25)
email_spam_test$cluster <- as.factor(kmeans_result$cluster)
table(email_spam_test$cluster)
if(ncol(email_spam_test.s) <= 3) {
  library(scatterplot3d)
  scatterplot3d(email_spam_test.s[,1:3], color = kmeans_result$cluster, pch = 19)
} else {
  print("Too many dimensions to plot.")
}
print(kmeans_result$centers)
print(table(email_spam_test$cluster))
par(mar = c(5, 4, 4, 2) + 0.1)
plot(email_spam_test, col = email_spam_test$cluster, main = "Clustering Results with k=5")
#### Hirearchical clustering #####
hc <- hclust(dist(email_spam_test.s))
plot(hc, cex = 0.6, main = "Dendrogram of Hierarchical Clustering email_spam_test")
#### Cluster distance ####
cluster_dist <- as.dendrogram(hc)
plot(cluster_dist, main = "Cluster Distance emai_spam_test", cex = 0.6)

####supervised machine learning logistic Regression ####
####Logistic Regression In R ####
library(mlbench)
df <- read.csv("email_spam_test.csv")
diabetes<-email_spam_test
unique(df$target)
df$target <- factor(df$target)
unique(df$target)
df$num_characters <- nchar(df$text)
logit <- glm(target ~ num_characters, family = binomial, data = df)
summary(logit)
library(tidyverse)
duplicate_names <- names(diabetes)[duplicated(names(diabetes))]
print(duplicate_names)
new_column_names <- make.unique(names(diabetes))
names(diabetes) <- new_column_names
train <- mutate(diabetes, prob = ifelse(diabetes == "pos", 1, 0))
library(ggplot2)
ggplot(train2, aes(glucose, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model in R",
    x = "num_characters",
    y = "Probability "
  )

####Multiple Logistic Regression ####
logit <- glm(target ~ num_characters, family = binomial, data = df)
summary(logit2)
#### Logistic regression using Caret package ####
library(caret)
#### Data Split ####
indexes <- sample(1:nrow(email_spam_test), 4/5 * nrow(email_spam_test))
train <- email_spam_test[indexes, ]
test <- email_spam_test[-indexes, ]
prop.table(table(train$target)) * 100
prop.table(table(test$target)) * 100
#### Training Model ####
train_cleaned <- na.omit(train)
caret_glm_mod <- train(
  form = diabetes ~ .,
  data = train_cleaned,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)
install.packages("mice")
library(mice)
imputed_data <- mice(train, method = 'pmm', m = 1, maxit = 5)
train_imputed <- complete(imputed_data, 1)
caret_glm_mod <- train(
  form = diabetes ~ .,
  data = train_imputed,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)
trainControl(method = "cv", number = 5)[1:3]
caret_glm_mod
caret_glm_mod$finalModel
summary(caret_glm_mod)
predicted_test<-predict(caret_glm_mod, newdata = test)
confusionMatrix(as.factor(predicted_test),test$diabetes,positive = "pos")
#### supervised Machine learning K Nearest Neighbour[KNN] ####
#### Transformation - normalizing numeric data #####
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))
wbcd_norm <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_norm$area_mean)
####supervised Machine learning K Nearest Neighbour Regression####
install.packages("MASS")
library(MASS)
boston<-Boston
miss<-apply(boston, 2, function(x) sum(is.na(x)))
miss
summary(boston)
boston_norm <- as.data.frame(lapply(boston[1:13], normalize))
boston_norm$Med_price<-boston$medv
install.packages(caret)
library(caret)
set.seed(123)
y <- boston_norm$Med_price
indxTrain <- createDataPartition(y, p = 0.7, list = FALSE)
boston_train <- boston_norm[indxTrain,]
boston_test <- boston_norm[-indxTrain,]
model <- train(
  Med_price~., data = boston_train, method = "knn",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(model)
#### supervised Machine learning : classification Using Naive Bayes####
#### Data preparation ####
sms_converted<-read.csv("email_spam_test.csv")
sms_converted[1:10, 1:10]
spam<-sms_converted[sms_converted$Type=="spam",-ncol(sms_converted)]
spam<- ifelse(spam >0, 1, 0)
spam_freq<-apply(spam, 2, sum)/nrow(spam)
spam_freq_ordered<- spam_freq[order(spam_freq, decreasing =T)]
spam_top <-data.frame(freq = spam_freq_ordered[1:20])
library(ggplot2)
ggplot(spam_top, aes(x=rownames(spam_top), y=freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90))
####################Decision Trees ########################
#### Data collection ####
head(iris)
str(iris)
library(ggplot2)
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, col=Species))+geom_point()
#### Regression #####
##### Data collection#####
data("Boston", package = "MASS")
head(Boston)
any(is.na(Boston))
ind<-createDataPartition(Boston$medv, p=0.7, list=F)
train<-Boston[ind, ]
test<-Boston[-ind,]
install.packages("randomForest")
library(randomForest)
Boston.rf=randomForest(medv ~ . , data = train)
Boston.rf
plot(Boston.rf)
#### Model Evaluation ####
library(caret)
pred<-predict(Boston.rf, test)
metrics_rmse = RMSE(pred,test$medv)
metrics_r2 = R2(pred, test$medv)
metrics_MEA = MAE(pred, test$medv)
c(metrics_rmse,metrics_r2,metrics_MEA )
df<-data.frame(pred=pred, obs=test$medv)
library(ggplot2)
ggplot(df, aes(x=obs, y=pred))+geom_point()
####Model Optimisation ####
oob.err=double(13)
test.err=double(13)
for(mtry in 1:13) {
  rf <- randomForest(medv ~ ., data = train, mtry = mtry, ntree = 500)
  oob.err[mtry] <- rf$mse[500] 
  pred <- predict(rf, newdata = test)
  test.err[mtry] <- mean((test$medv - pred)^2)
}
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","green"),type="b",
        ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","green"))
