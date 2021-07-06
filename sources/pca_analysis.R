install.packages("psych")
install.packages("tidyverse")
install.packages("reshape2")
install.packages("PerformanceAnalytics")

library(psych)
library(tidyverse)
library(reshape2)
library(PerformanceAnalytics)

data <- read.csv("files/Pokemon.csv", sep=",")

head(data)

data <- column_to_rownames(data, "name")

head(data)

# Columns to analyse
head(data[, 5:10])

data <- data[complete.cases(data[, 5:10]), ]

# Standardize the data using Z Score
data_std <- data.frame(scale(data[, 5:10]))

head(data_std)

# View the correlation
chart.Correlation(data[, 5:10])

# Creates a Pearson correlation matrix
pearson_matrix <- cor(data[, 5:10], method="pearson")

pearson_matrix

# Bartlett's statistical test
bartlett_test <- cortest.bartlett(
  R=pearson_matrix,
  n=nrow(data))

# Proving that the correlation matrix is ​​not equal to an identity matrix
bartlett_test$p.value < 0.05

# Psych package
pca <- prcomp(data_std)

summary(pca)

# Eigenvalues
pca$sdev ^ 2

# Weight that each variable has in each component
vectors <- mutate(
  data.frame(pca$rotation),
  var=rownames(pca$rotation))

vectors <- melt(vectors, id.vars="var")

vectors <- mutate(vectors, var=factor(var))

vectors %>%
  ggplot(aes(
    x=var,
    y=value,
    fill=var
  )) + geom_bar(
    stat="identity"
  ) + facet_wrap(~variable) + labs(
    x=NULL,
    y=NULL,
    fill="Legend"
  )

# Kaiser rule drop all components with eigenvalues ​​under 1
k <- sum(pca$sdev ^ 2 > 1)

k

# Factor loading
loads <- matrix(
  nrow=nrow(pca$rotation),
  ncol=k,
  dimnames=list(rownames(pca$rotation), paste0("factor_loading_", 1:k))
)

for(i in 1:k){
  loads[, i] <- pca$rotation[, i] * pca$sdev[i]
}

loads

# Factor loading and commonalities
report <- data.frame(
  mutate(
    data.frame(loads),
    comunalidades=rowSums(loads ^ 2)))

report

# Factor Scoring
n <- sum(pca$sdev ^ 2)

scores <- matrix(
  nrow=n,
  ncol=n,
  dimnames=list(
    paste0("score_factor_", 1:n),
    rownames(pca$rotation))
)

for(i in 1:n){
  scores[, i] <- pca$rotation[i, ] / pca$sdev
}

scores

# Store the scores of each factor to be used
score_factor_1 <- scores[1, ]
score_factor_2 <- scores[2, ]

# Multiply each factor score with its respective standardized observation
factor_1 <- sweep(
  x=data_std,
  MARGIN=2,
  STAT=score_factor_1,
  FUN=`*`
)

factor_2 <- sweep(
  x=data_std,
  MARGIN=2,
  STAT=score_factor_2,
  FUN=`*`
)

# Finally add each multiplication to get the factors
factor_1 <- rowSums(factor_1)
factor_2 <- rowSums(factor_2)

# Add factors to database
data["factor_1"] <- factor_1
data["factor_2"] <- factor_2  * -1

head(data)

# Proportion of variance
proportion_variance <- pca$sdev ^ 2 / sum(pca$sdev ^ 2)
proportion_variance

# Prove that the factors are not correlated
round(cor(
  data[c("factor_1", "factor_2")]),
  digits=10)

# Rating
data <- mutate(
  data,
  scores=factor_1 * proportion_variance[1] + factor_2 * proportion_variance[2])

head(data[order(-data$scores), ], n=10)

head(data[order(data$scores), ], n=10)
