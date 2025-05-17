data2 <- read.csv("task-2.csv", header = TRUE)

names(data2)
head(data2)
str(data2)

X1 <- data2$X1
X2 <- data2$X2
X3 <- data2$X3
X4 <- data2$RESPONSE


head(X1)
head(X2)
head(X3)
head(X4)

library(moments)
summary_stats <- function(x) {
  c(
    mean = mean(x),
    median = median(x),
    sd = sd(x),
    skewness = skewness(x),
    kurtosis = kurtosis(x)
  )
}

stats <- data.frame(
  X2 = summary_stats(X2),
  X3 = summary_stats(X3),
  X4 = summary_stats(X4)
)
print(stats)

# Корелационни матрици (Pearson и Spearman)
cor_pearson <- cor(stats, method = "pearson")
cor_spearman <- cor(stats, method = "spearman")
cor_pearson
cor_spearman


# 3a. Scatterplots с плътност на точките (ggplot2)
library(ggplot2)
pairs_plot <- function(a, b) {
  ggplot(data2, aes_string(x = a, y = b)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal()
}

# X1 vs X4
pairs_plot("X1", "X4")
# X2 vs X3
pairs_plot("X2", "X3")
# X2 vs X4
pairs_plot("X2", "X4")
# X3 vs X4
pairs_plot("X3", "X4")


# Проверка за нужда от трансформации
library(MASS)

# Например за X2 vs X4:
bc <- boxcox(lm(X4 ~ X2, data = data2), plotit = FALSE)
lambda_opt <- bc$x[which.max(bc$y)]
lambda_opt

X4_bc <- (X4^lambda_opt - 1) / lambda_opt
head(X4_bc)

cor(data2$X2, data2$RESPONSE)
cor(data2$X2, X4_bc)


# За X2 и X3
cor(data2$X2, data2$X3) # най-силна корелация
cor(log(X2), X3)
cor(X2, sqrt(X3))
# корелацията за двете трансформации намаля
