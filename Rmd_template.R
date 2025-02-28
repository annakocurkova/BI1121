################################################################################
######################## Reproducibility: RMarkdown ############################
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(here)

# Introduction
Introduction

#The Iris flower data set or Fisher’s Iris data set is a multivariate data set used and made famous by the British statistician and biologist Ronald Fisher in his 1936 paper The use of multiple measurements in taxonomic problems as an example of linear discriminant analysis. It is sometimes called Anderson’s Iris data set because Edgar Anderson collected the data to quantify the morphologic variation of Iris flowers of three related species. Two of the three species were collected in the Gaspé Peninsula “all from the same pasture, and picked on the same day and measured at the same time by the same person with the same apparatus”.

#The data set consists of 50 samples from each of three species of Iris (Setosa, Virginica and Versicolor). Four features were measured from each sample: the length and the width of the sepals and petals, in centimeters. Based on the combination of these four features, Fisher developed a linear discriminant model to distinguish the species from each other. Fisher’s paper was published in the Annals of Eugenics (today the Annals of Human Genetics).

# Data loading
iris <- read.csv(here('iris.csv'))
head(iris)

# Categories
iris$variety <- as.factor(iris$variety)
levels(iris$variety)

# Sepal length based on variety
boxplot(iris$sepal.length ~ iris$variety, 
        col = c("#F8746B", "#00BA38", "#619CFF"),  
        ylim = c(0,8),
        xlab = "Variety",
        ylab = "Sepal length (cm)")

# Statistics for sepal length
hist(iris$sepal.length)

shapiro.test(iris$sepal.length)

kruskal.test(sepal.length ~ variety, iris)

pairwise.wilcox.test(
  iris$sepal.length,
  iris$variety,
  p.adjust.method = 'BH')

# Sepal width
boxplot(iris$sepal.width ~ iris$variety, 
        col = c("#F8746B", "#00BA38", "#619CFF"),  
        ylim = c(0,6),
        xlab = "Variety",
        ylab = "Sepal width (cm)")

# Statistics for sepal width
hist(iris$sepal.width)

shapiro.test(iris$sepal.width)

sepal.width.anova <- aov(sepal.width ~ variety, iris)
summary(sepal.width.anova)

TukeyHSD(sepal.width.anova)

# Petal length
boxplot(iris$petal.length ~ iris$variety, 
        col = c("#F8746B", "#00BA38", "#619CFF"),  
        ylim = c(0,8),
        xlab = "Variety",
        ylab = "Petal length (cm)")


# Statistics for petal length
hist(iris$petal.length)

shapiro.test(iris$petal.length)

kruskal.test(petal.length ~ variety, iris)

pairwise.wilcox.test(
  iris$petal.length,
  iris$variety,
  p.adjust.method = 'BH'
)

# Petal width 
boxplot(iris$petal.width ~ iris$variety, 
        col = c("#F8746B", "#00BA38", "#619CFF"),  
        ylim = c(0,8),
        xlab = "Variety",
        ylab = "Petal width (cm)")


# Statistics for petal width
hist(iris$petal.width)

shapiro.test(iris$petal.width)

kruskal.test(petal.width ~ variety, iris)

pairwise.wilcox.test(
  iris$petal.width,
  iris$variety,
  p.adjust.method = 'BH')

# PCA

variety <- iris$variety
iris$variety <- NULL

sample_pca <- prcomp(iris)

pc_scores <- as.data.frame(sample_pca$x)
pc_scores$variety <- variety

plot(pc_scores$PC1 ~ pc_scores$PC2, col = pc_scores$variety)

# Conclusion

# The statistical analysis of the data showed that there are significant differences between the three Iris varieties in all 4 measured parameters. Principal component analysis has shown that the variety Setosa is more distinct from the other two Iris subtypes (Virginica and Versicolor which are clustering close to each other).

# Session info 

sessionInfo()

