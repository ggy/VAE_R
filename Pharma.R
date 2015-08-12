Pharma <- read.csv("./Data/Pharma.csv", sep=";")
names(Pharma)

Pharma[is.na(Pharma)]<-0

library(reshape)

# 2 CA 
# 3 TVA sur Bien immobilisé
# 4 CA a l'export
# 5 CA Europe



Pharma[,15:18] <- Pharma[,15:18]*12
Pharma[,19:22] <- Pharma[,19:22]*1
Pharma[,23:26]<- Pharma[,23:26]*4

V = data.frame(row.names=c('A', 'B', 'C', 'D'))

for(i in 1:length(Pharma[,1])){
  
  V[i] = c(sum(Pharma[i,c(3,7,11)]), sum(Pharma[i,c(4,8,12)]), sum(Pharma[i,c(5,9,13)]), sum(Pharma[i,c(6,10,14)]))
  
  
}

Z = data.frame(row.names=c('Diff'))

V <- t(V)

for(i in 1:length(Pharma[,1])){
  
  Z[i] = V[i,'A']-V[i,'C']-V[i,'D']
  
  
}

scaled.dat <- scale(V)

boxplot(scaled.dat, outline = FALSE)


d.pharma = dist(scaled.dat)
col  <- kmeans(V, 3)

library(FactoMineR)

P<-PCA(scaled.dat)

P$eig

pca = prcomp(V, scale. = TRUE)

names(pca)
pca$x

PC1 <- pca$x[,'PC1']
PC2 <- pca$x[,'PC2']

scores = as.data.frame(pca$x)

library(ggplot2)

# plot of observations
ggplot(data = V, aes(x = PC1, y = PC2, label = rownames(scores))) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "tomato", alpha = 0.8, size = 4) +
  ggtitle("PCA plot of USA States - Crime Rates")


fit <- hclust(d.pharma, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=3, border="red")

outlier(V, opposite = FALSE, logical = FALSE)

formule = A ~ B * C * D

reg <- lm(formule, data.frame(V))

boxplot(reg$residuals, outline = FALSE )
