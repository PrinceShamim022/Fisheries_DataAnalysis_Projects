#yt Codanics
#PCA in R | R-programming and statistics for Beginners (in Urdu & Hindi)-14

#read your data file
#data("iris")
df <- read.csv('clam.csv')

#principal component analysis
clam.pca <- prcomp(df[,6:21], center=TRUE, scale.= TRUE)
print(clam.pca)
summary(clam.pca)

#ggplot2 and grouping
clam <- cbind(df, clam.pca$x)
ggplot(clam, aes(PC1, PC2, col = Trt, fill = Trt)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.1) +
  geom_point(shape = 21, col = "black")

#install packages for PCA analysis 
install.packages("factoextra")
install.packages("FactoMineR")
library(factoextra)
library(FactoMineR)

# create a principle component table
clam.pca1 <- PCA(clam[,6:21], graph=TRUE, scale.unit = TRUE)

#to present most of the data screeplot
fviz_eig(clam.pca1, addlabels = TRUE, ylim=c(0,80))

#create a correlation plot
fviz_pca_var(clam.pca1, col.var = "cos2",
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033", "steelblue", "red", "green"),
             repel = TRUE) +
  labs(title = "PCA of parameters", x="PC1 (78.9%)", y="PC2 (6.2%)",
       colour = "Treatments")

ggsave("PCA.png",  units="in", width=6.5, height=5.5)
