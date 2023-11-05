# Load Libraries 
library(MASS) 
library(ggplot2)
library(factoextra)


####Read in teeth data from csv file.
# make the directory the one with the data 
#setwd("C:/Users/Spenc/Documents/Youtube/Machine Learning/R/Principal Component")
dff <-read.csv('clam.csv', header = TRUE)
# data head
head(dff)   #print a few observations

df <-dff[ , 6:21]   # select just the data to be used      

pca.df <- princomp(df, cor=TRUE)
pca.df
# note one problem with R is that the method works even when the code is
# not exactly correct pc.teeth <- princomp(teethpc, scale=TRUE, center=TRUE)
# produces results but they are wrong

# names() tells you what information is available from the output
names(pca.df)
# "sdev"     "loadings/Eigenvalues" "center"   "scale"    "n.obs"    "scores"   "call" 

# summarize the pca   - note the std devs are divided by n not n-1
# makes them the square root of the eigenvalue
summary(pca.df) # Proportion of variance = sd ^2 / number of components.
# cumultive proportion explains total variance.




# do things a bit better
eigenvectors<-pca.df$loadings
eigenvalues <- pca.df$sdev*pca.df$sdev 

# loading is the eigenvector.
pca.df$loadings    # note that these are scaled so the sum of squares is 1.0

# not loadings but eigenvectors
eigenvectors #These are the principal components that determine the directions of the new feature space
eigenvalues  # Eigenvalues determine the magnitude

# obtain the correlations with the variables
# Based on 2 Principal components (a new coordinate system and orthogonal to each other)
# the observations are then projected onto the coordinate plane.
# For more than 1 Principal Component, the score is
cor(dff[ , 6:21], pca.df$scores)
# make things a bit nicer
round(cor(dff[ , 6:21], pca.df$scores) , 3)   # round the output


# scree plot      - note the default is a barchart
# Scree plot is a line plot of Principal Components vs Variances
# Helps deteremine the number of factors to keep.
# This method is critized for its subjectivity because you are essentially looking
# for an 'elbow' within the graph. The graph can have many 'elbows'

screeplot(pca.df,type='l',main='Screeplot for Morphometric data') #screeplot with dots and lines
abline(1,0,col='red',lty=2) #horizontal line at 1

fviz_eig(pca.df, 
         addlabels = TRUE,
         ylim = c(0, 40))
ggsave("Scree.png",  units="in", width=6.5, height=5.5)

# Biplot with Default Settings
fviz_pca_biplot(pca.df)

# Biplot with Labeled Variables
fviz_pca_biplot(pca.df,
                label="var")

# Biplot with Colored Groups
fviz_pca_biplot(pca.df,
                label="var",
                habillage = dff$Trt)

ggsave("scatter.png",  units="in", width=6.5, height=5.5)

# Biplot with Customized Colored Groups and Variables
fviz_pca_biplot(pca.df,
                label="var",
                habillage = dff$Trt, 
                col.var = "black") +
  scale_color_manual(values=c("orange", "purple", "steelblue", "red", "green", "pink", "yellow"))

ggsave("PCa.png",  units="in", width=6.5, height=5.5)

# scatterplot of scores with labels given by mammal in col 1
#  type=n turns off the default symbol
plot(pca.df$scores[,1:2], type='n',xlab="PC1(38.7%)", ylab="PC2(12.7%)" )
points(pca.df$scores[,1:2],cex=0.5)   # this puts a small point at the center
text(pca.df$scores[,1:2],label= dff[,1], cex=0.5) #add tname to plot	

# create a new plot window
#windows()
# scatterplot of scores with labels given by mammal in col 1
# type = n turns off the default symbol
#plot(pc.teeth$scores[,1:2], type='n',xlab="PC1(59%)", ylab="PC2(18%)" )
#points(pc.teeth$scores[,1:2],cex=0.5)   # this puts a small point at the center
#text(pc.teeth$scores[,1:2],label=teeth[,1], cex=0.5) #add tname to plot


# another way to code the analysis    
pc.fit <- prcomp(~ SL+ SH+ SW + LL + PL + AL + UL + LCT + LPAS + 
                   PW + AW + PVM + PS + CT..RIGHT.TEETH. + Total.wt + muscle.wt,
                 data = df ,scale=TRUE)
eigenvalues <- pc.fit$sdev * pc.fit$sdev
eigenvectors <- pc.fit$rotation
eigenvalues
round(eigenvectors,3)

summary(pc.fit)


# third approach -------------------------------------------- #
# Another approach - fit using factor analysis function
# two factor solution   - useful for printing when p is large


library(psych)

#Factor Analysis - Principal Components Method
# Types of rotation... varimax, quartimax, promax, oblimin, simplimax, cluster
pc2 <- principal(df,nfactors = 6,rotate="none")
pc2
# Prints out the Correlation matrix with the corresponding factors.

