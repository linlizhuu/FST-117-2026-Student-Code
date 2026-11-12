library(readxl)
EBar.DA <- read_excel("Week 8 (PCA)/Energy Bar DA.xlsx")


#Turn Factors into Factors
str(EBar.DA)
EBar.DA[,c(1:3)] <- lapply(EBar.DA[,c(1:3)],factor)
str(EBar.DA)


# PCA ------------------------------------------------------------------
library(FactoMineR)
library(ggplot2)
library(factoextra)

#Removing Factors and creating a matrix for our variables
EBar.att <- as.matrix(EBar.DA[,c(4:16)])

#Create a means table (removing within group variance)
EBar.means <- aggregate(EBar.att ~ Product, data=EBar.DA, FUN=mean)

#make the row name, product names
row.names(EBar.means) <- EBar.means$Product 

#get rid of the product column
EBar.means$Product <- NULL #get rid of the product column

#Generate PCA
EBar.pca <- PCA(EBar.means, axes = c(1,2), ncp = 13, graph = TRUE)

#print list of eigen values into console
get_eigenvalue(EBar.pca)

#Generate Scree Plot
fviz_screeplot(EBar.pca, ncp = 19)

#Generate biplots
fviz_pca_biplot(EBar.pca, axes = c(1,2), repel = TRUE) #pc2 vs. pc1
fviz_pca_biplot(EBar.pca, axes = c(1,3), repel = TRUE) #pc3 vs. pc1
