## Self Organizing Map 
# reference : https://woosa7.github.io/R-Clustering-Kmens-SOM/

# Data loading
region <- read.csv("D:/Workplace/Environmental_Statistics_with_R/수질데이터분석(논문)/1번째/전처리 후 데이터/지점별 평균 데이터.csv", 
                   header=T, sep=",")
rownames(region) <- region[,1]
region <- data.frame(region[,-1])

# Install packages
install.packages("SOMbrero")
library(SOMbrero)
install.packages("kohonen")
library(kohonen)

# Normalization of data
region_scale <- data.frame(scale(region))
region_scale_matrix <- as.matrix(region_scale)

# Training the SOM model
som_grid <- somgrid(xdim=3, ydim=1, topo="hexagonal")
som_model1 <- som(region_scale_matrix, grid=som_grid)
str(som_model1)
som_model2 <- trainSOM(x.data=region_scale, dimension=c(1,3),
                       nb.save=10, maxit=2000, scaling="none",
                       radius.type="letremy")
str(som_model2)

# Visualization
plot(som_model1, main="feature distribution")
table(som_model2$clustering)
plot(som_model2, what="prototypes", type="umatrix", print.title=T)
plot(som_model2, what="obs", type="names", print.title=T, scale=c(1,1))
plot(som_model1, type="counts", main="cluster size")

# Clustering results
clusters <- superClass(model, k=5)
summary(clusters)
plot(clusters)
plot(clusters, type="dendro3d")
