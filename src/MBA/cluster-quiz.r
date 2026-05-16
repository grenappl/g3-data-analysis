
S <- c("S1", "S2", "S3", "S4", "S5")
M <- c(60, 50, 83, 75, 33)
Sc <- c(50, 50, 75, 83, 33)
E <- c(83, 80, 75, 75, 40)

data <- data.frame(S,M,Sc,E)
print("Original Data:")
print(data)


# 1. STANDARDIZE (Z-SCORE NORMALIZATIONS)
scaled_data <- scale(data[-1])
print("Scaled Data:")
print(scaled_data)


# 2. DISTANCE MATRIX (EUCLIDEAN)

dist_matrix <- dist(scaled_data, method="euclidean")
print("Distance Matrix:")
print(as.matrix(dist_matrix))



# 3-4. SMALLEST AND LARGEST DISTANCE

min_dist <- min(dist_matrix)
max_dist <- max(dist_matrix)

print(paste("Smallest Distance:", min_dist))
print(paste("Largest Distance:", max_dist))

print("Index of Smallest Distance:")
print(which(as.matrix(dist_matrix) == min_dist, arr.ind=TRUE))

print("Index of Largest Distance:")
print(which(as.matrix(dist_matrix) == max_dist, arr.ind=TRUE))


<<<<<<< HEAD
=======
# 5. K-MEANS CLUSTERING (2 CLUSTERS)
library("factoextra")
library("ggplot2")
 # 6
km_result <- kmeans(scaled_data,
centers = 3, nstart = 25)
km_result$centers
# 7
orig_result <- kmeans(data[-1], centers =3, nstart  =25)
orig_result$centers

# 8
km_result <- kmeans(scaled_data,
centers = 2, nstart = 25)
km_result$cluster
# 9

set.seed(123)
fviz_cluster(km_result, data = scaled_data,
geom = "point",
ellipse.type = "convex",
main = "K-means Clustering Results")


# 6. CONVERT CENTERS BACK TO ORIGINAL VALUES

original_centers_k2 <- k2$centers * attr(scaled_data, "scaled:scale") +
  attr(scaled_data, "scaled:center")


print("Original Centers (k=2):")
print(original_centers_k2)
>>>>>>> 062c248931108786950dddbf4bc085628d7661df





# 6
km_result <- kmeans(scaled_data,
                    centers = 2, nstart = 25)
km_result$centers
# 7
orig_result <- kmeans(data[-1], centers =3, nstart  =25)
orig_result$centers

#8
k2 <- kmeans(scaled_data, centers=2, nstart=25)
data.frame(S, Cluster=k2$cluster)

#9
print(data.frame(S, Cluster=k3$cluster))

