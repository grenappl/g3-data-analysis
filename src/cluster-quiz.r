
S <- c("S1", "S2", "S3", "S4", "S5")
M <- c(60, 50, 83, 75, 33)
Sc <- c(50, 50, 75, 83, 33)
E <- c(83, 80, 75, 75, 40)

data <- data.frame(S,M,Sc,E)
print("Original Data:")
print(data)


# 1. STANDARDIZE (Z-SCORE NORMALIZATION)
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

