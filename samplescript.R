
unique_clusters = sort(unique(clusters))

clusplot(twoColumns, unique_clusters[clusters], col.p = c("deeppink", "blue")[labels_behavior] )

# Create variables for the legend.

cluster_names = paste("Cluster", as.character(unique_clusters))
label_col_codes = as.numeric(unique(labels_behavior))
label_names = as.character(unique(labels_behavior))
black = rep("black", length(unique_clusters))
legend("topright",  
       legend = c(cluster_names, label_names), 
       col = c(black, "deeppink", "blue"), 
       pch = c( unique_clusters, 15, 15), 
       bty = "n", 
       text.col = "black") 


# clusplot(twoColumns, c(1, 2)[labels_genotype], col.p = c("deeppink", "blue")[clusters] )
# # Create variables for the legend.
# unique_clusters = sort(unique(clusters))
# cluster_names = paste("Cluster", as.character(unique_clusters))
# label_col_codes = as.numeric(unique(labels_genotype))
# label_names = as.character(unique(labels_genotype))
# legend("topright",  
#        legend = c(cluster_names, label_names), 
#        col = c("black", "black", "deeppink", "blue"), 
#        pch = c( unique_clusters, 15, 15), 
#        bty = "n", 
#        text.col = "black") 


par(xpd = T, mar = par()$mar + c(0,0,0,7))
plot(pca$scores[, 1],
     pca$scores[, 2],
     main = "PCA",
     xlab = "First component",
     ylab = "Second component",
     col = c("deeppink", "blue")[crabs[, 2]],
     pch = c(1, 2)[crabs[, 1]])
legend(0.03, 0.025,
       c("Male", "Female"),
       col = c("blue", "deeppink"),
       cex = 0.8,
       bty = "n",
       lwd = 1, lty = 1)
legend(0.03, 0.01,
       c("Blue species", "Orange species"),
       cex = 0.8,
       bty = "n",
       pch = c(1,2))
par(mar=c(5, 4, 4, 2) + 0.1)




library(MASS)
normalizedCrabs <- crabs[, c(4, 5, 7, 8)] / rowSums(crabs[, c(4, 5, 7, 8)])
pca <- princomp(normalizedCrabs)
plot(pca$scores[, 1],
     pca$scores[, 2],
     main = "PCA",
     xlab = "First component",
     ylab = "Second component",
     col = c("deeppink", "blue")[crabs[, 2]],
     pch = c(1, 2)[crabs[, 1]])






par(xpd = T, mar = par()$mar + c(0,0,0,7))
plot(pca$scores[, 1],
     pca$scores[, 2],
     main = "PCA",
     xlab = "First component",
     ylab = "Second component",
     col = c("deeppink", "blue")[crabs[, 2]],
     pch = c(1, 2)[crabs[, 1]])
legend(0.03, 0.025,
       c("Male", "Female"),
       col = c("blue", "deeppink"),
       cex = 0.8,
       bty = "n",
       lwd = 1, lty = 1)
legend(0.03, 0.01,
       c("Blue species", "Orange species"),
       cex = 0.8,
       bty = "n",
       pch = c(1,2))
par(mar=c(5, 4, 4, 2) + 0.1)

