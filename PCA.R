# library
library("ggfortify")
library("corrplot")
library("dplyr")
library("plotly")
library("readxl")
library("openxlsx")
library("Hmisc")
library("factoextra")
library("ggplot2")
library("RColorBrewer")
library("htmlwidgets")
library("reshape2")

data <- data.frame(
  Math1 = c(17.18, 19, 18, 17.5, 12.3, 18.5, 15.13, 18.25, 20, 16.7),
  Math2 = c(18.75, 20, 17, 16, 15, 15, 16, 20, 19.5, 18.25),
  English1 = c(16.75, 17.5, 18, 17.5, 16.75, 17, 18, 18.75, 17, 18.75),
  English2 = c(18.5, 20, 17, 15, 18.9, 18, 16.75, 19.9, 17.5, 20),
  Programming1 = c(19, 18, 18, 14.5, 18, 14, 13, 20, 18, 20),
  Programming2 = c(18.75, 18, 17, 17, 12, 18.5, 17.25, 20, 17.5, 20)
)

R = cor(data)
R |> round(digits = 2)
corrplot::corrplot.mixed(R,
                         lower = "number", upper = "circle", bg = "gray",
                         tl.pos = 'lt', tl.col = "black", tl.srt = 45,
                         number.cex = 0.7, number.digits = 2)


# eigen values and eigen vectors
eig = eigen(R)
eig$values |> round(digits = 3)

eig$vectors |> round(digits = 3)

# PCA: Principal Components Analysis
pca = prcomp(data, scale. = TRUE)
pca_summary = summary(pca)
pca_summary


pca_importance = as.data.frame(pca_summary[["importance"]])
pca_importance = t(pca_importance)

p = 1:dim(data)[2]

df <- data.frame(
  p = p,
  explained_variance = pca_importance[, 2],
  cumulative_variance = pca_importance[, 3]
)
ggplot(df, aes(x = p)) +
  geom_line(aes(y = explained_variance, color = "explained variance ratio")) +
  geom_point(aes(y = explained_variance, color = "explained variance ratio"), size = 2) +
  geom_line(aes(y = cumulative_variance, color = "cumulative explained variance ratio")) +
  geom_point(aes(y = cumulative_variance, color = "cumulative explained variance ratio"), size = 2) +
  scale_color_manual(values = c("red", "green")) +
  labs(x = "Principal Component", y = "Explained Variance Ratio", color = "Legend") +
  theme_bw() + theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 2, byrow = TRUE))


# Correlation matrix of variables and principal components
cor_matrix <- cor(data, pca$x) |> round(digits = 2)

cor_df <- as.data.frame(as.table(cor_matrix))
names(cor_df) <- c("Var1", "Var2", "Correlation")

ggplot(cor_df, aes(Var2, Var1, size = abs(Correlation), fill = Correlation)) +
  geom_point(shape = 21, color = "black") +
  scale_fill_gradient(low = "red", high = "lightgreen") +
  labs(title = "Bubble Plot of Correlation Matrix", x = "", y = "") +
  geom_text(aes(label = Correlation), colour = "black", size = 2) + 
  scale_x_discrete(position = "top") +
  scale_size_continuous(range = c(4, 14)) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())



# biplot

ggplot(as.data.frame(pca$rotation)) +
  #geom_point(data = as.data.frame(pca$x), aes(x = PC1, y = PC2)) +
  geom_segment(data = as.data.frame(pca$rotation), aes(x = 0, y = 0, xend = PC1, yend = PC2), color = "black") +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_text(data = as.data.frame(pca$rotation), aes(x = PC1, y = PC2, label = rownames(pca$rotation)), 
            color = "blue", nudge_x = 0, nudge_y = 0, size = 4)  +
  #scale_x_continuous(limits = c(-1, 1)) +
  #scale_y_continuous(limits = c(-1, 1)) +
  labs(title = "biplot", x = "PC1", y = "PC2") +
  theme_light()


# Graph of variables
factoextra::fviz_pca_var(pca) 


# Biplot of individuals and variables
factoextra::fviz_pca_biplot(pca) 


# Define the ease function
ease <- function(data1, data2, frames = 100) {
  n <- dim(data1)[1]
  data <- data1
  dataDiff <- (data2 - data1) / (frames - 1)
  for (i in 1:(frames - 1)) {
    data <- rbind(data, data1 + dataDiff * i)
  }
  data$frame <- rep(1:frames, each = n)
  data
}



# Prepare 3D plot data
dataxyz <- data[, c("Math1", "English1", "Programming2")]
cx <- cov(dataxyz)
eg <- eigen(cx)
vecs <- eg$vectors

# One-dimensional projection
DataNew <- as.matrix(dataxyz) %*% vecs
Dim1_Projection <- t(t(DataNew[, 1])) %*% t(vecs[, 1])
Dim1_Projection <- as.data.frame(Dim1_Projection)
colnames(Dim1_Projection) <- colnames(dataxyz)

# Change location for better visualization
m1 <- colMeans(Dim1_Projection)
m3 <- colMeans(dataxyz)
n <- dim(dataxyz)[1]
mm1 <- matrix(1, nrow = n) %*% t(m1)
mm3 <- matrix(1, nrow = n) %*% t(m3)
Dim1_Projection_locChanged <- Dim1_Projection - mm1 + mm3

# Define line points for the plot
LinePoints <- rbind(Dim1_Projection_locChanged[9,], Dim1_Projection_locChanged[4,])
LinePoints <- as.data.frame(LinePoints)
colnames(LinePoints) <- colnames(dataxyz)

# Create the 3D plot with plotly
fig1 <- plot_ly(Dim1_Projection_locChanged, x = ~Math1, y = ~English1, z = ~Programming2, colors = '#BFAAAA') %>%
  add_markers(size = 2) %>%
  add_trace(data = dataxyz, x = ~Math1, y = ~English1, z = ~Programming2, colors = '#00AAAA', type = 'scatter3d', mode = 'markers') %>%
  add_trace(data = LinePoints, x = ~Math1, y = ~English1, z = ~Programming2, type = "scatter3d", mode = 'lines')

# Display the plot
fig1

# Compute ease data
dataxyz$size <- 20
Dim1_Projection_locChanged$size <- 5
dataCum <- ease(dataxyz, Dim1_Projection_locChanged, 40)

# Create the animation plot with plotly
fig2 <- plot_ly(Dim1_Projection_locChanged, x = ~Math1, y = ~English1, z = ~Programming2, colors = '#BFAAAA') %>%
  add_markers(size = 2) %>%
  add_trace(data = dataxyz, x = ~Math1, y = ~English1, z = ~Programming2, colors = '#00AAAA', type = 'scatter3d', mode = 'markers') %>%
  add_trace(data = LinePoints, x = ~Math1, y = ~English1, z = ~Programming2, type = "scatter3d", mode = 'lines') %>%
  add_trace(data = dataCum, x = ~Math1, y = ~English1, z = ~Programming2, frame = ~frame, colors = '#00FF00', type = 'scatter3d', mode = 'markers') %>%
  animation_opts(10, easing = "linear", redraw = TRUE)

# Display the animation plot
fig2
