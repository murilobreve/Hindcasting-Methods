install.packages("ggcorrplot")
library(plotly)
library(ggcorrplot)
library(ggcorrplot)


# get the corr matrix
corr_mat <- round(cor(data.pca.scaled.na),2)

# replace NA with lower triangle matrix
corr_mat[lower.tri(corr_mat)] <- NA

# reduce the corr matrix
melted_corr_mat <- melt(corr_mat)

# plotting the corr heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
    geom_tile()

# create corr matrix and
# corresponding p-value matrix
corr_mat <- round(cor(data.pca.scaled.na),2)
p_mat <- cor_pmat(data)

# plotting the interactive corr heatmap
corr_mat <- ggcorrplot(
    corr_mat, hc.order = TRUE, type = "lower",
    outline.col = "white",
    p.mat = p_mat
)

ggplotly(corr_mat)