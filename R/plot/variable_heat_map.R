
library(corrplot)
colnames(data.pca.scaled.na) <-
    toupper(substr(station_name_ncdf4[av < -0.85], 1, 3))


# Reorder the correlation matrix
upper_tri <- round(get_upper_tri(cor(data.pca.scaled.na)), 2)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
        low = "#BB4444",
        high = "deepskyblue4",
        mid = "white",

        midpoint = 0,
        limit = c(-1, 1),
        space = "Lab",
        name = "Pearson\nCorrelation"
    ) +scale_y_discrete(position = "right")+
    theme_minimal() + # minimal theme
    theme(axis.text.x = element_text(
        angle = 45,
        vjust = 1.5,
        size = 6,
        hjust = 1
    )) + theme(axis.text.y = element_text(
        angle = 0,
        size = 6,
        hjust = -4
    )) +
    coord_fixed()
# Print the heatmap
print(ggheatmap)

pdf(
    file = paste(variables, ".pdf", sep = ""),
    # The directory you want to save the file in
    width = 2,
    # The width of the plot in inches
    height = 4
)

ggheatmap +
    geom_text(aes(Var2, Var1, label = value),
              color = "black",
              size = 2.57) +
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(10, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "vertical"
    ) +
    guides(fill = guide_colorbar(
        barwidth = 7,
        barheight = 1,
        title.position = "top",
        title.hjust = 0.5
    )) + ggtitle(variables) + theme(plot.title = element_text(color = "black", size = 10,
                                                                  face = "bold.italic", vjust = -12, hjust =  0.1))

dev.off()

