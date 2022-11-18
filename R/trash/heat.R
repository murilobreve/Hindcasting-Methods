
o <- 1 + o
setwd("~/")
setwd("AnEnMDataR/stations")


av_heat <- colMeans(is.na(data[[o]])) - 1
data.heat <- data[[o]][,av_heat < -0.85]

data.pca.na.heat <- na.omit(data.heat)

# Reorder the correlation matrix
upper_tri <- round(get_upper_tri(cor(data.pca.na.heat)), 2)

for(i in 1:length(upper_tri[,1])){

    upper_tri[i,i] <- NA
}

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
        vjust = 1,
        size = 30,
        hjust = 1
    )) + theme(axis.text.y = element_text(
        angle = 0,
        vjust = 1,
        size = 30,
        hjust = 1
    )) +
    coord_fixed()
# Print the heatmap
print(ggheatmap)


col <- colorRampPalette(c("#BB4444", "#EE9988",
                          "#FFFFFF", "#77AADD",
                          "#4477AA"))

pdf(
    file = paste(toupper(substr(station_name[o], 1, 5)), ".pdf", sep = ""),
    # The directory you want to save the file in
    width = 4,
    # The width of the plot in inches
    height = 4
)

ggheatmap +
    geom_text(aes(Var2, Var1, label = value),
              color = "black",
              size = 11) +
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
        barwidth = 3,
        barheight = 4,
        title.position = "right",
        title.hjust = 1
    )) + ggtitle(toupper(substr(station_name[o], 1, 3))) + theme(plot.title = element_text(color = "black", size = 37,
                                                              face = "bold.italic",vjust = 1, hjust =  0.1))

dev.off()
o <- o +1































