year1 <- substr(pred[,1]$stamp, 1, 4)

data.pca1 <- data.pca

colnames(data.pca1) <- toupper( station_name)
colnames(data.pca) <- toupper( station_name)

data.pca1 <- as.data.frame(cbind( data.pca1, year1))


pdf(
    file = paste(variables, "_av.pdf", sep = ""),
    # The directory you want to save the file in
    
)
data.nona <- data.pca[,av < -0.85]
colnames(data.nona) <- toupper( station_name[av < -0.85])

boxplot(as.matrix(data.nona)) 

gg_miss_var(data.pca1, show_pct = TRUE) + ggtitle(variables)+ labs(x = "")
gg_miss_var(data.pca1, facet = year1, show_pct = TRUE) + labs(x = "") + ggtitle(variables)



data.nona <- data.pca[,av < -0.85]
colnames(data.nona) <- toupper( station_name[av < -0.85])

boxplot(as.matrix(data.nona)) 

par(mfrow=c(3,3))
for(i in 1:length(station_name[av < -0.85]))
    plot(density(na.omit(data.nona[,i])), main = toupper(station_name[av < -0.85][i]))




dev.off()

library(tidyverse)
library(rstatix)
library(ggpubr)


