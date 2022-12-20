
loaded_stations <- LoadData(params_list, variables)
pred <- loaded_stations$pred
target <- loaded_stations$target


# Organize data -----------------------------------
data <-
    OrganizeIntoDataFrame(params_list, loaded_stations, avail_threshold)

colnames(data) <- str_sub(colnames(data),1,5)

library(akima)

corr <- cor(na.omit(data))




dInterp <- list()
for(i in seq_along(corr[,1])){
    
    k <- cbind(lat_lon[lat_lon[,1]  %in% names(corr[,i]),],corr[,i], toupper(colnames(corr)[i]))
    colnames(k) <- c("STATIONS", "lat", "lon", "cor")
    sp_data <- interp(x = k$lon,
                      y = k$lat,
                      z = k$cor)
    dInterp[[i]] <- data.frame(expand.grid(x = sp_data$x,
                                           y = sp_data$y), z = c(sp_data$z))
    dInterp[[i]] <- cbind(dInterp[[i]],toupper(colnames(corr)[i]))
    
}


k <- do.call(rbind,dInterp)
k <- as.data.frame(k)


colnames(k)[4] <- "STATIONS"


require('RColorBrewer')
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
ggplot(data = k,
       aes(x = x,
           y = y,
           z = z)) +
    stat_summary_2d(fun = median) + 
    scale_fill_gradientn(name = "Correla.",
                         colours = YlOrBr,
                         space = "Lab",  limits=c(0,1))+ facet_wrap(~ STATIONS,  ncol = 5) + theme_bw()+ theme(legend.position="bottom", legend.box = "horizontal")+theme_minimal()+ theme(axis.text.x = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(), axis.ticks.x = element_blank(), panel.background=element_rect(fill = "white"))


ggsave("WSPD_corr.pdf", height = 2.5)


print(pred.stat.bin.width)
ggsave(filename = "NYSubsamplePredStatBinWidth.png",
       plot = pred.stat.bin.width,
       scale = 1,
       width = 5, height = 3,
       dpi = 300)



