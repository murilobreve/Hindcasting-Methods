library(giscoR)
library(dplyr)
library(sf)

# CRS

library(cartography)
library(sp)
library(ggmap)

chicago <- get_stamenmap(
    bbox = c(
        left = -122.646,
        bottom = 37.447,
        right = -121.8,
        top = 38.1677
    ),
    zoom = 10,
    maptype = c("terrain-background")
)
ggmap(chicago)

lat_lon <- read.csv("lat_lon.csv")[,-1]
lat_lon[,3] <- abs(lat_lon[,3])*-1

lat_lon_RMSE <- list()
for (j in seq_along(variables)) {
    lat_lon_RMSE[[j]] <-
        cbind(lat_lon[lat_lon[, 1]  %in%  str_sub(result_variable[[j]][, 5], 1, 5), ], result_variable[[j]], variables[j])
    colnames(lat_lon_RMSE[[j]])[9] <- "Variable"
    colnames(lat_lon_RMSE[[j]])[3] <- "lon"
}

d.a <- as.data.frame(do.call(rbind, lat_lon_RMSE))

d.a <- d.a[, c("lan", "lon", "RMSE", "Variable")]

d.a$lan <- as.numeric(as.character(d.a$lan))
d.a$lon <- as.numeric(as.character(d.a$lon))
d.a$RMSE <- as.numeric(as.character(d.a$RMSE))
d.a$lon <- abs(d.a$lon) * -1

lat_lon_RMSE <- list()
for (j in seq_along(variables)) {
    lat_lon_RMSE[[j]] <-
        cbind(lat_lon[lat_lon[, 1]  %in%  str_sub(result_variable[[j]][, 5], 1, 5), ], result_variable[[j]], variables[j])
    colnames(lat_lon_RMSE[[j]])[9] <- "Variable"
    colnames(lat_lon_RMSE[[j]])[3] <- "lon"
}

d.a <- as.data.frame(do.call(rbind, lat_lon_RMSE))

d.a <- d.a[, c("lan", "lon", "RMSE", "Variable")]

d.a$lan <- as.numeric(as.character(d.a$lan))
d.a$lon <- as.numeric(as.character(d.a$lon))
d.a$RMSE <- as.numeric(as.character(d.a$RMSE))
d.a$lon <- abs(d.a$lon) * -1

d.pca <-d.a
d.a2 <- rbind(d.pca, d.pls, plsr)
D.A3 <- cbind(d.a2, Methods)


Methods <- c(rep("PLSR", 41))

Methods <- c(rep("PCClust", 41), rep("PLSClust", 41), rep("PLSR", 41))

D.A3[,3] <- round(D.A3[,3], 2)

D.A3[which(D.A3[,4] =="WSPD"),]

D.ALL <- read.csv("save.csv")[,-1]
D.PER <- D.A3

D <- rbind(D.ALL, D.PER)

Training <- c(rep("Training a)", length(D.ALL[,1])), rep("Training b)", length(D.PER[,1])))

D.training <- cbind(D, Training)
D.training[,3] <- round(D.training[,3], 2)

ggmap(chicago) +
    geom_point(
        data = D.training[which(D.training[,4] =="PRES"),],
        mapping = aes(x = lon, y = lan, color = RMSE),
        size = 4.8
    ) +
    ggtitle('') + scale_color_gradient(low = "yellow",
                                                                          high = "red")+ facet_grid(Training ~ Methods) + xlab("Longitude") + ylab("Latitude") +
    theme(
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 17, face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white")
    ) + theme(aspect.ratio = 1,
              axis.text.x = element_text(
                  angle = 45,
                  hjust = 0,
                  vjust = 0
              )) +theme(legend.position="right") + ggrepel::geom_text_repel(data =D.training[which(D.training[,4] =="PRES"),], aes(x = lon, y = lan, label = RMSE), 
                                                                                 fill = "white", box.padding = unit(.6, "lines"),
                                                                                 label.padding = unit(.25, "lines"),
                                                                                 segment.color = "black", segment.size = 0.5, size =5)+ theme(legend.position="right",legend.key.height= unit(0.4, 'cm'), legend.box = "horizontal")+ theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(), axis.ticks.x = element_blank(), panel.background=element_rect(fill = "white"))
    
ggsave("PRES_scenario.pdf",  height = 4,scale = 2)
