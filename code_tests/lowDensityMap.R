library(giscoR)
library(dplyr)
library(sf)

# CRS

library(cartography)
library(sp)
library(ggmap)

chicago <- get_stamenmap(
    bbox = c(
        left = -67.4,
        bottom = 44.6,
        right = -66.8,
        top = 45
    ),
    zoom = 11,
    maptype = c("terrain")
)
ggmap(chicago)

ggmap(chicago) +
    geom_point(aes(x=lon, y=lat), data=lat_lon, size=2.3, fill = "red", pch = 21) +
    ggrepel::geom_label_repel(data = lat_lon, aes(x = lon, y = lat, label = name), 
                              fill = "white", box.padding = unit(.6, "lines"),
                              label.padding = unit(.25, "lines"),
                              segment.color = "red", segment.size = 1) + ylab("Latitude")+ xlab("Longitude")

ggsave("map_norfolk.png", dpi = 200, scale = 1)

name <- c("CFWM1","PSBM1")
lat <- c(44.657,44.905)
lon <- c(-67.205,-66.983)

lat_lon <- cbind(name,lat,lon)
lat_lon <- as.data.frame(lat_lon)
lat_lon[,2] <- as.numeric(as.character(lat_lon[,2]))
lat_lon[,3] <-  as.numeric(as.character(lat_lon[,3]))



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

d.a2 <- rbind(d.a, d.a)
D.A3 <- cbind(d.a2, Methods)



Methods <- c(rep("PLSR", 41), rep("ClustAnEn", 41))

ggmap(chicago) +
    geom_point(
        data = D.A3,
        mapping = aes(x = lon, y = lan, color = RMSE),
        size = 4.8
    ) +
    ggtitle('PLSR predicting with all variables') + scale_color_gradient(low = "blue",
                                                                         high = "red" )+ facet_grid(Methods ~ Variable) + xlab("Longitude") + ylab("Latitude") +
    theme(
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 14, face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white")
    ) + theme(aspect.ratio = 1,
              axis.text.x = element_text(
                  angle = 45,
                  hjust = 0,
                  vjust = 0
              )) +theme(legend.position="bottom") 
