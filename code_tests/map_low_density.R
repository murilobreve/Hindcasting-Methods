library(giscoR)
library(dplyr)
library(sf)

# CRS

library(cartography)
library(sp)
library(ggmap)


STATIONS <- c("WDSV2", "DOMV2", "SWPV2", "MNPV2", "CRYV2", "CHYV2", "KPTV2", "YKTV2", "YKRV2")
lat <- c(36.977, 36.962, 36.943,  36.778, 36.888, 36.926,  37.165, 37.227, 37.251)
lon <- c(-76.315, -76.424, -76.329, -76.302, -76.338, -76.007,  -75.988, -76.47, -76.342)

ll_norfolk <- as.data.frame(cbind(STATIONS, lat, lon))
ll_norfolk[,2] <- as.numeric(as.character( ll_norfolk[,2]) ) 
ll_norfolk[,3] <- as.numeric(as.character( ll_norfolk[,3]) ) 

ll_norfolk <- ll_norfolk[order(ll_norfolk$STATIONS),]

chicago <- get_stamenmap(
    bbox = c(
        left = -76.65,
        bottom = 36.747,
        right = -75.9,
        top = 37.3677
    ),
    zoom = 10,
    maptype = c("terrain-background")
)
ggmap(chicago) +
    geom_point(aes(x=lon, y=lat), data=ll_norfolk, size=2.3, fill = "red", pch = 21) +
    ggrepel::geom_label_repel(data = ll_norfolk, aes(x = lon, y = lat, label = STATIONS), 
                              fill = "white", box.padding = unit(.6, "lines"),
                              label.padding = unit(.25, "lines"),
                              segment.color = "red", segment.size = 1) + ylab("Latitude")+ xlab("Longitude")

ggsave("map_norfolk.png", dpi = 300, scale = 1)


lat_lon_RMSE <- list()
for (j in seq_along(variables)) {
    lat_lon_RMSE[[j]] <-
        cbind(ll_norfolk[ll_norfolk[, 1]  %in%  toupper(str_sub(result_variable[[j]][, 5], 1, 5)), ], result_variable[[j]], variables[j])
    colnames(lat_lon_RMSE[[j]])[9] <- "Variable"
    colnames(lat_lon_RMSE[[j]])[3] <- "lon"
}

d.a <- as.data.frame(do.call(rbind, lat_lon_RMSE))

d.a <- d.a[, c("lat", "lon", "RMSE", "Variable")]

d.a$lat <- as.numeric(as.character(d.a$lat))
d.a$lon <- as.numeric(as.character(d.a$lon))
d.a$RMSE <- as.numeric(as.character(d.a$RMSE))
d.a$lon <- abs(d.a$lon) * -1

lat_lon_RMSE <- list()
for (j in seq_along(variables)) {
    lat_lon_RMSE[[j]] <-
        cbind(ll_norfolk[ll_norfolk[, 1]  %in%  toupper(str_sub(result_m[[j]][, 5], 1, 5)), ], result_m[[j]], variables[j])
    colnames(lat_lon_RMSE[[j]])[9] <- "Variable"
    colnames(lat_lon_RMSE[[j]])[3] <- "lon"
}

d.ac <- as.data.frame(do.call(rbind, lat_lon_RMSE))

d.ac <- d.ac[, c("lat", "lon", "RMSE", "Variable")]


d.ac$lat <- as.numeric(as.character(d.ac$lat))
d.ac$lon <- as.numeric(as.character(d.ac$lon))
d.ac$RMSE <- as.numeric(as.character(d.ac$RMSE))
d.ac$lon <- abs(d.ac$lon) * -1

d.a2 <- rbind(d.a, d.ac)
D.A3 <- as.data.frame(cbind(d.a2, Methods))
D.A3$RMSE <- as.numeric(D.A3$RMSE)



Methods <- c(rep("PLSR", 41), rep("ClustAnEn", 41))

ggmap(chicago) +
    geom_point(
        data = D.A3,
        mapping = aes(x = lon, y = lat, color = RMSE),
        size = 4.8
    ) +
    ggtitle('PLSR vs PCClust predicting with all variables') + scale_color_gradient(low = "blue",
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
              )) +theme(legend.position="right") 

ggsave("allmap_norfolk.png", dpi = 300, scale = 1.5)

