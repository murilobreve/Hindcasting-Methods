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
