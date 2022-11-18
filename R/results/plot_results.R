prevPCA1comp <- list.rbind(prevAle)
prevPCA3comp <- list.rbind(prevAle)
prevPSLR <- list.rbind(prevAle)
prevPSL3comp <- list.rbind(prevAle)


prevPCA <- list.rbind(prevAle)
predicted.values <- unlist(prevPCA[,variable.predict])
real.values <- target[[variable.predict]][(target$stamp >= forecasting.t0)]

time.plot <- target$stamp[target$stamp >= forecasting.t0]

length(time.plot)

df <- data.frame(predicted.values,real.values,time.plot)
df.m <- df[]

plot()

plot(time.plot,real.values, type="l", col="green", lwd=5, xlab="time", ylab=variable.predict)
lines(time.plot, predicted.values, col="red", lwd=2)
title(paste(variable.predict, "Prediction"))
legend(x = "top",
       col = c("green", "red"), lty = 1, lwd = 5,
       legend = c('Real', 'Predicted'))
predicted.values.pls <- predicted.values
predicted.values.plsan <- predicted.values

library(plotly)
x  <- seq(-2, 2, 0.05)
y1 <- pnorm(x)
y2 <- pnorm(x, 1, 1)

plot_ly(x = as.POSIXct(time.plot[-1], origin = "1970-01-01")) %>%
    add_lines(y = as.vector(prevPCA3comp)[-1], color = I("blue"), name = "PCAnEn (3 PC)")%>%
    add_lines(y = prevPSLR, color = I("black"), name = "PLSR (8 PC)")%>%
    add_lines(y = prevPSL3comp[-1] , color = I("red"), name = "PLS + AnEn (3 PC)")%>%
    add_lines(y = real.values[-1] , color = I("green"), name = "PRES observed")

prevpca3comp <- as.vector(prevPCA)


E <-
    unlist(prevAle)[variable.predict] - target[[variables[i]]][(target$stamp >= forecasting.t0)]

variable.predict

p = ggplot() +
    geom_line(data = prescription1, aes(x = dates, y = Difference), color = "blue") +
    geom_line(data = prescription2, aes(x = dates, y = Difference), color = "red") +
    xlab('Dates') +
    ylab('percent.change')
