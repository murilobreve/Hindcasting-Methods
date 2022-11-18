library(plotly)
x  <- seq(-2, 2, 0.05)
y1 <- pnorm(x)
y2 <- pnorm(x, 1, 1)


x <- seq(1,15,1)


y1 <- c(494.1,288.9,212.5,170.7,140.7,125.6,114.6,106.8,100.3,97.3,93.7,92.7,92.4,94.4,92.5)

y2 <- c(363,195,134,105,86.2,74.4,66.0,60.7,56.3,52.2,48.9,47.3,46.1,43.4,41.6)

library(plotly)     
x  <- seq(-2, 2, 0.05)
y1 <- pnorm(x)
y2 <- pnorm(x, 1, 1)

df=cbind.data.frame(x,y1,y2)

plot_ly(x = x) %>%
    add_lines(y = y1, color = I("red"), name = "R") %>%
    add_lines(y = y2, color = I("green"), name = "MATLAB") %>%
    layout(title = 'Running time comparison',
           xaxis = list(title = "Cores quantity"),
           yaxis = list(side = 'left', overlaying = "y", title = 'Time'))

plot(x, y1, type = "b", pch = 19, 
     col = "red", xlab = "x", ylab = "y")
# Add a second line
lines(x, y2, pch = 18, col = "blue", type = "b", lty = 2)
# Add a legend to the plot
legend("topleft", legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)

# plot the first curve by calling plot() function
# First curve is plotted
plot(x, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,500) )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(x, y2, col="red", pch="*")
lines(x, y2, col="red",lty=2)
legend(1,100,legend=c("y1","y2"), col=c("blue","red","black"),
       pch=c("o","*"),lty=c(1,2,3), ncol=1)


makePlot<-function(){
    x<-1:10; y1=x*x; y2=2*y1
    plot(x, y1, type="b", pch=19, col="red", xlab="x", ylab="y",tittle)
    lines(x, y2, pch=18, col="blue", type="b", lty=2)
}

cex <- 1.3
par(cex.lab=cex, cex.axis=cex, cex.main=cex)


plot(x, y2, type="b", pch=19, col="red", main="Processing time comparison",xlab="Number of CPU cores", ylab="Time (s)",ylim=c(0,500))
# Add a line
lines(x, y1, pch=18, col="blue", type="b", lty=2)
# Add a legend
op <- par(cex = 1.3)
legend(3, 500, legend=c("MATLAB", "R"),
       col=c("red", "blue"), lty=1:2, cex=0.6)


