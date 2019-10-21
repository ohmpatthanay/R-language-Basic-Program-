pizza <- function(y = 0.8){
	opar <- par()$mar
	on.exit(par(mar = opar))
	par(mar = rep(0, 4))
	plot.new()
	circle(0.5, 0.5, 4, "cm", , 4)
	circle(0.5, 0.5, 3.5, "cm", , 4)
	circle(0.42, 0.62, 0.3, "cm", , 4)
	circle(0.47, 0.4, 0.3, "cm", , 4)
	circle(0.60, 0.45, 0.3, "cm", , 4)
	circle(0.66, 0.54, 0.3, "cm", 4, 4)
	circle(0.55, 0.65, 0.3, "cm", 4, 4)
	circle(0.55, 0.65, 0.3, "cm", 4, 4)
	circle(0.36, 0.55, 0.3, "cm", 3, 4)
	circle(0.33, 0.44, 0.3, "cm", 3, 4)
	circle(0.56, 0.35, 0.3, "cm", 3, 4)
	segments(0.31, 0.35, 0.69, 0.65, lwd = 4)
	segments(0.5, 0.74, 0.5, 0.26, lwd = 4)
	segments(0.68, 0.34, 0.31, 0.65, lwd = 4)
	segments(0.26, 0.495, 0.74, 0.495, lwd = 4)
	points(0.38, y, pch=80,cex=3,col="orange")
	points(0.43, y, pch=73,cex=3,col="red")
	points(0.48, y, pch=90,cex=3,col="orange")
	points(0.55, y, pch=90,cex=3,col="red")
	points(0.62, y, pch=65,cex=3,col="orange")

}

circle <- function(x, y, radius, units=c("cm", "in"), segments=100,  
    lwd = NULL){ 
    units <- match.arg(units) 
    if (units == "cm") radius <- radius/2.54 
    plot.size <- par("pin") 
    plot.units <- par("usr") 
    units.x <- plot.units[2] - plot.units[1] 
    units.y <- plot.units[4] - plot.units[3] 
    ratio <- (units.x/plot.size[1])/(units.y/plot.size[2]) 
    size <- radius*units.x/plot.size[1] 
    angles <- (0:segments)*2*pi/segments 
    unit.circle <- cbind(cos(angles), sin(angles)) 
    shape <- matrix(c(1, 0, 0, 1/(ratio^2)), 2, 2) 
    ellipse <- t(c(x, y) + size*t(unit.circle %*% chol(shape))) 
    lines(ellipse, lwd = lwd) 
}

oopt <- animation::ani.options(interval = 0.1)

flipbook <- function(){
	lapply(seq(1.01, 0.18, by=-0.05), function(i){
		pizza(i)
		animation::ani.pause()

	})
}

pizza()
flipbook()