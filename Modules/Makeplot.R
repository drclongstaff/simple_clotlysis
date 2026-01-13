# This is the recipe for making a plot
plotmake_fun <- function(myPlate, TabRes, k, axx, axy, maxy) {
  Time <- myPlate[[1]] # Time is first column
  plateData <- myPlate[, -1] # All absorbance data without time column
  absWells <- length(plateData[1, ])
  mint <- min(Time, na.rm = TRUE)
  maxt <- max(Time, na.rm = TRUE)
  miny <- min(plateData, na.rm = TRUE)
  #maxy <- max(plateData, na.rm = TRUE) # To set the maximum absorbance for plotting
  samples <- colnames(plateData)

  yi <- plateData[[k]]
  plot <- plot(Time, yi,
               type = "l", col = "grey30", lwd = 2, xlim = c(0, maxt),
               ylim = c(0, maxy * 1.2), 
               ylab = "Absorbance", xaxt=axx, yaxt=axy
  )
  
  points(Time, yi, pch = 21, col = "grey60", cex = 1)
  # different coloured lines for start to clotting to max to %lysis and to full lysis
  #lines(Time[1:TabRes$startPoint[k]], yi[1:TabRes$startPoint[k]], col = "red", lwd = 1) #up to start
  lines(Time[TabRes$startPoint[k]:TabRes$maxPoint[k]], yi[TabRes$startPoint[k]:TabRes$maxPoint[k]], col = "red", lwd = 3) #start to peak
  lines(Time[TabRes$maxPoint[k]:TabRes$lysPoint[k]], yi[TabRes$maxPoint[k]:TabRes$lysPoint[k]], col = "green", lwd = 3) #peak to lyspoint
  lines(Time[TabRes$lysPoint[k]:TabRes$lastPoint[k]], yi[TabRes$lysPoint[k]:TabRes$lastPoint[k]], col = "blue", lwd = 3) #peak to lyspoin
  
  # legend including the well number and name
  #legend(DownResS$Well[k], maxy * 1.2, xjust = DownRes$lysTime[k]/maxt, bty = "n", paste0(samples[k], "=", k), cex = 2)
  legend("topright", paste0(samples[k], "=", k), bty = "n", cex = 1.5)
  
  #results lines for baseline and times
  abline("v"= TabRes$lysTime[k], lty=2, col = "black")
  abline("v"= TabRes$clotTime[k], lty=2, col = "deeppink")
  abline("v"= TabRes$maxTime[k], lty=2, col = "rosybrown")
  abline("v"= TabRes$endTime[k], lty=2, col = "steelblue")
  abline("h"= TabRes$minAbs[k], lty = 2, col = "grey30")
  abline("h"= TabRes$clotAbs[k], lty = 2, col = "royalblue")
  abline("h"= TabRes$maxAbs[k], lty = 2, col = "purple1")
  abline("h"= TabRes$lysAbs[k], lty = 2, col = "tomato")
}
