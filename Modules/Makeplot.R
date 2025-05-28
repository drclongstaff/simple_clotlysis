# This is the recipe for making a plot
plotmake_fun <- function(myPlate, Time, TabRes, mint, maxt, maxy, samples, k, axx, axy) {
  Time <- myPlate[[1]] # Time is first column
  plateData <- myPlate[, -1] # All absorbance data without time column
  absWells <- length(plateData[1, ])
  mint <- min(Time, na.rm = TRUE)
  maxt <- max(Time, na.rm = TRUE)
  miny <- min(plateData, na.rm = TRUE)
  maxy <- max(plateData, na.rm = TRUE) # To set the maximum absorbance for plotting
  samples <- colnames(plateData)

  yi <- plateData[[k]]
  plot <- plot(Time, yi,
    type = "l", col = "grey40", lwd = 2, xlim = c(0, maxt),
    ylim = c(0, maxy * 1.2), ylab = "Absorbance", xaxt=axx, yaxt=axy
  )

  points(Time, yi, pch = 21, col = "slategrey")
  # different coloured lines for start to clotting to max to %lysis and to full lysis
  lines(Time[1:TabRes[[k, 12]]], yi[1:TabRes[[k, 12]]], col = "red", lwd = 3)
  lines(Time[TabRes[[k, 12]]:TabRes[[k, 13]]], yi[TabRes[[k, 12]]:TabRes[[k, 13]]], col = "green", lwd = 3)
  lines(Time[TabRes[[k, 13]]:TabRes[[k, 14]]], yi[TabRes[[k, 13]]:TabRes[[k, 14]]], col = "blue", lwd = 3)
  lines(Time[TabRes[[k, 14]]:TabRes[[k, 15]]], yi[TabRes[[k, 14]]:TabRes[[k, 15]]], col = "darkorange2", lwd = 3)

  # legend including the well number and name
  legend(TabRes[k, 3], maxy * 1.2, xjust = TabRes[k, 3] / maxt, bty = "n", paste0(samples[k], "=", k), cex = 2)

  # dashed lines showing the baseline and times
  abline("v" = TabRes[k, 3], lty = 2)
  abline("v" = TabRes[k, 7], lty = 2)
  abline("v" = TabRes[k, 8], lty = 2)
  abline("h" = TabRes[k, 2], lty = 2)
  abline("v" = TabRes[k, 11], lty = 2)
}
