require(lubridate)

capitalise <- function(str) {
    return( paste0( toupper(substr(str, 1, 1)), substr(str, 2, nchar(str))))
}


#' Given a set of dates and frequencies on those dates, get a histogram
#'
#' Slow and inefficient way to do this, but it only needs to be done a few times
getHistogram <- function(date, freq, breaks="weeks", maxDate=as.Date("2020-06-28")) {
  result <- c()
  for (i in 1:length(date)) {
    if (!is.na(freq[i]) && date[i] <= maxDate) {
      result <- c(result, rep(format.Date(date[i]), freq[i]))
    }
  }
  result <- as.Date(result)
  
  return(hist(result, breaks=breaks, plot=FALSE, right=FALSE))
}


plotShadedAxes <- function(xlim = c("2020-01-01", "2020-04-16"), ylim=c(0,1), ylab="", xlab="", 
                           bigBreaks="weeks", smallBreaks="days", shading="weeks", shadeCol="#EDEDED", side=2,
                           thinXLabel=1, thinYLabel=1, las=2, srt=NA,
                           label=NA, line=1, decimal=FALSE, dateFormat="%b %d", 
                           axes = FALSE, scientific=FALSE, ...) {
  
      startDate  <- as.Date(xlim[1])
      endDate    <- as.Date(xlim[2])

      bigTicks   <- seq.Date(startDate, endDate, by=bigBreaks)
      smallTicks <- seq.Date(startDate, endDate, by=smallBreaks)
      shadeTicks <- seq.Date(startDate, endDate, by=shading)
      
      if (decimal) {
          startDate  <- lubridate::decimal_date(startDate)
          endDate    <- lubridate::decimal_date(endDate)
          
          bigTicks   <- lubridate::decimal_date(bigTicks)
          smallTicks <- lubridate::decimal_date(smallTicks)
          shadeTicks <- lubridate::decimal_date(shadeTicks)
      }
      
      plot(1, type='n', xlim=c(startDate, endDate), ylim=ylim, axes=FALSE, xaxs='i', ylab=ylab, xlab=xlab, ...)
      rect(xlim[1], ylim[1], xlim[2], ylim[2], border = NA, col = "#FFFFFF")
      
      # Shaded bars
      yheight <- abs(diff(ylim))
      shadeTickWidth <- shadeTicks[2]-shadeTicks[1]
      for (i in 1:(length(shadeTicks))) {
          if (i %% 2 == 0) {
              rect(shadeTicks[i], ylim[1]-0.1*yheight, shadeTicks[i]+shadeTickWidth, ylim[2]+0.1*yheight, col=shadeCol, border=NA)
          }
      }
      
      if (axes) {
          rect(xlim[1], ylim[1], xlim[2], ylim[2], xpd=TRUE)
      }
      
      # Time axis
      if (decimal) {
          xlabels <- format.Date(as.Date(lubridate::round_date(lubridate::date_decimal(bigTicks), unit = "day")), format=dateFormat)
      } else {
          xlabels <- format.Date(bigTicks, format=dateFormat)
      }
      #axis(1, at=bigTicks, labels=NA, lwd=0, lwd.ticks=1, las=las)
      if (!is.na(srt)) {
          axis(1, at=bigTicks[seq(1,length(xlabels), by=thinXLabel)], labels=NA, lwd=0, lwd.ticks=1, las=las)  
          text(x = bigTicks[seq(1,length(xlabels), by=thinXLabel)], y = (ylim[1]-0.1*(ylim[2]-ylim[1])), 
               labels = xlabels[seq(1,length(xlabels), by=thinXLabel)], srt=45, pos = 2, offset = -1,
               cex = par("cex.axis"), xpd=TRUE)  
      } else {
          axis(1, at=bigTicks[seq(1,length(xlabels), by=thinXLabel)], labels=xlabels[seq(1,length(xlabels), by=thinXLabel)], lwd=0, lwd.ticks=1, las=las)  
      }
      
      if (length(smallTicks) != length(bigTicks)) {
          axis(1, at=smallTicks, labels=NA, tcl=-0.25, lwd=0, lwd.ticks=1)
      }
      axis(1, at=c(startDate, endDate), labels=NA, tcl=-0.25, lwd=1, lwd.ticks=NA)
      
      if (side == 2 || side == 4) {
          #axis(side, las=1)
          yticks <- axTicks(side)
          yticks <- yticks[seq(1,length(yticks), by=thinYLabel)]
          axis(side, las=las, at=yticks, labels=format(yticks, scientific=scientific))
      }
      
      x <- startDate - 0.15*(endDate - startDate)
      mtext(label, side=3, line=line, at=x, cex = par("cex.main"))
}



sciNotation <- function(x, digits = 1) {
  if (length(x) > 1) {
    return(append(sciNotation(x[1]), sciNotation(x[-1])))
  }
  if (!x) return(0)
  exponent <- floor(log10(x))
  base <- round(x / 10^exponent, digits)
  if (base == 1) {
    as.expression(substitute(10^exponent, list(exponent = exponent))) 
  } else {
    as.expression(substitute(base %*% 10^exponent, list(base = base, exponent = exponent)))
  }
}

plotLogAxis <- function(lims, side=2, smallticks=TRUE, cex.axis=NA, las=2) {
  
  if (is.na(cex.axis)) {
      cex.axis <- par("cex.axis")
  }
  
  explims <- floor(log10(lims))
  ticks  <- 10^c(explims[1]:explims[2])
  axis(side, at=ticks, labels=sciNotation(ticks), las=las, cex.axis=cex.axis)
  
  if (smallticks == TRUE) {
      smallticks <- unlist(lapply(c((explims[1]-1):explims[2]), function(x) (2:9)*10^x))
      axis(side, at=smallticks, labels=NA, tcl=-0.25, lwd=1)
  }  
}

plotLogAxisReal <- function(lims, side=2, smallticks=TRUE, cex.axis=NA, las=2) {
  
  if (is.na(cex.axis)) {
    cex.axis <- par("cex.axis")
  }
  
  explims <- floor(log10(lims))
  ticks   <- 10^c(explims[1]:explims[2])
  axis(side, at=log(ticks), labels=sciNotation(ticks), las=las, cex.axis=cex.axis)
  
  if (smallticks == TRUE) {
      smallticks <- unlist(lapply(c((explims[1]-1):explims[2]), function(x) (2:9)*10^x))
      axis(side, at=log(smallticks), labels=NA, tcl=-0.25, lwd=1)
  }  
}


#' Plots a frequency distribution above a date axis. By default expects the frequencies in `heights` and the 
#' dates in `dates`. The frequency in slot `i` is assumed to be the count between `dates[i]` and `dates[i+1]`, 
#' that is: 
#' 
#'     `dates[i] <= X < dates[i+1]`  OR `sum(x > dates[i-1] && x <= dates[i])`
#'     
#' This is equivalent to running `heights = hist(data, breaks=dates, right=FALSE, plot=FALSE)$counts`
#' The reason for not using the standard behaviour of `hist()` is for the behaviour to mimic epi-weeks, 
#' e.g. 2020 epi-week 2 starts on 5 January and ends on 11 January. Using Sundays as breaks, `right=FALSE`
#'      should be used to get the correct limits. This week should then be plotted between Jan 05 and Jan 12
#'      on the x-axis when using a barplot. 
#' Using `right=FALSE` for daily breaks will mimic the same behaviour, where the count for e.g. Jan 05 will
#' be plot between Jan 05 and Jan 06. More detail below:
#' 
#' If `heights` is a one-dimensional vector:   
#' - If `barplot` is TRUE a barplot is drawn, with the bar starting on `dates[i]` indicating `heights[i]`.
#' - If `barplot` is FALSE a polygon is drawn, with the height on `dates[i]` equal to `heights[i]`.
#' 
#' If `heights` is a matrix:
#' - It is assumed that each column represents a date and rows represent frequencies of different classes 
#'   on that date.
#' - If `barplot` is TRUE a stacked barplot is drawn, with the bar starting on `dates[i]` indicating the 
#'   rowSum of `heights[, i]`.
#' - If `barplot` is FALSE a stacked polygon is drawn, with the height on `dates[i]` equal to the 
#'   rowSum of `heights[, i]`.
#' - In both cases, if `col` is a vector colors are recycled.
#' 
#' If `heights` is a matrix with exactly 3 rows and `plot.ci` is TRUE:
#' - It is assumed that the second row represents central (mean/median) frequency values and the other 
#'   2 rows lower and upper limits.
#' - If `barplot` is TRUE a barplot is drawn and CIs are plotted using `ci.color`.
#' - If `barplot` is FALSE a line is drawn for the central values in `col` and a polygon is drawn along 
#'   the upper and lower limits, with fill color `coi.color` and border color `border`.
#' 
dateFreqDistribution <- function(heights, dates, startDate="2020-01-01", endDate="2020-06-26", 
                                    col=dark$blue, border=NA, ci.color=mPal(dark$red, 0.5), lty=1, lwd=1, 
                                    ylab="Frequency of TMRCAs\n(per day)", ymax=NA, plot.ci=FALSE, barplot=TRUE, 
                                    add = FALSE, ...) {
  
  
  startDate <- as.Date(startDate)
  endDate   <- as.Date(endDate)
  dates     <- as.Date(dates)
  
  start <- which(dates == startDate)
  end   <- which(dates == endDate) 
  
  # print(paste(start, end))
  if (length(start) == 0 || length(end) == 0) {
    stop("Error: startDate and endDate need to be included in dates.")
  }
  
  
  if (is.na(ymax)) {
    ymax <- max(heights)*1.25
  }
  
  if (!add) {
    plotShadedAxes(xlim=c(startDate, endDate), ylim=c(0, ymax), ylab=ylab, yaxs='i', xaxs='i', ...)
  }
  
  if (!is.null(dim(heights)) && nrow(heights) > 1) {
    # Dealing with a matrix
    n <- ncol(heights)
    if (length(dates) != (n+1)) {
      stop("Error: Need one more date than number of elements in heights to delimit all intervals.")
    }
    
    if (plot.ci && nrow(heights) == 3) {
      # Have central, upper and lower values
      
      if (barplot) {
        par(new=TRUE)
        barplot2(heights[2, start:end], xlim=c(0,end-start), lty=lty, lwd=lwd, 
                 col=col, border=border, ci.color=ci.color, ci.lwd=0.5, ci.width = 0,  
                 plot.ci=TRUE, ci.l=heights[1, (start+1):end], ci.u=heights[3, (start+1):end], 
                 las=2, ylim=c(0,ymax), width=1, space=0, xaxs='i', ylab="", xpd=FALSE)
      } else {
        polygon(c(startDate:endDate, rev(startDate:endDate)), c(heights[1, start:end], rev(heights[3, start:end])),
                border=border, col=ci.color, lty=lty, lwd=lwd) 
        lines(startDate:endDate, heights[2, start:end], col=col, type='l')
        axis(2, las=2)
      }
      
    } else {
      
      if (barplot) {
        # Stacked barplot, set all 0's below the last nonzero value in a column to NA to prevent extra lines being drawn
        # on top of bars
        for (i in 1:ncol(heights)) {
            j <- max(c(0, which(heights[,i] != 0)))
            if (j < nrow(heights)) {
                heights[(j+1):nrow(heights), i] <- NA
            }
        }
        #heights[heights == 0] <- NA
        
        par(new=TRUE)
        barplot2(heights[, start:end], xlim=c(0,end-start), names.arg = NULL,
                 col=col, border=border, lty=lty, lwd=lwd,
                 las=2, ylim=c(0,ymax), width=1, space=0, xaxs='i', ylab="", xpd=FALSE)
      } else {  
        col  <- rep(col, ceiling(nrow(heights)/length(col)))
        prev <- rep(0, ncol(heights[, start:end]))
        i <- 1
        for (i in 1:nrow(heights)) {
          curr <- prev + heights[i, start:end]
          polygon(c(dates[start:end], rev(dates[start:end])), c(curr, rev(prev)), col=col[i], border=border, lty=lty, lwd=lwd)
          prev <- curr
          i <- i + 1
        }
        #rect(min(plotDates), 0, max(plotDates), 1, xpd=TRUE)
        axis(2, las=2)
      }
      
    }
  } else {
    # One-dimensional vector
    n <- length(heights)
    if (length(dates) != (n+1)) {
      stop("Error: Need one more date than number of elements in heights to delimit all intervals.")
    }
    
    if (barplot) {
      par(new=TRUE)
      barplot2(heights[start:end], xlim=c(0,end-start), col=col, border=border, lty=lty, lwd=lwd, 
               las=2, ylim=c(0,ymax), width=1, space=0, xaxs='i', ylab="", xpd=FALSE, axes = FALSE)
    } else {
      
      polygon(c(dates[start], dates[start:end], dates[end]), c(0, heights[start:end], 0),
              border=border, col=col, lty=lty, lwd=lwd) 
      axis(2, las=2)
    } 
  }
  
  
  # Draw x-axis line again (may have been plot over) 
  axis(1, at=0:n, labels=NA, las=2, lwd.ticks=NA)
  
}





#' Lag between tmrca and oldest sequence
plotLagScatter <- function(clusterSummary, metadata, stat="tmrca_median", 
                           startDate=as.Date("2020-01-01"), endDate=as.Date("2020-06-26"), ymax=50, method="pearson", addLine=FALSE, cex.lab=0.8, ...) {
  
      plotShadedAxes(xlim=c(startDate, endDate), ylim=c(0, ymax), ylab="Detection lag (days)", side=2, decimal=TRUE, yaxs='i', ...)
      mtext(side=1, line=4, text="Transmission lineage TMRCA", cex=cex.lab)
      
      lag <- (clusterSummary$oldest - clusterSummary[[stat]])*366
      points(clusterSummary[[stat]], lag, col=mPal(dark$blue, 0.25), pch=20, xpd=FALSE)
    
      corAll     <- round(cor(clusterSummary[[stat]], lag, method = method) ,3)
      corNonZero <- round(cor(clusterSummary[[stat]][lag >= 1], lag[lag >= 1], method = method) ,3)
      
      
      if (addLine == TRUE) {
          abline(linmodAll     <- lm(lag ~ clusterSummary[[stat]]), lwd=2, lty=2)
          abline(linmodNonZero <- lm(lag[lag >= 1] ~ clusterSummary[[stat]][lag >= 1]), lwd=2, lty=1)
          
          mtext(side=3, line=0.5, text=paste0("r = ", corNonZero, " (lineages with lag > 0)    r = ", corAll, " (all lineages)"), cex=0.8)
          mtext(side=3, line=1.5, text=paste0("slope = ", round(linmodNonZero$coefficients[2],2), " (lineages with lag > 0)    slope = ", 
                                              round(linmodAll$coefficients[2],2), " (all lineages)"), cex=0.8)    
      }
      
      
      par(new=TRUE)

      ukseqhist  <- hist(metadata$sample_date[metadata$country == "UK"], breaks="days", plot=FALSE)
      ukseqs     <- c(0, cumsum(ukseqhist$counts))
      ukseqdates <- lubridate::decimal_date(as.Date(ukseqhist$breaks, origin=as.Date("1970-01-01")))

      plot(ukseqdates, ukseqs, type='l', lwd=2, col=mPal(dark$red), ylim=c(0,30000),
           bty='n', axes=FALSE, xlim=c(lubridate::decimal_date(startDate), lubridate::decimal_date(endDate)),
           xlab="", ylab="", xaxs='i', yaxs='i')
      axis(4, at=c(0, axTicks(4)), las=2)
      mtext(side=4, text = "UK virus genome sequences", line=3.5, cex=cex.lab)
      
      return(list(corAll = corAll, corNonZero = corNonZero))
}




#' Scatterplot between two variables, stat1 (x-axis) has to be a date, stat2 (y-axis) has to be a real or integer number
plotLineageScatter <- function(clusterSummary, stat1="tmrca_median", stat2="seqs", 
                            startDate=as.Date("2020-01-01"), endDate=as.Date("2020-06-26"), ymax=500, method="pearson", addLine=FALSE, log=FALSE, 
                            xlab="Transmission lineage TMRCA", ylab="Transmission lineage size", cex.lab=0.8, ...) {
  
  
  if (log) { 
    plotShadedAxes(xlim=c(startDate, endDate), ylim=c(1, log(ymax)), ylab=ylab, side=0, decimal=TRUE, yaxs='i',  ...)
    plotLogAxisReal(lims=c(1, ymax), side = 2)
    y <- log(clusterSummary[[stat2]])
  } else {
    plotShadedAxes(xlim=c(startDate, endDate), ylim=c(0, ymax), ylab=ylab, side=2, decimal=TRUE, yaxs='i', ...)
    y <- clusterSummary[[stat2]]
  }
  mtext(side=1, line=4, text=xlab, cex=cex.lab)
  
  points(clusterSummary[[stat1]], y, col=mPal(dark$blue, 0.25), pch=20, xpd=FALSE)
  
  
  cor     <- cor.test(clusterSummary[[stat1]], y, method = method)
  if (addLine == TRUE) {
    abline(linmodAll     <- lm(y ~ clusterSummary[[stat1]]), lwd=2, lty=2)
    
    r <- round(cor$estimate, 3)
    p <- round(cor$p.value, 4)
    if ("conf.int" %in% names(cor)) {
      c <- paste0(" (", round(cor$conf.int[1],2), ", ", round(cor$conf.int[2],2), ")")
    } else {
      c <- ""
    }
    cortext <- paste0("r = ", r, c, "\np ", ifelse(p == 0, "< 0.0001", paste0("= ",p)))    
    #mtext(side=3, line=0.5, text=paste0("r = ", corAll, " (all lineages)"), cex=0.8)
    #mtext(side=3, line=1.5, text=paste0("slope = ", round(linmodAll$coefficients[2],2), " (all lineages)"), cex=0.8)    
    legend("bottomright", inset=c(0,0.8), title=cortext, cex=par("cex.lab"), legend=NA, bty='n', xpd=TRUE)
  }
  
}



#' Scatterplot between two variables, stat1 (x-axis) and stat2 (y-axis) have to be real or integer numbers
plotScatter <- function(clusterSummary, stat1="duration", stat2="seqs", 
                        xmax=200, ymax=500, method="pearson", addLine=FALSE, log=FALSE, 
                        plotCI=TRUE, plotPredict=FALSE,
                        xlab="Transmission lineage TMRCA", ylab="Transmission lineage size", cex.lab=0.8, label="", line=1, ...) {
  
  
  if (log) { 
    plot(1, type='n', xlim=c(0, xmax), ylim=c(1,log(ymax)), ylab=ylab, xlab="", yaxs='i', xaxs='i', yaxt='n', bty='n')
    plotLogAxisReal(lims=c(1, ymax), side = 2)
    y <- log(clusterSummary[[stat2]])
  } else {
    plot(1, type='n', xlim=c(0, xmax), ylim=c(0,ymax), ylab=ylab, xlab="", yaxs='i', xaxs='i', bty='n', las=1)
    y <- clusterSummary[[stat2]]
  }
  mtext(side=1, line=3, text=xlab, cex=cex.lab)
  
  clusterdf  <- data.frame(x = clusterSummary[[stat1]], y = y)
  linmodAll  <- lm(y ~ x, data = clusterdf)
  
  # Plot CI
  if (plotCI) {
    clusterdfNew <- data.frame(x = seq(from = min(clusterdf$x), to = max(clusterdf$x), length.out=100))
    ci_95 <- predict(linmodAll,
                     newdata  = clusterdfNew,
                     interval = "confidence",
                     level = 0.95)
    clusterdfNew <- cbind(clusterdfNew, ci_95)
    polygon(c(clusterdfNew$x, rev(clusterdfNew$x)), c(clusterdfNew$lwr, rev(clusterdfNew$upr)), col="#00000077", border=NA)
  }
  
  if (plotPredict) {
    clusterdfNew <- data.frame(x = seq(from = min(clusterdf$x), to = max(clusterdf$x), length.out=100))
    ci_95 <- predict(linmodAll,
                     newdata  = clusterdfNew,
                     interval = "predict",
                     level = 0.95)
    clusterdfNew <- cbind(clusterdfNew, ci_95)
    polygon(c(clusterdfNew$x, rev(clusterdfNew$x)), c(clusterdfNew$lwr, rev(clusterdfNew$upr)), col="#00000077", border=NA)
  }
  
  # Plot scatter
  points(clusterdf$x, clusterdf$y, col=mPal(dark$blue, 0.25), pch=20, xpd=FALSE)
  
  corAll  <- round(cor(clusterdf$x, clusterdf$y, method = method) ,3)
  
  cor     <- cor.test(clusterdf$x, clusterdf$y, method = method)
  if (addLine == TRUE) {
    abline(linmodAll, lwd=2, lty=2)
    
    r <- round(cor$estimate, 2)
    p <- round(cor$p.value, 4)
    if ("conf.int" %in% names(cor)) {
      c <- paste0(" (", round(cor$conf.int[1],2), ", ", round(cor$conf.int[2],2), ")")
    } else {
      c <- ""
    }
    cortext <- paste0("r = ", r, c, "\np ", ifelse(p == 0, "< 0.0001", paste0("= ",p)))
    
    #mtext(side=3, line=0.5, text=paste0("r = ", corAll, " (all lineages)"), cex=0.8)
    #mtext(side=3, line=1.5, text=paste0("slope = ", round(linmodAll$coefficients[2],2), " (all lineages)"), cex=0.8)    
    legend("bottomright", inset=c(0,0.8), title=cortext, cex=par("cex.lab"), legend=NA, bty='n', xpd=TRUE)
  }
  
  x <- -0.15*(xmax)
  mtext(label, side=3, line=line, at=x, cex = par("cex.main"))
  
}




#' Plot small 2 class piechart
plotPieProp <- function(count, total, title, line=-2, col=dark$blue, cex=0.8) {
  pie(c(count, total-count), labels=c(paste0(round(100*count/total,2), "%\nsequenced","")), col=c(mPal(col),mPal(col,0.5)), border=mPal(col), cex=cex)
  mtext(side=3, text = title, line=line, cex=cex)
}


plotStats <- function(stats, ylab="", xlab="", ylim=NULL, names=c(), plotGrid=TRUE, plotStrip=TRUE, plotN=TRUE, las=1, ny=NULL, boxMin=10) {
  
  if (is.null(ylim)) {
    ymin <- min(c(0, sapply(stats, min)))
    ymax <- max(sapply(stats, max))
    ylim <- c(ymin, ymax)  
  }

  
  #boxStats <- stats
  #boxStats[which(sapply(boxStats, length) < boxMin)] <- numeric(0)
  boxplot(stats, ylab = ylab, outline=ifelse(plotStrip, FALSE, TRUE), col=mPal(ukPal$nir,0.75),
          ylim=ylim, las = las, lwd = 1, xaxs='i', yaxs='i', xaxt='n')
  
  axis(1, at = 1:length(names), labels = names, las = las, tick = FALSE)
  mtext(side=1, text=xlab, line=4, cex=par("cex.lab"))
  
  if (plotGrid) {
    grid(nx=0, ny=ny, lwd=0.5, col="#000000")
  }
  
  if (plotStrip) {
    stripchart(stats, vertical = TRUE, method = "jitter", jitter = 0.2, add = TRUE, pch = 16, col = mPal(ukPal$eng, 0.5), cex=1)
  }
  
}

