################################################################################################################################
# Palettes
################################################################################################################################

dark  <- list(blue   = RColorBrewer::brewer.pal(12,"Paired")[2], 
              green  = RColorBrewer::brewer.pal(12,"Paired")[4], 
              red    = RColorBrewer::brewer.pal(12,"Paired")[6], 
              orange = RColorBrewer::brewer.pal(12,"Paired")[8], 
              purple = RColorBrewer::brewer.pal(12,"Paired")[10], 
              gray   = "#777777",
              black  = "#000000",
              white  = "#FFFFFF")

light <- list(blue   = RColorBrewer::brewer.pal(12,"Paired")[1], 
              green  = RColorBrewer::brewer.pal(12,"Paired")[3], 
              red    = RColorBrewer::brewer.pal(12,"Paired")[5], 
              orange = RColorBrewer::brewer.pal(12,"Paired")[7], 
              purple = RColorBrewer::brewer.pal(12,"Paired")[9], 
              gray   = "#777777",
              black  = "#000000",
              white  = "#FFFFFF")


################################################################################################################################

mPal <- function(c, alpha=1.0) {
  if (is.character(c) && substr(c,1,1) == "#") {
      return(paste0(c,format(as.hexmode(round(alpha*255)), width=2)))
  } else {
      return(rgb(red=c[1], green=c[2], blue=c[3], alpha=round(alpha*255), maxColorValue=255))
  }
}


plotPalette <- function(pal, alpha=1.0) {
  
  root <- sqrt(length(pal))
  layout(matrix(1:(round(root)*ceiling(root)), nrow=round(root)))
  
  par(mar=c(2,0,0,0))
  for (col in 1:length(pal)) {
      plot(1,type='n',xlim=c(0,1),ylim=c(0,1), axes=FALSE, ylab="", xlab="")
      rect(0,0,1,1,col=mPal(pal[[col]], alpha=alpha))
      if (is.null(names(pal)[col])) {
          mtext(col,line=0,side=1)
      } else {
          mtext(names(pal)[col],line=0,side=1)
      }
  }
}


