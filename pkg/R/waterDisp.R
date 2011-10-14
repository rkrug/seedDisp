##' Flow cells in m by one cell usind the direction given in agnps and adds the
##' 
##' @usage waterDisp(m, agnps, depRate)
##' @name waterDisp
##' @title Disperse seeds using water dispersal
##' 
##' @title Disperse seeds in \code{input} in direction given in \code{agnps} using deposit rates in \code{depRates} until all seeds are deposited
##' @param input \code{matrix} of seeds to be dispersed by water flow
##' @param depRate \code{matrix} of deposit rates
##' @param agnps \code{matrix} of same dim=ension as \code{m} containing agnps flow directions as returned from GRASS of same dimensionality as \code{input}
##' @param zeroToNULL \code{boolean} if TRUE convert zeros to NA, otherwise NA to 0
##' @param progress \code{boolean} if TRUE, showing progress, otherwise silent
##' @param m \code{matrix} to be "flown" of same dimensionality as \code{input}
##' @return 
##' @author Rainer M Krug
##' @export 
##' @callGraphPrimitives
waterDisp <- function(input, depRate, agnps, zeroToNULL=TRUE, progress=TRUE) {
  ## define vectors for flowing direction translation from agnps to matrix
  ## agnps 1,  2,  3,  4,  5,  6,  7,  8) 
  dx <- c( 0, +1, +1, +1,  0, -1, -1, -1) # first index in matrix
  dy <- c(-1, -1,  0, +1, +1, +1,  0, -1) # second index in matrix
  ## create index Matix
  im <- cbind(
              rep(1:nrow(input), each=ncol(input)),
              rep(1:ncol(input), nrow(input))
              )
  ## create "flowed" index matrix
  ## this matrix will contain the NEW locations of the cells
  ## Cells not included in ims will be set to NA, as nothing flows into them.
  ims <- im
  ## calculate x-flow
  ims[,1] <- im[,1] + dx[agnps[im]]
  ## and correct for "out of matrix flows"
  ims[,1][ims[,1] < 1      ] <- NA
  ims[,1][ims[,1] > nrow(input)] <- NA
  ## calculate y-flow
  ims[,2] <- im[,2] + dy[agnps[im]]
  ## and correct for "out of matrix flows"
  ims[,2][ims[,2] < 1      ] <- NA
  ims[,2][ims[,2] > ncol(input)] <- NA
  ## remove all which have NA from im and ims
  sel <- !is.na(ims[,1]) & !is.na(ims[,2])
  im <- im[sel,]
  ims <- ims[sel,]
  ## build flowed matrix m
  output.dep <- input
  output.dep[] <- 0
  output.disp <- input
  ## populate output
  j <- 0
  ## call recursively, until the number of seeds in output.disp == 0
  repeat {
    ## set new input.disp to previous output.disp
    input.disp <- output.disp
    input.disp[is.na(input.disp)] <- 0
    output.disp[] <- 0
    ## define imX andimsX, as they will be modified in the while loop
    imX <- im
    imsX <- ims
    j <- j+1
    while (nrow(imsX) > 0) {
      if (progress) {
        print( paste(j, sum(output.disp, na.rm=TRUE),  sum(input.disp, na.rm=TRUE)) )
      }
      ## identify uniques
      uni <- !duplicated(imsX)
      ## calculate seeds to be leaving cell 
      flow <-  ceiling( (1 - depRate[imX[uni,]]) * input.disp[imX[uni,]] )
      ## subtract flowing seeds from initial cells
      output.dep[imX[uni,]] <- output.dep[imX[uni,]] + ( input.disp[imX[uni,]] - flow )
      ## add flowing seeds to target cells
      output.disp[imsX[uni,]] <- output.disp[imsX[uni,]] + flow
      ## take unique calls away and start again
      imX  <- imX [!uni,]
      imsX <- imsX[!uni,]
    }
    if (sum(output.disp, na.rm=TRUE) == 0) {
      break()
    }
  }
  ##
  if (zeroToNULL) {
    output.dep[output.dep==0] <- NA
  } else {
    output.dep[is.na(output.dep)] <- 0
  }
  return(output.dep)
}
