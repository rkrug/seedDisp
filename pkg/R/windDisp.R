##' Disperses seeds from a seed \code{matrix} by using a
##' 2 dimensional dispersal \code{matrix}
##'
##' A \code{matrix} of the same size as SEEDS containing the dispersed seeds 
##' @usage windDisp(SD2D, SEEDS, MASK)
##' @name windDisp
##' @title Dispersal of seeds by wind
##' @param SD2D \code{matrix} defining the 2D seed dispersal kernel 
##' @param SEEDS \code{matrix} specifying the number of seeds to be dispersed
##' @param MASK \code{matrix} defining the area in which processing takes place (\code{!is.na(MASK)}) 
##' @return \code{matrix} of same size as SEEDS containing the dispersed seeds
##' @author Rainer M Krug \email{Rainer@@krugs.de}
##' @export 
##' @callGraphPrimitives
windDisp <- function(SD2D, SEEDS, MASK) {
  ## Calculate size parameter of sd2D
  dx2 <- (ncol(SD2D) - 1)
  dy2 <- (nrow(SD2D) - 1)
  dx <- dx2 / 2
  dy <- dy2 / 2
  ## buffer for dispersal into cells at the edge
  buffer <- matrix(NA, nrow=nrow(SEEDS), ncol=dx)
  SEEDS <- cbind(buffer, SEEDS, buffer)
  buffer <- matrix(NA, ncol=ncol(SEEDS), nrow=dy)
  SEEDS <- rbind(buffer, SEEDS, buffer)
  ## call C++ function
  return(
         .Call(
               "windDispCpp",
               dx2,
               dy2,
               SD2D,
               SEEDS,
               MASK,
               PACKAGE = "windDispCpp"
               )
         )
}
