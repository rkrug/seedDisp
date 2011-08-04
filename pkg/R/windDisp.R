MAINVERSION <- 0
SVNVERSION <- "3"
SVNSTATE <- 0
##' Disperses seeds from a seed \code{matrix} by using a
##' 2 dimensional dispersal \code{matrix}
##'
##' .. content for \details{} ..
##' @title Dispersal of seeds by wind
##' @param SD2D matrix defining the 2D seed dispersal kernel 
##' @param SEEDS matrix specifying the number of seeds to be dispersed
##' @param MASK matrix defining the area in which processing takes place (!is.na(MASK))
##' @return Matrix of same size as SEEDS containing the dispersed seeds
##' @author Rainer M Krug
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
               MASK
               )
         )
}
