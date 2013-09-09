## [[file:~/Documents/Projects/AlienManagementDrakensberg/sim/packages/seedDisp/seedDisp.org::*windDisp][windDisp:1]]
##' Disperses seeds from a matrix by using a 2 dimensional dispersal kernel
##'
##' This function uses a 2D seed dispersal kernel in form of a \code{matrix} to disperse seeds
##' from a seed source in form of a \code{matrix} and returns a \code{matrix} of the same size
##' containing the seed distribnution after the seeds in the original seed matrix are dispersed.
##' 
##' The function uses an inverse approach, by going over all cells and determining the number of
##' seeds dispersed \bold{into} the cell from all the cells under the dispersel kernel.
##' To calculate the number of seeds dispersed into each cell, the foolowing steps are done:
##' 
##' 1) The dispersal kernel is centered over the for which the number of seeds to be dispersed into
##' will be calculated
##' 
##' 2) for each cell under the dispersal kernel \code{SD2D} the following is done
##' 
##' 2.1) if the corresponding cell in MASK is \code{NA}, no seeds are dispersed into this cell
##' 
##' 2.2) for each cell under the kernel \code{SD2D} for which the underlying MASK is not NA a binominal distributed random number is drawn with
##' 
##'         \code{rbinom(1, noOfSeeds, prob)} \cr
##'         \code{noOfSeeds} : number of seeds in the cell from which the seeds orriginate \cr
##'         \code{prob} : probability from \code{SD2D} kernel of the corresponding cell
##' 
##'      and summed up for the whole kernel.
##' 
##' This function is implemented in C (\code{windDispCpp})
##'
##' The C function requires the seed raster to be buffered by half
##' the width of the seed dispertsal kernel \code{SD2D} which is done
##' in the function before calling the C function.
##' 
##' 
##' @usage windDisp(SD2D, SEEDS, MASK, zeroToNULL)
##' @name windDisp
##' @title Dispersal of seeds by wind
##' @param SD2D Sedd Dispersal kernel 2D - \code{matrix} defining the 2D seed dispersal kernel 
##' @param SEEDS \code{matrix} specifying the number of seeds to be dispersed
##' @param MASK \code{matrix} defining the area in which processing takes place (\code{!is.na(MASK)}) 
##' @param zeroToNULL \code{boolean} if TRUE replace 0 with NA in the returned \code{matrix}, 
##' otherwise all NA will be replaced with 0
##' 
##' @return \code{matrix} of same size as \code{SEEDS} containing the dispersed seeds
##' @author Rainer M Krug \email{Rainer@@krugs.de}
##' 
##' @useDynLib seedDisp
##' @export 

windDisp <- function(SD2D, SEEDS, MASK, zeroToNULL) {
    ## Calculate size parameter of sd2D
    dx2 <- (ncol(SD2D) - 1)
    dy2 <- (nrow(SD2D) - 1)
    dx <- dx2 / 2
    dy <- dy2 / 2
    ## buffer MASK and SEEDS for dispersal into cells at the edge
    buffer <- matrix(NA, nrow=nrow(SEEDS), ncol=dx)
    SEEDS <- cbind(buffer, SEEDS, buffer)
    MASK <- cbind(buffer, MASK, buffer)
    buffer <- matrix(NA, ncol=ncol(SEEDS), nrow=dy)
    SEEDS <- rbind(buffer, SEEDS, buffer)
    MASK <- rbind(buffer, MASK, buffer)
    ## call C++ function
    output <- .Call(
        "windDispCpp",
        dx2,
        dy2,
        SD2D,
        SEEDS,
        MASK,
        PACKAGE = "seedDisp"
        )
    if (zeroToNULL) {
        output[output==0] <- NA
    } else {
        output[is.na(output)] <- 0
    }
    return(output)
}
## windDisp:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
