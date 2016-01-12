## [[file:../seedDisp.org::*birdDispGRASS][birdDispGRASS:1]]
##' Seed dispersal by birds from a seed layer using GRASS
##'
##' This is a "dumb" implementation of sedd dispersal by birds, simply randomly distributing
##' all seeds in the output raster.
##' 
##' The resulting seed layer is saved and, if it exists and \code{overwrite==TRUE}, overwritten.
##'
##' The MASK in GRASS is respected.
##' 
##' @usage birdDispGRASS(input, output, zeroToNULL, overwrite)
##' @name birdDispGRASS
##' @title Dispersal of seeds by birds
##' 
##' @param input name of GRASS raster layer specifying number of seeds to be dispersed - \code{character}
##' @param output name of GRASS raster layer generated, containing the dispersed seeds - \code{character} 
##' @param zeroToNULL \code{boolean} if TRUE replace 0 with NA in the returned \code{matrix}, otherwise all NA will be replaced with 0
##' @param overwrite \code{boolean} if TRUE, \code{output} will be overwritten if it exists
##' 
##' @return invisibly \code{character} name of the output layer
##' @author Rainer M Krug \email{Rainer@@krugs.de}
##' @export 
birdDispGRASS <- function(
    input,
    output = "birdDispSeeds",
    zeroToNULL = TRUE,
    overwrite = FALSE
    ) {
    if ( length( execGRASS("g.list", type="rast", pattern=output, intern=TRUE) )  & !overwrite ) {
        stop(paste("Layer", output, "exists! Please specify 'overwrite=TRUE' or use different output name!"))
    } 
    MASK <- "MASK"
    seeds <- readRAST(
        c(
            input,
            MASK
            ),
        NODATA=-1
        )
    oldWarn <- options()$warn
    options(warn=-1)
    seeds[[3]] <- 0
    seeds[[3]][!is.na(seeds[[MASK]])] <- rmultinom(
        n = 1,
        size = sum(seeds[[input]], na.rm=TRUE),
        prob = rep(1, length.out=sum(!is.na(seeds[[MASK]])))
        )
    ## seeds[[3]][!is.na(seeds[[MASK]])] <- rbinom(                                     # Bird dispersal
    ##                                             cells <- sum(!is.na(seeds[[MASK]])), # into all cells which are not NULL in the region
    ##                                             sum(seeds[[input]], na.rm=TRUE),     # seeds to disperse
    ##                                             1/cells                              # probability is the same for each cell
    ## )
    options(warn=oldWarn)

    if (zeroToNULL) {
        seeds[[3]][seeds[[3]]==0] <- NA
    } else {
        seeds[[3]][is.na(seeds[[3]])] <- 0
    }
    writeRAST(
        seeds,
        output,
        NODATA = -1,
        zcol=3,
        overwrite = TRUE
        )
    ## return name of output layer
    invisible(output)
} 
## birdDispGRASS:1 ends here

## Local Variables:
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
