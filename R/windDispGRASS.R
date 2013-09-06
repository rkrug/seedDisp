## [[file:~/Documents/Projects/AlienManagementDrakensberg/sim/packages/seedDisp/seedDisp.org::*windDispGRASS][windDispGRASS:1]]
##' Disperses seeds from a input raster layer in GRASS by using a 2 dimensional dispersal kernel \code{matrix}
##' and stores the dispersed seeds in an output layer in RASS
##'
##' This function is a wrapper around \code{windDisp} to facilitate the usage of GRASS as a backend.
##' It retrieves the raster from GRASS and stores the resulting layer in GRASS again.
##'
##' The MASK in GRASS is respected.
##' 
##' @usage windDisp(SD2D, input, output="windDispSeeds", overwrite=FALSE)
##' @name windDispGRASS
##' @title Dispersal of seeds by wind
##' @param SD2D Sedd Dispersal kernel 2D - \code{matrix} defining the 2D seed dispersal kernel 
##' @param input name of GRASS raster layer specifying number of seeds to be dispersed - \code{character} 
##' @param output name of GRASS raster layer generated, containing the dispersed seeds - \code{character} 
##' @param zeroToNULL \code{boolean} if TRUE replace 0 with NA in the returned \code{matrix},
##' otherwise all NA will be replaced with 0
##' @param overwrite \code{boolean} if TRUE, \code{output} will be overwritten if it exists
##' 
##' @return invisibly \code{character} name of the output layer
##' @author Rainer M Krug \email{Rainer@@krugs.de}
##' @export 
windDispGRASS <- function(
    SD2D,
    input,
    output = "windDispSeeds",
    zeroToNULL = TRUE,
    overwrite = FALSE
    ) {
    if ( length( execGRASS("g.mlist", type="rast", pattern=output, intern=TRUE) ) & !overwrite ) {
        stop(paste("Layer", output, "exists! Please specify 'overwrite=TRUE' or use different output name!"))
    } 
    ##
    seeds <- readRAST6(
        c(
            input,
            "MASK"
            ),
        NODATA=-1
        )
    ## seeds.m <- sgdfToMatrix(seeds, 1)
    seeds.m <- matrix(
        seeds[[1]],
        nrow=gridparameters(seeds)$cells.dim[1],
        ncol=gridparameters(seeds)$cells.dim[2])
    ## mask.m <- sgdfToMatrix(seeds, 2)
    mask.m <- matrix(
        seeds[[2]],
        nrow=gridparameters(seeds)$cells.dim[1],
        ncol=gridparameters(seeds)$cells.dim[2])
    
    dispSeeds.m <- windDisp(
        SD2D = SD2D,
        SEEDS = seeds.m,
        MASK = mask.m,
        zeroToNULL = zeroToNULL
        )
    
    seeds@data[[2]] <- as.vector(dispSeeds.m)
    
    mode(seeds[[2]]) <- "double"
    ## seeds@proj4string <- parameter$proj4string
    writeRAST6(
        seeds,
        output,
        NODATA = -1,
        zcol=2,
        overwrite = overwrite
        )
    invisible(output)
}
## windDispGRASS:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
