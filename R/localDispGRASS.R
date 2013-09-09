## [[file:~/Documents/Projects/AlienManagementDrakensberg/sim/packages/seedDisp/seedDisp.org::*localDispGRASS][localDispGRASS:1]]
##' Disperses seeds locally, i.e. in neighbouring cells, from an input raster layer in GRASS
##' and stores the dispersed seeds in an output layer in GRASS
##'
##' The seeds in the \code{input} layer are dispersed from each cell into the neighbouring layers following these
##' likelihoods:
##' 
#'' \preformatted{
#'' +------+------+------+
#'' | 1/16 | 1/16 | 1/16 |
#'' +------+------+------+
#'' | 1/16 | 8/16 | 1/16 |
#'' +------+------+------+
#'' | 1/16 | 1/16 | 1/16 |
#'' +------+------+------+
#'' }
##'
##' The resulting seed layer is saved and, if it exists and \code{overwrite==TRUE}, overwritten.
##'
##' The MASK in GRASS is respected.
##' 
##' @usage localDispGRASS(input, output = "localDispSeeds", zeroToNULL = TRUE, overwrite = FALSE)
##' @name localDispGRASS
##' @title Dispersal of seeds in neighbouring cells
##' @param input name of GRASS raster layer specifying number of seeds to be dispersed - \code{character}
##' @param output name of GRASS raster layer generated, containing the dispersed seeds - \code{character} 
##' @param zeroToNULL \code{boolean} if TRUE replace 0 with NA in the returned \code{matrix},
##' otherwise all NA will be replaced with 0
##' @param overwrite \code{boolean} if TRUE, \code{output} will be overwritten if it exists
##' 
##' @return invisibly \code{character} name of the output layer
##' @author Rainer M Krug \email{Rainer@@krugs.de}
##' @export 
localDispGRASS <- function(
    input,
    output = "localDispSeeds",
    zeroToNULL = TRUE,
    overwrite = FALSE
    ) {
    if ( length( execGRASS("g.mlist", type="rast", pattern=output, intern=TRUE) )  & !overwrite ) {
        stop(paste("Layer", output, "exists! Please specify 'overwrite=TRUE' or use different output name!"))
    } 
    r.mapcalc <- function(...)
        {
            comm <- paste( "r.mapcalc ", " \"", ..., "\" ", sep="" )
            system( comm, intern=TRUE )
        }
    ## temporary layer name
    tmp <- "TMP"
    ## calculate 16th of to be dispersed seeds and set nulls to 0
    r.mapcalc(
        tmp,
        " = ",
        "double( ", input, " / 16 )"
        ## 8/16 will remain in source cell,
        ## 8/16 will be evenly distributed in neighbouring cells
        )
    execGRASS(
        "r.null",
        map  = tmp,
        null = 0
        )
    ## Local Dispersal of all seeds in input
    r.mapcalc(
        output,
        " = ",
        "double( round(", 
        tmp, "[-1,-1] + ",
        tmp, "[-1, 0] + ",
        tmp, "[-1, 1] + ",
        tmp, "[ 0,-1] + ",
        " 8 * ", tmp, "[ 0, 0] + ",
        tmp, "[ 0, 1] + ",
        tmp, "[ 1,-1] + ",
        tmp, "[ 1, 0] + ",
        tmp, "[ 1, 1]",
        " ) )"
        )
    ## remove tmp
    execGRASS(
        cmd = "g.remove",
        rast = tmp
        )
    ## if zeroToNULL
    if (zeroToNULL) {
        execGRASS(
            "r.null",
            map=output,
            setnull="0"
            )
    } else {
        execGRASS(
            "r.null",
            map=output,
            null=0
            )    
    }
    ## return name of output layer
    invisible(output)
} 
## localDispGRASS:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
