##' Locally disperse seeds from a seed layer using GRASS
##' 
##' @usage localDispGRASS(input, output, overwrite)
##' @name localDispGRASS
##' @title Dispersal of seeds locally, i.e. in surounding cells (half, evenly distributed)
##' @param input \code{character} name of GRASS raster layer specifying number of seeds to be dispersed
##' @param output \code{character} name of GRASS raster layer generated, containing the dispersed seeds
##' @param zeroToNULL \code{boolean} if TRUE convert zeros to NULL, otherwise NA to 0
##' @param overwrite \code{boolean} TRUE to overwrite existing output raster
##' @return \code{character} name of the output layer
##' @author Rainer M Krug \email{Rainer@@krugs.de}
##' @export 
##' @callGraphPrimitives
localDispGRASS <- function(input, output="localDispSeeds", zeroToNULL=TRUE, overwrite=FALSE) {
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
            parameters = list(
              map  = tmp,
              null = 0
              )
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
            parameter = list(
              rast = tmp
              )
            )
  ## if zeroToNULL
  if (zeroToNULL) {
    execGRASS(
              "r.null",
              parameters = list(
                map=output,
                setnull="0"
                ),
              ignore.stderr=!options("asmDebug")[[1]]
              )
  } else {
    execGRASS(
              "r.null",
              parameters = list(
                map=output,
                null=0
                ),
              ignore.stderr=!options("asmDebug")[[1]]
              )    
  }
  ## return name of output layer
  return(output)
}
