##' Bird disperse seeds from a seed layer using GRASS
##' 
##' @usage birdDispGRASS(input, output, overwrite)
##' @name birdDispGRASS
##' @title Dispersal of seeds by birds
##' 
##' @param input \code{character} name of GRASS raster layer specifying number of seeds to be dispersed
##' @param output \code{character} name of GRASS raster layer generated, containing the dispersed seeds
##' @param zeroToNULL \code{boolean} if TRUE convert zeros to NULL, otherwise NA to 0
##' @param overwrite \code{boolean} TRUE to overwrite existing output raster
##' @return \code{character} name of the output layer
##' @author Rainer M Krug \email{Rainer@@krugs.de}
##' @export 
##' @callGraphPrimitives
birdDispGRASS <- function(input, output="birdDispSeeds", zeroToNULL=TRUE, overwrite=FALSE) {
  if ( length( execGRASS("g.mlist", type="rast", pattern=output, intern=TRUE) )  & !overwrite ) {
    stop(paste("Layer", output, "exists! Please specify 'overwrite=TRUE' or use different output name!"))
  } 
  MASK <- "MASK"
  seeds <- readRAST6(
                     c(
                       input,
                       MASK
                       ),
                     NODATA=-1
                     )
  oldWarn <- options()$warn
  options(warn=-1)
  seeds[[3]] <- 0
  seeds[[3]][!is.na(seeds[[MASK]])] <- rbinom(                                     # Bird dispersal
                                              cells <- sum(!is.na(seeds[[MASK]])), # into all cells which are not NULL in the region
                                              sum(seeds[[input]], na.rm=TRUE),     # seeds to disperse
                                              1/cells                              # probability is the same for each cell
                                              )
  options(warn=oldWarn)

  if (zeroToNULL) {
    seeds[[3]][seeds[[3]]==0] <- NA
  } else {
    seeds[[3]][is.na(seeds[[3]])] <- 0
  }
  writeRAST6(
             seeds,
             output,
             NODATA = -1,
             zcol=3,
             overwrite = overwrite
             )
  ## return name of output layer
  return(output)
}
