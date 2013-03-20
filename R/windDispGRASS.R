##' Disperses seeds from a seed \code{matrix} by using a
##' 2 dimensional dispersal \code{matrix}
##' and doing the calculations based on raster in GRASS
##'
##' @usage windDisp(SD2D, input, output="windDispSeeds", overwrite=FALSE)
##' @name windDispGRASS
##' @title Dispersal of seeds by wind
##' @param SD2D \code{matrix} defining the 2D seed dispersal kernel 
##' @param input \code{character} name of GRASS raster layer specifying number of seeds to be dispersed
##' @param output \code{character} name of GRASS raster layer generated, containing the dispersed seeds
##' @param zeroToNULL  \code{boolean} if TRUE convert zeros to NULL, otherwise NA to 0
##' @param overwrite \code{boolean} TRUE to overwrite existing output raster
##' @return \code{character} name of the output layer
##' @author Rainer M Krug \email{Rainer@@krugs.de}
##' @export 
##' @callGraphPrimitives
windDispGRASS <- function(SD2D, input, output="windDispSeeds", zeroToNULL=TRUE, overwrite=FALSE) {
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
  return(output)
}
