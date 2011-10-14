##' Water disperse seeds from a seed layer using GRASS
##' 
##' @usage waterDispGRASS(input, output="waterDispSeeds", slope="SLOPE", flowdir="FLOWDIR", overwrite=FALSE)
##' @name waterDispGRASS
##' @title Dispersal of seeds by water
##' 
##' @param input \code{character} name of GRASS raster layer specifying number of seeds to be dispersed
##' @param output \code{character} name of GRASS raster layer generated, containing the dispersed seeds
##' @param slope \code{character} name of GRASS raster layer containing the slope in degrees
##' @param flowdir \code{character} name of GRASS raster containing flow direction (in GRASS agnps format)
##' @param depRates \code{matrix} where the first column are slope and the second column the responding deposit rates
##' @param overwrite \code{boolean} TRUE to overwrite existing output raster
##' @param zeroToNULL \code{boolean} if TRUE convert zeros to NULL, otherwise NA to 0
##' @param progress \code{boolean} if TRUE, showing progress, otherwise silent
##' @return \code{character} name of the output layer
##' @author Rainer M Krug \email{Rainer@@krugs.de}
##' @export 
##' @callGraphPrimitives
waterDispGRASS <- function(input, output="waterDispSeeds", slope="slope", flowdir="flowdir", depRates, overwrite=FALSE, zeroToNULL=TRUE, progress=TRUE) {
  if ( length( execGRASS("g.mlist", type="rast", pattern=output, intern=TRUE) )  & !overwrite ) {
    stop(paste("Layer", output, "exists! Please specify 'overwrite=TRUE' or use different output name!"))
  } 
#######################
  names(depRates) <- c("depDegrees", "depRates")
  ## Load seeds layer
  seeds <- readRAST6(
                     input,
                     NODATA = -1
                     )
  seeds.m <- matrix(
                    seeds[[1]],
                    nrow=gridparameters(seeds)$cells.dim[1],
                    ncol=gridparameters(seeds)$cells.dim[2]
                    )
  ## Calculate flow and deposit parameter
  ## read topo laywers
  slope <- readRAST6(
                     slope,
                     NODATA = -1
                     )
  slope.m <- matrix(
                    slope[[1]],
                    nrow=gridparameters(slope)$cells.dim[1],
                    ncol=gridparameters(slope)$cells.dim[2]
                    )
  ##
  flowdir <- readRAST6(
                       flowdir,
                       NODATA = -1
                       )
  
  flowdir.m <- matrix(
                      flowdir[[1]],
                      nrow=gridparameters(flowdir)$cells.dim[1],
                      ncol=gridparameters(flowdir)$cells.dim[2]
                      )
  ## calculate deposit rates (depRate)
  depRate <- slope
  depRate[[1]] <- cut(
                      x      = slope[[1]],
                      breaks = depRates$depDegrees,
                      labels = FALSE
                      )
  depRate[[1]] <- depRates$depRate[depRate[[1]]]
  depRate.m <- matrix(
                      depRate[[1]],
                      nrow=gridparameters(depRate)$cells.dim[1],
                      ncol=gridparameters(depRate)$cells.dim[2]
                      )
  ## Create output layer (seeds$output)
  seeds$output <- NA
  FLOW <- seeds.m * NA
  seeds@data[[1]] <- as.vector(
                               waterDisp(
                                         input = seeds.m,
                                         depRate = depRate.m,
                                         agnps = flowdir.m,
                                         zeroToNULL = zeroToNULL,
                                         progress = TRUE)
                               )
  
  mode(seeds[[1]]) <- "double"
  ## seeds@proj4string <- parameter$proj4string
  writeRAST6(
             seeds,
             output,
             NODATA = -1,
             zcol=1,
             overwrite = overwrite
             )
  return(output)
}
