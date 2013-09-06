\name{windDispGRASS}
\alias{windDispGRASS}
\title{Dispersal of seeds by wind}
\usage{
  windDisp(SD2D, input, output="windDispSeeds",
    overwrite=FALSE)
}
\arguments{
  \item{SD2D}{Sedd Dispersal kernel 2D - \code{matrix}
  defining the 2D seed dispersal kernel}

  \item{input}{name of GRASS raster layer specifying number
  of seeds to be dispersed - \code{character}}

  \item{output}{name of GRASS raster layer generated,
  containing the dispersed seeds - \code{character}}

  \item{zeroToNULL}{\code{boolean} if TRUE replace 0 with
  NA in the returned \code{matrix}, otherwise all NA will
  be replaced with 0}

  \item{overwrite}{\code{boolean} if TRUE, \code{output}
  will be overwritten if it exists}
}
\value{
  invisibly \code{character} name of the output layer
}
\description{
  Disperses seeds from a input raster layer in GRASS by
  using a 2 dimensional dispersal kernel \code{matrix} and
  stores the dispersed seeds in an output layer in RASS
}
\details{
  This function is a wrapper around \code{windDisp} to
  facilitate the usage of GRASS as a backend. It retrieves
  the raster from GRASS and stores the resulting layer in
  GRASS again.

  The MASK in GRASS is respected.
}
\author{
  Rainer M Krug \email{Rainer@krugs.de}
}

