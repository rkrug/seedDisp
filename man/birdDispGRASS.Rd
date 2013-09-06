\name{birdDispGRASS}
\alias{birdDispGRASS}
\title{Dispersal of seeds by birds}
\usage{
  birdDispGRASS(input, output, zeroToNull, overwrite)
}
\arguments{
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
  Seed dispersal by birds from a seed layer using GRASS
}
\details{
  This is a "dumb" implementation of sedd dispersal by
  birds, simply randomly distributing all seeds in the
  output raster.

  The resulting seed layer is saved and, if it exists and
  \code{overwrite==TRUE}, overwritten.

  The MASK in GRASS is respected.
}
\author{
  Rainer M Krug \email{Rainer@krugs.de}
}

