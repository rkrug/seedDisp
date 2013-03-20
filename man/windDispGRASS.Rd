\name{windDispGRASS}
\alias{windDispGRASS}
\title{Dispersal of seeds by wind}
\usage{
  windDisp(SD2D, input, output="windDispSeeds",
  overwrite=FALSE)
}
\arguments{
  \item{SD2D}{\code{matrix} defining the 2D seed dispersal
  kernel}

  \item{input}{\code{character} name of GRASS raster layer
  specifying number of seeds to be dispersed}

  \item{output}{\code{character} name of GRASS raster layer
  generated, containing the dispersed seeds}

  \item{overwrite}{\code{boolean} TRUE to overwrite
  existing output raster}
}
\value{
  \code{character} name of the output layer
}
\description{
  Disperses seeds from a seed \code{matrix} by using a 2
  dimensional dispersal \code{matrix} and doing the
  calculations based on raster in GRASS
}
\author{
  Rainer M Krug \email{Rainer@krugs.de}
}

