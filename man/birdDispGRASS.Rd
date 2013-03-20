\name{birdDispGRASS}
\alias{birdDispGRASS}
\title{Dispersal of seeds by birds}
\usage{
  birdDispGRASS(input, output, overwrite)
}
\arguments{
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
  Bird disperse seeds from a seed layer using GRASS
}
\author{
  Rainer M Krug \email{Rainer@krugs.de}
}

