\name{waterDispGRASS}
\alias{waterDispGRASS}
\title{Dispersal of seeds by water}
\usage{
  waterDispGRASS(input, output="waterDispSeeds",
  slope="SLOPE", flowdir="FLOWDIR", overwrite=FALSE)
}
\arguments{
  \item{input}{\code{character} name of GRASS raster layer
  specifying number of seeds to be dispersed}

  \item{output}{\code{character} name of GRASS raster layer
  generated, containing the dispersed seeds}

  \item{slope}{\code{character} name of GRASS raster layer
  containing the slope in degrees}

  \item{flowdir}{\code{character} name of GRASS raster
  containing flow direction (in GRASS agnps format)}

  \item{depRates}{\code{matrix} where the first column are
  slope and the second column the responding deposit rates}

  \item{overwrite}{\code{boolean} TRUE to overwrite
  existing output raster}
}
\value{
  \code{character} name of the output layer
}
\description{
  Water disperse seeds from a seed layer using GRASS
}
\author{
  Rainer M Krug \email{Rainer@krugs.de}
}

