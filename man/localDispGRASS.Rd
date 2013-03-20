\name{localDispGRASS}
\alias{localDispGRASS}
\title{Dispersal of seeds locally, i.e. in surounding cells (half, evenly distributed)}
\usage{
  localDispGRASS(input, output, overwrite)
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
  Locally disperse seeds from a seed layer using GRASS
}
\author{
  Rainer M Krug \email{Rainer@krugs.de}
}

