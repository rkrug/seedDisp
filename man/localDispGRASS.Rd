\name{localDispGRASS}
\alias{localDispGRASS}
\title{Dispersal of seeds in neighbouring cells}
\usage{
  localDispGRASS(input, output = "localDispSeeds",
    zeroToNULL = TRUE, overwrite = FALSE)
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
  Disperses seeds locally, i.e. in neighbouring cells, from
  an input raster layer in GRASS and stores the dispersed
  seeds in an output layer in GRASS
}
\details{
  The seeds in the \code{input} layer are dispersed from
  each cell into the neighbouring layers following these
  likelihoods:

  ' \preformatted{ ' +------+------+------+ ' | 1/16 | 1/16
  | 1/16 | ' +------+------+------+ ' | 1/16 | 8/16 | 1/16
  | ' +------+------+------+ ' | 1/16 | 1/16 | 1/16 | '
  +------+------+------+ ' }

  The resulting seed layer is saved and, if it exists and
  \code{overwrite==TRUE}, overwritten.

  The MASK in GRASS is respected.
}
\author{
  Rainer M Krug \email{Rainer@krugs.de}
}

