\name{windDisp}
\alias{windDisp}
\title{ Dispersal of seeds by wind }
\description{ Disperses seeds from a seed \code{matrix} by using a
  2 dimensional dispersal \code{matrix} }
\usage{ windDisp(DX2, DY2, SD2D, SEEDS, MASK) }
\arguments{
  \item{SD2D}{
    A \code{matrix} containing the 2 dimensional seed dispersal kernel
  }
  \item{SEEDS}{
    A \code{matrix} containing the seeds to be dispersed
  }
  \item{MASK}{
    A \code{matrix} containing a mask of seed cells to be processed 
    - 1  : processing
    - NA : do not process
  }
}
\details{
}
\value{
  A \code{matrix} of the same size as SEEDS containing the dispersed seeds 
}
\author{  Rainer M Krug <Rainer@krugs.de>  }
\examples{
}
\keyword{ manip }
