\name{windDisp}
\alias{windDisp}
\title{Dispersal of seeds by wind}
\usage{windDisp(SD2D, SEEDS, MASK)}
\description{Disperses seeds from a seed \code{matrix} by using a
2 dimensional dispersal \code{matrix}}
\details{A \code{matrix} of the same size as SEEDS containing the dispersed seeds}
\value{\code{matrix} of same size as SEEDS containing the dispersed seeds}
\author{Rainer M Krug \email{Rainer@krugs.de}}
\arguments{\item{SD2D}{\code{matrix} defining the 2D seed dispersal kernel}
\item{SEEDS}{\code{matrix} specifying the number of seeds to be dispersed}
\item{MASK}{\code{matrix} defining the area in which processing takes place (\code{!is.na(MASK)})}}
