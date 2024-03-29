% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_offset.R
\name{calc_offset}
\alias{calc_offset}
\title{Calculate offsets for a specific point, in a layer, position}
\usage{
calc_offset(position, layer, side, offset_x = 0.1, offset_y = 0.1)
}
\arguments{
\item{position}{number for position at the particular side on the layer}

\item{layer}{number of layer}

\item{side}{side for offset
1 - top
2 - right
3 - bottom
4 - left}

\item{offset_x}{offset for x axis}

\item{offset_y}{offset for y axis}
}
\value{
integer vector of length 2
position 1 is new x value,
position y is new y value
}
\description{
each side has several layers, with a number of positions in the layer
}
