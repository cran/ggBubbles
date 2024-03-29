% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/position-surround.R
\name{position_surround}
\alias{position_surround}
\title{Surrounds exact overlapping points around the center}
\usage{
position_surround(offset = 0.1)
}
\arguments{
\item{offset}{setting offset for x and y axis added to the
  points surrounding the exact position.

  Default is 0.1}
}
\value{
ggproto
}
\description{
Bubble plots sometimes can be hard to interpret, especially if you want
to overlay an additional feature. Instead of having to colour one blob
with this function you can plot the individuals contributing to the bubble
and colour them accordingly.
}
\examples{
 library(ggplot2)
 library(ggBubbles)
 data(MusicianInterestsSmall)

 ggplot(data = MusicianInterestsSmall, aes(x = Instrument, y = Genre, col = Level)) +
        geom_point(position = position_surround(), size = 4) +
        scale_colour_manual(values = c("#333333", "#666666", "#999999", "#CCCCCC")) + theme_bw()
}
\concept{position adjustments}
