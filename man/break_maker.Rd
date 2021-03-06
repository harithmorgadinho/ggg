% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/break_maker.R
\name{break_maker}
\alias{break_maker}
\title{break_maker}
\usage{
break_maker(input, exclude_0 = NULL, round_to = 0, break_n = 7)
}
\arguments{
\item{input}{raster}

\item{exclude_0}{in case you want 0 values to be treated as NA}

\item{round_to}{rounding breaks to}

\item{break_n}{number of breaks}
}
\description{
A function to make breaks for plotting.
}
\examples{
crop_map_world(df)
crop_map_world(df,crs='longlat')
}
