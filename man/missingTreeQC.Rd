% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aspen_qc.R
\name{missingTreeQC}
\alias{missingTreeQC}
\title{Check that there is at least one tree for every observation entry and flag entries with more than 250 trees}
\usage{
missingTreeQC(data = fetchAndWrangleAspen())
}
\arguments{
\item{data}{Aspen dataset to run through QC functions}
}
\description{
Check that there is at least one tree for every observation entry and flag entries with more than 250 trees
}
