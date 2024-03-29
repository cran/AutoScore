% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common.R
\name{add_baseline}
\alias{add_baseline}
\title{Internal Function: Add baselines after second-step logistic regression (part of AutoScore Module 3)}
\usage{
add_baseline(df, coef_vec)
}
\arguments{
\item{df}{A \code{data.frame} used for logistic regression}

\item{coef_vec}{Generated from logistic regression}
}
\value{
Processed \code{vector} for generating the scoring table
}
\description{
Internal Function: Add baselines after second-step logistic regression (part of AutoScore Module 3)
}
