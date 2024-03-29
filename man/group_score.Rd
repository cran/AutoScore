% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoScore_Ordinal.R
\name{group_score}
\alias{group_score}
\title{Internal function: Group scores based on given score breaks, and use friendly
names for first and last intervals.}
\usage{
group_score(score, max_score, score_breaks)
}
\arguments{
\item{score}{numeric vector of scores.}

\item{max_score}{Maximum attainable value of final scores.}

\item{score_breaks}{A vector of score breaks to group scores. The average
predicted risk will be reported for each score interval in the lookup
table. Users are advised to first visualise the predicted risk for all
attainable scores to determine \code{scores} (see
\code{\link{plot_predicted_risk}})}
}
\description{
Internal function: Group scores based on given score breaks, and use friendly
names for first and last intervals.
}
