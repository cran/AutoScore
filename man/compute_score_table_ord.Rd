% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoScore_Ordinal.R
\name{compute_score_table_ord}
\alias{compute_score_table_ord}
\title{Internal function: Compute scoring table for ordinal outcomes based on
  training dataset}
\usage{
compute_score_table_ord(train_set_2, max_score, variable_list, link)
}
\arguments{
\item{train_set_2}{Processed training set after variable transformation}

\item{max_score}{Maximum total score}

\item{variable_list}{List of included variables}

\item{link}{The link function used to model ordinal outcomes. Default is
\code{"logit"} for proportional odds model. Other options are
\code{"cloglog"} (proportional hazards model) and \code{"probit"}.}
}
\value{
A scoring table
}
\description{
Compute scoring table based on training dataset
}
