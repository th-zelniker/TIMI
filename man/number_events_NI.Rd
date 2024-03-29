% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/number_events_NI.R
\name{number_events_NI}
\alias{number_events_NI}
\title{Calculate number of events for NI-trials}
\usage{
number_events_NI(alpha, beta, HR_effect, NI_margin, v = 0.5)
}
\arguments{
\item{alpha}{1-sided alpha (Type I error)}

\item{beta}{Type II error}

\item{HR_effect}{Assumed effect size}

\item{NI_margin}{Non-inferiority Margin}

\item{v}{Proportion of patients that are assigned to the intervention arm  (Default is  0.5)}
}
\value{
Number of events required
}
\description{
Based on Schoenfeld's formula
}
