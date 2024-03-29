% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_serialisers.R
\name{serialise_pdf}
\alias{serialise_pdf}
\title{Serialise PDF document to pass as response to API Gateway}
\usage{
serialise_pdf(body, status_code = 200L, headers = list())
}
\arguments{
\item{body}{The content to be serialised}

\item{status_code}{The HTTP response status code to return}

\item{headers}{Additional content headers to append}
}
\description{
Serialise PDF document to pass as response to API Gateway
}
