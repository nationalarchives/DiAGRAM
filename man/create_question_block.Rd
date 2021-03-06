% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/questions.R
\name{create_question_block}
\alias{create_question_block}
\title{create question block}
\usage{
create_question_block(questions, default_response = NA, ns)
}
\arguments{
\item{questions}{A named list of question data}

\item{default_response}{A named list of default response values for the questions}

\item{ns}{A namespacing object, typically created by shiny::NS}
}
\value{
a list of all question data
}
\description{
Given a list of questions and associate default responses, create all
lists of question components
}
