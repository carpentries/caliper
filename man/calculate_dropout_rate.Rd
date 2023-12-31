% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/instructor_training.R
\name{calculate_dropout_rate}
\alias{calculate_dropout_rate}
\title{Calculate Instructor Checkout Dropout Rate}
\usage{
calculate_dropout_rate(trainee_progress, days_threshold = 90)
}
\arguments{
\item{trainee_progress}{data.frame
A data frame containing progress indicators for each trainee.}

\item{days_threshold}{integer
The number of days since training to consider. Default is 90 days.}
}
\value{
numeric
Returns the dropout rate as a percentage.
}
\description{
This function calculates the dropout rate among trainees who have started the checkout process within a specified number of days since their training.
}
\examples{
\dontrun{
  raw_data <- data.frame(...)
  dropout_rate <- calculate_dropout_rate(raw_data, 90)
}
}
