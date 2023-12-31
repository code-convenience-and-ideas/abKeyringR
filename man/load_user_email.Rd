% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/special_entries.R
\name{load_user_email}
\alias{load_user_email}
\title{Infers the user email via a user supplied method and}
\usage{
load_user_email(email_format, method_to_infer_name)
}
\arguments{
\item{email_format}{a string specifying the required email address format}

\item{method_to_infer_name}{a function to infer email address start}
}
\value{
a user-specific email to use in keyring entries
}
\description{
Infers the user email via a user supplied method and
}
\examples{
email_start_inference <- function(x) {
  "hello"
}
specified_email_format <- "first_name.last_name@my_domain.com"
load_user_email(specified_email_format, email_start_inference)
}
