% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/amber.forms.R
\name{amber.form_revision}
\alias{amber.form_revision}
\title{Get a form revision}
\usage{
amber.form_revision(amber, form, revision, study = NULL)
}
\arguments{
\item{amber}{A Amber object}

\item{form}{Form's name or identifier}

\item{revision}{Revision number}

\item{study}{Study identifier (name or id), optional.}
}
\description{
Get a form revision by its form name or identifier and its revision number.
}
\examples{
\dontrun{
a <- amber.login("https://amber-demo.obiba.org")
amber.form_revision(a, form = "Adult trauma", revision = 10)
amber.form_revision(a, form = "61e69a22fea2df2f3108b508", revision = 10)
amber.logout(a)
}
}
\seealso{
Other studies functions: 
\code{\link{amber.case_report_export}()},
\code{\link{amber.case_report_forms}()},
\code{\link{amber.case_report_form}()},
\code{\link{amber.case_reports}()},
\code{\link{amber.form_revisions}()},
\code{\link{amber.forms}()},
\code{\link{amber.form}()},
\code{\link{amber.studies}()},
\code{\link{amber.study}()}
}
\concept{studies functions}
