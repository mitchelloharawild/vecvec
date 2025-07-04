% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vecvec.R
\name{unvecvec}
\alias{unvecvec}
\title{Convert a vecvec object into its underlying vector type}
\usage{
unvecvec(x, ..., ptype = NULL)
}
\arguments{
\item{x}{A vecvec to unvecvec (convert to its underlying vector type)}

\item{...}{These dots are for future extensions and must be empty.}

\item{ptype}{If \code{NULL}, the default, the output type is determined by
computing the common type across all elements of \code{x}. Alternatively, you
can supply \code{ptype} to give the output a known type.}
}
\value{
A simple vector, all containing the same type of data.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
