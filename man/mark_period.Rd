% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abpm_mark_phase.R
\name{mark_period}
\alias{mark_period}
\title{ABPM Periods}
\usage{
mark_period(x, period_value, impute_from = c("left", "right"))
}
\arguments{
\item{x}{(character) status values, e.g. \code{c('Awake', 'Asleep', 'Awake')}}

\item{period_value}{(character) what status values should constitute
a period? Status values not included in \code{period_value} will be
ignored.}

\item{impute_from}{(character) if 'left', then missing values in \code{x}
will be filled in using the nearest value to the left (i.e., earlier).
If 'right', then missing values are filled in using the nearest value
on the right (i.e., later). Missing values on the boundaries are always
filled in using the right if the first value is missing and the left
if the last value is missing.}
}
\value{
a numeric vector with values of 0, 1, 2, and so on.
A value of 0 indicates that the corresponding \code{x} value
was not in \code{period_value}. A value i > 0 indicates that the
corresponding \code{x} value was in the ith ABPM period.
}
\description{
During ABPM (ambulatory blood pressure monitoring), a person wears an
ABPM device for a number of hours - usually 24. In most cases,
this duration includes two periods where the person is awake and one
period where they are asleep. For example, I woke up on Monday at
7am, began wearing my ABPM device on Monday at 9am, went to bed on
Monday at 11pm, woke up on Tuesday at 7am, and took off my ABPM device
on Tuesday at 9am. My first awake period is from 9am to 11pm on Monday.
My second awake period is from 7am to 9am on Tuesday. My only asleep
period is from 11pm Monday to 7am Tuesday.
}
\details{
In practice, it is helpful to label these periods. This framework
makes it easier to compute blood pressure means and variability during
specific periods, and filter out periods that aren't part of the
intended 24-hour measurement. For example, suppose I had worn the
ABPM device for another day and on the second night I slept at different
times. If I had only given my sleep diary for the first night, then
my second night of ABPM data would be mislabeled! In this case,
it would be safer to include only my first night of blood pressure
readings.
}
\examples{
# count awake periods
x <- c("A","A","A","S","S",NA,"A","A")

mark_period(x, "A")
# count awake and asleep periods
x <- c("A","A","A","S","S",NA,"A","A")

# count awake periods, imputing from the right
x <- c("A","A","A","S","S",NA,"A","A")

mark_period(x, c("A","S"), impute_from = 'right')#'

mark_period(x, c("A","S"))

}
