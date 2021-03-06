% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nexminer.R
\name{lm_item_wday}
\alias{lm_item_wday}
\title{explore linear relation between price and weekday}
\usage{
lm_item_wday(item_clean)
}
\arguments{
\item{item_clean}{raw json item of api request - generated with import_item function}
}
\value{
tie-fighter plot of item value compared to the weekday
}
\description{
explore linear relation between price and weekday
}
\examples{

bl_raw <- import_item("black lotus", "patchwerk", "horde")
bl_clean <- clean_json(bl_raw)
lm_item_wday(bl_clean)

}
