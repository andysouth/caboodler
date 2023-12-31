% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{dynamic_by}
\alias{dynamic_by}
\title{Produces a list from string variables and/or strings suitable for use in join expressions
BEWARE function also specified in omop_es}
\usage{
dynamic_by(lhs, rhs)
}
\arguments{
\item{lhs}{left hand side of join expression}

\item{rhs}{right hand side of join expression}
}
\description{
Produces a list from string variables and/or strings suitable for use in join expressions
BEWARE function also specified in omop_es
}
\examples{
df1 <- data.frame(concept_id=999L,concept_name="good drug")
df2 <- data.frame(drug_concept_id=999L,patient_id=666L)
name2 <- "drug_concept_id"
df3 <- df1 |> dplyr::left_join(df2,by=caboodler::dynamic_by("concept_id",name2))
}
