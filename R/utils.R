#' Produces a list from string variables and/or strings suitable for use in join expressions
#' BEWARE function also specified in omop_es
#' @param lhs left hand side of join expression
#' @param rhs right hand side of join expression
#' @export
#' @examples
#' df1 <- data.frame(concept_id=999L,concept_name="good drug")
#' df2 <- data.frame(drug_concept_id=999L,patient_id=666L)
#' name2 <- "drug_concept_id"
#' df3 <- df1 |> dplyr::left_join(df2,by=caboodler::dynamic_by("concept_id",name2))
dynamic_by <- function(lhs,rhs) {
  res <- c()
  res[lhs] <- rhs
  res
}
