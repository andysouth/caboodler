#' join PrimaryMrn from Caboodle onto passed dataframe
#'
#' @param df dataframe containing a durablekey column
#' @param name_dk optionally specify durable key name if it is not "DurableKey"
#' @param name_mrn optionally specify mrn column name in output if not "PrimaryMrn"
#' @examples
#' #cab_mrn(con,name_dk="PatientDurableKey,name_mrn="PatientMrn")
cab_mrn <- function(df,con,
                     name_dk = "DurableKey",
                     name_mrn = "PrimaryMrn") {

  pd <- tbl(con,"PatientDim")  |>
    #deliberate mrn first so appears that way in output
    distinct(PrimaryMrn,DurableKey) |>
    right_join(df, by = dynamic_by("DurableKey",name_dk), copy=TRUE) |>
    rename_with(~name_dk, DurableKey) |>
    rename_with(~name_mrn, PrimaryMrn) |>
    collect()
}
