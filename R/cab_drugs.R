#' get drugs records from different places in caboode
#'
#' @param con database connection to caboodle
#' @param tablefrom 'orders', 'events', 'components'
#' @param patients 'in', 'out', 'both'
#' @param dayfrom optional 20230601 #caboodle date format
#' @param dayto optional 20230602
#' @param name_part e.g. 'POSACONAZOLE'
#' @param mrns optional patient mrns
#' @param collect whether to collect results from database at end
cab_drugs <- function(con,
                      tablefrom = "events",
                      patients = "out",
                      dayfrom = NULL,
                      dayto = NULL,
                      name_part = 'POSACONAZOLE',
                      mrns = NULL,
                      collect = TRUE) {

  if (tablefrom == "events")
  {
    drugs <- cab_drugs_events(con,
                              patients = patients,
                              dayfrom = dayfrom,
                              dayto = dayto,
                              name_part = name_part,
                              mrns = mrns)
  }


  return(drugs)
}

#' get drugs records from events table in caboode
#'
#' @param con connection to caboodle (or maybe clarity too later)
#' @param patients 'in', 'out', 'both'
#' @param dayfrom optional 20230601 #caboodle date format
#' @param dayto optional 20230602
#' @param name_part e.g. 'POSACONAZOLE'
#' @param mrns optional patient mrns
#' @param collect whether to collect results from database at end
#' @examples
#' # example code
cab_drugs_events <- function(con,
                             patients = "out",
                             dayfrom = NULL,
                             dayto = NULL,
                             name_part = 'POSACONAZOLE',
                             mrns = NULL,
                             collect = TRUE) {

  mef <- tbl(con, "MedicationEventFact")
  md <- tbl(con, "MedicationDim")

  #joing to MedicationDim for med names etc
  mef_md <- mef |>
    #filter(PatientDurableKey > 0) |>
    left_join(md, by = "MedicationKey")

  #select important column names to keep clear
  #TODO could make this optional
  mef_md_sel <- mef_md |>

    filter(PatientDurableKey > 0) |>

    select(StartDateKey,EndDateKey,
           PatientDurableKey,
           SourceKey,Mode,Type,
           Quantity,MinimumDose,MaximumDose,
           #from MedicationDim
           Name,GenericName,SimpleGenericName,TherapeuticClass,
           PharmaceuticalClass,PharmaceuticalSubclass)

  drugs_final <- mef_md_sel |>
    #TODO add if !NULL
    filter(StartDateKey >= dayfrom & StartDateKey <= dayto)  |>
    collect() |>
    #TODO add if !NULL
    #grepl failing I think due to name_part being object rather than a string
    #filter(grepl(name_part,Name,ignore.case=TRUE)) |>
    #have to collect before str_detect "not available in this SQL variant"
    filter(stringr::str_detect(stringr::str_to_lower(Name),stringr::str_to_lower(name_part))) |>
    cab_mrn(con = con, name_dk = "PatientDurableKey") |>
    filter(PrimaryMrn %in% mrns)

}
