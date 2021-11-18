.onAttach <- function(libname, pkgname){

packageStartupMessage(
  "Columns 'Prelims_Time' and 'Finals_Time' have been changed to 'Prelims' and 'Finals' respectively.  Please update your workflows."
)
}
