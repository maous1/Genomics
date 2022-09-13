#' Title
#'
#' @param prefix
#'
#' @import reticulate
#' @import dplyr
#' @return
#' @export
#' @examples
create_envconda <- function(prefix)
{

  conda_create(envname = paste0("./",prefix))
  conda_install(envname = paste0("./",prefix),packages = "conda")
  conda_install(envname = paste0("./",prefix),packages = "blast",channel="bioconda",version="2.13.0")
  conda_install(envname = paste0("./",prefix),packages = "ncbi-amrfinderplus",forge = T,channel = "bioconda")
  system2(command = paste0(prefix,"/bin/amrfinder"),args = "-u")
}
