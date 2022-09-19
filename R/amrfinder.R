#' Title
#'
#' @param path_fasta
#' @param organism
#' @param path_amrfinder
#' @param element
#'
#' @param path_database
#'
#' @import dplyr
#' @import tidyr
#' @return
#' @export amrfinder
#'
#' @examples
amrfinder <- function(path_fasta,organism,path_amrfinder,element,path_database){
  arg = paste0("-n ",path_fasta," -O ", organism," -d ",path_database ," > amrfinder.csv")
  system2(command = path_amrfinder,args = arg)
  amr = read.csv(file = "amrfinder.csv",sep = '\t',header = T)
  amr = amr %>%
    select(Gene.symbol,Element.subtype,Class) %>%
    filter(Element.subtype ==element) %>%
    mutate(name = paste0(Class,"_", Gene.symbol))

  amr_vector = rep(1,length(amr$name))
  names(amr_vector) = amr$name
  file.remove("amrfinder.csv")
  return(amr_vector)
}
