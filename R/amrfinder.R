#' Title
#'
#' @param path_fasta
#' @param organism
#' @param path_amrfinder
#' @param element
#'
#'
#' @import dplyr
#' @import tidyr
#' @return
#' @export amrfinder
#'
#' @examples
amrfinder <- function(path_fasta,organism,path_amrfinder,element){
  arg = paste0("-n ",path_fasta," -O ", organism," > amrfinder.csv")
  system2(command = path_amrfinder,args = arg)
  amr = read.csv(file = "amrfinder.csv",sep = '\t',header = T)
  amr = amr %>%
    select(Gene.symbol,Element.subtype,Class) %>%
    filter(Element.subtype ==element) %>%
    mutate(name = paste0(Class," ", Gene.symbol)) %>%
    mutate(value = 1) %>%select(name,value)%>%
    pivot_wider(names_from = name,values_from = value)
  file.remove("amrfinder.csv")
  return(amr)
}


