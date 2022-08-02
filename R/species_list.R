#' Title
#'
#'
#' @import Biostrings
#' @return
#' @export
#'
#' @examples
species_list <- function(path)
{
  library(dplyr)
  sequences <- readDNAStringSet(path)
  nom = names(sequences)
  data = data.frame(nom)
  data <- data %>%
    rowwise()%>%
    mutate(species = unlist(strsplit(nom, '\\['))[length( unlist(strsplit(nom, '\\[')))]) %>%
    mutate(species = paste0(unlist(strsplit(species, ' '))[1:2],collapse = " "))
  list <- gsub(x = unique(data$species),pattern = "\\]",replacement = '')
  return(list)
}

