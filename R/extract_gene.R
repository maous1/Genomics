#' Title
#'
#' @param species
#' @param path
#' @import Biostrings dplyr
#' @return
#' @export
#'
#' @examples
extract_gene <- function(species,path)
{
  dir.create(paste0("virulence_",species))
  sequences <- readDNAStringSet(path)
  sequences <- sequences[grepl(x = names(sequences),pattern = species)]
  nom = names(sequences)
  data <- data.frame(nom)
  data <- data %>%
    rowwise()%>%
    mutate(gene = unlist(strsplit(nom, ' '))[2]) %>%
    mutate(gene = gsub(pattern = '\\(',replacement = "",x = gene))%>%
    mutate(gene = gsub(pattern = '\\)',replacement = "",x = gene))%>%
    mutate(gene = gsub(pattern = '\\/',replacement = "-",x = gene))%>%
    group_by(gene)
  gene = unique(data$gene)
  names(sequences) = data$gene
  for (currentgene in gene)
  {
    currentsequences = sequences[names(sequences)==currentgene]
    if(length(currentsequences)>1){
      for (i in 1:length(currentsequences)) {
        names(currentsequences)[i] = paste0(names(currentsequences)[i],i)
      }
    }
    writeXStringSet(x = currentsequences,filepath = paste0("virulence_",species,"/",currentgene,".fasta"))
  }
}
