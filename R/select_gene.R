#' Title
#'
#' @param species
#' @param path
#' @import Biostrings dplyr
#' @return
#' @export
#'
#' @examples
select_gene <- function(species,path)
{
  sequences <- readDNAStringSet(path)
  sequences <- sequences[grepl(x = names(sequences),pattern = species)]
  nom = names(sequences)
  data <- data.frame(nom)
  data <- data %>%
    rowwise()%>%
    mutate(gene = unlist(strsplit(nom, ' '))[2]) %>%
    mutate(gene = gsub(pattern = '\\(',replacement = "",x = gene))%>%
    mutate(gene = gsub(pattern = '\\)',replacement = "",x = gene))
  gene = unique(data$gene)
  for (currentgene in gene) {
    writeXStringSet(x = sequences[grepl(x = names(sequences),pattern = currentgene)],filepath = paste0(currentgene,".fasta"))
  }
}

