#' Screen within a reference sequence the presence of a querry sequence
#'
#' With this function, the user provides a reference sequence and a querry sequence and the function compute the percentage of the reference which is covered by the querry. This computation is computed using the GenomicRanges Bioconductor objects.
#'
#'
#' @param subject The path of the reference sequence that you want to screen. The sequence should be a .fasta file including one or several sequences
#' @param querry The path of the querry sequence. he sequence should be a .fasta file including one or several sequences
#' @param threshold the minimum percentage of identity obtained with Blast that should be obtained in order to consider that the query match the reference sequence
#' @param path the directory where Blastn is located
#'
#' @return numeric value of the percentage of the reference sequence which is covered by the querry sequence.
#' @import Biostrings
#' @import GenomicRanges
#' @import IRanges
#' @import dplyr
#' @export

screen_Blast <- function (subject, querry,threshold,path)
{
  myarg <- paste0(" -subject ",subject," -query ",querry," -out blast.txt  -outfmt \"6 qacc qlen length qstart qend pident sacc \"")
  system2(command = path, args = myarg)
  blast <- try(read.table("blast.txt"), silent = T)
  if (class(blast) == "data.frame")
  {
    colnames(blast) <- c( "querry_access", "querry_length", "alignment_lenght", "querry_start", "querry_end","pc_ident", "subject_access")

    blast <- blast %>% filter(pc_ident>threshold)

    GR <- GRanges(seqnames = blast$querry_access,ranges = IRanges(start =blast$querry_start ,end = blast$querry_end))
    GR_reduce <- reduce(GR)

    coverage_df <- tibble(querry_access = seqnames(GR_reduce)|>as.character(),cover_length = width(GR_reduce))
    blast = blast %>% select(querry_access,querry_length) %>%
      distinct() %>%
      left_join(coverage_df,by ="querry_access") %>%
      group_by(querry_access,querry_length)%>%
      summarise(cover_length = sum(cover_length))%>%
      mutate(pc_coverage = round(100*cover_length/querry_length,1))


    pc_coverage = blast$pc_coverage
    names(pc_coverage) = blast$querry_access

    return(pc_coverage)
  }
  else{stop("error : No hit with blast")}
}
