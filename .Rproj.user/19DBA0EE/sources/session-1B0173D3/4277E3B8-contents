list.fq.files <- function(directory,stringtoremove)
{
  mylist <- list.files(directory,full.names = T)
  mylist <- mylist[grep('fastq',mylist)]
  forward <- mylist[grep('R1',mylist)]
  reverse <- mylist[grep('R2',mylist)]
  filename <- gsub(basename(forward),pattern = stringtoremove,replacement = '')
  result <- data.frame(forward,reverse,filename)
  return(result)
}
