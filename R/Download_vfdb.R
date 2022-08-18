#' Title
#'
#' @param prefix
#' @import R.utils
#' @return
#' @export
#' @examples
Download_vfdb <- function(prefix)
{
  download.file(url = "http://www.mgc.ac.cn/VFs/Down/VFDB_setA_nt.fas.gz",
                destfile = paste0(prefix,"/VFDB_setB_nt.fas.gz"))
  gunzip(filename = paste0(prefix,"/VFDB_setB_nt.fas.gz"), remove=T)
}
