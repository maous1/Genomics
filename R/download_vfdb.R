#' Title
#'
#' @param prefix
#' @import R.utils
#' @return
#' @export
#' @examples
download_vfdb <- function(path)
{
  download.file(url = "http://www.mgc.ac.cn/VFs/Down/VFDB_setA_nt.fas.gz",
                destfile = paste0(path,"/VFDB_setA_nt.fas.gz"))
  gunzip(filename = paste0(path,"/VFDB_setA_nt.fas.gz"), remove=T)
}
