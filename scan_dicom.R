
#' scan_dicom
#' 
#' 
#' scan a dicom folder structure and return basic information about the discovered contents
#' @import tractor.base
#' @import map
#' @export
scan_dicom <- function(dirname) {
  fnames <- list.files(dirname, ".*dcm", recursive=TRUE)
  fset <- do.call(rbind, map(strsplit(fnames, "/"), function(x) {
    group <- paste0(unlist(x[1:(length(x)-1)]), collapse="/")
    file <- x[[length(x)]]
    data.frame(file=file, group=group)
  }))
  
  fset_split <- split(fset$file, fset$group)
  fset_groups <- names(fset_split)
  
  dicom_set <- lapply(names(fset_split), function(gname) {
    path <- paste0(dirname, "/", gname, "/", fset_split[[gname]][1])
    dicom.info <- readDicomFile(path)
    nslices <- dicom.info$getTagValue(25,4106)
    desc <- dicom.info$getTagValue(8,4158)
    series_num <- dicom.info$getTagValue(0x20, 0x11)
    list(group=gname, desc=desc,
         nslices=nslices, series_num=series_num,
         sequence=dicom.info$getTagValue(24,36))
  })
  
  df_ord <- do.call(rbind, lapply(dicom_set, function(x) {
    data.frame(series=x$series_num, sequence=x$sequence)
  }))
  
  ord <- with(df_ord, order(sequence, series))
  dicom_set <- dicom_set[ord]
  dicom_set <- do.call(rbind, dicom_set)
}
