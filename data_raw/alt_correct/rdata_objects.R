#' rdata_objects
#'
#' @param dir
#' @param pattern
#'
#' @returns
#' @export
#'
#' @examples
#'  ol <- rdata_objects(dir = "../../data")
#' ol
#' ol[ol$file %in% ol$file[duplicated(ol$file)], ]   # 複数オブジェクトを含むファイル
#'
#' f2<-rdata_objects(dir = "old/")
#'　#write.csv(f2,file="rdata_objects_old.csv")
rdata_objects <- function(dir = "data", pattern = "\\.RData$") {
  files <- dir(dir, pattern = pattern, ignore.case = TRUE)
  res <- lapply(files, function(f) {
    e <- new.env()
    objs <- load(file.path(dir, f), envir = e)
    data.frame(
      file   = f,
      object = objs,
      class  = sapply(objs, function(x) class(e[[x]])[1]),
      dim    = sapply(objs, function(x) {
        d <- dim(e[[x]])
        if (is.null(d)) length(e[[x]]) else paste(d, collapse = " x ")
      }),
      row.names = NULL
    )
  })
  do.call(rbind, res)
}

