#' To Clear data for wuyuan shagnhai
#'
#' @param filenames filenames
#' @param query query words, by fuzzy match
#'
#' @return dataframe
#' @export
#'
wuyuan.shanghai <- function(filenames,query) {
    if (missing(filenames)) filenames=list.files()
    filenames=filenames[grepl('txt',filenames)]
    for (i in 1:length(filenames)) {
        if (i==1) res=NULL
        filename=filenames[i]
        res.i=Hosp.wuyuan.onefile(filename,query)
        if (is.null(res.i)) next(i)
        res.i2=cbind(filename,res.i)
        res=plyr::rbind.fill(res,res.i2)
        cat(i,'/',length(filenames),filename,'---OK','\n')
    }
    res
}
