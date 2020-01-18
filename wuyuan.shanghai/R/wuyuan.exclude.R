#' To exclude
#'
#' @param df result by wuyuan.shanghai
#' @param exclude exclude, by fuzzy match
#'
#' @return df
#' @export
#'
wuyuan.exclude <- function(df,exclude){
        df2=df[,-unique(unlist(do::`%s=%`(exclude,colnames(df))))]
        judge=rowSums(do::Nchar(df2),na.rm = T) == rowSums(do::Nchar(df2[,1:2]),na.rm = T)
        df2[!judge,]
}
