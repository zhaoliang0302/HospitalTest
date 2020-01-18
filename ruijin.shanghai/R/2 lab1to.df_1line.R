to.df_1line<-function(x,split,...){
  options(stringsAsFactors = FALSE)
    dd.i2=x[nchar(x) !=0]
    df=do::col_split(x = dd.i2,split = ' ')
    df=as.matrix(df)
    df=ifelse(is.na(df),"",df)
    if (ncol(df)>2){
        #message('Caution! ',...,'\n')
        for (i in 3:ncol(df)) {
            df[,2]=paste0(df[,2],df[,i])
        }
      df=df[,1:2]
    }
    df
}
