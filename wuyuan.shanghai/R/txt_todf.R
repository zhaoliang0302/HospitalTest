txt_todf<-function(x,split1,split2){
    #convet text to dataframe with one row
    options(stringsAsFactors = FALSE)
    res_spt1=strsplit(x,split1)[[1]]
    res_spt2=do::col_split(res_spt1,':')
    df=res_spt2[!duplicated(res_spt2[,1]),]
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
