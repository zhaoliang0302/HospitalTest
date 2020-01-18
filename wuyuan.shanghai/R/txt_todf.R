txt_todf<-function(x,split1,split2){
    #convet text to dataframe with one row
    options(stringsAsFactors = FALSE)
    res_spt1=strsplit(x,split1)[[1]]
    res_spt2=do::col_split(res_spt1,':')
    res_spt2=res_spt2[!duplicated(res_spt2[,1]),]
    df=data.frame(t(res_spt2[,2]),check.names = FALSE)
    colnames(df)=res_spt2[,1]
    df
}
