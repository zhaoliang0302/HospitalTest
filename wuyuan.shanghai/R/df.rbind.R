df.rbind<-function(df,df.i3){
    if (is.null(df)){
        df=df.i3
        return(df)   
    }
    if (df.i3[,1]==df[nrow(df),1]){
        loc=do::`%==%`(colnames(df.i3)[-1],colnames(df)[-1])
        if (all(loc=='integer(0)')){
            #all is new and the same date
            mtr=matrix('',nrow = nrow(df)-1,ncol = ncol(df.i3),
                       dimnames = list(NULL,colnames(df.i3)))
            df.rbind=rbind(mtr,df.i3)
            df=cbind(df,df.rbind)
        }else{
            df=plyr::rbind.fill(df,df.i3)
        }
    }else{
        df=plyr::rbind.fill(df,df.i3)
    }
    df
}

