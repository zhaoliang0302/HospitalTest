Hosp.wuyuan.onefile<-function(filename,query){
    options(stringsAsFactors = FALSE)
    query=stringi::stri_trans_nfkd(query)
    res=suppressWarnings(toString(readLines(filename)))
    res=stringi::stri_trans_nfkd(res)
    res=do::Replace(res,tmcn::toUTF8('B\u578B\u94A0\u5C3F\u80BD\u808C\u9499\u86CB\u767DI'),
                    tmcn::toUTF8('\u808C\u9499\u86CB\u767DI'))
    date1=stringr::str_extract_all(string =res,
                          pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}")[[1]]
    res1=strsplit(x = res,split =  "[ ,]{0,}[0-9]{4}-[0-9]{2}-[0-9]{2}")
    res2=res1[[1]]
    res3=res2[nchar(res2) != 0]
    res4=do::Replace0(res3,
                      c("[ ,]{0,}[0-9][0-9]:[0-9][0-9]:[0-9][0-9][ ,]{0,}",
                        tmcn::toUTF8('\\(\u7531HST\u68C0\u6D4B\\)'),
                        tmcn::toUTF8('\\(Astep\u68C0\u6D4B\\)'),
                        tmcn::toUTF8('\\(Arist\u68C0\u6D4B\\)'),
                        tmcn::toUTF8('\\(XN9000\u68C0\u6D4B\\)'),
                        tmcn::toUTF8('\\(XT4000i\u68C0\u6D4B\\)')))
    #delet poo and pee
    for (i in 1:length(res4)) {
        if (grepl(tmcn::toUTF8('\u7CAA'),res4[i])) res4[i]=''
        if (any(grepl(tmcn::toUTF8('\u5C3F\u5E38\u89C4'),res4[i]),
                grepl(tmcn::toUTF8('\u5C3F\u6DB2\u5206\u6790'),res4[i]),
                grepl(tmcn::toUTF8('\u5C3F\u6BD4\u91CD'),res4[i]))) res4[i]=''
    }
    res5=res4[nchar(res4) != 0]
    date2=date1[nchar(res4) != 0]
    res6=do::Replace(res5,from = ' {0,}: {0,}',to=':',
                     pattern = c(' {0,}, {0,}:,',
                                 ',{1,}:,'))
    #order by date is key for df.rbind later
    date2=date2[order(date2)]
    res6=res6[order(date2)]
    for (i in 1:length(res6)) {
        res6[i]
        first.colon=gregexpr(':',res6[i])[[1]][1]
        comma.loc=gregexpr(',',res6[i])[[1]]
        first.comma=comma.loc[1]
        if (first.comma < first.colon){
            comma.nearest.to.colon=max(comma.loc[comma.loc< first.colon])
            res6[i]=do::mid(res6[i],comma.nearest.to.colon+1,nchar(res6[i]))
        }
    }
    date2=date2[grepl(':',res6)]
    res6=res6[grepl(':',res6)]
    res6
    date2
    for (i in 1:length(res6)) {
        if (i==1) df=NULL
        df.i=txt_todf(x = res6[i],split1 = ',',split2 = ':')
        find=do::`%s=%`(query,colnames(df.i))
        find2=find[find !='integer(0)' ]
        find3=unique(unlist(find2))
        if (is.null(find3)) next(i)
        df.i2=df.i[,find3]
        if (length(find3)==1){
            df.i2=data.frame(df.i2)
            colnames(df.i2)=colnames(df.i)[find3]
        }
        df.i3=cbind(date=date2[i],df.i2)
        df=df.rbind(df,df.i3)
    }
    df
}
