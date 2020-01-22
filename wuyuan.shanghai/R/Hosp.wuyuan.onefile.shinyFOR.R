Hosp.wuyuan.onefile.shinyFOR<-function(path,filename,query,exact,duplicated.choose){
    options(stringsAsFactors = FALSE)
    query=stringi::stri_trans_nfkd(query)
    res=suppressWarnings(toString(readLines(paste0(path,'/',filename))))
    res=stringi::stri_trans_nfkd(res)
    res=do::Replace(res,tmcn::toUTF8('B\u578B\u94A0\u5C3F\u80BD\u808C\u9499\u86CB\u767DI'),
                    tmcn::toUTF8('\u808C\u9499\u86CB\u767DI'))
    res=do::Replace(res,tmcn::toUTF8('\u8840\u5C0F\u677F\u805A\u96C6\u7387\u8840\u5C0F\u677F\u805A\u96C6\u7387'),
                    tmcn::toUTF8('\u8840\u5C0F\u677F\u805A\u96C6\u7387'))
    res=do::Replace(res,tmcn::toUTF8('\u8840\u5E38\u89C4\u8840\u5C0F\u677F\u538B\u79EF'),
                    tmcn::toUTF8('\u8840\u5C0F\u677F\u538B\u79EF'))
    res=do::Replace(res,tmcn::toUTF8('\u7535\u89E3\u8D28\u56DB\u9879\u5929\u95E8\u51AC\u6C28\u9178\u6C28\u57FA\u8F6C\u79FB\u9176-\u540C\u5DE5\u9176'),
                    tmcn::toUTF8('\u5929\u95E8\u51AC\u6C28\u9178\u6C28\u57FA\u8F6C\u79FB\u9176-\u540C\u5DE5\u9176'))
    res=do::Replace(res,tmcn::toUTF8('CRPC\u53CD\u5E94\u86CB\u767D'),
                    tmcn::toUTF8('\u53CD\u5E94\u86CB\u767D'))
    res=do::Replace(res,tmcn::toUTF8('\u51DD\u8840\u529F\u80FDD-\u4E8C\u805A\u4F53'),
                    tmcn::toUTF8('D-\u4E8C\u805A\u4F53'))
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
    res6=do::Replace0(res6,tmcn::toUTF8(',\u5907\u6CE8:\u5177\u4F53\u5206\u6790\u89C1\u8BE6:\u7EC6\u89E3\u91CA\u62A5\u544A'))
    res6=do::Replace0(res6,tmcn::toUTF8('\u5907\u6CE8:\u5CF0\u578B\u5F02\u5E38,\u5DF2\u8054\u7CFB\u533B\u751F,'))
    date2
    res6
    for (i in 1:length(res6)) {
        if (i==1) df=NULL
        df.i=txt_todf(x = res6[i],split1 = ',',split2 = ':')
        df.i
        df2=cbind(date=date2[i],df.i)
        colnames(df2)=tolower(colnames(df2))
        df2
        df=rbind(df,df2)
        df
    }
    df=data.frame(df)
    if (any(duplicated(paste0(df[,'date'],df[,'x1'])))){
        df=unique(df)
        df=dup_connect(data = df,id = c('date','x1'),dup.var = 'x2')
    }
    rownames(df)=NULL
    df
    if (exact){
        find=do::`%==%`(query,df[,2])
    }else{
        find=do::`%s=%`(query,df[,2])
    }
    find=find[find!='integer(0)']
    if (duplicated.choose=='all'){
        find=find
    }else if (duplicated.choose=='first'){
        find=lapply(find, min)
    }else if (duplicated.choose=='last'){
        find=lapply(find, max)
    }
    find3=unique(unlist(find))
    if (is.null(find3)) return(NULL)
    df.i2=df[find3,]
    df.i2
    if (any(colnames(df.i2)=='Freq')){
        if (any(df.i2[,'Freq']>1)){
            df.check=df.i2[df.i2[,'Freq']>1,]
            item.and=paste0(df.check[,'x1'],collapse = ', ')
            date.and=paste0(unique(df.check[,'date']),collapse = tmcn::toUTF8(' \u548C '))
            message(filename,tmcn::toUTF8(' \u5728 '),date.and,tmcn::toUTF8(' \u91CD\u590D\u6D4B\u91CF\u4E86'),item.and)
        }
        df.i2=df.i2[,set::not(colnames(df.i2),'Freq')]
    }
    date.u=unique(df.i2[,'date'])
    for (i in 1:length(date.u)) {
        date.ui=date.u[i]
        df.i2.t=df.i2[df.i2[,'date']==date.ui,]
        df.i4=data.frame(t(df.i2.t[,3]),check.names = FALSE,stringsAsFactors = FALSE)
        colnames(df.i4)=df.i2.t[,2]
        df.i5=cbind(date=df.i2.t[1,1],df.i4)
        if (i==1) {
            res.df.i=df.i5
        }else{
            res.df.i=plyr::rbind.fill(res.df.i,df.i5)
        }
    }
    res.df.i
}
