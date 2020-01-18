labtxt_todf <- function(x){
    options(stringsAsFactors = FALSE)
    for (i in 1:length(x)) {
        if (i==1) DF=NULL
        txt.i=strsplit(x[i],';')[[1]]
        txt.i
        series_id=sub(".*:","",txt.i[1])
        txt.j=txt.i[3]
        txt.j=do::Replace(txt.j,"[0-9]{4}-[0-9]{2}-[0-9]{2}",'thisisforsplit')
        txt.j2=unlist(strsplit(txt.j,',{0,}thisisforsplit,{0,}'))
        txt.j3=txt.j2[nchar(txt.j2) != 0]
        date=unlist(stringr::str_extract_all(
            pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}",
            string = txt.i[3]))
        for (j in 1:length(date)) {
            if (j==1) df.3col=NULL
            #skip in some condition
            dd.j=txt.j3[j]
            if (length(do::`%s=%`(tmcn::toUTF8('\u5C3F\u6C89\u6E23\u5B9A\u91CF\u68C0\u6D4B'),
                                  dd.j)[[1]]) !=0) next(j)
            dd.j=do::Replace0(data = dd.j,
                              from = tmcn::toUTF8('\u4E2A\u4F53\u5316\u7528\u836F\u5EFA\u8BAE.*'))
            dd.j=strsplit(x = dd.j,split = ',')[[1]]
            line1df=to.df_1line(x = dd.j,split = ' ')
            line1df
            df.3col.j=cbind(series_id=series_id,
                            date=date[j],line1df)
            df.3col=plyr::rbind.fill(data.frame(df.3col,check.names = FALSE),
                                     data.frame(df.3col.j))
        }
        DF=plyr::rbind.fill(DF,df.3col)
    }
    DF
}
