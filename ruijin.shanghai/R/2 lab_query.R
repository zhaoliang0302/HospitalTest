lab_query<-function(path,query){
    options(stringsAsFactors = FALSE)
    lab_filename=tmcn::toUTF8('2 \u5B9E\u9A8C\u5BA4\u68C0\u67E5.txt')
    lab_filename2=paste0(path,'/',lab_filename)
    txt1=suppressWarnings(readLines(lab_filename2))
    library(magrittr)
    txt2=  enc2utf8(txt1) %>%
        do::Replace0('<ff>') %>%
        stringi::stri_trans_nfkd()  %>%
        paste0(collapse = ';') %>%
        do::Replace(from = c(', {1,}',',{1,}'),to=',',
                    pattern = c('={3,}:===',
                                ';{1,}:;')) %>%
        strsplit(split = '===')
    txt3=txt2[[1]][nchar(txt2[[1]])!=0]
    df=labtxt_todf(txt3)
    df=as.data.frame(df)
    colnames(df)=tolower(colnames(df))
    query=stringi::stri_trans_nfkd(query)
    id=df[,"series_id"]
    id.u=as.character(unique(id))
    for (i in 1:length(id.u)) {
        if (i==1) res=NULL
        df.i=df[id==id.u[i],]
        location=do::`%==%`(query,
                            df.i[,'x1'])
        if (any(location=='integer(0)')){
            names.integer0=names(location)[location=='integer(0)']
            message(i,": ",id.u[i],
                    tmcn::toUTF8(' \u4E2D\u672A\u627E\u5230: '),
                    length(names.integer0),tmcn::toUTF8('\u4E2A\u9879\u76EE'),
                    paste0('\n',
                           paste0(
                               paste0('                 ',
                                      names.integer0),
                           collapse = '\n')),
                    '\n')
            location=location[location!='integer(0)']
        }
        if (is.atomic(location)) location=list(location)

        location.max=unlist(lapply(location, min))
        df.i=df.i[location.max,set::not(colnames(df.i),'date')]
        df.i2=t(as.character(df.i[,'x2']))
        df.i3=cbind(df.i[1,1],df.i2)
        rownames(df.i3)=NULL
        colnames(df.i3)=c(tmcn::toUTF8('\u6837\u672C\u53F7'),
                          as.character(df.i[,'x1']))
        if (i==1){
            res=df.i3
        }else{
            res=plyr::rbind.fill(data.frame(res,check.names = FALSE),
                             data.frame(df.i3,check.names = FALSE))
        }
    }
    res=as.matrix(res)
    df2=ifelse(is.na(res),"",res)
    df3=data.frame(df2,check.names = FALSE)
    df3
}
