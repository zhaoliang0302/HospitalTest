cag <- function(path){
    options(stringsAsFactors = FALSE)
    cag_file=paste0(path,'/3 CAG.txt')
    library(magrittr)
    txt1=suppressWarnings(readLines(cag_file)) %>%
        paste0(collapse = ';') %>%
        stringi::stri_trans_nfkd() %>%
        do::Replace(from = tmcn::toUTF8('\u9020\u5F71\u663E\u793A'),
                    to=tmcn::toUTF8('\u9020\u5F71\u793A'),
                    pattern = c('==={3,}:===',
                                ';{1,}:;',
                                ' {0,}, {0,}:,',
                                ' {0,}; {0,}:;'))
    txt1=do::Replace(txt1,from = ' {0,}: {0,}',to=':')
    txt1=strsplit(x = txt1,split = ';{0,}===;{0,}')
    txt2=unlist(txt1)[nchar(unlist(txt1)) != 0]
    for (i in 1:length(txt2)){
        if (i==1) df=NULL
        series_id=do::Replace0(strsplit(txt2[i],';')[[1]][1],'.*:')
        operation=do::Replace0(strsplit(txt2[i],';')[[1]][3],'.*:')
        CAG_time=do::Replace0(strsplit(txt2[i],';')[[1]][4],tmcn::toUTF8('\u64CD\u4F5C\u65F6\u95F4: {0,}'))
        #message('------')
        #print(series_id)
        reslut=do::Replace0(txt2[i],tmcn::toUTF8('.*;\u7ED3\u679C:'))
        #message('------')
        #print(reslut)
        Syntax=if (grepl('Syntax',txt2[i])){
            syn=do::Replace0(txt2[i],'.*Syntax')
            stringr::str_extract(do::left(syn,10),'[0-9\\.]{1,10}')
        }else{
            ""
        }
        #message('------')
        #print(Syntax)
        CAG=do::Replace0(txt2[i],c(tmcn::toUTF8('.*\u9020\u5F71\u793A:'),
                                   tmcn::toUTF8(';\u7ED3\u679C:.*'),
                                   '[,;]{0,}Syntax.*'))

        #message('------')
        #print(CAG)
        txt.txt=do::Replace0(txt2[i],c(tmcn::toUTF8(';\u7ED3\u679C:.*'),
                                       paste0(tmcn::toUTF8('.*Syntax\u603B\u5206:'),Syntax,',{0,}')))
        PCI=if (grepl('PCIfor',txt.txt) ||
                grepl('PCI\\+',txt.txt) ||
                grepl('\\+PCI',txt.txt)){
            pcifor=do::mid(txt.txt,unlist(gregexpr('PCIfor',txt.txt))[1]+6,nchar(txt.txt))
            do::Replace0(pcifor,c(tmcn::toUTF8(';\u7ED3\u679C:.*')))
        }else{""}
        #message('------')
        #message('------')
        #print(IVUS)
        df.i=data.frame(series_id,operation,CAG_time,Syntax,reslut,CAG,PCI)
        #message('------')
        #print(txt2[i])
        df=rbind(df,df.i)
    }
    colnames(df)=c(tmcn::toUTF8('\u6837\u672C\u53F7'),
                   tmcn::toUTF8('CAG\u64CD\u4F5C\u540D\u79F0'),
                   tmcn::toUTF8('CAG\u64CD\u4F5C\u65F6\u95F4'),
                   'Syntax',
                   tmcn::toUTF8('CAG\u7ED3\u679C'),
                   tmcn::toUTF8('CAG\u9020\u5F71\u663E\u793A'),
                   tmcn::toUTF8('PCI\u7ED3\u679C'))
    df
}
