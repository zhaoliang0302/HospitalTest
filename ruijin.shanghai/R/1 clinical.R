clinical <- function(path){
    options(stringsAsFactors = FALSE)
    library(magrittr)
    dd_file=tmcn::toUTF8('1 \u75C5\u53F2\u8D44\u6599.txt')
    dd1 = suppressWarnings(readLines(paste0(path, '/', dd_file)))
    dd2 = paste0(dd1, collapse = ";")
    dd3 = stringi::stri_trans_nfkd(dd2) %>%
        do::Replace(pattern = c('==={3,}:===',
                                ' {0,}; {0,}:;',
                                ';{2,}:;')) %>%
        strsplit(split = ';{0,}===;{0,}')
    dd=dd3[[1]][nchar(dd3[[1]]) != 0]
    for (j in 1:length(dd)) {
        if (j == 1) df = NULL
        dd4 = dd[j]
        series_id=sub('.*:',"",strsplit(dd4,';')[[1]][1])
        name=do::Replace0(dd4,c(tmcn::toUTF8('.*\u59D3\u540D[ :]{0,}'),
                                tmcn::toUTF8('[ ;:]{0,}\u6027\u522B.*'),
                                ' '))
        hosp_id=strsplit(dd4,';')[[1]][2]
        sex=do::Replace0(dd4,c(tmcn::toUTF8('.*\u6027\u522B[ :]{0,}'),
                                tmcn::toUTF8('[ ;:]{0,}\u5E74\u9F84.*'),
                               ' '))
        age=do::Replace0(dd4,c(tmcn::toUTF8('.*\u5E74\u9F84[ :]{0,}'),
                               tmcn::toUTF8('[ ;:]{0,}\u6C11\u65CF.*'),
                               ' '))
        nation=do::Replace0(dd4,c(tmcn::toUTF8('.*\u6C11\u65CF[ :]{0,}'),
                               tmcn::toUTF8('[ ;:]{0,}\u5A5A\u59FB.*'),
                               ' '))
        marry=do::Replace0(dd4,c(tmcn::toUTF8('.*\u5A5A\u59FB[ :]{0,}'),
                                 tmcn::toUTF8('[ ;:]{0,}\u51FA\u751F\u5730.*')))
        birthplace=do::Replace0(dd4,c(tmcn::toUTF8('.*\u51FA\u751F\u5730[ :]{0,}'),
                                      tmcn::toUTF8('[ ;:]{0,}\u804C\u4E1A.*'),
                                      ' '))
        work=do::Replace0(dd4,c(tmcn::toUTF8('.*\u804C\u4E1A[ :]{0,}'),
                                      tmcn::toUTF8('[ ;:]{0,}\u5165\u9662\u65E5\u671F.*'),
                                ' '))
        income_date=do::Replace0(dd4,c(tmcn::toUTF8('.*\u5165\u9662\u65E5\u671F[ :;]{0,}'),
                                       tmcn::toUTF8('[;: ]{0,}\u75C5\u53F2\u9648\u8FF0\u8005.*')))
        speaker=do::Replace0(dd4,c(tmcn::toUTF8('.*\u75C5\u53F2\u9648\u8FF0\u8005[ :;]{0,}'),
                                   tmcn::toUTF8('[;: ]{0,}\u4E3B\u8BC9.*'),
                                   tmcn::toUTF8('[;: ]{0,}\u65E2\u5F80.*'),
                                   ' '))
        compaint = do::Replace0(dd4,c(tmcn::toUTF8('.*\u4E3B\u8BC9:[ ;]{0,}'),
                                      tmcn::toUTF8('[;: ]{0,}\u73B0\u75C5\u53F2.*'),
                                      ' '))
        history=do::Replace0(dd4,c(tmcn::toUTF8('.*\u73B0\u75C5\u53F2:[ ;]{0,}'),
                                   tmcn::toUTF8('[;: ]{0,}\u65E2\u5F80\u53F2.*'),
                                   ' '))
        old_disease=do::Replace0(dd4,c(tmcn::toUTF8('.*\u75BE\u75C5\u53F2:[ ;]{0,}'),
                           tmcn::toUTF8('[;: ]{0,}\u4F20\u67D3\u75C5\u53F2.*'),
                           ' '))
        privet_history=do::Replace0(dd4,c(tmcn::toUTF8('.*\u4E2A\u4EBA\u53F2:[ ;]{0,}'),
                           tmcn::toUTF8('[;: ]{0,}\u4F53\u683C\u68C0\u67E5.*'),
                           tmcn::toUTF8('[;: ]{0,}\u5A5A\u80B2\u53F2.*'),
                           ' '))
        BMI=ifelse(grepl('BMI=',dd4),
                   do::Replace0(dd4,c('.*BMI=',';')),
                   "")
        sbp=ifelse(grepl(tmcn::toUTF8('\u8840\u538B:'),dd4),
                    do::Replace0(dd4,c(tmcn::toUTF8('.*\u8840\u538B:'),
                                       'mmHg.*','/.*')),
                    "")
        dbp=ifelse(grepl(tmcn::toUTF8('\u8840\u538B:'),dd4),
                   do::Replace0(dd4,c(tmcn::toUTF8('.*\u8840\u538B:'),
                                      'mmHg.*','.*/')),
                   "")
        df.i=data.frame(series_id,name,hosp_id,sex,age,nation,marry,birthplace,
                        work,income_date,speaker,compaint,history,
                        old_disease,privet_history,BMI,sbp,dbp)
        df = rbind(df, df.i)
    }
    colnames(df)=c(tmcn::toUTF8('\u6837\u672C\u53F7'),
                   tmcn::toUTF8('\u59D3\u540D'),
                   tmcn::toUTF8('\u4F4F\u9662\u53F7'),
                   tmcn::toUTF8('\u6027\u522B'),
                   tmcn::toUTF8('\u5E74\u9F84'),
                   tmcn::toUTF8('\u6C11\u65CF'),
                   tmcn::toUTF8('\u5A5A\u59FB'),
                   tmcn::toUTF8('\u51FA\u751F\u5730'),
                   tmcn::toUTF8('\u804C\u4E1A'),
                   tmcn::toUTF8('\u5165\u9662\u65E5\u671F'),
                   tmcn::toUTF8('\u75C5\u53F2\u9648\u8FF0\u4EBA'),
                   tmcn::toUTF8('\u4E3B\u8BC9'),
                   tmcn::toUTF8('\u73B0\u75C5\u53F2'),
                   tmcn::toUTF8('\u65E2\u5F80\u53F2'),
                   tmcn::toUTF8('\u4E2A\u4EBA\u53F2'),
                   'BMI',
                   tmcn::toUTF8('\u6536\u7F29\u538B'),
                   tmcn::toUTF8('\u8212\u5F20\u538B'))
    df
}

