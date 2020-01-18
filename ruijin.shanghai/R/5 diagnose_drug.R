diagnose_drug <- function(path){
    options(stringsAsFactors = FALSE)
    library(magrittr)
    dd_file=tmcn::toUTF8('5 \u51FA\u5165\u9662\u8BCA\u65AD\u548C\u5E26\u836F.txt')
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
        dia_in=do::Replace0(dd4,c(tmcn::toUTF8('.*\u5165\u9662\u8BCA\u65AD[: ]{0,}'),
                                  tmcn::toUTF8('[; ]{0,}\u51FA\u9662\u8BCA\u65AD.*')))
        dia_out=do::Replace0(dd4,c(tmcn::toUTF8('.*\u51FA\u9662\u8BCA\u65AD[: ]{0,}'),
                                   tmcn::toUTF8('[; ]{0,}\u51FA\u9662\u5E26\u836F.*')))

        drug=do::Replace0(dd4,c(tmcn::toUTF8('.*\u51FA\u9662\u5E26\u836F[;: \\*]{0,}')))

    df.i=data.frame(series_id,dia_in,dia_out,drug)
    df = rbind(df, df.i)
    }
    colnames(df)=c(tmcn::toUTF8('\u6837\u672C\u53F7'),
                   tmcn::toUTF8('\u5165\u9662\u8BCA\u65AD'),
                   tmcn::toUTF8('\u51FA\u9662\u8BCA\u65AD'),
                   tmcn::toUTF8('\u51FA\u9662\u5E26\u836F'))
    df
    }

