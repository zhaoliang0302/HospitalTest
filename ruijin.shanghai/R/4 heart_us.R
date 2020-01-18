heart_us <- function(path) {
    options(stringsAsFactors = FALSE)
    heart_file=tmcn::toUTF8('4 \u5FC3\u8D85.txt')
    dd1 = suppressWarnings(readLines(paste0(path, '/', heart_file)))
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
        ######1
        tmcn::toUTF8('\u4E3B\u52A8\u8109\u6839\u90E8\u5185\u5F84')
        if (grepl(tmcn::toUTF8('\u4E3B\u52A8\u8109\u6839\u90E8\u5185\u5F84'),
                  dd4)) {
            index1 = do::Replace0(dd4,
                c(tmcn::toUTF8('.*\u4E3B\u52A8\u8109\u6839\u90E8\u5185\u5F84 {0,}'),
                ' .*'
            ))
        } else{
            index1 = ""
        }
        index1
        ######2
        tmcn::toUTF8('\u5DE6\u623F\u5185\u5F84')
        dd4
        if (grepl(tmcn::toUTF8('\u5DE6\u623F\u5185\u5F84'), dd4)) {
            index2 = do::Replace0(dd4, c(
                tmcn::toUTF8('.*\u5DE6\u623F\u5185\u5F84 {0,}'),
                ' .*'
            ))
        } else{
            index2 = ""
        }
        index2
        #####3
        tmcn::toUTF8('\u5DE6\u5BA4\u8212\u5F20\u672B\u671F\u5185\u5F84')
        dd4
        if (grepl(tmcn::toUTF8('\u5DE6\u5BA4\u8212\u5F20\u672B\u671F\u5185\u5F84'),
                  dd4)) {
            index3 = do::Replace0(dd4, c(
                tmcn::toUTF8(
                    '.*\u5DE6\u5BA4\u8212\u5F20\u672B\u671F\u5185\u5F84 {0,}'
                ),
                ' .*'
            ))
        } else{
            index3 = ""
        }
        index3
        #####4
        tmcn::toUTF8('\u5DE6\u5BA4\u6536\u7F29\u672B\u671F\u5185\u5F84')
        dd4
        if (grepl(tmcn::toUTF8('\u5DE6\u5BA4\u6536\u7F29\u672B\u671F\u5185\u5F84'),
                  dd4)) {
            index4 = do::Replace0(dd4, c(
                tmcn::toUTF8(
                    '.*\u5DE6\u5BA4\u6536\u7F29\u672B\u671F\u5185\u5F84 {0,}'
                ),
                ' .*'
            ))
        } else{
            index4 = ""
        }
        index4
        #####5
        tmcn::toUTF8('\u5BA4\u95F4\u9694\u539A\u5EA6')
        dd4
        if (grepl(tmcn::toUTF8('\u5BA4\u95F4\u9694\u539A\u5EA6'),
                  dd4)) {
            index5 = do::Replace0(dd4, c(
                tmcn::toUTF8('.*\u5BA4\u95F4\u9694\u539A\u5EA6 {0,}'),
                ' .*'
            ))
        } else{
            index5 = ""
        }
        index5
        #####6
        tmcn::toUTF8('\u5DE6\u5BA4\u540E\u58C1\u539A\u5EA6')
        dd4
        if (grepl(tmcn::toUTF8('\u5DE6\u5BA4\u540E\u58C1\u539A\u5EA6'),
                  dd4)) {
            index6 = do::Replace0(dd4, c(
                tmcn::toUTF8('.*\u5DE6\u5BA4\u540E\u58C1\u539A\u5EA6 {0,}'),
                ' .*'
            ))
        } else{
            index6 = ""
        }
        index6
        #####7
        tmcn::toUTF8('\u5DE6\u5BA4\u5C04\u8840\u5206\u6570\\(%\\)')
        dd4
        if (grepl(tmcn::toUTF8('\u5DE6\u5BA4\u5C04\u8840\u5206\u6570\\(%\\)'),
                  dd4)) {
            index7 = do::Replace0(dd4, c(
                tmcn::toUTF8(
                    '.*\u5DE6\u5BA4\u5C04\u8840\u5206\u6570\\(%\\) {0,}'
                ),
                ' .*'
            ))
        } else{
            index7 = ""
        }
        index7
        #####8
        tmcn::toUTF8('\u80BA\u52A8\u8109\u6536\u7F29\u538B')
        dd4
        if (grepl(tmcn::toUTF8('\u80BA\u52A8\u8109\u6536\u7F29\u538B'),
                  dd4)) {
            index8 = do::Replace0(dd4, c(
                tmcn::toUTF8(
                    '.*\u4F30\u6D4B\u80BA\u52A8\u8109\u6536\u7F29\u538B\u7EA6 {0,}'
                ),
                'mmHg.*'
            ))
        } else{
            index8 = ""
        }
        index8
        #####9
        if (grepl('E=',
                  dd4)) {
            index9 = do::Replace0(dd4, c(tmcn::toUTF8('.*E= {0,}'),
                                         'cm.*'))
        } else{
            index9 = ""
        }
        index9
        #####10
        if (grepl(', A=',
                  dd4)) {
            index10 = do::Replace0(dd4, c(tmcn::toUTF8('.*, A= {0,}'),
                                          'cm.*'))
        } else{
            index10 = ""
        }
        index10
        #####11
        tmcn::toUTF8('\u8BCA\u65AD\u610F\u89C1')
        if (grepl(tmcn::toUTF8('\u8BCA\u65AD\u610F\u89C1'),
                  dd4)) {
            index11 = do::Replace0(dd4, c(
                tmcn::toUTF8('.*\u8BCA\u65AD\u610F\u89C1 {0,}'),
                '.*:'
            ))
        } else{
            index11 = ""
        }
        index11
        df.i = data.frame(
            series_id,
            index1,
            index2,
            index3,
            index4,
            index5,
            index6,
            index7,
            index8,
            index9,
            index10,
            index11
        )
        df = rbind(df, df.i)
    }
    index.names = c(
        tmcn::toUTF8('\u6837\u672C\u53F7'),
        tmcn::toUTF8('\u4E3B\u52A8\u8109\u6839\u90E8\u5185\u5F84'),
        tmcn::toUTF8('\u5DE6\u623F\u5185\u5F84'),
        tmcn::toUTF8('\u5DE6\u5BA4\u8212\u5F20\u672B\u671F\u5185\u5F84'),
        tmcn::toUTF8('\u5DE6\u5BA4\u6536\u7F29\u672B\u671F\u5185\u5F84'),
        tmcn::toUTF8('\u5BA4\u95F4\u9694\u539A\u5EA6'),
        tmcn::toUTF8('\u5DE6\u5BA4\u540E\u58C1\u539A\u5EA6'),
        tmcn::toUTF8('\u5DE6\u5BA4\u5C04\u8840\u5206\u6570(%)'),
        tmcn::toUTF8('\u80BA\u52A8\u8109\u6536\u7F29\u538B'),
        tmcn::toUTF8('E\u5CF0'),
        tmcn::toUTF8('A\u5CF0'),
        tmcn::toUTF8('\u5FC3\u8D85\u8BCA\u65AD\u610F\u89C1')
    )
    colnames(df) =        index.names
    df
}
