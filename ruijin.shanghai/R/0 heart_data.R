#' to clear data
#'
#' @param path path
#'
#' @return
#' @export
#'
heart_data <- function(path){
        # 1 clinical
        res_clinical = clinical(path)
        # 2 lab
        res_lab=lab_query(path,query_words())
        # 3 CAG
        res_cag=cag(path)
        # 4 heart us
        res_heart_us=heart_us(path)
        # 5 diagnose and drug
        res_dd = diagnose_drug(path)

        #merge
        mg1=merge(res_clinical,res_lab,by = tmcn::toUTF8('\u6837\u672C\u53F7'),all = TRUE)
        mg2=merge(mg1,res_cag,by = tmcn::toUTF8('\u6837\u672C\u53F7'),all = TRUE)
        mg3=merge(mg2,res_heart_us,by=tmcn::toUTF8('\u6837\u672C\u53F7'),all = TRUE)
        mg4=merge(mg3,res_dd,by=tmcn::toUTF8('\u6837\u672C\u53F7'),all = TRUE)
        mg4
}
