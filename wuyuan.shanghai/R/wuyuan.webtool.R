#' Web Tool to Clear Lab Data for Wuyuan Shanghai
#'
#' @return web
#' @export
#'
wuyuan.webtool <- function(){
    # Define UI for dataset viewer app ----
    ui <- fluidPage(

        # App title ----
        titlePanel(title  = "上海市第五人民医院(报告整理工具)"),

        # Sidebar layout with input and output definitions ----
        sidebarLayout(

            # Sidebar panel for inputs ----
            sidebarPanel(
                #文件所在目录
                textInput(inputId = "dir",
                          label = "请输入文件目录,例如:c:/文件/检验"),
                #整理的项目
                textInput(inputId = "query",
                          label = "请输入要采集的项目名称，使用逗号隔开，如：白细胞，血小板"),
                #查找的方式
                selectInput(inputId = "exact",
                            label = "匹配方式",
                            choices = c("精确", "模糊")),
                #同一项目不同日期
                selectInput(inputId = "oneitem_difdate",
                            label = "同一项目不同日期",
                            choices = c("所有","第一个的日期", "最后的日期")),
                #开始
                actionButton("go", "开始"),
                br(),
                downloadLink("downloadData", "Download")
            ),
            # 输出结果
            mainPanel(
                h3('结果'),
                DT::dataTableOutput("view")
            )
        )
    )

    server <- function(input, output) {
        res <- reactiveVal()
        observeEvent(input$go, {
            res(NULL)
            filenames=list.files(input$dir)
            #filenames=paste0(input$dir,'/',filenames)
            query=stringi::stri_trans_nfkd(input$query)
            query=unlist(strsplit(query,','))
            exact=ifelse(input$exact=='精确',TRUE,FALSE)
            duplicated.choose=ifelse(input$oneitem_difdate=='所有','all',
                                     ifelse(input$oneitem_difdate=='第一个的日期','first','last'))
            k=wuyuan.shanghai:::wuyuan.shanghai.shinyFOR(
                path=input$dir,filenames = filenames,
                query = query,exact = exact,
                duplicated.choose = duplicated.choose)
            res(k)
        })
        output$view <- DT::renderDataTable({
            res()
        })
        output$downloadData <- downloadHandler(
            filename = function() paste("data-", Sys.Date(), ".csv", sep=""),
            content = function(file) write.csv(res(), file)
        )
    }

    # Create Shiny app ----
    shinyApp(ui, server)
}
