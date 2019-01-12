library(shiny)
library(shinydashboard)
library(dplyr)
library(recharts)
library(lubridate)

# the location of all cities
CityLocation <- read.csv(
    "cityLocation.csv",
    header = T,
    stringsAsFactors = F,
    encoding = "UTF-8"
)

# the cost of the TianTian
TianTianExpressCost <- read.csv(
    "TianTianExpressCost.csv",
    header = T,
    stringsAsFactors = F,
    encoding = "UTF-8"
)
rownames(TianTianExpressCost) <- TianTianExpressCost$city

# the cost of the BaiShiKuaiYun
BaiShiKuaiYunExpressCost <- read.csv(
    "BaiShiKuaiYunExpressCost.csv",
    header = T,
    stringsAsFactors = F,
    encoding = "UTF-8"    
)
rownames(BaiShiKuaiYunExpressCost) <- BaiShiKuaiYunExpressCost$city

# set of echarts
line.effect <- list(
    show=TRUE,
    scaleSize=1,
    period=30,
    color='#fff',
    shadowBlur=10
)
line.style <- list(
    normal=itemStyle(
            borderWidth=3,
            lineStyle=lineStyle(
                type='solid',
                shadowBlur=10)
            )
)

# shinyApp: shinyDashBoard
shinyApp(
    ui <- dashboardPage(
        dashboardHeader(
            title = "物流快递费用查询"
        ),
        dashboardSidebar(
            br(),
            selectInput(
                "which_express",
                "请选择快递或者物流：",
                c("", "天天快递", "百世快运")
            ),
            hr(),
            uiOutput(
                "input_ui_show"
            )
        ),
        dashboardBody(
            uiOutput(
                "output_ui_show"
            )
            # hr(),
            # eChartOutput(
            #     "output_echart_show",
            #
            # )
        )
    ),
    server <- function(input, output){
#*****************************************************************
        
        # which ui to show
        express_show <- reactive({
            if (input$which_express == "天天快递"){
                "TianTian"
            } else if(input$which_express == "百世快运"){
                "BaiShiKuaiYun"
            }
        })
        output$input_ui_show <- renderUI({
            uiOutput(
                paste0(express_show(),"_input_ui")
            )
        })
        output$output_ui_show <- renderUI({
            fluidPage(
                uiOutput(
                    paste0(express_show(),"_output_ui")               
                ),
                hr(),
                eChartOutput(
                    paste0(express_show(),"_express_echart"),
                    width = "100%",
                    height = "600px"
                )
            )
        })

        # TianTian Express
        
        # TianTian input_ui
        output$TianTian_input_ui <- renderUI({
            fluidPage(
                selectInput(
                    "TianTian_city_destination",
                    "请选择目的地：",
                    TianTianExpressCost %>%
                        select(city)
                ),
                numericInput(
                    "TianTian_weight",
                    "请输入重量：(单位:kg)",
                    0,
                    min = 0
                ),
                selectInput(
                    "TianTian_pay",
                    "请输入付费方式：",
                    c(
                        "现付",
                        "到付"
                    )
                )
            )
        })
        # TianTian output_ui
        TianTian_markLineDataInfo <- reactive({
            data.frame(
                name1 = "天津",
                name2 = input$TianTian_city_destination
            )
        })
        TianTian_expressCost_firstWeightPrice <- reactive({
            if(input$TianTian_pay == "现付"){
                TianTianExpressCost[input$TianTian_city_destination, 2]
            } else{
                TianTianExpressCost[input$TianTian_city_destination, 4]
            }
        })
        TianTian_expressCost_continueWeightPrice <- reactive({
            if(input$TianTian_pay == "现付"){
                TianTianExpressCost[input$TianTian_city_destination, 3]
            } else{
                TianTianExpressCost[input$TianTian_city_destination, 5]
            }
        })
        TianTian_expressCost_firstWeight <-reactive({
            min(ceiling(input$TianTian_weight), 1)
        })        
        TianTian_expressCost_continueWeight <-reactive({
            max(ceiling(input$TianTian_weight) - 1, 0)
        })
        output$TianTian_express_text <- renderText({
            paste0("天天快递：\n从 天津 发往 ",
                   input$TianTian_city_destination,
                   ",\n快递重量： ",
                   input$TianTian_weight,
                   ",\n计费重量： ",
                   ceiling(input$TianTian_weight),
                   ",\n首重费用： ",
                   TianTian_expressCost_firstWeightPrice(),
                   " * ",
                   TianTian_expressCost_firstWeight(),
                   " = ",    
                   TianTian_expressCost_firstWeightPrice() * TianTian_expressCost_firstWeight(),
                   ",\n续重费用： ",
                   TianTian_expressCost_continueWeightPrice(),
                   " * ",
                   TianTian_expressCost_continueWeight(),
                   " = ",
                   TianTian_expressCost_continueWeightPrice() * TianTian_expressCost_continueWeight(),
                   ",\n费用合计： ",
                   TianTian_expressCost_firstWeightPrice() * TianTian_expressCost_firstWeight() + TianTian_expressCost_continueWeightPrice() * TianTian_expressCost_continueWeight()
            )
        })
        output$TianTian_express_echart <- renderEChart(
           echartr(NULL,
                  type = "china_map") %>%
               addGeoCoord(
                   CityLocation
               ) %>%
               addML(
                   series = "运输线",
                   data = TianTian_markLineDataInfo(),           
                   smooth=TRUE,
                   effect=line.effect,
                   itemStyle=line.style
               ) %>%
               setSeries(
                   hoverable=FALSE,
                   itemStyle=list(
                        normal=itemStyle(
                           borderColor='rgba(100,149,237,1)',
                           borderWidth=0.5,
                           areaStyle=areaStyle(color='#1b1b1b')
                    )
               )) %>%
               set_title("感受一下距离远近", pos = 12)
        )
        output$TianTian_output_ui <- renderUI({
            fluidPage(
                tags$style(
                    type='text/css',
                    ".shiny-text-output{
                    font-size: 20px;
                    font-weight: bold;
                    }"
                ),
                verbatimTextOutput(
                    "TianTian_express_text"
                )
            )
        })
#*****************************************************************
        # BaiShiKuaiYun Express
        
        # BaiShiKuaiYun input_ui
        output$BaiShiKuaiYun_input_ui <- renderUI({
            fluidPage(
                selectInput(
                    "BaiShiKuaiYun_city_destination",
                    "请选择目的地：",
                    BaiShiKuaiYunExpressCost %>%
                        select(city)
                ),
                numericInput(
                    "BaiShiKuaiYun_weight",
                    "请输入重量：(单位:kg)",
                    0,
                    min = 0
                ),
                numericInput(
                    "BaiShiKuaiYun_length",
                    "请输入长度：(单位:cm)",
                    0,
                    min = 0
                ),
                numericInput(
                    "BaiShiKuaiYun_width",
                    "请输入宽度：(单位:cm)",
                    0,
                    min = 0
                ),
                numericInput(
                    "BaiShiKuaiYun_height",
                    "请输入高度：(单位:cm)",
                    0,
                    min = 0
                ),
                dateInput(
                    "BaiShiKuaiYun_date",
                    "请选择货物发出时间：\n(默认今天)",
                    today()
                )
            )
        })
        # BaiShiKuaiYun output_ui
        BaiShiKuaiYun_markLineDataInfo <- reactive({
            data.frame(
                name1 = "天津",
                name2 = input$BaiShiKuaiYun_city_destination
            )
        })
        BaiShiKuaiYun_expressCost_actualWeight <- reactive({
            ceiling(input$BaiShiKuaiYun_weight)
        })
        BaiShiKuaiYun_expressCost_volumeWeight <- reactive({
            ceiling(input$BaiShiKuaiYun_length * input$BaiShiKuaiYun_width * input$BaiShiKuaiYun_height / 6000)
        })
        BaiShiKuaiYun_expressCost_weight <- reactive({
            max(BaiShiKuaiYun_expressCost_actualWeight(), BaiShiKuaiYun_expressCost_volumeWeight())
        })
        BaiShiKuaiYun_expressCost_price <- reactive({
            colPrice <- if_else(BaiShiKuaiYun_expressCost_weight() <= 200, 2, if_else(BaiShiKuaiYun_expressCost_weight() <= 500, 3, 4))
            BaiShiKuaiYunExpressCost[input$BaiShiKuaiYun_city_destination, colPrice]
        })
        output$BaiShiKuaiYun_express_text <- renderText({
            paste0("百世快运：\n从 天津 发往 ",
                   input$BaiShiKuaiYun_city_destination,
                   ",\n实际重量： ",
                   BaiShiKuaiYun_expressCost_actualWeight(),
                   ",\n体积重量： ",
                   BaiShiKuaiYun_expressCost_volumeWeight(),
                   ",\n计费重量： ",
                   BaiShiKuaiYun_expressCost_weight(),
                   ",\n运费： ",
                   BaiShiKuaiYun_expressCost_price(),
                   " * ",
                   BaiShiKuaiYun_expressCost_weight(),
                   " = ",    
                   BaiShiKuaiYun_expressCost_price() * BaiShiKuaiYun_expressCost_weight(),
                   ",\n货物发出时间： ",
                   input$BaiShiKuaiYun_date,
                   ",\n最早到达时间： ",
                   input$BaiShiKuaiYun_date + BaiShiKuaiYunExpressCost[input$BaiShiKuaiYun_city_destination,5],
                   ",\n最迟到达时间： ",
                   input$BaiShiKuaiYun_date + BaiShiKuaiYunExpressCost[input$BaiShiKuaiYun_city_destination,6]
            )
        })
        output$BaiShiKuaiYun_output_ui <- renderUI({
            fluidPage(
                tags$style(
                    type='text/css',
                    ".shiny-text-output{
                    font-size: 20px;
                    font-weight: bold;
                    }"
                ),
                verbatimTextOutput(
                    "BaiShiKuaiYun_express_text"
                )
            )
        })
        output$BaiShiKuaiYun_express_echart <- renderEChart(
            echartr(NULL,
                    type = "china_map") %>%
                addGeoCoord(
                    CityLocation
                ) %>%
                addML(
                    series = "运输线",
                    data = BaiShiKuaiYun_markLineDataInfo(),           
                    smooth=TRUE,
                    effect=line.effect,
                    itemStyle=line.style
                ) %>%
                setSeries(
                    hoverable=FALSE,
                    itemStyle=list(
                        normal=itemStyle(
                            borderColor='rgba(100,149,237,1)',
                            borderWidth=0.5,
                            areaStyle=areaStyle(color='#1b1b1b')
                        )
                    )) %>%
                set_title("感受一下距离远近", pos = 12)
        )
        output$testText <- renderText({
            print(express_show())
        })
        output$test <- renderUI({
            verbatimTextOutput(
                "testText"
            )
        })
    }
)
