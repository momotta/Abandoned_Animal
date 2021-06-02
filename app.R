load("app.RData")

library(shiny)
library(bslib)
library(tidyverse)
library(data.table)
library(magrittr)
library(plotly)
library(lubridate)
library(RColorBrewer)
library(htmltools)
library(shinythemes)
library(zoo)


map1 <- korea_sido %>% 
  ggplot() + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=count1), color="lightgray") +
  coord_fixed() +
  scale_fill_gradient2("Obs\nDensity", high="#0D47A1", mid = "#64B5F6",
                       low="aliceblue", midpoint=0.5, name=NULL,
                       breaks=c(0, 0.25, 0.5, 0.75, 1), 
                       labels=c("최소", "", "", "", "최대")) +
  theme_map +
  theme(legend.position=c(0.8,0.3)) +
  guides(fill = guide_colorbar(barwidth = 0.5, barheight = 4)) +
  geom_text(sido_center_adjusted1, mapping = aes(x=lon, y=lat, label=sido),
            size=2.8) +
  xlab("") + ylab("")

map2 <- korea_sido %>% 
  ggplot() + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=count2), color="lightgray") +
  coord_fixed() +
  scale_fill_gradient2("Obs\nDensity", high="#0D47A1", mid = "#64B5F6",
                       low="aliceblue", midpoint=0.5, name=NULL,
                       breaks=c(0, 0.25, 0.5, 0.75, 1), 
                       labels=c("최소", "", "", "", "최대")) +
  theme_map +
  theme(legend.position=c(0.8,0.3)) +
  guides(fill = guide_colorbar(barwidth = 0.5, barheight = 4)) +
  geom_text(sido_center_adjusted1, mapping = aes(x=lon, y=lat, label=sido),
            size=2.8) +
  xlab("") + ylab("")

map3 <- korea_sido %>% 
  ggplot() + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=count3), color="lightgray") +
  coord_fixed() +
  scale_fill_gradient2("Obs\nDensity", high="#0D47A1", mid = "#64B5F6",
                       low="aliceblue", midpoint=0.5, name=NULL,
                       breaks=c(0, 0.25, 0.5, 0.75, 1), 
                       labels=c("최소", "", "", "", "최대")) +
  theme_map +
  theme(legend.position=c(0.8,0.3)) +
  guides(fill = guide_colorbar(barwidth = 0.5, barheight = 4)) +
  xlab("") + ylab("")


theme <- bs_theme(
  bootswatch = "minty",
  #bg = "#FFFFFF",
  #fg = "#730505",
  heading_font = font_google("Jua"),
  base_font = font_google("Nanum Gothic")
)
#bs_theme_preview(theme)



# Define UI ----
ui <- fluidPage(
  navbarPage(titlePanel("한국 유기동물 실태", paste0(puppy, kitty, bunny)),
             theme = theme,
             #theme = shinytheme("journal"),
             
             tabPanel("년도별 유기동물 수",
                      #년도별
                      
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     checkboxGroupInput("by_year_kind", label = h5("축종선택"),
                                                        choices = list("개" = "개", 
                                                                       "고양이" = "고양이",
                                                                       "기타축종" = "기타축종"),
                                                        selected = c("개", "고양이", "기타축종")),
                                     helpText("그래프에 마우스 커서를", br(), "위치하면 정확한 수가", br(), "표시됩니다.",
                                              style = "font-size:8px;")
                                     ),
                        mainPanel(plotlyOutput("by_year_plot"), width = 8)
                      )
             ),
  
  
  # 월별
  tabPanel("월별 유기동물 수",
           sidebarLayout(
             sidebarPanel(width = 3,
                          radioButtons("by_month_kind" , label = h5("축종선택"),
                                       choices =list(
                                         "전체" = "전체",
                                         "개" = "개", 
                                         "고양이" = "고양이",
                                         "기타축종" = "기타축종"),
                                       selected = "전체"),
                          helpText("그래프에 마우스 커서를", br(), "위치하면 정확한 수가", br(), "표시됩니다.",
                                    style = "font-size:8px;"),
                          ),
             mainPanel(plotlyOutput("by_month_plot"), width=8)
             )
           ),
  
  
  
  # 그룹별
  tabPanel("개체 특성별 비율",
      sidebarLayout(
        sidebarPanel(width = 3,
      selectInput("by_group_criteria", label = h5("기준선택"),
                   choices = list("나이 범주" = "age_category",
                                  "성별" = "sexCd",
                                  "중성화 여부" = "neuterYn"),
                   selected = "age_category")
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("개", plotlyOutput("by_group_plot", width = "100%")),
        tabPanel("고양이", plotlyOutput("by_group_plot2"))
      )
    )
  )
),
  
  
  
  # 시/도별
  tabPanel("지역별 상대적 유기동물 발생 규모",
         sidebarLayout(
           sidebarPanel(width = 3,
                        selectInput("by_sido_condition", label = h5("반영 조건 선택"),
                                    choices = list("선택 안함" = 0,
                                                   "인구규모 대비" = 1, 
                                                   "보호기관 수 대비" = 2),
                                    selected = 0),
                        conditionalPanel(
                          condition = "input.by_sido_condition == 2",
                          radioButtons("shelter_condition", label = "",
                                       choices = list("기관 수 표시" = 1,
                                                      "기관 분포 표시" = 2))
                        ),
                        helpText("처음 로딩시 시간이", br(), "소요될 수 있습니다.",
                                 style = "font-size:8px;"),
           ),
           mainPanel(plotOutput("by_sido_plot", width="100%"),
                     helpText("[0,1]범위로 표준화한 상대적 규모", 
                              align = "center")
           )
         )
      )
   )
)





# Define server logic ----
server <- function(input, output) {
  
  # 년도별
  by_year_kind2 <- reactive({
    validate(
      need(!is.null(input$by_year_kind), " 축종을 선택해주세요.")
      )
    input$by_year_kind
  })
  
  by_year2 <- reactive({
    by_year[kindCd_L %in% input$by_year_kind]
  })
  
  
  output$by_year_plot <- renderPlotly({
    by_year3 <- full_join(by_year2()[, .(합 = sum(count)), by = .(year)],
                          by_year2() %>% spread(kindCd_L, count), by = "year")
    fig <- plot_ly(by_year3, type = "bar", 
                   x = ~year, y = ~eval(as.name(by_year_kind2()[1])),
                   name = by_year_kind2()[1], hoverinfo = 'text',
                   text = ~paste0(by_year_kind2()[1],": ", eval(as.name(by_year_kind2()[1])),"건<br>",
                                  "합: ",합, "건"),
                   marker = list(color = choices_df$color[choices_df$kind==by_year_kind2()[1]])) %>% 
      layout(barmode = 'stack', xaxis = list(title = ""), 
             yaxis = list(title = "건수"), showlegend = TRUE, legend = list(orientation = 'h'))
    if (length(by_year_kind2()) == 2) {
      fig %<>% add_trace(y = ~eval(as.name(by_year_kind2()[2])), name = by_year_kind2()[2], hoverinfo = 'text',
                         text = ~paste0(by_year_kind2()[2],": ", eval(as.name(by_year_kind2()[2])),"건<br>합: ",합, "건"),
                         marker = list(color = choices_df$color[choices_df$kind==by_year_kind2()[2]]))
    } else if (length(by_year_kind2()) == 3) {
      fig %<>% add_trace(y = ~eval(as.name(by_year_kind2()[2])), name = by_year_kind2()[2], hoverinfo = 'text',
                         text = ~paste0(by_year_kind2()[2],": ", eval(as.name(by_year_kind2()[2])),"건<br>합: ",합, "건"),
                         marker = list(color = choices_df$color[choices_df$kind==by_year_kind2()[2]])) %>% 
        add_trace(y = ~eval(as.name(by_year_kind2()[3])), name = by_year_kind2()[3], hoverinfo = 'text',
                  text = ~paste0(by_year_kind2()[3],": ", eval(as.name(by_year_kind2()[3])),"건<br>합: ",합, "건"),
                  marker = list(color = choices_df$color[choices_df$kind==by_year_kind2()[3]]))
    }
    fig %>% layout(legend = list(traceorder = "normal"))
  })
  

    
  # 월별
  by_month2 <- reactive({
    by_month[kindCd_L == input$by_month_kind]
  })
  
  output$by_month_plot <- renderPlotly({
    plot_ly(by_month2(), x = ~happenDt, y = ~count, type = 'scatter', mode = 'lines',
            hoverinfo = 'text', text = ~paste0(year(happenDt), "년 ", month(happenDt), "월<br>",count,"건"),
            line = list(color = choices_df$color[choices_df$kind==input$by_month_kind], width = 3)) %>%
      layout(xaxis = list(title = ""), 
             yaxis = list(title = "건수", range = c(0, 15000)))
  })
  
  
  
  # 그룹별
  by_group_criteria2 <- reactive({
    as.name(input$by_group_criteria)
  })
  
  by_group1 <- reactive({
    abandonment_group[kindCd_L == "개", .(count= sum(count)), keyby = .(eval(by_group_criteria2()))]
  })
  
  by_group2 <- reactive({
    abandonment_group[kindCd_L == "고양이", .(count= sum(count)), keyby = .(eval(by_group_criteria2()))]
  })
  
  output$by_group_plot <- renderPlotly({
    plot_ly(by_group1(),
            type = "pie", labels = ~by_group_criteria2, values = ~count,
            marker = list(colors = brewer.pal(8, "Set2")[1:length(by_group1()$count)]),
            textinfo = 'percent', sort = FALSE,
            hovertemplate = "%{label} <br> %{percent}<extra></extra>",
            insidetextorientation = "horizontal") %>% 
      layout(font = list(size = 14), 
             legend = list(x = 100, y = 0.5, font = list(size = 12), 
                           title = list(text= paste('<b>', choices_df$names[choices_df$id==input$by_group_criteria], '</b>'))))
  })
  
  output$by_group_plot2 <- renderPlotly({
    plot_ly(by_group2(),
            type = "pie", labels = ~by_group_criteria2, values = ~count,
            marker = list(colors = brewer.pal(8, "Set2")[1:length(by_group2()$count)]),
            textinfo = 'percent', sort = FALSE,
            hovertemplate = "%{label} <br> %{percent}<extra></extra>",
            insidetextorientation = "horizontal") %>% 
      layout(font = list(size = 14), 
             legend = list(x = 100, y = 0.5, font = list(size = 12), 
                           title = list(text=paste('<b>', choices_df$names[choices_df$id==input$by_group_criteria], '</b>'))))
  })
  
  
  
  # 시/도별
  output$by_sido_plot <- renderCachedPlot({
    if (input$by_sido_condition == 0) {
      map1
    } else if (input$by_sido_condition == 1) {
      map2
    } else {
      if (input$shelter_condition == 1) {
        map3 + 
          geom_text(data = care_count %>% merge(., sido_center_adjusted2, by = "sido"),
                    mapping = aes(x = lon , y = lat, 
                                  label = paste0("(",care_count,")")),
                    size = 3, color = "red")
      } else {
        map3 +
          geom_point(care_address, mapping = aes(x = lon, y = lat), 
                     color = "darkblue", shape = 3)
      }
    }
  },cacheKeyExpr = {c(input$by_sido_condition, input$shelter_condition)}
  )
  encoding = "UTF-8"
}


# Run the app ----
#shinyApp(ui = ui, server = server)
runGadget(ui, server, viewer = browserViewer(browser = getOption("browser")))