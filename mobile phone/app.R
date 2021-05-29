
#### public 링크:  https://jaaaamj0711.shinyapps.io/shiny/ ####

library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(DT)
library(dplyr)  
library(broom)
library(caret)
library(cvms)
require(PerformanceAnalytics)


library(dplyr)
library(stringr)
library(png)
library(ggplot2)
library(tidyverse)
library(RWeka)
library(fBasics)
library(pracma)
library(signal)
library(seewave)
library(e1071)
library(caret)
library(xgboost)
library(changepoint)

load("a.RData")

##### ui define #####
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Seo Min Ji"),
                    #### Sideber define #####
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem('What is a Project?', tabName='start',
                                     menuSubItem('About the subject', tabName='subject'),
                                     menuSubItem('Data description & Reference', tabName='dr')),
                            
                            menuItem('Static model', tabName='st',
                                     menuSubItem('Dataset', tabName='st_dataset'),
                                     menuSubItem('Feature plot', tabName='st_plot'),
                                     menuSubItem('Modeling', tabName='st_model'),
                                     menuSubItem('Feature importance', tabName='st_imp')),
                            
                            menuItem('Peak model', tabName='pk',
                                     menuSubItem('Dataset', tabName='pk_dataset'),
                                     menuSubItem('Feature plot', tabName='pk_plot'),
                                     menuSubItem('Modeling', tabName='pk_model'),
                                     menuSubItem('Feature importance', tabName='pk_imp')),
                            
                            menuItem('Change point model', tabName='ch',
                                     menuSubItem('Dataset', tabName='ch_dataset'),
                                     menuSubItem('Feature plot', tabName='ch_plot'),
                                     menuSubItem('Modeling', tabName='ch_model'),
                                     menuSubItem('Feature importance', tabName='ch_imp')),
                            
                            menuItem('Fourier_model', tabName='fo',
                                     menuSubItem('Dataset', tabName='fo_dataset'),
                                     menuSubItem('Feature plot', tabName='fo_plot'),
                                     menuSubItem('Modeling', tabName='fo_model'),
                                     menuSubItem('Feature importance', tabName='fo_imp'))
                            )),
                            
                    #### Body define #####
                    dashboardBody(
                        tabItems( #### What is a Project? ####
                            tabItem(tabName = "subject", 
                                    h1("MotionSense Data Analysis"),
                                    br(),
                                    h3("비정형 데이터에 속하는 센서 데이터로 분석한 내용을 담은 대시보드입니다.")),
                            tabItem(tabName = "dr",
                                    h3("원본 데이터 설명"),
                                    h4("- 다음 데이터는 가속도계 및 자이로 스코프 센서(attitude, gravity, userAcceleration 및 rotationRate)에서 생성 된 시계열 데이터로 구성되어 있습니다."),
                                    h4("- 성별, 나이, 몸무게, 키 등 총 24 명의 참가자가 동일한 환경과 조건에서 아래층, 위층, 걷기, 조깅, 앉기, 서기 등의 활동을 하였습니다."),
                                    br(),
                                    h3("분석 목표"),
                                    h4("- 원본 신호 데이터를 바탕으로 다양한 특징을 추출하여 유의미한 변수를 생성합니다."),
                                    h4("- 참가자의 행동을 잘 분리하는 모델을 생성합니다."),
                                    br(),
                                    h3("데이터셋 출처"),
                                    p("- 데이터는 이 링크에서 확인할 수 있습니다.",
                                    a("click here", 
                                        href = "https://www.kaggle.com/malekzadeh/motionsense-dataset")),
                                    br(),
                                    h3("참고"),
                                    p("- 분석시 사용된 코드를 확인할 수 있습니다.",
                                    a("click here",
                                      href = "https://github.com/jaaaamj0711/Unstructured_Data_Analysis/blob/main/mobile%20phone/code_resilt.R")),
                                    
                                    ),
                                        
                            
                            #### Static model ####
                            tabItem(box(title = "Data set description", background = "navy", solidHeader = TRUE, width = 15, 
                                        verbatimTextOutput("st_text1"),
                                        verbatimTextOutput("st_text12")),
                                    box(title = "Data set", background = "navy", solidHeader = TRUE, width = 15, 
                                        verbatimTextOutput("st_text2")),
                                    tabName = "st_dataset", DT::dataTableOutput("st_datasetTable")),
                            
                            tabItem(tabName = "st_plot",
                                    fluidPage(
            
                                        box(title = "Feature Line Plot", background = "navy", solidHeader = TRUE, width = 15, 
                                            verbatimTextOutput("st_text3")),
                                    
                                        fluidRow(    
                                            box(selectInput(inputId = "col_name_st",
                                                    label = "choose a column :",
                                                    choices = names(ST)[5:154]), width = 4)),
                                        fluidRow( 
                                        box(background = "navy", 
                                            plotOutput("st_plot_", height = 300),width = 13)))),
                            
                            tabItem(tabName = "st_model", background = "navy", 
                                    box(title = "Modeling result", background = "navy", solidHeader = TRUE,  width = 13, 
                                        verbatimTextOutput("st_text4"),
                                        verbatimTextOutput("st_text42")),
                                    
                                    fluidRow(
                                        box(title = 'Confusion Matrix',background = "navy",width = 12, 
                                            verbatimTextOutput('st_model_')))),
                            
                            tabItem(tabName = "st_imp",
                                    background = "navy",
                                    box(title = 'Feature Importance Top 20', background = "navy", solidHeader = TRUE, width=13,  
                                        verbatimTextOutput("st_text5")),
                                    box(background = "navy",
                                        plotOutput("st_imp_"))),
                            
                            #### Peak model ####
                            tabItem(box(title = "Data set description", background = "navy", solidHeader = TRUE, width = 15, 
                                        verbatimTextOutput("pk_text1"),
                                        verbatimTextOutput("pk_text12")),
                                    box(title = "Data set", background = "navy", solidHeader = TRUE, width = 15, 
                                        verbatimTextOutput("pk_text2")),
                                    tabName = "pk_dataset", DT::dataTableOutput("pk_datasetTable")),
                            
                            tabItem(tabName = "pk_plot",
                                    
                                    fluidPage(
                                        box(background = "navy", width = 15, title = "Peak example", solidHeader = TRUE,
                                            verbatimTextOutput("pk_text_peak")),
                                        box(background = "navy",
                                            plotOutput("pk_plot2_", height = 250),
                                            width = 13)),
                                    
                                    fluidPage(
                                        box(background = "navy", width = 13, title = "Feature Line Plot", solidHeader = TRUE,
                                            verbatimTextOutput("pk_text3")),
                                        
                                        fluidRow(
                                        box(selectInput(inputId = "col_name_pk",
                                                        label = "choose a column :",
                                                        choices = names(PK)[5:46]),width = 4)),
                                        fluidRow(
                                        box(background = "navy", 
                                            plotOutput("pk_plot_",height = 250),width = 12)),
                                    )),
                            
                            tabItem(tabName = "pk_model", background = "navy", 
                                    box(title = "Modeling result", background = "navy", solidHeader = TRUE,  width = 13, 
                                        verbatimTextOutput("pk_text4"),
                                        verbatimTextOutput("pk_text42")),
                                    
                                    fluidRow(
                                        box(title = 'Confusion Matrix',background = "navy",width = 12, 
                                            verbatimTextOutput('pk_model_')))),
                            
                            tabItem(tabName = "pk_imp",
                                    background = "navy",
                                    box(title = 'Feature Importance Top 20', background = "navy", solidHeader = TRUE, width=13,  
                                        verbatimTextOutput("pk_text5")),
                                    box(background = "navy",
                                        plotOutput("pk_imp_"))),
                            
                            #### Change model ####
                            tabItem(box(title = "Data set description", background = "navy", solidHeader = TRUE, width = 15, 
                                        verbatimTextOutput("ch_text1"),
                                        verbatimTextOutput("ch_text12")),
                                    box(title = "Data set", background = "navy", solidHeader = TRUE, width = 15, 
                                        verbatimTextOutput("ch_text2")),
                                    tabName = "ch_dataset", DT::dataTableOutput("ch_datasetTable")),
                            
                            tabItem(tabName = "ch_plot",
                                    fluidPage(
                                        box(title = "Feature Line Plot", background = "navy", solidHeader = TRUE, width = 15, 
                                            verbatimTextOutput("ch_text3")),
                                        
                                        fluidRow(
                                            box(selectInput(inputId = "col_name_ch",
                                                            label = "choose a column :",
                                                            choices = names(CH)[5:34]),width = 4)),
                                        
                                        box(background = "navy", 
                                            plotOutput("ch_plot_", height = 300),width = 13))),
                            
                            tabItem(tabName = "ch_model", background = "navy", 
                                    box(title = "Modeling result", background = "navy", solidHeader = TRUE,  width = 13, 
                                        verbatimTextOutput("ch_text4"),
                                        verbatimTextOutput("ch_text42")),
                                    
                                    fluidRow(
                                        box(title = 'Confusion Matrix',background = "navy",width = 12, 
                                            verbatimTextOutput('ch_model_')))),
                            
                            tabItem(tabName = "ch_imp",
                                    background = "navy",
                                    box(title = 'Feature Importance Top 20', background = "navy", solidHeader = TRUE, width=13,  
                                        verbatimTextOutput("ch_text5")),
                                    box(background = "navy",
                                        plotOutput("ch_imp_"))),
                            
                            #### Fourier model ####
                            tabItem(box(title = "Data set description", background = "navy", solidHeader = TRUE, width = 15, 
                                        verbatimTextOutput("fo_text1"),
                                        verbatimTextOutput("fo_text12")),
                                    box(title = "Data set", background = "navy", solidHeader = TRUE, width = 15, 
                                        verbatimTextOutput("fo_text2")),
                                    tabName = "fo_dataset", DT::dataTableOutput("fo_datasetTable")),
                            
                            tabItem(tabName = "fo_plot",
                                    fluidPage(
                                        box(title = "Feature Line Plot", background = "navy", solidHeader = TRUE, width = 15, 
                                            verbatimTextOutput("fo_text3")),
                                        
                                        fluidRow(
                                            box(selectInput(inputId = "col_name_fo",
                                                            label = "choose a column :",
                                                            choices = names(fft)[5:34]),width = 4)),
                                        
                                        box(background = "navy", 
                                            plotOutput("fo_plot_", height = 300),width = 13))),
                            
                            tabItem(tabName = "fo_model", background = "navy", 
                                    box(title = "Modeling result", background = "navy", solidHeader = TRUE,  width = 13, 
                                        verbatimTextOutput("fo_text4"),
                                        verbatimTextOutput("fo_text42")),
                                    
                                    fluidRow(
                                        box(title = 'Confusion Matrix',background = "navy",width = 12, 
                                            verbatimTextOutput('fo_model_')))),
                            
                            tabItem(tabName = "fo_imp",
                                    background = "navy",
                                    box(title = 'Feature Importance Top 20', background = "navy", solidHeader = TRUE, width=13,  
                                        verbatimTextOutput("fo_text5")),
                                    box(background = "navy",
                                        plotOutput("fo_imp_")))
                            
        )))



server<- function(input, output){
    
    #### Static model ####
    output$st_text1<- renderText({"- 원본 데이터에서 통계적 특징을 활용하여 도출된 변수들이 담긴 데이터셋입니다."})
    output$st_text12<- renderText({"- mean, min, max, sd, skewness, rms, rss, IQR, kurtosis 등의 특징들을 사용하였습니다."})
    output$st_text2<-renderText({"데이터셋은 살펴보세요!"})
    output$st_text3<- renderText({"각 변수별로 라인 그래프를 그려줍니다."})
    output$st_text4<- renderText({"- XGB 부스팅을 사용하여 모델링을 진행합니다."})
    output$st_text42<- renderText({"- 10-fold cross validation을 사용합니다."})
    output$st_text5<- renderText({"Gain(feature가 모델에 어느 정도 영향을 미쳤는가)값을 기준으로 중요한 Feature들을 파악합니다."})
    output$st_datasetTable <- renderDataTable({ST}, options = list(scrollX = TRUE))
    
    output$st_plot_ <- renderPlot({
        temp<- ST
        temp$idx<- 1:nrow(temp)
        
        ggplot(temp)+
            geom_line(aes_string(x="idx", y=input$col_name_st),col = "mediumseagreen", size=2)+
            ggtitle(paste0("Feature Name: ", input$col_name_st))+
            theme(title = element_text(size=15, hjust = 0.5))})

    output$st_model_<- renderPrint({
        st_conf})
    
    
    output$st_imp_<- renderPlot({
           st_im<- as.data.frame(xgb.importance(model = st_model)[1:20])
            ggplot(st_im,aes(x=reorder(Feature,Gain),y=Gain))+
                geom_bar(stat="identity",aes(fill=Feature))+
                coord_flip()})
    
    #### Peak model ####
    output$pk_text1<-renderText({"- 원본 데이터셋에서 피크 특징을 활용하여 도출된 변수들이 담긴 데이터셋입니다."}) 
    output$pk_text12<- renderText({"- 피크의 수, 피크의 최대값, 피크의 최솟값 등이 있습니다."})
    output$pk_text2<-renderText({"데이터셋은 살펴보세요!"})
    output$pk_text3<- renderText({"각 변수별로 라인 그래프를 그려줍니다."})
    output$pk_text_peak<- renderText({"피크 추출 예시입니다. 빨간색으로 표시된 부분이 피크를 의미합니다."})
    output$pk_text4<- renderText({"- XGB 부스팅을 사용하여 모델링을 진행합니다."})
    output$pk_text42<- renderText({"- 10-fold cross validation을 사용합니다."})
    output$pk_text5<- renderText({"Gain(feature가 모델에 어느 정도 영향을 미쳤는가)값을 기준으로 중요한 Feature들을 파악합니다."})
    output$pk_datasetTable <- renderDataTable({PK}, options = list(scrollX = TRUE))
    
    output$pk_plot_ <- renderPlot({
        temp2<- PK
        temp2$idx<- 1:nrow(temp2)
        
        ggplot(temp2)+
            geom_line(aes_string(x="idx", y=input$col_name_pk),col = "gold", size=2)+
            ggtitle(paste0("Feature Name: ", input$col_name_pk))+
            theme(title = element_text(size=15, hjust = 0.5))})
    
    output$pk_plot2_ <- renderPlot({
        p_ro<-findpeaks(f$magrotationRate,threshold = 4)
        plot(f$magrotationRate, type="l", col="gray")
        points(p_ro[,2], p_ro[,1], pch=20, col='maroon')})
    
    output$pk_model_<- renderPrint({
        Peak_conf})
    
    output$pk_imp_<- renderPlot({
        pk_im<- as.data.frame(xgb.importance(model = Peak_model)[1:20])
        ggplot(pk_im,aes(x=reorder(Feature,Gain),y=Gain))+
            geom_bar(stat="identity",aes(fill=Feature))+
            coord_flip()})
    
    #### Change Point model ####
    output$ch_text1<- renderText({"- 원본 데이터에서 변화 시점에 관한 특징을 활용하여 도출된 변수들이 담긴 데이터셋입니다."})
    output$ch_text12<- renderText({"- 변화가 나타나는 시점의 값에서 mean, min, max, std 통계값을 추출합니다."})
    output$ch_text2<-renderText({"데이터셋은 살펴보세요!"})
    output$ch_text3<- renderText({"각 변수별로 라인 그래프를 그려줍니다."})
    output$ch_text4<- renderText({"- XGB 부스팅을 사용하여 모델링을 진행합니다."})
    output$ch_text42<- renderText({"- 10-fold cross validation을 사용합니다."})
    output$ch_text5<- renderText({"Gain(feature가 모델에 어느 정도 영향을 미쳤는가)값을 기준으로 중요한 Feature들을 파악합니다."})
    output$ch_datasetTable <- renderDataTable({CH}, options = list(scrollX = TRUE))
    
    output$ch_plot_ <- renderPlot({
        temp3<- CH
        temp3$idx<- 1:nrow(temp3)
        
        ggplot(temp3)+
            geom_line(aes_string(x="idx", y=input$col_name_ch),col = "mediumpurple", size=2)+
            ggtitle(paste0("Feature Name: ", input$col_name_ch))+
            theme(title = element_text(size=15, hjust = 0.5))})
    
    output$ch_model_<- renderPrint({
        CH_conf})
    
    
    output$ch_imp_<- renderPlot({
        ch_im<- as.data.frame(xgb.importance(model = CH_model)[1:20])
        ggplot(ch_im,aes(x=reorder(Feature,Gain),y=Gain))+
            geom_bar(stat="identity",aes(fill=Feature))+
            coord_flip()})
    
    #### Fourier model ####
    output$fo_text1<- renderText({"- 원본 데이터에서 푸리에 변환 적용 후 통계적 특징을 활용하여 도출된 변수들이 담긴 데이터셋입니다."})
    output$fo_text12<- renderText({"- mean, min, max, sd, skewness, rms, rss, IQR, kurtosis 등의 특징들을 사용하였습니다."})
    output$fo_text2<-renderText({"데이터셋은 살펴보세요!"})
    output$fo_text3<- renderText({"각 변수별로 라인 그래프를 그려줍니다."})
    output$fo_text4<- renderText({"- XGB 부스팅을 사용하여 모델링을 진행합니다."})
    output$fo_text42<- renderText({"- 10-fold cross validation을 사용합니다."})
    output$fo_text5<- renderText({"Gain(feature가 모델에 어느 정도 영향을 미쳤는가)값을 기준으로 중요한 Feature들을 파악합니다."})
    output$fo_datasetTable <- renderDataTable({fft}, options = list(scrollX = TRUE))
    
    output$fo_plot_ <- renderPlot({
        temp4<- fft
        temp4$idx<- 1:nrow(temp4)
        
        ggplot(temp4)+
            geom_line(aes_string(x="idx", y=input$col_name_fo),col = "tomato", size=2)+
            ggtitle(paste0("Feature Name: ", input$col_name_fo))+
            theme(title = element_text(size=15, hjust = 0.5))})
    
    output$fo_model_<- renderPrint({
        fft_conf})
    
    output$fo_imp_<- renderPlot({
        fo_im<- as.data.frame(xgb.importance(model = fft_model)[1:20])
        ggplot(fo_im,aes(x=reorder(Feature,Gain),y=Gain))+
            geom_bar(stat="identity",aes(fill=Feature))+
            coord_flip()})
}


shinyApp(ui= ui, server = server)


