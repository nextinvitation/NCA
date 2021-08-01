library(shiny)
#library(PKNCA)
library(ggiraph)
library(ggplot2)
library(Gingko)
library(DT)


shinyServer(function(input, output,session) {
  conc_sub_4<-reactiveValues()
  conc_sub_5<-reactiveValues()
  conc_sub_6<-reactiveValues()
  infile1<-reactive({input$file1})
  infile2<-reactive({input$file2})
  conc<-reactive({read.csv(infile1()$datapath)})
  dose<-reactive({read.csv(infile2()$datapath)})
  #method<-reactive(as.character(unlist(strsplit(infile1()$name,"."))))

  output$data<-renderDT({
    # inFile1 <- input$file1
    # iptt<-input$tex
    # if (is.null(inFile1))
    #   return(NULL)
    # read.csv(inFile1$datapath)
    # print(conc)
    conc()
  })

  observeEvent(input$taskRun,{
    #print(as.numeric(input$method))
    #print(switch(as.numeric(input$method)+1,'Extravascular','iv bolus','iv infusion','steady state'))
    conc_sub<-subset(conc(),Route==switch(as.numeric(input$method)+1,'Extravascular','iv bolus','iv infusion','steady state'))
    conc_sub_4$values<-subset(conc_sub,Subject=='4')
    conc_sub_5$values<-subset(conc_sub,Subject=='5')
    conc_sub_6$values<-subset(conc_sub,Subject=='6')
    #print(conc_sub_4$values)
  })

  updated_parameters<-eventReactive(input$taskRun,input$parameters)
  updated_auc_calc_type<-eventReactive(input$taskRun,input$auc_calc_type)

  output$plot_sub_4<-renderPlot({
    plot(conc_sub_4$values$Time,conc_sub_4$values$Conc)
    #plot(conc_sub_4$values$Time[6,],conc_sub_4$values$Conc)
    #ggplot(conc_sub_4$values,aes(x=Time,y=Conc))+geom_line()
  },height=200)

  output$plot_sub_5<-renderPlot({
    plot(conc_sub_5$values$Time,conc_sub_5$values$Conc)
    #ggplot(conc_sub_5$values,aes(x=Time,y=Conc))+geom_line()
  },height=200)

  output$plot_sub_6<-renderPlot({
    plot(conc_sub_6$values$Time,conc_sub_6$values$Conc)
    #ggplot(conc_sub_6$values,aes(x=Time,y=Conc))+geom_line()
  },height = 200)


  output$contents <- renderTable({
    inFile1 <- input$file1
    inFile2 <- input$file2
    if (is.null(inFile1)|is.null(inFile2))
      return(NULL)

    concdata<- read.csv(inFile1$datapath)
    dosedata<- read.csv(inFile2$datapath)

    print(updated_parameters())

    if(as.numeric(input$method)==0){
      inputdata_linear = Calculate_ev(concdata, dosedata,auc_calc_type=updated_auc_calc_type())
    }
    if(as.numeric(input$method)==1){
      inputdata_linear = Calculate_iv_bolus(concdata, dosedata,auc_calc_type=updated_auc_calc_type())
    }
    if(as.numeric(input$method)==2){
      inputdata_linear = Calculate_iv_infusion(concdata, dosedata,auc_calc_type=updated_auc_calc_type())
    }
    if(as.numeric(input$method)==3){
      inputdata_linear = calc_urine(concdata, dosedata,auc_calc_type=updated_auc_calc_type())
    }
    if(as.numeric(input$method)==4){
      inputdata_linear = calc_steady_state(concdata, dosedata,auc_calc_type=updated_auc_calc_type())
    }

    final_result <<-inputdata_linear$result
    #print(class(input$parameters))
    #指定访问某几列的数据
    #table<-final_result[input$parameters]
    table<-final_result[updated_parameters()]
    #subset(inputdata_linear$result)
  })
  output$summary <- renderTable({

    colMax <- function(data) sapply(data, max, na.rm = TRUE)
    colMin <- function(data) sapply(data, min, na.rm = TRUE)
    colMed <- function(data) sapply(data, median, na.rm = TRUE)
    colSD <- function(data) sapply(data, sd, na.rm = TRUE)
    colVar <- function(data) sapply(data, var, na.rm = TRUE)
    df<-data.frame(
      Mean=colMeans(final_result),
      Max=colMax(final_result),
      Min=colMin(final_result),
      Med = colMed(final_result),
      SD = colSD(final_result),
      Var = colVar(final_result)

    )
    stat_data <<- df[-c(1),]
  })

  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(n = final_result,stat = stat_data)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  output$plot <- renderPlot({
    inFile1 <- input$file1
    inFile2 <- input$file2
    if (is.null(inFile1)|is.null(inFile2))
      return(NULL)
    conc<- read.csv(inFile1$datapath)
    print(conc)

    #plot(conc$Time, conc$conc,type = "o",xlab = "time",ylab="conc")
    ggplot(conc, aes(x=Time, y=Conc, group=Subject))+ geom_line()
  })












####################可能会用的代码1##################
  # output$plot_sub_4<-renderPlot({
  #   pushTime<-input$taskRun
  #   if(pushTime > 0)
  #     inFile1 <- input$file1
  #     conc<-read.csv(inFile1$datapath)
  #     conc_sub<-subset(conc,Route==input$type)
  #     conc_sub_4<-subset(conc_sub,Subject=='4')
  #     print(conc_sub_4)
  #     plot(conc_sub_4$Time,conc_sub_4$Conc)
  #     pushTime<-0
  # })

  # output$plot_sub_5<-renderPlot({
  #   if(input$taskRun > 0)
  #     inFile1 <- input$file1
  #     conc<-read.csv(inFile1$datapath)
  #     conc_sub<-subset(conc,Route==input$type)
  #     conc_sub_5<-subset(conc_sub,Subject=='5')
  #     ggplot(conc_sub_5,aes(x=Time,y=Conc))+geom_line()
  # })
  #
  # output$plot_sub_6<-renderPlot({
  #   if(input$taskRun > 0)
  #     inFile1 <- input$file1
  #     conc<-read.csv(inFile1$datapath)
  #     conc_sub<-subset(conc,Route==input$type)
  #     conc_sub_6<-subset(conc_sub,Subject=='6')
  #     ggplot(conc_sub_6,aes(x=Time,y=Conc))+geom_line()
  # })
})

######################可能会用到的代码2#######################
# observe({
#   update_checkbox<-input$parameters
#   if(is.null(update_checkbox))
#     update_checkbox<-character(0)
#   updateCheckboxGroupInput(session,'parameters',NULL,inline=TRUE,choices=list('auclast',
#                                                                               'aumclast',
#                                                                               'lambda_Z',
#                                                                               'rsq',
#                                                                               'adj_rsq',
#                                                                               'lambda_z_npoints',
#                                                                               'half_life',
#                                                                               'span',
#                                                                               'cmax',
#                                                                               'tmax',
#                                                                               'clast',
#                                                                               'lambda_z_intercept',
#                                                                               'clast_pred',
#                                                                               'tlast',
#                                                                               'aucinf_obs'),
#                            selected=update_checkbox)
#
#   update_selectInput<-input$auc_calc_type
#   updateSelectInput(session,'auc_calc_type',NULL,c('Linear trapezoidal linear'=0,
#                                               'Linear log trapezoidal'=1,
#                                               'Linear up log down'=2,
#                                               'Linear trapezoidal linear/log'=3),
#                     selected = update_selectInput)
# })

