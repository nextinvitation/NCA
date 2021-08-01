library(shiny)
library(ggiraph)
library(shinydashboard)
library(DT)
# define user interface

#' 管理整合project，master端
#' ui任务，1. liu data& tasks 界面
#'         2. wei results界面
#'         3. wang plots界面
shinyUI(dashboardPage(
  #设置页面布局格式
  dashboardHeader(
    title = 'NCA分析工具',titleWidth=150
  ),
  dashboardSidebar(
    width = 150,
    sidebarMenu(
      menuItem('数据',tabName = 'data'),
      menuItem('任务',tabName = 'Tasks'),
      menuItem('结果',tabName = 'Results'),
      menuItem('作图',tabName = 'Plots')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'data',
        fluidRow(
          box(
            title = 'Data Files 数据输入',
            status = 'primary',
            solidHeader = TRUE,
            fileInput('file1','conc file 浓度数据'),
            fileInput('file2','dose file 给药数据'),
            width = 12,
          ),
          tags$hr(),
          box(
            title='Data Table 数据展示',
            status = 'primary',
            solidHeader = TRUE,
            width = 12,
            DT::DTOutput('data')
          ),
        )
      ),
      tabItem(
        tabName = 'Tasks',
        fluidPage(
          tabsetPanel(
            tabPanel(
              'CHECK𝜆𝑧',
              fluidRow(
                box(
                  title = 'Check lambda',
                  width = 12,
                  status = 'primary',
                  solidHeader = TRUE,
                  box(
                    title = 'subject 4',
                    plotOutput('plot_sub_4'),
                    height = 250
                  ),
                  box(
                    title = 'subject 5',
                    plotOutput('plot_sub_5'),
                    height = 250
                  ),
                  box(
                    title = 'subject 6',
                    plotOutput('plot_sub_6'),
                    height = 250
                  )
                )
              )
            ),
            tabPanel(
              "task",
              fluidRow(
                #actionButton("taskRun", "NON COMPARTMENTAL ANALYSIS",style='color:white;border:2px;background-color:blue;boder-color:black; margin: .2rem auto 0'),
                # box(
                #   #title = 'Non compartmental analysis',
                #   width = 12,
                #   actionButton("taskRun", "RUN",class='btn-primary',style='color:white')
                # ),
                box(
                  title = 'Data information 数据信息',
                  width = 12,
                  solidHeader = TRUE,
                  status = 'primary',
                  fluidRow(
                    box(
                      title = 'Administration type 类型设置',
                      radioButtons('method',NULL,c('Extravascular'=0,
                                               'iv bolus'=1,
                                               'iv infusion'=2,
                                               'urine'=3,
                                               'steady state'=4)),
                      height=275
                    ),
                    box(
                      title = 'Units 单位',
                      textInput('time','Time','Please enter the unit'),
                      textInput('conc','Conc','Please enter the unit'),
                      textInput('amt','AMT','Please enter the unit'),
                      # numericInput('time',h6('TIME'),1,min = 0,step = 1),
                      # numericInput('conc',h6('CONC'),1,min = 0,step = 1),
                      # numericInput('amt',h6('AMT'),1,min = 0,step = 1),
                      height=275
                    ),
                    box(
                      title = 'parameter to compute 参数选择',
                      checkboxGroupInput('parameters',NULL,inline=TRUE,c('auclast'='auclast',
                                                             'aumclast'='aumclast',
                                                             'lambda_Z'='lambda_Z',
                                                             'rsq'='rsq',
                                                             'adj_rsq'='adj_rsq',
                                                             'lambda_z_n_points'='lambda_z_n_points',
                                                             'half_life'='half_life',
                                                             'span'='span',
                                                             'cmax'='cmax',
                                                             'tmax'='tmax',
                                                             'clast'='clast',
                                                             'lambda_z_intercept'='lambda_z_intercept',
                                                             'clast_pred'='clast_pred',
                                                             'tlast'='tlast',
                                                             'aucinf_obs'='aucinf_obs')),
                      height = 185
                    ),
                    box(
                      title='Integral Method 整合方法',
                      selectInput('auc_calc_type',NULL,c('Linear trapezoidal linear'=0,
                                                'Linear log trapezoidal'=1,
                                                'Linear up log down'=2,
                                                'Linear trapezoidal linear/log'=3)),
                      width = 6,
                      height=185
                    )
                  ),
                  actionButton("taskRun", "RUN",class='btn-primary',style='color:white')
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = 'Results',tableOutput("contents"),tableOutput("summary"),downloadButton("report", "Generate report")
      ),
      tabItem(
        tabName = 'Plots',plotOutput("plot",
                                     click = "plot_click",
                                     dblclick = "plot_dblclick",
                                     hover = "plot_hover",
                                     brush = "plot_brush"),
        verbatimTextOutput("info")
      )
    )
  )
))




