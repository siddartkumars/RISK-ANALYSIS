
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
jscode <- "
shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.unbind('click.tab');
tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
background-color: #aaa !important;
color: #333 !important;
cursor: not-allowed !important;
border-color: #aaa !important;
}"




library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyjs)

sample <- read.csv('sample.csv',stringsAsFactors = F)


shinyUI(navbarPage("Predictive System",shinyjs::useShinyjs(),theme = shinytheme("flatly"),
                   tags$head(tags$script(src = "message-handler.js")),
                   
                   useShinyjs(),
                   extendShinyjs(text = jscode,functions = c("disableTab","enableTab")),
                   inlineCSS(css),
                   
                   #tabPanel("Introduction",h1("Welcome to ShinyApps")),
                   
                  
                              
                              tabPanel('CSV Upload',sidebarLayout(sidebarPanel(
                                fileInput('file1', 'Choose file to upload',
                                          accept = c(
                                            'text/csv',
                                            'text/comma-separated-values',
                                            'text/tab-separated-values',
                                            'text/plain',
                                            '.csv',
                                            '.tsv'
                                          )
                                ),
                                tags$hr(),
                                checkboxInput('header', 'Header', TRUE),
                                radioButtons('sep', 'Separator',
                                             c(Comma=',',
                                               Semicolon=';',
                                               Tab='\t'),
                                             ','),
                                radioButtons('quote', 'Quote',
                                             c(None='',
                                               'Double Quote'='"',
                                               'Single Quote'="'"),
                                             '"'),
                                tags$hr()
                              ),
                              
                              mainPanel(
                                tableOutput('contents'),helpText("Note: After pressing the Submit button please wait unitl an alert message appears before going to adjacent tabs"),actionButton("submit1","Submit")
                              ))),
                   
                   
                   tabPanel("Result", id='tab1',
                            mainPanel(dataTableOutput("table1"))),
                   tabPanel("Tabulation",id='tab2',sidebarLayout(sidebarPanel(),
                                                                 mainPanel(dataTableOutput("table2"), downloadButton('downloadData', 'Download')))),
                   tabPanel("Visualization",sidebarLayout(sidebarPanel(selectInput('xcol','X Variable',names(sample[2:17])),
                                                              sliderInput("job", "Job Type",
                                                                          1, 12,value=c(0,12), step = 1),
                                                              sliderInput("age", "Age Range",
                                                                          18, 95,value=c(18,95), step = 1),
                                                              sliderInput("marriage", "Marital Status",
                                                                          1, 3,value=c(1,3), step = 1),
                                                              sliderInput("edu", "Education",
                                                                          1, 4,value=c(1,4), step = 1),
                                                              sliderInput("contact", "Type of Contact",
                                                                          1, 3,value=c(1,3), step = 1),
                                                              selectInput('ycol','Y Variable',names(sample[18])),
                                                              radioButtons("plot_type","Plot Type",c("barplot")),
                                                              sliderInput('range','Numeric Range',min=10,max=100,value=50)
                   ),
                   
                   
                   mainPanel(
                     column(width=12,class="well",
                            fluidRow(column(width=6,
                                            
                                            plotOutput("plot1",height=300,click = clickOpts(id="plot_click"),
                                                       dblclick=dblclickOpts(id="plot_dblclick"),
                                                       hover = hoverOpts(id="plot_hover"),
                                                       brush = brushOpts(id="plot_brush",resetOnNew=TRUE))),
                                     column(width=6,
                                            plotOutput("plot2",height=300)))),
                     
                     fluidRow(
                       column(width=6, verbatimTextOutput("click_info")),
                       
                       column(width=6, verbatimTextOutput("brush_info"))
                     )))
                   
                   )
))
