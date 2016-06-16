library(shinydashboard)
# chatt <- character(0)
header <- dashboardHeader(title = "IBM NLC service integration with R-shiny", titleWidth = "100%")

sidebar <- dashboardSidebar(disable = T)

body <- dashboardBody(
  fluidRow(
    column(12, align="center", offset = 0,
           h3("Enter details about you and your tennis experience.")),
    column(2, align="right", offset = 0),
    column(8, align="left", offset = 0,
           uiOutput("chatO")),
    column(2, align="right", offset = 0),
    br(),
    column(12, align="center", offset = 0,
           textInput("txt1", "Input text", width = "67%"),
           actionButton("send", "Send")) #,
    # # br(),
    # column(6, align="center", offset = 0, 
    #        h3("Your profile:"),
    #        DT::dataTableOutput("tbl1")),
    # br(),
    # column(12, align="center", offset = 0,
    #        textOutput("txt.out1"))
  ),
  tags$head(tags$style("#txt1{color: black;
                       font-size: 24px;
                       }"
  )),
  includeCSS("imessage.css"), 
  includeScript("sendOnEnter.js")
  )


### the UI html
ui <- dashboardPage(header, sidebar, body,
                    tags$head(tags$style(HTML('
                                              /* logo */
                                              .skin-blue .main-header .logo {
                                              background-color: #000000;
                                              }
                                              
                                              /* logo when hovered */
                                              .skin-blue .main-header .logo:hover {
                                              background-color: #888888;
                                              }
                                              
                                              /* navbar (rest of the header) */
                                              .skin-blue .main-header .navbar {
                                              background-color: #000001;
                                              }        
                                              
                                              .content-wrapper,
                                              .right-side {
                                              background-color: #ffffff;
                                              }
                                              
                                              '))))

server <- function(input, output, session){
  session$onSessionEnded(function() {
    if(file.exists(session$token)){
      file.remove(session$token)
    }
  })
  
  observe({
    btn.val <- input$send
    if(btn.val < 1){
      # initialize
      chatt <- character(0)
      writeLines(chatt, session$token)
      return()
    } else {
      output$chatO <- renderUI({
        in.txt1 <- isolate(input$txt1)
        if(in.txt1 == ""){
          return()
        } else {
          chatt <- readLines(session$token)
          chatt <- c(chatt,
                      as.character(p(in.txt1, align = "left", class = "them")),
                      # as.character(br()),
                      as.character(p(watson.nlc.processtextreturnclass(classifier, in.txt1)$class[1], 
                                     align = "right", class = "me")))
          writeLines(chatt, session$token)
          return(HTML(chatt))
        }
      })
    }
  })
  

}

shinyApp(ui, server)
