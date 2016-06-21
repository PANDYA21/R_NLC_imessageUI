library(shiny)
library(shinydashboard)
source("udfsB.R")
source("udfs.R")

# the IBM NLC
classifier <<- "2373f5x67-nlc-3778"

# ui
header <- dashboardHeader(title = "IBM NLC service integration with R-shiny", titleWidth = "100%")

sidebar <- dashboardSidebar(disable = T)

body <- dashboardBody(includeCSS("imessage.css"), 
  fluidRow(
    column(12, align="center", offset = 0,
           h3("Enter details about you and your tennis experience.")),
    column(7, align="left", offset = 1, 
           column(12, align="left", offset = 0, uiOutput("chatO")),
           wellPanel(style = "background-color: #ffffff; ",
                     fluidRow(
                       column(10, align="center", offset = 0,
                              # textInput as HTML
                              tags$div(HTML('<div class="form-group shiny-input-container" style="width: 100%;">
                                            <label for="txt1"></label>
                                            <input id="txt1" type="text" class="form-control" value=""/>
                                            </div>'))),
                       column(2, align="center", offset = 0,
                              # Send button
                              tags$div(HTML('<button id="send" type="button" class="btn btn-default action-button isend">Send</button>'))
                              )
                       )
                     )
           ),
    column(3, align="center", offset = 0, 
           h3("Your profile:"),
           DT::dataTableOutput("tbl1")),
    column(1, align="center", offset = 0)
  ),
  tags$head(tags$style("#txt1{color: black;
                       font-size: 24px;
                       }"
  )),
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


## server
server <- function(input, output, session){
  # delete the temp files on each session end
  session$onSessionEnded(function() {
    if(file.exists(session$token)){
      file.remove(session$token)
    }
    if(file.exists(paste0(session$token, "_usr_inputs.csv"))){
      file.remove(paste0(session$token, "_usr_inputs.csv"))
    }
  })
  
  # observe the send button to chat
  output$chatO <- renderUI({
    btn.val <- input$send
    if(btn.val < 1){
      # initialize
      first.msg <- "Guide me through your details and I will recommend you some rackets..."
      chatt <- HTML(c(as.character(br()),
                      as.character(p(first.msg, 
                                   align = "right", class = "me"))))
      # write a temp file for chat history
      writeLines(chatt, session$token) 
      # write another temp csv file for user inputs
      usr.fil <- paste0(session$token, "_usr_inputs.csv")
      usr.data <- data.frame(x1 = "", x2 = "", stringsAsFactors = F)
      write.table(usr.data, usr.fil, sep = ",", dec = ".", col.names = T, row.names = F)
      return(chatt)
      # end of initialization
    } else {
      in.txt1 <- isolate(input$txt1)
      if(in.txt1 == ""){
        chatt <- readLines(session$token)
        return(HTML(chatt))
      } else {
        chatt <- readLines(session$token)
        # do not process if same text
        last.msg <- tail(chatt[grep("class=\"them\"", chatt)], 1)
        last.msg <- gsub("<p align=\"left\" class=\"them\">", "", last.msg)
        last.msg <- gsub("</p>", "", last.msg)
        if(identical(in.txt1, last.msg)){
          # do nothing
          return(HTML(chatt))
        } 
        # send query to NLC since input changed
        ress <- watson.nlc.processtextreturnclass(classifier, in.txt1)
        if(as.numeric(as.character(ress$confidence[1])) < 0.5){
          reply.txt <- "I did not understand..."
        } else {
          reply.txt <- ress$class[1]
          # write user profile 
          usr.fil <- paste0(session$token, "_usr_inputs.csv")
          usr.data <- data.frame(x1 = in.txt1, x2 = reply.txt)
          # writeLines(usr.data, usr.fil)
          write.table(usr.data, usr.fil, append = T, 
                      sep = ",", dec = ".", 
                      row.names = F, col.names = F)
          #
        }
        chatt <- c(chatt,
                   as.character(p(in.txt1, align = "left", class = "them")),
                   as.character(p(reply.txt, 
                                  align = "right", class = "me")))
        writeLines(chatt, session$token)
        
        # auto remove text from the inout filed when message is sent
        updateTextInput(session, "txt1", value = "")
        return(HTML(chatt))
      }
    }
  })
  
  observe({
    btn.val <- input$send
    if(btn.val > 0){
      output$tbl1 <- DT::renderDataTable({
        usr.fil <- paste0(session$token, "_usr_inputs.csv")
        return(convertUserProfile(read.csv(usr.fil)))
        # return(read.csv(usr.fil))
      }, rownames = FALSE, selection = "none", 
      options = list(pageLength = 15, lengthChange = F, searching = F, paging = F, info = F))
    }
  })

}

shinyApp(ui, server)
