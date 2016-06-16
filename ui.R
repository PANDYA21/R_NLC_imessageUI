library(shinydashboard)

header <- dashboardHeader(title = "IBM NLC service integration with R-shiny", titleWidth = "100%")

sidebar <- dashboardSidebar(disable = T)

body <- dashboardBody(
  fluidRow(
    column(6, align="center", offset = 0,
           h3("Enter details about you and your tennis experience."),
           # textInput("txt1", "Input text", width = "67%"),
           tags$textarea(id="txt1", rows=3, cols=30, ""),
           submitButton(text = "Submit to Watson")), #,
           # br(),
    column(6, align="center", offset = 0, 
           h3("Your profile:"),
           DT::dataTableOutput("tbl1")),
    br(),
    column(12, align="center", offset = 0,
           textOutput("txt.out1"))
  ),
  tags$head(tags$style("#txt1{color: black;
                                 font-size: 24px;
                                 height: 100px;
                                 }"
  )
  )
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

