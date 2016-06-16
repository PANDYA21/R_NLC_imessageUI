library(shinydashboard)
source("udfs.R")
source("udfsB.R")

# the IBM NLC
classifier <<- "2373f5x67-nlc-3778"

# server script
server <- shinyServer(function(input, output, session) {
  
  getProfileReact <- reactive({
    in.txt <- tolower(as.character(input$txt1))
    return(getProfile(classifier, in.txt))
  })
  
  output$tbl1 <- DT::renderDataTable({
    in.txt <- tolower(as.character(input$txt1))
      if(in.txt != ""){
        return(getProfileReact())
      }
  }, rownames = FALSE, # selection = "single", 
  options = list(pageLength = 15, lengthChange = F, searching = F, paging = F, info = F))
  
  output$txt.out1 <- renderText({
    ans.df2 <- getProfileReact()
    if(length(which(ans.df2$value == "")) < 6){
      return("Some fields are missing...")
    } else if(length(which(ans.df2$value == "")) == 6){ 
      return("Nothing recognised...")
    } else {
      return("All complete!")
    }
  })
})
