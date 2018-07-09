library(shiny)
library(ggvis)

sharktank<-read.csv("sharktank.csv", sep = ";", stringsAsFactors = FALSE)
sharktank$gender<-as.factor(sharktank$gender)
sharktank$gender<-as.numeric(sharktank$gender)

runApp(list(ui = pageWithSidebar(
  div(),
  sidebarPanel(
    h3('Select a maximum deal amount to see all those deals made under that amount'),
    sliderInput("n", "Deal amount (in thousands)", min = min(sharktank$deal_amount), max = max(sharktank$deal_amount),
                 value = mean(sharktank$deal_amount), step = 10),
    br(),
    h3('Interested in deals from a particular Season?'),
    selectInput('var', 'Select the season', choices = c("All" = 0, "1" = 1, "2" = 2, "3" = 3), selected = 0),

    br(),
    h3('You can also select by gender of the presenters.'),
    radioButtons("gend", "Select the gender", choices = c("All" = 0, "Female" = 1, "Male" = 2, "Mixed Team"= 3), selected = 0),
    
    uiOutput("plot_ui")
  ),
  mainPanel(
    h3("The sharkTank-shinyApp helps to visualize the deals that have been made on the Tv show, Shark Tank."),
    ggvisOutput("plot")
  )
)
, server= function(input, output, session) {
  mtc <- reactive({
    amnt = as.numeric(input$n)
    seas = as.numeric(input$var)
    gend = as.numeric(input$gend)
    if (seas == 0 & gend == 0){
      df = subset(sharktank, deal_amount < amnt)
    } else if (seas == 0){
      df = subset(sharktank, deal_amount < amnt & gender == gend)
    } else if (gend == 0) {
      df = subset(sharktank, deal_amount < amnt & Season == seas)
    } else {
      df = subset(sharktank, deal_amount < amnt & Season == seas & gender == gend)
    }
    df$long = as.character(paste0("Company: ",df$Name,"<br>", " Valuation: ",df$deal_eval, "K"))
    df
  })
  
  # Creating the visualization
  mtc %>%
    ggvis(~deal_equity, ~deal_amount, key:= ~long) %>% 
    layer_points(fill = ~factor(Industry)) %>% 
    add_tooltip(function(df){
      paste0(as.character(df$long), "<br>", "Deal amount: ", df$deal_amount,"K", "<br>", "Equity: ", df$deal_equity,"%")
    }, "hover") %>%
    bind_shiny("plot", "plot_ui") 
  
})
)

