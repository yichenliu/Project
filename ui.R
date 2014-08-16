require(shiny)
require(markdown)

shinyUI(
  navbarPage("Impact of Severe Weather Events",
    tabPanel("About",
             mainPanel(includeMarkdown("about.md"))),
    tabPanel("Analysis",
             
      sidebarPanel(sliderInput("YEAR_INPUT", "Year Selection", min = 1996, max = 2011, value = c(1996, 2011), format = "####"),
      checkboxGroupInput("EVENT_INPUT", label = "Event Selection", 
                         choices = list("Drought" = "drought", "Flood" = "flood", "Hail" = "hail", "Heat" = "heat",
                                        "Hurricane" = "hurricane", "Lightning" = "lightning", "Rip" = "rip", "Storm" = "storm",
                                        "Tornado" = "tornado", "Other" = "other"), 
                         selected = c("drought", "flood", "hail", "heat", "hurricane", "lightning", "rip", "storm","tornado",
                                      "other")),
      
      checkboxGroupInput("REGION_INPUT", label = "Region Selection", 
                         choices = list("Central" = "central", "Mid-Atlantic" = "mid-atlantic", "North-East" = "northeast",
                         "North-West" = "northwest", "South" = "south", "South-East" = "southeast", "South-West" = "southwest"), 
                         selected = c("central", "mid-atlantic", "northeast", "northwest", "south", "southeast", "southwest")
                         )),

     mainPanel(tabsetPanel(
      tabPanel('Economic Impact',
        wellPanel(radioButtons("ECONOMIC_INPUT", "Economic Impact Measurement", 
                             c("Both" = "ECONOMIC", "Crop Damage" = "DAMAGE_CROP", "Property Damage" = "DAMAGE_PROPERTY"), 
                             inline = TRUE)),                                                     
        plotOutput("chart1"),
        plotOutput("chart2")),
      
      tabPanel('Population Impact',                                                    
        wellPanel(radioButtons("POPULATION_INPUT", "Population Impact Measurement",
                             c("Both" = "POPULATION", "Fatality Count" = "FATALITIES", "Injury Count" = "INJURIES"),
                             inline = TRUE)),
        plotOutput("chart3"),
        plotOutput("chart4")))))))          