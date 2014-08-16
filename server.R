require(shiny)
require(markdown)

dt <- read.csv('Data/data.csv', stringsAsFactors = FALSE)
dt$EVENT_TYPE <- tolower(dt$EVENT_TYPE)
dt$REGION <- tolower(dt$REGION)
dt$POPULATION <- dt$FATALITIES + dt$INJURIES
dt$ECONOMIC <- dt$DAMAGE_PROPERTY + dt$DAMAGE_CROP

shinyServer(function(input, output){
    chart_economic_1 <- reactive({
       dt1 <- dt[dt$YEAR >= input$YEAR_INPUT[1] & dt$YEAR <= input$YEAR_INPUT[2],]
       if(is.null(input$REGION_INPUT)) stop("Please select regions") else {
         if(is.null(input$EVENT_INPUT)) stop("Please select events") else {
           dt2 <- dt1[dt1$REGION %in% input$REGION_INPUT,]
           dt3 <- dt2[dt2$EVENT_TYPE %in% input$EVENT_INPUT,]
           measurement <- match(input$ECONOMIC_INPUT, names(dt3))
            
           economic <- aggregate(dt3[,measurement], by = list(dt3$EVENT_TYPE), FUN = sum)
           economic <- economic[order(economic[,2]), ]
           economic[,3] <- economic[,2]/1000000000
           economic <- economic[,c(1,3)]}}})
    
    chart_economic_2 <- reactive({
      dt1 <- dt[dt$YEAR >= input$YEAR_INPUT[1] & dt$YEAR <= input$YEAR_INPUT[2],]
      if(is.null(input$REGION_INPUT)) stop("Please select regions") else {
        if(is.null(input$EVENT_INPUT)) stop("Please select events") else {
          dt2 <- dt1[dt1$REGION %in% input$REGION_INPUT,]
          dt3 <- dt2[dt2$EVENT_TYPE %in% input$EVENT_INPUT,]
          measurement <- match(input$ECONOMIC_INPUT, names(dt3))
          
          economic <- aggregate(dt3[,measurement], by = list(dt3$REGION), FUN = sum)
          economic <- economic[order(economic[,2]), ]
          economic[,3] <- economic[,2]/1000000000
          economic <- economic[,c(1,3)]}}})
    
    chart_economic_3 <- reactive({
      dt1 <- dt[dt$YEAR >= input$YEAR_INPUT[1] & dt$YEAR <= input$YEAR_INPUT[2],]
      if(is.null(input$REGION_INPUT)) stop("Please select regions") else {
        if(is.null(input$EVENT_INPUT)) stop("Please select events") else {
          dt2 <- dt1[dt1$REGION %in% input$REGION_INPUT,]
          dt3 <- dt2[dt2$EVENT_TYPE %in% input$EVENT_INPUT,]
          measurement <- match(input$ECONOMIC_INPUT, names(dt3))
          
          economic <- aggregate(dt3[,measurement], by = list(dt3$YEAR), FUN = sum)
          economic <- economic[order(economic[,1]), ]
          economic[,3] <- economic[,2]/1000000000
          economic <- economic[,c(1,3)]}}})
    
    chart_population_1 <- reactive({
      dt1 <- dt[dt$YEAR >= input$YEAR_INPUT[1] & dt$YEAR <= input$YEAR_INPUT[2],]
      if(is.null(input$REGION_INPUT)) stop("Please select regions") else {
        if(is.null(input$EVENT_INPUT)) stop("Please select events") else {
          dt2 <- dt1[dt1$REGION %in% input$REGION_INPUT,]
          dt3 <- dt2[dt2$EVENT_TYPE %in% input$EVENT_INPUT,]
          measurement <- match(input$POPULATION_INPUT, names(dt3))
          
          economic <- aggregate(dt3[,measurement], by = list(dt3$EVENT_TYPE), FUN = sum)
          economic <- economic[order(economic[,2]), ]}}})
    
    chart_population_2 <- reactive({
      dt1 <- dt[dt$YEAR >= input$YEAR_INPUT[1] & dt$YEAR <= input$YEAR_INPUT[2],]
      if(is.null(input$REGION_INPUT)) stop("Please select regions") else {
        if(is.null(input$EVENT_INPUT)) stop("Please select events") else {
          dt2 <- dt1[dt1$REGION %in% input$REGION_INPUT,]
          dt3 <- dt2[dt2$EVENT_TYPE %in% input$EVENT_INPUT,]
          measurement <- match(input$POPULATION_INPUT, names(dt3))
          
          economic <- aggregate(dt3[,measurement], by = list(dt3$REGION), FUN = sum)
          economic <- economic[order(economic[,2]), ]}}})
    
    chart_population_3 <- reactive({
      dt1 <- dt[dt$YEAR >= input$YEAR_INPUT[1] & dt$YEAR <= input$YEAR_INPUT[2],]
      if(is.null(input$REGION_INPUT)) stop("Please select regions") else {
        if(is.null(input$EVENT_INPUT)) stop("Please select events") else {
          dt2 <- dt1[dt1$REGION %in% input$REGION_INPUT,]
          dt3 <- dt2[dt2$EVENT_TYPE %in% input$EVENT_INPUT,]
          measurement <- match(input$POPULATION_INPUT, names(dt3))
          
          economic <- aggregate(dt3[,measurement], by = list(dt3$YEAR), FUN = sum)
          economic <- economic[order(economic[,1]), ]}}})
    
    output$chart1 <- renderPlot({
      data1 <- chart_economic_1()
      data2 <- chart_economic_2()
      par(bg = rgb(248, 248, 248, max = 255), mfcol = c(1, 2), mar = c(4, 5, 2, 2))
      barplot(data1[,2], names.arg = data1[,1], horiz = TRUE, col = "steelblue", las =1, main = "Economic Damage by Event")
      mtext(side = 1, "Damages in US$ Billions", line = 2.2)
      barplot(data2[,2], names.arg = data2[,1], horiz = TRUE, col = "steelblue", las =1, main = "Economic Damage by Region")
      mtext(side = 1, "Damages in US$ Billions", line = 2.2)
      box(which = "outer", lty = "solid", col = rgb(204, 204, 204, max = 255))})
    
    output$chart2 <- renderPlot({
      data3 <- chart_economic_3()
      par(bg = rgb(248, 248, 248, max = 255), mar = c(4, 5, 2, 2))
      barplot(data3[,2], names.arg = data3[,1], col = "steelblue",las=1, main = "Economic Damage by Year")
      mtext(side = 2, "Damages in US$ Billions", line = 3)
      box(which = "outer", lty = "solid", col = rgb(204, 204, 204, max = 255))})
    
    output$chart3 <- renderPlot({
      data1 <- chart_population_1()
      data2 <- chart_population_2()
      par(bg = rgb(248, 248, 248, max = 255), mfcol = c(1, 2), mar = c(4, 5, 2, 2))
      barplot(data1[,2], names.arg = data1[,1], horiz = TRUE, col = "steelblue", las =1, main = "Population Impact by Event")
      mtext(side = 1, "Population Impact Counts", line = 2.2)
      barplot(data2[,2], names.arg = data2[,1], horiz = TRUE, col = "steelblue", las =1, main = "Population Impact by Region")
      mtext(side = 1, "Population Impact Counts", line = 2.2)
      box(which = "outer", lty = "solid", col = rgb(204, 204, 204, max = 255))})
    
    output$chart4 <- renderPlot({
      data3 <- chart_population_3()
      par(bg = rgb(248, 248, 248, max = 255), mar = c(4, 5, 2, 2))
      barplot(data3[,2], names.arg = data3[,1], col = "steelblue",las=1, main = "Population Impact by Year")
      mtext(side = 2, "Population Impact Counts", line = 3.5)
      box(which = "outer", lty = "solid", col = rgb(204, 204, 204, max = 255))})
})



