#Loading Required Libraries

library(shiny)
library(shinydashboard)
library(leaflet)
library(DBI)
library(odbc)
library(DT)
library(tidyverse)
library(dplyr)
require("RColorBrewer")


# Reading database credentials
source("./credentials_v4.R")


ui <- dashboardPage( skin = 'purple',
  dashboardHeader(title = "Sales Dashboard", titleWidth  = 200 ),
  #Sidebar content
  dashboardSidebar( width = 200,
    #sidebar menus 
    sidebarMenu(
      menuItem("Description", tabName = "Welcome", icon = icon("fa-sharp fa-solid fa-magnifying-glass")),
      menuItem("Add new sales", tabName = "newsales", icon = icon("fa-solid fa-handshake")),
      menuItem("Market Analytics", tabName = "Market_Analytics", icon = icon("fa-solid fa-location-dot")),
      menuItem("Performance tracker", tabName = "Perf", icon = icon("fa-solid fa-street-view")),
      menuItem("Incentive Tracking", tabName = "Incentives", icon = icon("fa-solid fa-sack-dollar")),
      menuItem("Delete Sales", tabName = "DelSales", icon = icon("fa-solid fa-handshake-slash"))
  )),
  
  dashboardBody(
    tabItems(
      #Contents for first tab
      tabItem(tabName = "Welcome",
              h2("Real time performance tracking dashboard!"),
              h3(""),
              h3("Basic Usage Instructions :"),
              h1(""),
              h4("Add new sales"),
              h5("1. Enter all details to add a sale"),
              h5("2. Estimate customer income if not certainly known"),
              h5("3. To be filled strictly by Sales Associates only"),
              h3(""),
              h4("Market and customer analytics"),
              h5("1. Map to help you target prospects from specific regions"),
              h5("2. Customers divided by Income Levels, Occupation and Family members"),
              h3(""),
              h4('Performance and inputs Tracker'),
              h5('1. Compare your input and output metrics with the top sales representatives'),
              h5('2. Use this to understand areas of improvements'),
              h4("Incentive  Tracker"),
              h5("1. Track your current incentives made for the month"),
              h5("2. Check what's pending for incentives"),
              h3(""),
              h4("Delete Sales"),
              h5("1. Delete sales if information entered incorrectly"),
              h5("2. Add the sale again post deletion"),
              h3("")
             
              
              
              
              
              
              
              
      ),
      #Contents for second tab
      tabItem(tabName = "newsales", 
              fluidPage(
                h2("Add New Sales!"),
                h3("Please do not leave any column blank"),
                h1(""),
                selectInput("Empoyee_ID", label = h4("Employee ID"),
                            c(  "21826889",
                                "21826912",
                                "21826920",
                                "21826983",
                                "21827009",
                                "21827013",
                                "21827113",
                                "21827132",
                                "21827170",
                                "21827204",
                                "21827273",
                                "21827325",
                                "21827396",
                                "21827484",
                                "21827569",
                                "21827613"
                            )),
                textInput("Order_ID", label = h4("Order ID"), "6 digit order ID ( 6 digit ID printed on receipt ) "),
                textInput("Order_Value", label = h4("Order Value"), "Enter exact value"),
                #textInput("Product_ID", label = h4("Product ID"), "0/1000/2000/3000/4000"),
                selectInput("Product_ID", label = h4("Product ID"),
                            c("0",
                              "1000",
                              "2000",
                              "3000",
                              "4000")),
                textInput("CName", label = h4("Customer Name")),
                selectInput("CInc", label = h4("Customer income ( Estimate if not ascertained )"),
                            c("<100k",
                              "100k - 300k",
                              "300k - 600k",
                              "600k - 1mil"
                                     )),
                textInput("CFam", label = h4("Family members in the customers house")),
                textInput("Cx_Address", label = h4("Customer Address"), "House No. ,Street Name, City, State"),
                textInput("Zip", label = h4("Customer Zip Code"), ""),
                selectInput("Cx_Occupation", label = h4("Customer Occupation"),
                            c("Private",
                              "Public",
                              "Self Employed",
                              "Unemployed"
                            )),
                actionButton("Add", "Add sale to records"),
                
                #For adding Sale Added response
                textOutput("title"),
                tags$head(tags$style("#title{color: green;
                                 font-size: 30px;
                                 font-style: times new roman;
                                 }"
                )
                ),
                
                
                
              )
              
              
              
              
      ),
      
      
      #---------------------------End of New Sales UI----------------------------    
      
      
      #Contents for third tab
      tabItem(tabName = "Market_Analytics",
              fluidPage(
                tabBox( width = '100px',
                  tabPanel(title = "Sales by Region", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, 
                h3("View Sales mapped by Zip-Codes"),
                actionButton("Map", "View Map"),
                h5(""),
                leafletOutput("finmap"),
                h5(""),
                h3("View Customer Details by Zip-Codes"),
                textInput("Zcode", label = h4("Enter Zip Code")),
                actionButton("Cx", "View Customer Data by Pin Code"),
                h5(""),
                DT::dataTableOutput("cxtable"),
                h5(""),
                  ),
                tabPanel(title = "Sales by Customer Details", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, 
                h3("View Sales by Customer Details"),
                actionButton("Plot","Show"),
                h5(""),
               box(title = "Occupation", status = "primary", solidHeader = TRUE,
                       collapsible = TRUE, plotOutput("plot1", click = "plot_click")),
               box(title = "Income Levels", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE, plotOutput("plot2", click = "plot_click")),
               box(title = "Family Size", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE, plotOutput("plot3", click = "plot_click")),
               ),
               tabPanel(title = "Sales by Product", status = "primary", solidHeader = TRUE,
                        collapsible = FALSE, 
                        h3('View Sales by product'),
                        actionButton("Plot2", "Show"),
                        h5(""),
                        plotOutput("plotprod", click = 'plot_click'),
                        h5(""),
                        h4(" 0 : Trial"),
                        h4(" 1000 : 1 year module"),
                        h4(" 2000 : 2 year module"),
                        h4(" 3000 : 3 year module"),
                        h4(" 4000 : 4 year module")
               )
               
               )
                
                
              )
 #-----------------------End of Market Analytics UI-------------------------             
      ),
      tabItem(tabName = "Perf",
              fluidPage(
                h2("You Vs Top Rep's"),
                h4(""),
                selectInput("Emp_ID", label = h4("Enter your employment ID"),
                            c(  "21826889",
                                "21826912",
                                "21826920",
                                "21826983",
                                "21827009",
                                "21827013",
                                "21827113",
                                "21827132",
                                "21827170",
                                "21827204",
                                "21827273",
                                "21827325",
                                "21827396",
                                "21827484",
                                "21827569",
                                "21827613"
                            )),
                actionButton("Compare", "Compare your metrics with the average"),
                h3(""),
                DT::dataTableOutput("mytable"),
                h3(""),
                box(title = "Leads Assigned", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,plotOutput("coldleads", click = "plot_click")),
                box(title = "Calls Dialled", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,plotOutput("coldcalls", click = "plot_click")),
                box(title = "Meetings Taken", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,plotOutput("Meetings" , click = "plot_click")),
                box(title = "Revenue Generated", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,plotOutput("Revenue" , click = "plot_click")),
                box(title = "Revenue/Sale (ARPU)", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,plotOutput("ARPU" , click = "plot_click")),
                box(title = "Sales/Meeting (Conversion)", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,plotOutput("Conv" , click = "plot_click")),
                
                
              )
              ),
 #----------------------End of Performance UI---------------------------
        tabItem(tabName = "Incentives",
                fluidPage(
                  h2("Incentive schemes in place :"),
                  h3("a). 5% of Revenue Generated"),
                  h4("Requirements :"),
                  h5("Sales : 5"),
                  h5("Sales Meetings : 12"),
                  h4(""),
                  h3("b). 8% of Revenue Generated"),
                  h4("Requirements :"),
                  h5("Sales : 8"),
                  h5("Sales Meetings : 12"),
                  h4(""),
                  h3("c). 10% of Revenue Generated"),
                  h4("Requirements :"),
                  h5("Sales : 10"),
                  h5("Sales Meetings : 15"),
                  h4(""),
                  selectInput("SPID", label = h4("Check incentive for the month"),
                              c(  "21826889",
                                  "21826912",
                                  "21826920",
                                  "21826983",
                                  "21827009",
                                  "21827013",
                                  "21827113",
                                  "21827132",
                                  "21827170",
                                  "21827204",
                                  "21827273",
                                  "21827325",
                                  "21827396",
                                  "21827484",
                                  "21827569",
                                  "21827613"
                              )),
                  h4(""),
                  actionButton("money", label = h5("Click to see incentive")),
                  h4(""),
                  textOutput("incentive"),
                  tags$head(tags$style("#incentive{color: black;
                                 font-size: 30px;
                                 font-style: times new roman;
                                 }"
                  )
                  ),
                  h4(""),
                  DT::dataTableOutput("incentable"),
                  
                  
                )),
 
 #----------------------End of incentive UI----------------------------
 
      tabItem(tabName = "DelSales",
              fluidPage(
                h3("Delete sales and enter again if an error was made"),
                h3(""),
                textInput("OID", label = "Enter Order ID of sale to delete"),
                h3(""),
                actionButton("del",label = h4("Delete Sale")),
                h3(""),
                textOutput("warning"),
                tags$head(tags$style("#warning{color: red;
                                 font-size: 15px;
                                 font-style: times new roman;
                                 }")),
                h3(""),
                textOutput("Success"),
                tags$head(tags$style("#Success{color: green;
                                 font-size: 30px;
                                 font-style: times new roman;
                                 }"))
                
                
              ))
      
    )
  )
)

server <- function(input, output) {
  
  #Code for adding Sale Added response
  observeEvent(input$Add, {output$title <- renderText("Sale Added")
  
  db2 <- dbConnector(
    server   = getOption("database_server"),
    database = getOption("database_name"),
    uid      = getOption("database_userid"),
    pwd      = getOption("database_password"),
    port     = getOption("database_port")  
  )
  #Adding new sales to Order_Data
  dbGetQuery(db2, paste0("INSERT into Order_Data values( "
                         , input$Order_ID , " , "
                         , input$Empoyee_ID , " , "
                         , input$Order_Value , " , "
                         , input$Product_ID , " , '"
                         , input$Cx_Address, "' , "
                         , input$Zip, " ) ; "
                         ))
  #Adding new sales to Cx_Data
  dbGetQuery(db2, paste0("Insert into CxData values("
                         ,"'"
                         ,input$CName, "' , "
                         ,input$Order_ID, " , "
                         ,input$Empoyee_ID, " , '"
                         ,input$Cx_Address, "' , "
                         ,input$Zip, " , '"
                         ,input$Cx_Occupation, "' , '"
                         ,input$CInc, "' , "
                         ,input$CFam, " ) ;"
                         ))
  #Adding new sales to Associate Data
  dbGetQuery(db2, paste0(" UPDATE AssociateData 
                         SET Sales_Done = Sales_Done +1, Revenue_Generated = Revenue_Generated + ",
                         input$Order_Value, " WHERE Employee_ID = ",input$Empoyee_ID, " ; "))
  #Updating ARPU and Conversion as soon as Revenue is added
  dbGetQuery(db2, "update associatedata 
             Set 
             ARPU = Revenue_Generated/Sales_Done,
             Conversion = (Sales_Done)/Sales_Meetings_Conducted 
             where Sales_Done != 0;")
  })
  
  #insert into order_data values
#(233628,21827569,399,3000,'4600 West Pioneer Dr, Irving, TX 75061', 75061);
  
  #Develop your server side code (Model) here
  observeEvent(input$Map, {
    source("./credentials_v4.R")
    
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    #on.exit(dbDisconnect(db), add = TRUE)
    
    ordata <- dbGetQuery(db, "select count(Order_ID) as Orders,Customer_Zip_Code from order_data group by Customer_Zip_Code;"
    )
    zipdata <- dbGetQuery(db, "select * from Zipdata")
    
    latlongzip <- left_join(ordata,zipdata, by = "Customer_Zip_Code")
    
    latlongzip$colorsm = cut(latlongzip$Orders,
                             breaks = c(1,3,7,50),
                             right = FALSE,
                             labels = c("Cold Regions","Warm Regions","HotRegions")
    )
    pal = colorFactor(palette = c("blue","orange","red"),
                      domain = latlongzip$colorsm)
    
    
    output$finmap <- renderLeaflet({
      finmap <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addCircleMarkers(lng=latlongzip$lng,
                         lat=latlongzip$lat,
                         radius = 18,
                         popup=paste0("Zip Code : ",latlongzip$Customer_Zip_Code," Orders : ",latlongzip$Orders),
                         color = pal(latlongzip$colorsm),
                         label = paste0("Zip Code : ",latlongzip$Customer_Zip_Code," Orders : ",latlongzip$Orders)
        ) %>%
        addLegend("bottomright", pal = pal, values = latlongzip$colorsm,
                  title = "Sales by Zip Code",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 1
        )
      
      })
    
    
    
    
    
    
    
  })
  
  observeEvent(input$Plot, {
    db3 <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    
    CxOcc <-  dbGetQuery(db3, "select Cx_Occupation, count(order_ID) as Orders from CxData group by Cx_Occupation;")
    CxIC <- dbGetQuery(db3, "select income_Level, count(Order_ID) as orders from CxData group by Income_Level;")
    CxFM <- dbGetQuery(db3, "select Family_Members, count(Order_ID) as orders from CxData group by Family_members;")
    

   output$plot1 <- renderPlot({
     chart <- pie(CxOcc$Orders, labels = paste0(CxOcc$Cx_Occupation," ",round((CxOcc$Orders/sum(CxOcc$Orders))*100, digits = 2),"%"), col = brewer.pal(length(CxOcc$Cx_Occupation),"PuOr"),main = "Customers by Occupation")
     legend("bottom", legend = CxOcc$Cx_Occupation, fill = brewer.pal(length(CxOcc$Cx_Occupation),"PuOr"), title = "Employment Sectors", cex = 0.7)
   })

  

    #CxIC <- dbGetQuery(db100, "select income_Level, count(Order_ID) as orders from CxData group by Income_Level;")
    
    output$plot2 <- renderPlot({ 
     chart2 <- pie(CxIC$orders, labels = paste0(CxIC$income_Level," ",round((CxIC$orders/sum(CxIC$orders))*100, digits = 2),"%"), col = brewer.pal(length(CxIC$income_Level),"PuOr"),main = "Customers by Income Level")
     legend("bottomleft", legend = CxIC$income_Level, fill = brewer.pal(length(CxIC$income_Level),"PuOr"), title = "Income Levels", cex = 0.7)
    }) 
    
       output$plot3 <- renderPlot({
       chart3 <- pie(CxFM$orders, labels = paste0(CxFM$Family_Members," members ",round((CxFM$orders/sum(CxFM$orders))*100, digits = 2),"%"), col = brewer.pal(length(CxFM$Family_Members),"PuOr"),main = "Customers by Family Members")
       legend("bottom", legend = CxFM$Family_Members, fill = brewer.pal(length(CxFM$Family_Members),"PuOr"), title = "Family Size of Customers", cex = 0.7)
       
     }) 
     })
  
  observeEvent(input$Plot2, {
    db100 <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    
    productid <- dbGetQuery(db100, "select product_id as Product_ID, count(order_ID) as Orders from Order_Data group by Product_ID;")
    
    output$plotprod <- renderPlot({
      barplot(height = productid$Orders,names.arg=productid$Product_ID, xlab="Product ID", ylab = "Number of orders", col = 'orange') 
      })
    
  })
    
     
   
  
  
  observeEvent(input$Cx, {
    
    db5 <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    
    cust_table <- dbGetQuery(db5,paste0("select Cx_Name, Sales_Person_ID, Customer_Address, Cx_Occupation , income_Level, Family_Members
    from Cxdata where Customer_Zip_Code = ",input$Zcode," ;"))
    output$cxtable = DT::renderDataTable({
      cust_table
    })
  })
  
  observeEvent(input$Compare, {
    db4 <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    
    top5 <- dbGetQuery(db4, "select avg(Cold_Leads_Assigned) as Avg_Cold_Leads
,avg(Cold_Calls_Dialled) as Avg_Cold_Calls
, avg(Sales_Meetings_Conducted) as Avg_Meetings
, avg(Sales_Done) as Avg_Sales
, avg(Revenue_Generated) as Avg_Revenue
, avg(ARPU) as Avg_ARPU
, avg(Conversion) as Avg_Conv 
from AssociateData
where Employee_ID in (select top 5 Employee_ID from AssociateData order by Revenue_Generated desc);")
    
    ind <- dbGetQuery(db4,paste0("select Cold_leads_Assigned as Avg_Cold_Leads, Cold_Calls_Dialled as Avg_Cold_Calls, 
Sales_Meetings_Conducted as Avg_Meetings, Sales_Done as Avg_Sales ,
Revenue_Generated as Avg_Revenue, ARPU as Avg_ARPU ,
                    Conversion as Avg_Conv  from AssociateData where Employee_ID = ",
                                 input$Emp_ID, " ; "))
    compt <- rbind(top5, ind)
    row.names(compt) <- c("Top 5 Average", "Selected Representative") 
    comptdf <- as.data.frame(compt)
    
    count1 <- (comptdf$Avg_Cold_Leads)
    #barplot(count, main = "Plot", xlab = "Average Cold Leads",ylab = "Average Cold Leads" ,names.arg = c("Average","Individual"),col = c("blue","green"), border = TRUE)
    count2 <- (comptdf$Avg_Cold_Calls)
    count3 <- (comptdf$Avg_Meetings)
    count3 <- (comptdf$Avg_Sales)
    count4 <- (comptdf$Avg_Revenue)
    count5 <- (comptdf$Avg_ARPU)
    count6 <- (comptdf$Avg_Conv)
    
    comptdf$Avg_ARPU <- round(comptdf$Avg_ARPU, digit = 0)
    comptdf$Avg_Conv <- round(comptdf$Avg_Conv, digit = 2)
    
    output$mytable = DT::renderDataTable({
      comptdf
    })
    
    output$coldleads <- renderPlot({
      barplot(count1, main = "Cold Leads Assigned", xlab = "You Vs Top 5",ylab = "Average Cold Leads" ,names.arg = c("Top 5 Average", "You"),col = c("blue","green"), border = TRUE)
    })
    output$coldcalls <- renderPlot({
      barplot(count2, main = "Cold Calls Made", xlab = "You Vs Top 5",ylab = "Average Cold Calls" ,names.arg = c("Top 5 Average","You"),col = c("blue","green"), border = TRUE)
    })
    output$Meetings <- renderPlot({
      barplot(count3, main = "Sales Meetings Done", xlab = "You Vs Top 5",ylab = "Average Meetings Done" ,names.arg = c("Top 5 Average","You"),col = c("blue","green"), border = TRUE)
    })
    output$Revenue <- renderPlot({
      barplot(count4, main = "Revenue Generated", xlab = "You Vs Top 5",ylab = "Average Revenue" ,names.arg = c("Top 5 Average","You"),col = c("blue","green"), border = TRUE)
    })
    output$ARPU <- renderPlot({
      barplot(count5, main = "Revenue/Sale", xlab = "You Vs Top 5",ylab = "Average Revenue per Sale" ,names.arg = c("Top 5 Average","You"),col = c("blue","green"), border = TRUE)
    })
    output$Conv <- renderPlot(({
      barplot(count6, main = "Conversion Rate", xlab = "You Vs Top 5",ylab = "Average Conversion" ,names.arg = c("Top 5 Average","You"),col = c("blue","green"), border = TRUE)
    }))
    
    
  })
  observeEvent(input$money, {
    db6 <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    
    spdata <- dbGetQuery(db6, paste0("select Employee_Name, Sales_Meetings_Conducted, Sales_Done,
                         Revenue_Generated
                         from associatedata where Employee_ID = ",input$SPID," ;"))
    inc <- character(100)
    
    if(spdata$Sales_Meetings_Conducted >=12 & spdata$Sales_Done >= 5 & spdata$Sales_Done < 8){
      inc = ((spdata$Revenue_Generated)*5)/100
    } else if(spdata$Sales_Meetings_Conducted >=12 & spdata$Sales_Done >= 8 & spdata$Sales_Done < 10){
      inc = ((spdata$Revenue_Generated)*8)/100
    } else if(spdata$Sales_Meetings_Conducted >=15 & spdata$Sales_Done >=10){
      inc = ((spdata$Revenue_Generated)*10)/100
    } else {inc = 0}
    
    pending_revenue_1 <- max((5-spdata$Sales_Done),0)
    pending_demos_1 <- max((12-spdata$Sales_Meetings_Conducted),0)
    pending_revenue_2 <- max((8-spdata$Sales_Done),0)
    pending_demos_2 <- max((12-spdata$Sales_Meetings_Conducted),0)
    pending_revenue_3 <- max((10-spdata$Sales_Done),0)
    pending_demos_3 <- max((15-spdata$Sales_Meetings_Conducted),0)
    
    Slabs <- c("5% slab", "8% slab", "10% slab")
    Pending_Sales <- c(pending_revenue_1,pending_revenue_2,pending_revenue_3)
    Pending_Demos <- c(pending_demos_1,pending_demos_2,pending_demos_3)
    
    incentivedf <- data.frame(Slabs,Pending_Sales,Pending_Demos)
    
    output$incentive <- renderText(paste0("Incentive made : $",inc))
    
    output$incentable <- DT::renderDataTable({
      incentivedf
    })
      
  })
  
  output$warning <- renderText("WARNING : Data will be deleted from everywhere and can not be recovered")
  
  observeEvent(input$del,{
    db7 <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    
    temp_delete <- dbGetQuery(db7, paste0("Select * from order_data where order_ID = ",input$OID," ;"))
    
    dbGetQuery(db7, paste0("Update AssociateData Set Sales_Done = Sales_Done -1, Revenue_Generated = Revenue_Generated - ",temp_delete$Order_Value, " where Employee_ID = ",temp_delete$Sales_Person_ID," ;"))
    dbGetQuery(db7, paste0("DELETE FROM order_data where order_ID = ",input$OID," ;"))
    dbGetQuery(db7, paste0("DELETE FROM cxdata where order_ID = ", input$OID," ;"))
    dbGetQuery(db7, "update associatedata 
             Set 
             ARPU = Revenue_Generated/Sales_Done,
             Conversion = (Sales_Done)/Sales_Meetings_Conducted 
             where Sales_Done != 0;")
    output$Success <- renderText("Sale Deleted")
    
  })
}

shinyApp(ui, server)


