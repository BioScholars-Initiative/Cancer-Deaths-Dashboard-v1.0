#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(dplyr)

data<-read.csv("updated-cancer-deaths.csv")
data<-data[,2:ncol(data)]
colnames(data)<-gsub("\\."," ",colnames(data))


## Define UI for application that draws a histogram
ui <- dashboardPage(skin="purple", dashboardHeader(title="Worldwide Cancer Deaths (1990-2019) Dashboard",
                                                   titleWidth=500
),
dashboardSidebar(
  sidebarMenu(
    menuItem("home", tabName = "Home", icon = icon("dashboard")),
    menuItem("user guide",tabName="user_guide",icon=icon("tree"))    
  )
),
dashboardBody(
  tabItems(
    tabItem("Home",
            fluidPage(
              fluidRow(
                column(width=8,
                       box(width=12,title="Worldwide Cancer Deaths",solidHeader=TRUE,status="success",collapsible=TRUE,
                           plotOutput("bar_graph")),
                ),
                column(width=4,
                       box(width=12,
                           selectInput("country",label="Choose a Country/Region",choices=c("All Countries/Regions",unique(data$Entity))),
                           selectInput("type",label="Choose a Cancer Type",choices=c("All Types",unique(colnames(data)[4:32]))),
                           selectInput("year",label="Choose a Year",choices=c("1990-2019 (all)",unique(data$Year)))
                       )
                )),
              fluidRow(
                column(width=6,
                       box(width=12,title="Cancer Case Trends from 1990-2019",solidHeader=TRUE,status="warning",collapsible=TRUE,
                           plotOutput("line_graph"))
                ),
                column(width=6,
                       box(width=12,title="Cancer Deaths by Type of Cancer",solidHeader=TRUE,status="primary",collapsible=TRUE,
                           plotOutput("pie_chart")))
              )
            )),
    tabItem("user_guide",
            fluidPage(
              fluidRow(
                box(width=12,solidHeader=TRUE,status="primary",title="User Instructions Part 1",collapsible=TRUE,
                    imageOutput("instruc_one",height="auto")
                ),
                box(width=12,solidHeader=TRUE,status="primary",title="User Instructions Part 2",collapsible=TRUE,
                    imageOutput("instruc_two",height="auto")
                ),
                box(width=12,solidHeader=TRUE,status="primary",title="User Instructions Part 3",collapsible=TRUE,
                    imageOutput("instruc_three",height="auto")
                )
              )
            )
    ))))


# Define server logic required to draw a bar graph
server <- function(input, output) {
  output$bar_graph<-renderPlot({
    if(input$country=="All Countries/Regions"&&input$year=="1990-2019 (all)"&&input$type=="All Types"){
      values<-c()
      for(i in unique(data$Entity)){
        indexes<-which(data$Entity==i)
        values<-c(values,sum(data$total_deaths[indexes]))
      }
      bar_data<-data.frame(
        country=unique(data$Entity),
        value=log(values)
      )
      bar_data$country<-reorder(bar_data$country,-bar_data$value)
      p<-ggplot(bar_data,aes(x=country,y=value))+geom_bar(stat="identity",fill="skyblue")+labs(title="Worldwide Cancer Deaths from 1990-2019",x="Country",y="Log (base 2) Deaths")+theme(axis.text.x=element_text(size=5.5,angle=90,hjust=1))+theme(axis.title.x = element_text(face = "bold",color="black"),axis.title.y=element_text(face="bold",color="black"),plot.title=element_text(face="bold",color="black"))
      print(p)
    }else if(input$country=="All Countries/Regions"&&input$year!="1990-2019 (all)"&&input$type!="All Types"){
      col_index<-which(colnames(data)==input$type)
      indexes<-which(data$Year==input$year)
      bar_data<-data.frame(
        country=unique(data$Entity),
        value = log(data[indexes,col_index])
      )
      bar_data$country<-reorder(bar_data$country,-bar_data$value)
      p<-ggplot(bar_data,aes(x=country,y=value))+geom_bar(stat="identity",fill="skyblue")+labs(title=paste0("Worldwide Cancer Deaths from ",input$type," during ",input$year),x="Country",y="Log (base 2) Deaths")+theme(axis.text.x = element_text(size=5.5,angle = 90, hjust = 1))+theme(axis.title.x = element_text(face = "bold",color="black"),axis.title.y=element_text(face="bold",color="black"),plot.title=element_text(face="bold",color="black"))
      print(p)
    }else if(input$country=="All Countries/Regions"&&input$year!="1990-2019 (all)"&&input$type=="All Types"){
      indexes<-which(data$Year==input$year)
      bar_data<-data.frame(
        country=unique(data$Entity),
        value=log(data$total_deaths[indexes])
      )
      bar_data$country<-reorder(bar_data$country,-bar_data$value)
      p<-ggplot(bar_data,aes(x=country,y=value))+geom_bar(stat="identity",fill="skyblue")+labs(title=paste0("Worldwide Cancer Deaths from during ",input$year),x="Country",y="Log (base 2) Deaths")+theme(axis.text.x = element_text(size=5.5,angle = 90, hjust = 1))+theme(axis.title.x = element_text(face = "bold",color="black"),axis.title.y=element_text(face="bold",color="black"),plot.title=element_text(face="bold",color="black"))
      print(p)
    }else{
      values<-c()
      col_index<-which(colnames(data)==input$type)
      for(i in unique(data$Entity)){
        values<-c(values,sum(data[which(data$Entity==i),col_index]))
      }
      bar_data<-data.frame(
        country=unique(data$Entity),
        value=log(values)
      )
      bar_data$country <- reorder(bar_data$country, -bar_data$value)
      p<-ggplot(bar_data,aes(x=country,y=value))+geom_bar(stat="identity",fill="skyblue")+labs(title=paste0("Worldwide Cancer Deaths from ",input$type," 1990-2019"),x="Country",y="Log (base 2) Deaths")+theme(axis.text.x = element_text(size=5.5,angle = 90, hjust = 1))+theme(axis.title.x = element_text(face = "bold",color="black"),axis.title.y=element_text(face="bold",color="black"),plot.title=element_text(face="bold",color="black"))
      print(p)
    }
  })
  output$line_graph<-renderPlot({
    if(input$country!="All Countries/Regions"&&input$year=="1990-2019 (all)"&&input$type=="All Types"){
      country_indexes<-which(data$Entity==input$country)
      country_years<-data$Year[country_indexes]
      country_data<-data$total_deaths[country_indexes]
      line_data<-data.frame(
        year=country_years,
        value=country_data
      )
      p<-ggplot(line_data,aes(x=year,y=value))+geom_line(color="black",size=1.5)+geom_point(size=3)+labs(title=paste0("Cancer Deaths from 1990-2019 in ",input$country),x="Year",y="Deaths")+theme(axis.title.x = element_text(face = "bold",color="black"),axis.title.y=element_text(face="bold",color="black"),plot.title=element_text(face="bold",color="black"))
      print(p)
    }else if(input$country!="All Countries/Regions"&&input$year=="1990-2019 (all)"&&input$type!="All Types"){
      country_indexes<-which(data$Entity==input$country)
      type_index<-which(colnames(data)==input$type)
      line_data<-data.frame(
        year=data$Year[country_indexes],
        value=data[country_indexes,type_index]
      )
      p<-ggplot(line_data,aes(x=year,y=value))+geom_line(color="black",size=1.5)+geom_point(size=3)+labs(title=paste0(input$type," Deaths from 1990-2019 in ",input$country),x="Year",y="Deaths")+theme(axis.title.x = element_text(face = "bold",color="black"),axis.title.y=element_text(face="bold",color="black"),plot.title=element_text(face="bold",color="black"))
      print(p)
    }else if(input$country=="All Countries/Regions"&&input$year=="1990-2019 (all)"&&input$type!="All Types"){
      type_index<-which(colnames(data)==input$type)
      annual_death_global<-c()
      for (i in unique(data$Year)){
        annual_global_num<-sum(data[which(data$Year==i),type_index])
        annual_death_global<-c(annual_death_global,annual_global_num)
      }
      line_data<-data.frame(
        year=unique(data$Year),
        value=annual_death_global
      )
      p<-ggplot(line_data,aes(x=year,y=value))+geom_line(color="black",size=1.5)+geom_point(size=3)+labs(title=paste0(input$type," Deaths from 1990-2019 in Worldwide"),x="Year",y="Deaths")+theme(axis.title.x = element_text(face = "bold",color="black"),axis.title.y=element_text(face="bold",color="black"),plot.title=element_text(face="bold",color="black"))
      print(p)
    }else if(input$country=="All Countries/Regions"&&input$year=="1990-2019 (all)"&&input$type=="All Types"){
      annual_death_global<-c()
      for (i in unique(data$Year)){
        annual_global_num<-sum(data$total_deaths[which(data$Year==i)])
        annual_death_global<-c(annual_death_global,annual_global_num)
      }
      line_data<-data.frame(
        year=unique(data$Year),
        value=annual_death_global
      )
      p<-ggplot(line_data,aes(x=year,y=value))+geom_line(color="black",size=1.5)+geom_point(size=3)+labs(title=paste0("All Cancer Deaths from 1990-2019 Worldwide"),x="Year",y="Deaths")+theme(axis.title.x = element_text(face = "bold",color="black"),axis.title.y=element_text(face="bold",color="black"),plot.title=element_text(face="bold",color="black"))
      
      print(p)
    }
  })
  output$pie_chart<-renderPlot({
    if(input$country!="All Countries/Regions"&&input$year!="1990-2019 (all)"&&input$type=="All Types"){
      index<-which(data$Entity==input$country&data$Year==input$year)
      pie_data_values<-data[index,5:(ncol(data)-1)]
      new_values<-c()
      for(i in 1:length(pie_data_values)){
        new_values<-c(new_values,pie_data_values[[i]])
      }
      pie_data<-data.frame(
        type=colnames(data)[5:(ncol(data)-1)],
        value=new_values
      )
      pie_data_df<-head(pie_data%>%arrange(desc(value)),6)
      p<-ggplot(pie_data_df,aes(x="",y=value,fill=type))+geom_col()+
        geom_label(aes(label=round(value/sum(value)*100,2)),position=position_stack(vjust=0.5))+coord_polar(theta="y")+theme_void()+
        labs(title=paste0("% Death of TOP 6 Common Cancer Types in ",input$year," in ",input$country))+theme(legend.position="right",plot.title=element_text(face="bold",color="black"))
      print(p)
    }else if(input$country!="All Countries/Regions"&&input$year=="1990-2019 (all)"&&input$type=="All Types"){
      country_indexes<-which(data$Entity==input$country)
      type_deaths<-c()
      for(i in 5:(ncol(data)-1)){
        type_deaths<-c(type_deaths,sum(data[country_indexes,i]))
      }
      pie_data<-data.frame(
        type=colnames(data)[5:(ncol(data)-1)],
        value=type_deaths
      )
      pie_data_df<-head(pie_data%>%arrange(desc(value)),6)
      p<-ggplot(pie_data_df,aes(x="",y=value,fill=type))+geom_col()+
        geom_label(aes(label=round(value/sum(value)*100,2)),position=position_stack(vjust=0.5))+coord_polar(theta="y")+theme_void()+
        labs(title=paste0("% Death of TOP 6 Common Cancer Types in ",input$year," in ",input$country))+theme(legend.position="right",plot.title=element_text(face="bold",color="black"))      
      print(p)
    }else if(input$country=="All Countries/Regions"&&input$year=="1990-2019 (all)"&&input$type=="All Types"){
      type_deaths<-c()
      for(i in 5:(ncol(data)-1)){
        type_deaths<-c(type_deaths,sum(data[,i]))
      }
      pie_data<-data.frame(
        type=colnames(data)[5:(ncol(data)-1)],
        value=type_deaths
      )
      pie_data_df<-head(pie_data%>%arrange(desc(value)),6)
      p<-ggplot(pie_data_df,aes(x="",y=value,fill=type))+geom_col()+
        geom_label(aes(label=round(value/sum(value)*100,2)),position=position_stack(vjust=0.5))+coord_polar(theta="y")+theme_void()+
        labs(title=paste0("% Death of TOP 6 Common Cancer Types in ",input$year," in ",input$country))+theme(legend.position="right",plot.title=element_text(face="bold",color="black"))
      print(p)
    }else if(input$country=="All Countries/Regions"&&input$year!="1990-2019 (all)"&&input$type=="All Types"){
      type_deaths<-c()
      for(i in 5:(ncol(data)-1)){
        type_deaths<-c(type_deaths,sum(data[which(data$Year==input$year),i]))
      }
      pie_data<-data.frame(
        type=colnames(data)[5:(ncol(data)-1)],
        value=type_deaths
      )
      pie_data_df<-head(pie_data%>%arrange(desc(value)),6)
      p<-ggplot(pie_data_df,aes(x="",y=value,fill=type))+geom_col()+
        geom_label(aes(label=round(value/sum(value)*100,2)),position=position_stack(vjust=0.5))+coord_polar(theta="y")+theme_void()+
        labs(title=paste0("% Death of TOP 6 Common Cancer Types in ",input$year," in ",input$country))+theme(legend.position="right",plot.title=element_text(face="bold",color="black"))
      print(p)
    }
  })
  output$instruc_one<-renderImage({
    img_source<-"instruc-1.jpg"
    width<-"100%"
    list(src=img_source,
         contentType="image/jpg",
         width=width,
         height="auto")
  },deleteFile=FALSE)
  output$instruc_two<-renderImage({
    img_source<-"instruc-2.jpg"
    width<-"100%"
    list(src=img_source,
         contentType="image/jpg",
         width=width,
         height="auto")
  },deleteFile=FALSE)
  output$instruc_three<-renderImage({
    img_source<-"instruc-3.jpg"
    width<-"100%"
    list(src=img_source,
         contentType="image/jpg",
         width=width,
         height="auto")
  },deleteFile=FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)





##IGNORE: INITIAL DATA CLEANING##

#pattern_prefix<-"Deaths..."
#pattern_suffix<-"...Sex..Both...Age..All.Ages..Number."
#colnames(data)<-sub(pattern_prefix,"",colnames(data))
#colnames(data)<-sub(pattern_suffix,"",colnames(data))
#total_deaths<-c()
#for(i in 1:nrow(data)){
#  total_deaths<-c(total_deaths,sum(data[i,4:32]))
#}
