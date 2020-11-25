
server <- function(input, output){
    
    df<-tibble(read.csv("crime_data.csv", sep=","))
    
    df_2<-melt(df,id.vars=c("classes","CRIME_CATEGORY"),
              measure.vars = c("April_2006_to_March_2007","April_2007_to_March_2008",
                               "April_2008_to_March_2009","April_2009_to_March_2010",
                               "April_2010_to_March_2011","April_2011_to_March_2012",
                               "April_2012_to_March_2013","April_2013_to_March_2014",
                               "April_2014_to_March_2015","April_2015_to_March_2016"),
              variable.name="Different_Years") 
    
    Graph_One_Reactor<-reactive({
        aggregate_1<- aggregate(value~Different_Years+classes,df_2,sum)
        df_3<-aggregate_1 %>% filter(classes==input$Select_input)
        
        return(df_3)
    })
    
    Graph_Two_Reactor<- reactive({
        aggregate_2<- aggregate(value~Different_Years,df_2,sum) 
        df_4<-aggregate_2 %>% filter(value>input$Slider_input)
        
        return(df_4)
    })
    
    The_Table_Reactor <- reactive({
        df_5 <- df %>% filter(classes==input$Select_input)
        
        return(df_5)
    })
    
    output$Graph_One<-renderPlot({
        Graph_One_Reactor() %>%
            ggplot(aes(x=reorder(Different_Years,value), y=value, fill=reorder(Different_Years,value)))+
            geom_col()+
            labs(x= "Different years",y="Total Crimes",title = "Crimes Statistics per Class")+
            coord_flip()
        
    })
    
    output$Graph_Two <- renderPlot({
        Graph_Two_Reactor() %>% 
            ggplot(aes(x=reorder(Different_Years,value), y=value, fill=reorder(Different_Years,value)))+
            geom_col()+
            labs(x="Different years", y="Number of cases per year", title = "Total Crime Cases")+
            coord_flip()
    })
    
    output$The_Table = DT::renderDataTable({
        The_Table_Reactor()
    })
}

shinyApp(ui =ui, server= server)
