library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(
    title = h1("Visualizing the SD using a quadratic curve",align ="center"),
    windowTitle = "Quadratic SD"
    ),
  navbarPage("", 
                 id ="navbar",
    tabPanel("Guess the Mean",
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins",
                      "Guess the mean using ECDF",
                      min = round(min(data1),2),
                      max = round(max(data1),2),
                      value = round(min(data1),2),
                      step = 0.05),
          
          actionButton(inputId = "btn",
                       label = "submit"),
          htmlOutput("message")
        ),
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot"),
        )
      )
    ),
    tabPanel("Guess the MD",
      sidebarLayout(
        sidebarPanel(
          sliderInput("md",
                      "Guess the mean deviation from reflected ECDF",
                      min = 0,
                      max = round(max(data1)-mean(data1),2),
                      value = 0,
                      step = 0.05),
           actionButton(inputId = "btnmd",
                        label = "submit"),
           htmlOutput("messagemd")
        ),
        mainPanel(
          plotOutput("sdPlot")
        )
      )
    ),
    tabPanel("Guess the RMSD",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("msd",
                             "Guess the horizontal equalizer in top left quadrant",
                             min = 0,
                             max = 1,
                             value = 0.5,
                             step = 0.005),
                 actionButton(inputId = "btnmsd",
                              label = "submit"),
                
                 htmlOutput("messagemsd"),
                 htmlOutput("showSD"),
                 htmlOutput("explainDetails")
               ),
               mainPanel(
                 plotOutput("msdPlot")
               )
             )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  hideTab("navbar","Guess the RMSD")
  hideTab("navbar", "Guess the MD")
  sorted_data <- sort(data1)
  lofdata <- length(sorted_data)
  vector <- c()
  fre <- 0
  ecdf<- function(x){
    for (y in 1:lofdata){
      if(sorted_data[y]<=x){
        fre <- fre + 1
      }
    }
    return(fre)
  }
  for (i in 1:lofdata){
    vector <- c(vector,ecdf(sorted_data[i])/lofdata) 
  }
  
  mdv<-c()
  mean <- mean(sorted_data)
  for(j in 1:length(sorted_data)){
    mdv<-c(mdv,abs(mean - sorted_data[j]))
  }

  range1<- max(sorted_data)-min(sorted_data)
  x_values<-c(min(sorted_data)-0.1*range1,sorted_data,max(sorted_data)+0.1*range1)
  prob<-c(0,vector,1)
  df<- data.frame(x_values,prob)
  
  
    # mean plot:
  
  
    output$distPlot <- renderPlot({
      p<- ggplot(data = df,
                 aes(x_values,prob),
                 )+
        labs(x="data",
             y="ECDF") +
        
        geom_hline(
          yintercept = 0,
          color = 'black'
        ) +
        
        geom_hline(
          yintercept = 1,
          color = 'black'
        ) +
        scale_x_continuous(
          n.breaks = 20
        ) +
        scale_y_continuous(
          n.break = 10
        )
      
      #Blue Region: Second Half of the Graph
      seq2 <- seq(input$bins,max(x_values),length=5000)
      yseq2<-c()
      l <- length(seq2)
      c <- 0
      for (i in 1:length(x_values)){
        if (input$bins >= x_values[i]){
          c<-c+1
        }
      }
      for(i in 1:l){
        if(seq2[i]<x_values[c+1]){
          yseq2<-c(yseq2,prob[c])
        } else{
          yseq2<-c(yseq2,prob[c+1])
          c <- c+1 
        }
      }
      df2<-data.frame(x=seq2,y=yseq2)
      
      p<- p + geom_ribbon(
        data = df2,
        aes(ymin=yseq2,ymax=1,x=x,y=y),
        fill='blue'
      )
      #Green Region: First half of the graph
      seq1 <- seq(min(x_values),input$bins,length=5000)
      yseq<-c()
      l <- length(seq1)
      c<-1
      for(i in 1:l){
        if(seq1[i]<x_values[c+1]){
          yseq<-c(yseq,prob[c])
        } else{
          yseq<-c(yseq,prob[c+1])
          c <- c+1 
        }
      }
      p<- p + geom_vline(
        xintercept = input$bins
      )
      
      df1<-data.frame(x=seq1,y=yseq)
      p<- p + geom_ribbon(
        data = df1,
        aes(ymin=0,ymax=y,x=x,y=y),
        fill='green'
      )
      
      p
    })
    observeEvent(input$btn,{
      if (abs(-input$bins+mean(data1))<0.05){
        output$message <- renderUI({
          tags$span(paste("Congratulations! Your guess is close to the correct mean: "),
                 tags$p(paste(mean(data1)),style="display:inline;color:red"))
        })
        showTab("navbar", "Guess the MD")
      }  else if(-input$bins+mean(data1)>0.15){
        output$message <- renderText("incorrect! move right")
      } else if((-input$bins+mean(data1)<0.15)&(-input$bins+mean(data1)>0)){
        output$message <- renderText("incorrect! move right slightly")
      } else if(input$bins-mean(data1)>0.15){
        output$message <- renderText("incorrect! move left")
      } else if((input$bins-mean(data1)<0.15)&(input$bins-mean(data1)>0)){
        output$message <- renderText("incorrect! move left slightly")
      }
    })
    
    
    # sd plot:
    
    
    output$sdPlot <- renderPlot({
      x_values_1<-c(min(sorted_data)-0.2*range1,min(sorted_data)-0.1*range1,sorted_data,max(sorted_data)+0.1*range1,max(sorted_data)+0.2*range1)
      prob_1<-c(-0.25,0,vector,1,2)
      df_1<- data.frame(x_values_1,prob_1)
      plt <- ggplot(
        data = df_1,
        aes(x=x_values_1,y=prob_1)
      ) +
        labs(x="data",
             y="Reflected ECDF") +
        geom_hline(
          yintercept=0
        ) + 
        geom_hline(
          yintercept=1
        )+
        geom_vline(
          xintercept = mean(sorted_data)
        ) + 
        geom_step(
          data =df,
          mapping= aes(x=x_values, y=prob),
          linetype="dashed"
        )
      lower_x_values <- c(mean(sorted_data))
      lower_prob <- c(ecdf(mean(sorted_data))/lofdata)
      higher_x_values <- c(mean(sorted_data))
      higher_prob <- c(ecdf(mean(sorted_data))/lofdata)
      for (i in (1:length(x_values))){
        if(x_values[i]<= mean(sorted_data)){
          lower_x_values<- c(lower_x_values,x_values[i])
          lower_prob <- c(lower_prob, prob[i])
        } 
        if(x_values[i]>= mean(sorted_data)){
          higher_x_values <- c(higher_x_values,x_values[i])
          higher_prob <- c(higher_prob, prob[i])
        }
      }
      for ( i in 1:length(lower_x_values)){
        lower_x_values[i]<- mean(sorted_data)+(mean(sorted_data)-lower_x_values[i])
      }
      df_2 <- data.frame(lower_x_values,lower_prob)
      df_3 <- data.frame(higher_x_values,higher_prob)
      plt<- plt +
        geom_step(
          data = df_2,
          mapping = aes(x=lower_x_values,y=lower_prob),
          direction="vh"
        ) +
        geom_step(
          data = df_3,
          mapping = aes(x=higher_x_values,y=higher_prob),
        )+
        geom_vline(
          xintercept = input$md + mean(sorted_data),
          color ="red"
        ) 
      
      #upper right corner for md blue area
      seq2 <- seq(mean(sorted_data)+input$md,max(x_values),length=5000)
      yseq2<-c()
      l <- length(seq2)
      c <- 0
      for (i in 1:length(x_values)){
        if (mean(sorted_data)+input$md >= x_values[i]){
          c<-c+1
        }
      }
      for(i in 1:l){
        if(seq2[i]<x_values[c+1]){
          yseq2<-c(yseq2,prob[c])
        } else{
          yseq2<-c(yseq2,prob[c+1])
          c <- c+1 
        }
      }
      df2<-data.frame(x=seq2,y=yseq2)
      
      plt<- plt + geom_ribbon(
        data = df2,
        aes(ymin=yseq2,ymax=1,x=x,y=y),
        fill='blue'
      )
      
      #lower right corner: blue area for md
      seq1 <- seq(min(x_values),-input$md+mean(sorted_data),length=5000)
      yseq<-c()
      l <- length(seq1)
      c<-1
      for(i in 1:l){
        if(seq1[i]<x_values[c+1]){
          yseq<-c(yseq,prob[c])
        } else{
          yseq<-c(yseq,prob[c+1])
          c <- c+1 
        }
        seq1[i]<- mean(sorted_data)+abs(mean(sorted_data)-seq1[i])
      }
      df1<-data.frame(x=seq1,y=yseq)
      plt<- plt + geom_ribbon(
        data = df1,
        aes(ymin=0,ymax=y,x=x,y=y),
        fill='blue'
      )
      
      #left upper corner: green part for md
      seq3 <- seq(mean(sorted_data),mean(sorted_data)+input$md,length=5000)
      yseq3<-c()
      l <- length(seq3)
      c <- 0
      for (i in 1:length(x_values)){
        if (mean(sorted_data)>= x_values[i]){
          c<-c+1
        }
      }
      for(i in 1:l){
        if(seq3[i]<x_values[c+1]){
          yseq3<-c(yseq3,prob[c])
        } else{
          yseq3<-c(yseq3,prob[c+1])
          c <- c+1 
        }
      }
      df3<-data.frame(x=seq3,y=yseq3)
      plt<- plt + geom_ribbon(
        data = df3,
        aes(ymin=ecdf(mean(sorted_data))/lofdata,ymax=yseq3,x=x,y=y),
        fill='green'
      )
      
      #left bottom corner: green part for md
      seq4 <- seq(-input$md+mean(sorted_data),mean(sorted_data),length=5000)
      yseq4<-c()
      l <- length(seq4)
      c<-1
      for(i in 1:l){
        if(seq4[i]<x_values[c+1]){
          yseq4<-c(yseq4,prob[c])
        } else{
          yseq4<-c(yseq4,prob[c+1])
          c <- c+1 
        }
        seq4[i]<- mean(sorted_data)+abs(mean(sorted_data)-seq4[i])
      }
      df4<-data.frame(x=seq4,y=yseq4)
      plt<- plt + geom_ribbon(
        data = df4,
        aes(ymin=y,ymax=ecdf(mean(sorted_data))/lofdata,x=x,y=y),
        fill='green'
      )
      x_ticks <- c()
      lab <- c()
      len <- max(higher_x_values) - mean(sorted_data)
      len <- round(len,0)+1
      for (j in 1:len){
        x_ticks <- c(x_ticks,mean(sorted_data) + j)
        lab <- c(lab,j)
      }
      dfNew <- data.frame(x1=x_ticks)
      plt <- plt +
        annotate("text",x_ticks,-0.03,label=lab,size = 3)+
        geom_segment(
          data = dfNew,
          aes(
            x = x1,
            xend = x1,
            y = -0.02,
            yend = 0
          )
        )
      plt
    })
    observeEvent(input$btnmd,{
      mean <- mean(sorted_data)
      mdv<-c()
      for(i in 1:length(sorted_data)){
        mdv<-c(mdv,abs(mean-sorted_data))
      }
      if(abs(-input$md+mean(mdv))<0.05){
        output$messagemd <- renderUI({
          tags$span(paste("Congratulations! Your guess is close to the correct mean deviation: "),
                    tags$p(paste(mean(mdv)),style="display:inline; color: red"))
          
        })  
        showTab("navbar","Guess the RMSD")
      }  else if(-input$md+mean(mdv)>0.15){
        output$messagemd <- renderText("incorrect! move right")
      } else if((-input$md+mean(mdv)<0.15)&(-input$md+mean(mdv)>0)){
        output$messagemd <- renderText("incorrect! move right slightly")
      } else if(input$md-mean(mdv)>0.15){
        output$messagemd <- renderText("incorrect! move left")
      } else if((input$md-mean(mdv)<0.15)&(input$md-mean(mdv)>0)){
        output$messagemd <- renderText("incorrect! move left slightly")
      }
     })
    

    call <- function(correct = FALSE){
      x_values_1<-c(min(sorted_data)-0.1*range1,sorted_data,max(sorted_data)+0.1*range1)
      prob_1<-c(0,vector,1)
      len<- length(prob_1)
      for(i in 1:len){
        prob_1[i] <- prob_1[i] - 1 
      }
      df_1<- data.frame(x_values_1,prob_1)
      mean <- mean(sorted_data)
      md <- mean(mdv)
      pl <- ggplot() +
        geom_hline(
          data = df_1,
          aes(x=x_values_1,y=prob_1),
          yintercept=-1
        ) + 
        geom_hline(
          data = df_1,
          aes(x=x_values_1,y=prob_1),
          yintercept=0
        ) +
        geom_hline(data = df_1,
                   aes(x=x_values_1,y=prob_1),
                   yintercept=1
        ) +
        geom_vline(
          data = df_1,
          aes(x=x_values_1,y=prob_1),
          xintercept = mean(sorted_data)
        ) + 
        geom_step(
          data =df_1,
          mapping= aes(x=x_values_1, y=prob_1),
          linetype="dashed"
        ) + 
        geom_segment(
          data = df_1,
          aes(x=mean(sorted_data)+md,y=-1,xend=mean(sorted_data)+md,yend=0),
          colour ="red"
        ) 
      # + ylim(-1,1.05)
      lower_x_values <- c(mean(sorted_data))
      lower_prob <- c(ecdf(mean(sorted_data))/lofdata)
      higher_x_values <- c(mean(sorted_data))
      higher_prob <- c(ecdf(mean(sorted_data))/lofdata)
      lab <- c()
      for (i in (1:length(x_values))){
        if(x_values[i]<= mean(sorted_data)){
          lower_x_values<- c(lower_x_values,x_values[i])
          lower_prob <- c(lower_prob, prob[i])
        } 
        if(x_values[i]>= mean(sorted_data)){
          higher_x_values <- c(higher_x_values,x_values[i])
          higher_prob <- c(higher_prob, prob[i])
        }
      }
      for ( i in 1:length(lower_x_values)){
        lower_x_values[i]<- mean(sorted_data)+(mean(sorted_data)-lower_x_values[i])
      }
      lower_prob <- lower_prob - 1
      higher_prob <- higher_prob - 1
      df_2 <- data.frame(lower_x_values,lower_prob)
      df_3 <- data.frame(higher_x_values,higher_prob)
      pl<- pl +
        geom_step(
          data = df_2,
          mapping = aes(x=lower_x_values,y=lower_prob),
          size = 0.75,
          direction="vh"
        ) +
        geom_step(
          data = df_3,
          mapping = aes(x=higher_x_values,y=higher_prob),
          size = 0.75
        )
      
      
      #upper right corner for md blue area
      seq2 <- seq(mean(sorted_data)+md,max(x_values),length=5000)
      yseq2<-c()
      l <- length(seq2)
      c <- 0
      for (i in 1:length(x_values)){
        if (mean(sorted_data)+md >= x_values[i]){
          c<-c+1
        }
      }
      for(i in 1:l){
        if(seq2[i]<x_values[c+1]){
          yseq2<-c(yseq2,prob[c])
        } else{
          yseq2<-c(yseq2,prob[c+1])
          c <- c+1 
        }
      }
      yseq2 <- yseq2 - 1
      df2<-data.frame(x=seq2,y=yseq2)
      
      pl<- pl + geom_ribbon(
        data = df2,
        aes(ymin=yseq2,ymax=0,x=x,y=y),
        fill='blue'
      )
      
      #lower right corner: blue area for md
      seq1 <- seq(min(x_values),-md+mean(sorted_data),length=5000)
      yseq<-c()
      l <- length(seq1)
      c<-1
      for(i in 1:l){
        if(seq1[i]<x_values[c+1]){
          yseq<-c(yseq,prob[c])
        } else{
          yseq<-c(yseq,prob[c+1])
          c <- c+1 
        }
        seq1[i]<- mean(sorted_data)+abs(mean(sorted_data)-seq1[i])
      }
      yseq <- yseq - 1 
      df1<-data.frame(x=seq1,y=yseq)
      pl<- pl + geom_ribbon(
        data = df1,
        aes(ymin=-1,ymax=y,x=x,y=y),
        fill='blue'
      )
      
      #left upper corner: green part for md
      seq3 <- seq(mean(sorted_data),mean(sorted_data)+md,length=5000)
      yseq3<-c()
      l <- length(seq3)
      c <- 0
      for (i in 1:length(x_values)){
        if (mean(sorted_data)>= x_values[i]){
          c<-c+1
        }
      }
      for(i in 1:l){
        if(seq3[i]<x_values[c+1]){
          yseq3<-c(yseq3,prob[c])
        } else{
          yseq3<-c(yseq3,prob[c+1])
          c <- c+1 
        }
      }
      yseq3 <- yseq3 - 1
      df3<-data.frame(x=seq3,y=yseq3)
      pl<- pl + geom_ribbon(
        data = df3,
        aes(ymin=(ecdf(mean(sorted_data))/lofdata)-1,ymax=yseq3,x=x,y=y),
        fill='green'
      )
      
      #left bottom corner: green part for md
      seq4 <- seq(-md+mean(sorted_data),mean(sorted_data),length=5000)
      yseq4<-c()
      l <- length(seq4)
      c<-1
      for(i in 1:l){
        if(seq4[i]<x_values[c+1]){
          yseq4<-c(yseq4,prob[c])
        } else{
          yseq4<-c(yseq4,prob[c+1])
          c <- c+1 
        }
        seq4[i]<- mean(sorted_data)+abs(mean(sorted_data)-seq4[i])
      }
      yseq4 <- yseq4 - 1 
      df4<-data.frame(x=seq4,y=yseq4)
      pl<- pl + geom_ribbon(
        data = df4,
        aes(ymin=y,ymax=(ecdf(mean(sorted_data))/lofdata)-1,x=x,y=y),
        fill='green'
      )
      
      
      funct <- function(x){
        ifelse(x>=mean(sorted_data),((x-mean(sorted_data))^2)/(max(mdv)^2),0)
      }
      pl <- pl + geom_function(
        fun = funct
      )
      
      # #msd second quadrant:
      
      
      sorted_lower_x_values <- sort(lower_x_values)
      sorted_lower_prob<- sort(lower_prob,decreasing = T)
      sorted_higher_x_values <- sort(higher_x_values)
      sorted_higher_prob <- sort(higher_prob)
      
      x_ticks <- c()
      lab <- c()
      len <- max(higher_x_values) - mean(sorted_data)
      len <- round(len,0)
      for (j in 1:len){
        x_ticks <- c(x_ticks,mean(sorted_data) + j)
        lab <- c(lab,j)
      }
      
      #for higher x_values
      xmin <- c()
      xmax <- c()
      ymin <- c()
      ymax <- c()
      
      for(i in 1:(length(sorted_higher_x_values)-2)){
        #coordinators for the rectangle
        x1 <- sorted_higher_x_values[i]
        y1 <- sorted_higher_prob[i]
        x2 <- sorted_higher_x_values[i+1]
        y2 <- sorted_higher_prob[i+1]
        
        
        #transforming the coordinates for the graph according to the plot
        transformed_x1 <- (y1 * mean(mdv)) + mean(sorted_data)
        transformed_y1 <- (x1 - mean(sorted_data))^2/(max(mdv)^2)
        transformed_x2 <- (y2 * mean(mdv)) + mean(sorted_data)
        transformed_y2 <- (x2 - mean(sorted_data))^2/(max(mdv)^2)
        
        #constructing data frame
        xmin <- c(xmin, transformed_x1)
        xmax <- c(xmax, transformed_x2)
        ymin <- c(ymin, transformed_y1)
        ymax <- c(ymax, transformed_y2)
        
      }
      
      
      for(i in 1:(length(sorted_lower_x_values)-2)){
        #coordinators for the rectangle
        x1 <- sorted_lower_x_values[i]
        y1 <- sorted_lower_prob[i+1]
        x2 <- sorted_lower_x_values[i+1]
        y2 <- sorted_lower_prob[i+2]
        
        
        #transforming the coordinates for the graph according to the plot
        transformed_x1 <- (y1 * mean(mdv)) + mean(sorted_data)
        transformed_y1 <- (x1 - mean(sorted_data))^2/(max(mdv)^2)
        transformed_x2 <- (y2 * mean(mdv)) + mean(sorted_data)
        transformed_y2 <- (x2 - mean(sorted_data))^2/(max(mdv)^2)
        
        #constructing data frame
        xmin <- c(xmin, transformed_x1)
        xmax <- c(xmax, transformed_x2)
        ymin <- c(ymin, transformed_y1)
        ymax <- c(ymax, transformed_y2)
        
      }
      
      dfrect <- data.frame(
        x1 <- xmin, 
        x2 <- xmax,
        y1 <- ymin, 
        y2 <- ymax
      )
      
      pl <- pl + geom_rect(
        data = dfrect,
        aes(
          xmin = x1,
          xmax = x2,
          ymin = 0,
          ymax = y2
        ), fill = "NA", 
        colour = "black",
        size = 0.5
      )+
        annotate("text",x_ticks,-1.05,label=lab,size = 3)+
        geom_segment(
          aes(
            x = x_ticks,
            xend = x_ticks,
            y = -1.03,
            yend = -1
          )
        )+ ylim(-1.05,1)
      
      ymaxsecond <- c()
      xmaxsecond <- c()
      xminsecond <- c()
      ymaxfirst <- c()
      xmaxfirst <- c()
      xminfirst <- c()
      yminfirst <- c()
      yminsecond <- c()
      
      for(i in 1:nrow(dfrect)){
        if(dfrect$y2[i] < input$msd){
          ymaxfirst <- c(ymaxfirst, dfrect$y2[i])
          xmaxfirst <- c(xmaxfirst, dfrect$x2[i])
          xminfirst <- c(xminfirst, dfrect$x1[i])
          yminfirst <- c(yminfirst, dfrect$y1[i])
        } else {
          ymaxsecond <- c(ymaxsecond, dfrect$y2[i])
          xmaxsecond <- c(xmaxsecond, dfrect$x2[i])
          xminsecond <- c(xminsecond, dfrect$x1[i])
          yminsecond <- c(yminsecond, dfrect$y1[i])
        }
      }
      
      df122 <- data.frame(
        xmin <- xminfirst,
        xmax <- xmaxfirst,
        ymax <- ymaxfirst,
        ymin <- yminfirst
      )
      
      df123 <- data.frame(
        xmin <- xminsecond,
        xmax <- xmaxsecond,
        ymax <- ymaxsecond,
        ymin <- yminsecond
      )
      
      xEndForSegment = (sqrt(input$msd) * max(mdv) ) + mean(sorted_data)
      xBegForSegment = min(xmax)
      pl <- pl +   geom_rect(
        data = df122,
        aes(
          xmin = xminfirst, 
          xmax = xmaxfirst,
          ymin = input$msd, 
          ymax = ymaxfirst
        ),fill = c("#05f80b")
      ) + 
        geom_rect(
          data = df123,
          aes(
            xmin = xminsecond,
            xmax = xmaxsecond,
            ymin = input$msd,
            ymax = ymaxsecond
          ), fill = "#054ef8"
        ) +  
        geom_segment(aes(x = xBegForSegment, xend = xEndForSegment, y=input$msd, yend=input$msd),colour = "red",linetype = 2,size=0.75) +
        geom_segment(aes(x = xEndForSegment, xend = xEndForSegment, y = input$msd, yend =-1), colour = "red", linetype =2,size=0.75)
      pl + theme(legend.position = "none")
      if(correct == TRUE){
        yEnd = input$msd * (min(xmax) - mean(sorted_data))/(min(xmin)-mean(sorted_data))
        xEnd = (sqrt(yEnd) * max(mdv) ) + mean(sorted_data)
        x_val <- c(mean(sorted_data),min(xmin),min(xmax),xEnd,xEnd)
        y_val <- c(0,input$msd,yEnd,yEnd,-1)
        laab <- c("O","H","K","L","SD")
        pl <- pl + 
          geom_segment(aes(x = mean(sorted_data), y =0, xend = min(xmax) , yend = yEnd ),colour = "brown",linetype = 2,size =1)+
          geom_segment(aes(x= min(xmax), y = yEnd, xend =xEnd, yend = yEnd),colour = "brown",linetype = 2,size =1) +
          geom_segment(aes(x=xEnd, xend = xEnd, y = yEnd, yend = -1),colour = "brown",linetype = 2,size =1)+
          geom_point(aes(
            x = x_val,
            y = y_val
          ),
          colour = "red",
          size = 3
          ) + annotate("text",x_val-0.03*mean(mdv),y_val-0.02,label=laab,size = 3)
          
      }
      pl <- pl + 
        xlab("data")+
        ylab("")
      return(pl)
    }
    #msd plot:
    output$msdPlot <- renderPlot({
      call(correct = FALSE)
    })
    
    observeEvent(input$btnmsd,{
      RMSD <- sd(sorted_data) * sqrt((length(sorted_data)-1)/(length(sorted_data)))
      guessedRMSD <- (sqrt(input$msd) * max(mdv))
      correctEqualizer <- (RMSD^2)/(max(mdv)^2)
      if(abs(guessedRMSD - RMSD)<0.05){
        output$messagemsd <- renderUI({
          tags$span(paste("Congratulations! Your guess is close to the correct horizontal equalizer: "),
                    tags$p(paste(round(correctEqualizer,6)),style="display:inline; color: red"),tags$br(),
                    paste("The corresponding RMSD is: "),
                    tags$p(paste(round(RMSD,6)),style="display:inline; color: red"),
                    tags$br())
        })
        
        output$showSD <- renderUI({
          actionButton(inputId = "showsd",
                       label = "Show SD")
        })
        
        output$msdPlot <- renderPlot({
          plt <- call(correct = FALSE)
          plt
        })
      }  else if(guessedRMSD - RMSD>0.15){
        output$messagemsd <- renderText("incorrect! decrease")
      } else if((guessedRMSD - RMSD<0.15)&(guessedRMSD-RMSD>0)){
        output$messagemsd <- renderText("incorrect! decrease slightly")
      } else if(guessedRMSD - RMSD < -0.15){
        output$messagemsd <- renderText("incorrect! increase")
      } else if((guessedRMSD - RMSD > -0.15)&(guessedRMSD-RMSD<0)){
        output$messagemsd <- renderText("incorrect! increase slightly")
      }
    })
    
    observeEvent(input$showsd,{
      
      output$msdPlot <- renderPlot({
        plt <- call(correct = TRUE)
        plt
      })
      
      output$showSD <- renderUI({
        tags$span(actionButton(inputId = "showsd",
                               label = "Show SD"),
                  tags$br(),
                  paste("See the three brown dotted lines starting from the origin."), 
                  tags$br(),
                  paste("The correct SD is: "),
                  tags$p(paste(round(sd(data1),6)),style="display:inline; color: red"),
                  tags$br(),
                  actionButton(inputId = "explainDetail",
                     label = "Explain Details"))
      })
    })
    
    observeEvent(input$explainDetail,{
      output$explainDetails <- renderUI({
        tags$span(
          tags$br(),
          paste("Starting from origin 0 go towards H and continue to K.
                Then go horizontally back to the first quardant until it hits 
                the quadratic curve at L. Finally, go vertically down to the SD")
                  
        )
                  
      })
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
