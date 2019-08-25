# libraries
library(shiny)
library(ggplot2)
library(plotly)
library(RColorBrewer)

# library(rdrop2)
# token<-drop_auth()
# saveRDS(token, "droptoken.rds")
# token<-readRDS("droptoken.rds")
# drop_acc(dtoken=token)

# data and varibles
# index for variables and years starts from column 8, year 2000 is in column 8 (9, 10), 23 (24, 25), 38 (39, 40), etc., 
# variable mat is from column 8 to 52, land data is in column 8, 11, 14, etc. 
# combine all index together: year 2000, variable mat, land data is in colunmn 8, year 2008, variable ntw, beetle data
# is in 7+45*3(three variables before ntw, each variable has 15 years and 3 datasets)+8*3(8 years before 2008 and each year
# has three datasets)+3(beetle is the third dataset), so the selected data will be 
# var - (8+45*(i-1)):(7+45*i); year - repetitive, range, ((7+45*(i-1))+3*(k1-1)+1):((7+45*(i-1))+3*k2); data - multiple years, 
# ((7+45*(i-1))+3*(k1-1)+1/2/3/1,2/1,3/2,3/1,2,3); i is the index of variable from 1 to 15, k is the index of year
# from 1 to 15; k1 is the minimum, k2 is the maximum in the range

#setwd("/Users/dongmeichen/Documents/beetle/data")
#save(list = ls(all.names = TRUE), file = "climate_space_test.RData", envir = .GlobalEnv)

knitr::knit(text ='```{r}
            load(url("https://github.com/dongmeic/climate-space/blob/master/data/climate_space_test.RData?raw=true"))
            ```')

years <- 2000:2014
dtnms <- c("Land", "Tree", "Beetle")
varnames <- c("mat", "mtaa", "mta", "ntw", "nto", "ntj", "ntm", "xta","map", "cpja", "pja", "cpos", "pos", 
            "gsp", "vgp")
varlongnames <- c("annual mean monthly average of daily mean temperature in the past water year - mat",
                "mean of monthly average of daily mean temperature from April to August - mtaa",
                "monthly average of daily mean temperature in August - mta",
                "minimum of monthly average of daily minimum temperature between Dec. and Feb. - ntw",
                "monthly average of daily minimum temperature in October - nto",
                "monthly average of daily minimum temperature in January - ntj",
                "monthly average of daily minimum temperature in March - ntm",
                "monthly average of daily maximum temperature in August - xta",
                "mean annual precipitation in the past water year - map",
                "cumulative precipitation from June to August the current and previous year - cpja",
                "precipitation from June to August the previous year - pja",
                "cumulative precipitation from October to September the current and previous year - cpos",
                "precipitation from October to September the previous year - pos",
                "growing season precipitation the current year - gsp",
                "variability of growing season precipitation - vgp")

axislabs <- c("Previous water year mean T (°C)",
              "Mean T from Apr to Aug (°C)","Mean Aug T (°C)",
              "Minimum winter T (°C)", "Minimum Oct T (°C)","Minimum Jan T (°C)",
              "Minimum Mar T (°C)", "Maximum Aug T (°C)",
              "Previous water year mean P (mm)",
              "Two-year cumulative P from Jun to Aug (mm)",
              "P from Jun to Aug the previous year (mm)",
              "Two-year cumulative P from Oct to Sep (mm)",
              "P from Oct to Sep the previous year (mm)",
              "Growing season precipitation (mm)",
              "Variability of growing season precipitation"
              )

#tgtpts <- drop_read_csv(paste0("shiny/climatic_variables_longlat_var_test.csv"), dtoken=token)
#tgtpts <- read.csv("/Users/dongmeichen/Dropbox/shiny/climatic_variables_longlat_var_test.csv")
tgtpts <- tgtpts[sample(1:length(tgtpts$x), 800, replace = TRUE),]
# user interface
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://bootswatch.com/cerulean/bootstrap.css');
      .col-sm-4 { width: 25%;}
      .col-sm-8 { width: 75%;}
    "))
  ),
  titlePanel("Climate space for the mountain pine beetle in North America"),
  sidebarLayout(
    sidebarPanel(
      h4("Filter"),
      checkboxGroupInput("year", "Select MPB outbreak years", choices = c(2000:2014), 2014, inline = TRUE),
      sliderInput("lon", "Longtitude range", -142, -92, value = range(tgtpts$lon)),
      sliderInput("lat", "Latitude range", 32, 62, value = range(tgtpts$lat)),
      checkboxGroupInput("DEM","Select a DEM model", c("ETOPO","SRTM"), inline = TRUE),
      conditionalPanel(
        condition = "input.DEM == 'ETOPO'",
        sliderInput("etopo1", "Elevation from the ETOPO1 Model",
                    -71, 3799, value = range(tgtpts$etopo1))
      ),
      conditionalPanel(
        condition = "input.DEM == 'SRTM'",
        sliderInput("srtm30", "Elevation from the SRTM Model",
                    -129, 3702, value = range(tgtpts$srtm30))
      ),
      checkboxGroupInput("data", "Select groups of data",
                         choices=c("Land","Tree","Beetle"), c("Land","Tree","Beetle"),inline = TRUE),
      selectInput("colors", "Color scheme",
                  rownames(subset(brewer.pal.info, category == "qual")),
                  "Set1"),
      sliderInput("trans", "Transparancy", 0.1, 1, c(0.1,0.7)),
      selectInput("xvar", "X-axis variable", varnames, selected = "ntw"),
      selectInput("yvar", "Y-axis variable", varnames, selected = "xta"),
      span(textOutput("vartext"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Climate space", plotlyOutput("plot1")),
        tabPanel("Boxplot - X", plotlyOutput("plot2")), 
        tabPanel("Boxplot - Y", plotlyOutput("plot3")),
        tabPanel("Map - X", plotlyOutput("plot4")), #width = 800, height = 600
        tabPanel("Map - Y", plotlyOutput("plot5"))
        )
    )
  )
)
# server
server <- function(input, output,session){
  
  fltedpts <- reactive({
    minlon <- input$lon[1]
    maxlon <- input$lon[2]
    minlat <- input$lat[1]
    maxlat <- input$lat[2]
    mineto <- input$etopo1[1]
    maxeto <- input$etopo1[2]
    minsrt <- input$srtm30[1]
    maxsrt <- input$srtm30[2]
    # Apply filters
    tgtpts %>%
      filter(
        lon <= maxlon,
        lon >= minlon,
        lat <= maxlat,
        lat >= minlat,
        etopo1 <= maxeto,
        etopo1 >= mineto,
        srtm30 <= maxsrt,
        srtm30 >= minsrt
      )
  })
  
  get_df <- function(){
    i <- which(varnames==input$xvar)
    j <- which(varnames==input$yvar)
    n <- length(input$year)
    d <- length(input$data)
    dtsets <- c(match(input$data,dtnms))
    if (n==1){
      k <- which(years==input$year)
      xdtpts <- fltedpts()[,(7+45*(i-1)+3*(k-1)+1):(7+45*(i-1)+3*k)]
      ydtpts <- fltedpts()[,(7+45*(j-1)+3*(k-1)+1):(7+45*(j-1)+3*k)]
      if(d==1){
        xpltpts <- c(xdtpts[,dtsets])
        ypltpts <- c(ydtpts[,dtsets])
        df <- data.frame(xvar=xpltpts,yvar=ypltpts, key=rep(dtnms[dtsets],length(xpltpts)))
      }else if(d==2){
        xpltpts.1 <- c(xdtpts[,dtsets[1]])
        xpltpts.2 <- c(xdtpts[,dtsets[2]])
        ypltpts.1 <- c(ydtpts[,dtsets[1]])
        ypltpts.2 <- c(ydtpts[,dtsets[2]])
        xpltpts <- c(xpltpts.1,xpltpts.2)
        ypltpts <- c(ypltpts.1,ypltpts.2)
        df <- data.frame(xvar=xpltpts,yvar=ypltpts,
                         key=c(rep(dtnms[dtsets[1]],length(xpltpts.1)),
                               rep(dtnms[dtsets[2]],length(xpltpts.2)),
                               rep(dtnms[dtsets[1]],length(xpltpts.1)),
                               rep(dtnms[dtsets[2]],length(xpltpts.2))))
      }else{
        xpltpts.1 <- c(xdtpts[,dtsets[1]])
        xpltpts.2 <- c(xdtpts[,dtsets[2]])
        xpltpts.3 <- c(xdtpts[,dtsets[3]])
        ypltpts.1 <- c(ydtpts[,dtsets[1]])
        ypltpts.2 <- c(ydtpts[,dtsets[2]])
        ypltpts.3 <- c(ydtpts[,dtsets[3]])
        xpltpts <- c(xpltpts.1,xpltpts.2,xpltpts.3)
        ypltpts <- c(ypltpts.1,ypltpts.2,ypltpts.3)
        df <- data.frame(xvar=xpltpts,yvar=ypltpts,
                         key=c(rep(dtnms[dtsets[1]],length(xpltpts.1)),
                               rep(dtnms[dtsets[2]],length(xpltpts.2)),
                               rep(dtnms[dtsets[3]],length(xpltpts.3)),
                               rep(dtnms[dtsets[1]],length(xpltpts.1)),
                               rep(dtnms[dtsets[2]],length(xpltpts.2)),
                               rep(dtnms[dtsets[3]],length(xpltpts.3))))
      }
    }else{
      k <- which(years==min(input$year))
      xyrpts <- fltedpts()[,(7+45*(i-1)+3*(k-1)+1):(7+45*(i-1)+3*k)]
      yyrpts <- fltedpts()[,(7+45*(j-1)+3*(k-1)+1):(7+45*(j-1)+3*k)]
      for (m in 2:n){
        k <- which(years==input$year[m])
        xyrpts_m <- fltedpts()[,(7+45*(i-1)+3*(k-1)+1):(7+45*(i-1)+3*k)]
        xyrpts <- cbind(xyrpts, xyrpts_m)
        yyrpts_m <- fltedpts()[,(7+45*(j-1)+3*(k-1)+1):(7+45*(j-1)+3*k)]
        yyrpts <- cbind(yyrpts, yyrpts_m)
      }
      xdtpts <- array(data.matrix(xyrpts), dim=c(dim(xyrpts)[1],3,(dim(xyrpts)[2]/3)))
      xdtpts <- xdtpts[,dtsets,]
      ydtpts <- array(data.matrix(yyrpts), dim=c(dim(xyrpts)[1],3,(dim(yyrpts)[2]/3)))
      ydtpts <- ydtpts[,dtsets,]
      if (d ==1){
        ncols <- dim(xdtpts)[2]
      }else{
        ncols <- dim(xdtpts)[2] * dim(xdtpts)[3]
      }
      dim(xdtpts) <- c(dim(xyrpts)[1], ncols)
      dim(ydtpts) <- c(dim(xyrpts)[1], ncols)
      if (d==1){
        xpltpts <- c(xdtpts)
        ypltpts <- c(ydtpts)
        df <- data.frame(xvar=xpltpts,yvar=ypltpts, key=rep(dtnms[dtsets],length(xpltpts)))
      }else if(d==2){
        xpltpts.1 <- c(xdtpts[,seq(1,ncols,d)])
        xpltpts.2 <- c(xdtpts[,seq(2,ncols,d)])
        ypltpts.1 <- c(ydtpts[,seq(1,ncols,d)])
        ypltpts.2 <- c(ydtpts[,seq(2,ncols,d)])
        xpltpts <- c(xpltpts.1,xpltpts.2)
        ypltpts <- c(ypltpts.1,ypltpts.2)
        df <- data.frame(xvar=xpltpts,yvar=ypltpts,
                         key=c(rep(dtnms[dtsets[1]],length(xpltpts.1)),
                               rep(dtnms[dtsets[2]],length(xpltpts.2)),
                               rep(dtnms[dtsets[1]],length(xpltpts.1)),
                               rep(dtnms[dtsets[2]],length(xpltpts.2))))
      }else{
        xpltpts.1 <- c(xdtpts[,seq(1,ncols,d)])
        xpltpts.2 <- c(xdtpts[,seq(2,ncols,d)])
        xpltpts.3 <- c(xdtpts[,seq(3,ncols,d)])
        ypltpts.1 <- c(ydtpts[,seq(1,ncols,d)])
        ypltpts.2 <- c(ydtpts[,seq(2,ncols,d)])
        ypltpts.3 <- c(ydtpts[,seq(3,ncols,d)])
        xpltpts <- c(xpltpts.1,xpltpts.2,xpltpts.3)
        ypltpts <- c(ypltpts.1,ypltpts.2,ypltpts.3)
        df <- data.frame(xvar=xpltpts,yvar=ypltpts,
                         key=c(rep(dtnms[dtsets[1]],length(xpltpts.1)),
                               rep(dtnms[dtsets[2]],length(xpltpts.2)),
                               rep(dtnms[dtsets[3]],length(xpltpts.3)),
                               rep(dtnms[dtsets[1]],length(xpltpts.1)),
                               rep(dtnms[dtsets[2]],length(xpltpts.2)),
                               rep(dtnms[dtsets[3]],length(xpltpts.3))))
      }
    }
    df$order <- c(1:length(df$key))
    df$key <- factor(df$key,levels=unique(df$key[order(df$order)]))
    return(df)
  }
  
  get_df2 <- function(){
    i <- which(varnames==input$xvar)
    j <- which(varnames==input$yvar)
    n <- length(input$year)
    if (n==1){
      k <- which(years==input$year)
      xyrpts <- fltedpts()[,(7+45*(i-1)+3*(k-1)+1):(7+45*(i-1)+3*k)]
      yyrpts <- fltedpts()[,(7+45*(j-1)+3*(k-1)+1):(7+45*(j-1)+3*k)]
    }else{
      k <- which(years==min(input$year))
      xyrpts <- fltedpts()[,(7+45*(i-1)+3*(k-1)+1):(7+45*(i-1)+3*k)]
      yyrpts <- fltedpts()[,(7+45*(j-1)+3*(k-1)+1):(7+45*(j-1)+3*k)]
      for (m in 2:n){
        k <- which(years==input$year[m])
        xyrpts_m <- fltedpts()[,(7+45*(i-1)+3*(k-1)+1):(7+45*(i-1)+3*k)]
        xyrpts <- cbind(xyrpts, xyrpts_m)
        yyrpts_m <- fltedpts()[,(7+45*(j-1)+3*(k-1)+1):(7+45*(j-1)+3*k)]
        yyrpts <- cbind(yyrpts, yyrpts_m)
      }
    }
    d <- length(input$data)
    dtsets <- c(match(input$data,dtnms))
    if (n==1){
      if (d==1){
        xdtpts <- xyrpts[,dtsets]
        ydtpts <- yyrpts[,dtsets]
        df <- cbind(fltedpts()[,1:7],xdtpts,ydtpts)
      }else if(d==2){
        xdtpts.1 <- xyrpts[,dtsets[1]]
        ydtpts.1 <- yyrpts[,dtsets[1]]
        xdtpts.2 <- xyrpts[,dtsets[2]]
        ydtpts.2 <- yyrpts[,dtsets[2]]
        df <- cbind(fltedpts()[,1:7],xdtpts.1,xdtpts.2,ydtpts.1,ydtpts.2)
      }else{
        xdtpts.1 <- xyrpts[,dtsets[1]]
        ydtpts.1 <- yyrpts[,dtsets[1]]
        xdtpts.2 <- xyrpts[,dtsets[2]]
        ydtpts.2 <- yyrpts[,dtsets[2]]
        xdtpts.3 <- xyrpts[,dtsets[3]]
        ydtpts.3 <- yyrpts[,dtsets[3]]  
        df <- cbind(fltedpts()[,1:7],xdtpts.1,xdtpts.2,xdtpts.3,ydtpts.1,ydtpts.2,ydtpts.3)
      }
    }else{
      ncols <- dim(xyrpts)[2]
      if (d==1){
        xdtpts <- xyrpts[,seq(dtsets,ncols,3)]
        ydtpts <- yyrpts[,seq(dtsets,ncols,3)]
        df <- cbind(fltedpts()[,1:7],xdtpts,ydtpts)
      }else if(d==2){
        xdtpts.1 <- xyrpts[,seq(dtsets[1],ncols,3)]
        ydtpts.1 <- yyrpts[,seq(dtsets[1],ncols,3)]
        xdtpts.2 <- xyrpts[,seq(dtsets[2],ncols,3)]
        ydtpts.2 <- yyrpts[,seq(dtsets[2],ncols,3)]
        df <- cbind(fltedpts()[,1:7],xdtpts.1,xdtpts.2,ydtpts.1,ydtpts.2)
      }else{
        xdtpts.1 <- xyrpts[,seq(dtsets[1],ncols,3)]
        ydtpts.1 <- yyrpts[,seq(dtsets[1],ncols,3)]
        xdtpts.2 <- xyrpts[,seq(dtsets[2],ncols,3)]
        ydtpts.2 <- yyrpts[,seq(dtsets[2],ncols,3)]
        xdtpts.3 <- xyrpts[,seq(dtsets[3],ncols,3)]
        ydtpts.3 <- yyrpts[,seq(dtsets[3],ncols,3)]  
        df <- cbind(fltedpts()[,1:7],xdtpts.1,xdtpts.2,xdtpts.3,ydtpts.1,ydtpts.2,ydtpts.3)
      }
    }
    return(df)
  }
  
  get_df3 <- function(){
    df <- get_df2()
    df2 <- df[,-7:-1]; d2 <- (dim(df2)[2])
    df2.x <- df2[,1:(d2/2)]; df2.y <- df2[,((d2/2)+1):d2]
    d <- length(input$data)
    n <- length(input$year)
    if (n==1 & d==1){
      df3 <- cbind(df[,1:7],xvar=df2.x,yvar=df2.y)
    }else if (n==1 & d <=3){
      df2.x.2 <- rowMeans(df2.x, na.rm = TRUE, dims = 1)
      df2.y.2 <- rowMeans(df2.y, na.rm = TRUE, dims = 1)
      df3 <- data.frame(xvar=df2.x.2,yvar=df2.y.2)
      df3 <- cbind(df[,1:7],df3)
    }else{
      d3 <- (dim(df2.x)[2])/d
      df2.x.yr <- df2.x[,1:d3]; df2.y.yr <- df2.y[,1:d3]
      df3.x <- rowMeans(df2.x.yr, na.rm = TRUE, dims = 1)
      df3.y <- rowMeans(df2.y.yr, na.rm = TRUE, dims = 1)
      df3 <- data.frame(xvar=df3.x,yvar=df3.y)
      df3 <- cbind(df[,1:7],df3)
    }
    return(df3)
  }
  
  plttle <- function(a){
    n <- length(input$year)
    if (n==1){
      if (a==1){
        title = paste0("Climate space for MPB in Year ", input$year)
      }else if(a==2){
        title = paste0("Boxplot for X-axis variable in Year ", input$year)
      }else{
        title = paste0("Boxplot for Y-axis variable in Year ", input$year)
      }
    }else{
      if (a==1){
        title = paste0("Climate space for MPB in years (",do.call(paste, c(as.list(input$year), sep=",")),")")
      }else if(a==2){
        title = paste0("Boxplot for X-axis variable in years (",do.call(paste, c(as.list(input$year), sep=",")),")")
      }else{
        title = paste0("Boxplot for Y-axis variable in years (",do.call(paste, c(as.list(input$year), sep=",")),")")
      }
      
    }
    return(title)
  }
  
  output$plot1 <- renderPlotly({
    df <- get_df()
    cols <- brewer.pal(length(input$data), input$colors)[c(2,3,1)]
    plot1 <- ggplot(data=df, aes(x=xvar, y=yvar, color=factor(key), alpha=factor(key))) + geom_point() +
      labs(x= axislabs[which(varnames==input$xvar)], y = axislabs[which(varnames==input$yvar)],
           title=plttle(1))+
      scale_colour_manual(name="Presence", values = cols) +
      scale_alpha_discrete(range=c(input$trans[1], input$trans[2]),guide=FALSE) +
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
    print(ggplotly(plot1, width = 900, height = 900))
  })
  
  output$vartext <- renderText(
    paste0("Note: ",
           "X-axis variable is the ", varlongnames[which(varnames==input$xvar)],
           ", and Y-axis variable is the ", varlongnames[which(varnames==input$yvar)],
           ".")
  )
  output$plot2 <- renderPlotly({
    df <- get_df()
    cols <- brewer.pal(length(input$data), input$colors)[c(2,3,1)]
    plot2 <- ggplot(data=df, aes(x=key, y=xvar, fill=factor(key))) + geom_boxplot() +
      scale_fill_manual(name="Presence", values = cols) +geom_point(size=1.5, stat = "summary", fun.y = "mean", color="white")+ 
      theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
      labs(x="Presence", y=axislabs[which(varnames==input$xvar)], title=plttle(2))
    print(ggplotly(plot2, width = 700, height = 800))
  })
  output$plot3 <- renderPlotly({
    df <- get_df()
    cols <- brewer.pal(length(input$data), input$colors)[c(2,3,1)]
    plot3 <- ggplot(data=df, aes(x=key, y=yvar, fill=factor(key)))+geom_boxplot() + 
      scale_fill_manual(name="Presence", values = cols) +geom_point(size=1.5, stat = "summary", fun.y = "mean", color="white")+ 
      theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
      labs(x="Presence", y=axislabs[which(varnames==input$yvar)], title=plttle(3))
    print(ggplotly(plot3, width = 700, height = 800))
  })
  
  # settings for mapping
  g <- list(
    scope = c('north america'),
    showframe = F,
    showland = T,
    landcolor = toRGB("grey90"),
    coastlinecolor = toRGB("white"),
    projection = list(type = 'azimuthal equal area')
  )
  
  maptle <- function(b){
    n <- length(input$year)
    if (n==1){
      if (b==1){
        title=paste0("A map of Year ", input$year, " values for X-axis variable - ", input$xvar)
      }else{
        title=paste0("A map of Year ", input$year, " values for Y-axis variable - ", input$yvar)
      }
    }else{
      if (b==1){
        title=paste0("A map of the ", n, " years mean values for X-axis variable - ", input$xvar)
      }else{
        title=paste0("A map of the ", n, " years mean values for Y-axis variable - ", input$yvar)
      }
    }
    return(title)
  }
  
  output$plot4 <- renderPlotly({
    df <- get_df3()
    df2 <- df[!is.na(df$xvar) & !is.na(df$yvar),] 
    n <- length(input$year)
    plot4 <- df %>% plot_geo(x = ~lon, y = ~lat) %>% 
      add_text(
        x = -106.3468, y = 56.1304, text = 'Canada', showlegend = F
      ) %>%
      add_text(
        x = -95.7129, y = 37.0902, text = 'United States', showlegend = F
      ) %>%
      add_markers(
        data=df2, x = ~lon, y = ~lat, color=~xvar, text = ~paste(input$xvar,": ",xvar)
      ) %>%
      layout(title=maptle(1), geo = g)
    print(plot4)
  })
  
  output$plot5 <- renderPlotly({
    df <- get_df3()
    df2 <- df[!is.na(df$xvar) & !is.na(df$yvar),] 
    plot5 <- df %>% plot_geo(x = ~lon, y = ~lat) %>% 
      add_text(
        x = -106.3468, y = 56.1304, text = 'Canada', showlegend = F
      ) %>%
      add_text(
        x = -95.7129, y = 37.0902, text = 'United States', showlegend = F
      ) %>%
      add_markers(
        data=df2, x = ~lon, y = ~lat, color=~yvar, text = ~paste(input$yvar,":",yvar)
      ) %>%
      layout(title=maptle(2), geo = g)
    print(plot5)
  })
}
# create the Shiny App
shinyApp(ui = ui, server = server)