# libraries
library(shiny)
library(ggplot2)
library(plotly)
library(RColorBrewer)

# data and varibles
# index for variables and years starts from column 8, year 2000 is in column 8 (9, 10), 23 (24, 25), 38 (39, 40), etc., 
# variable mat is from column 8 to 52, land data is in column 8, 11, 14, etc. 
# combine all index together: year 2000, variable mat, land data is in colunmn 8, year 2008, variable ntw, beetle data
# is in 7+45*3(three variables before ntw, each variable has 15 years and 3 datasets)+8*3(8 years before 2008 and each year
# has three datasets)+3(beetle is the third dataset), so the selected data will be 
# var - (8+45*(i-1)):(7+45*i); year - repetitive, range, ((7+45*(i-1))+3*(k1-1)+1):((7+45*(i-1))+3*k2); data - multiple years, 
# ((7+45*(i-1))+3*(k1-1)+1/2/3/1,2/1,3/2,3/1,2,3); i is the index of variable from 1 to 15, k is the index of year
# from 1 to 15; k1 is the minimum, k2 is the maximum in the range

years <- 2000:2014
dtnms <- c("Land", "Tree", "Beetle")
varnames <- c("mat", "mtaa", "mta", "ntw", "nto", "ntj", "ntm", "xta","map", "cpja", "pja", "cpos", "pos", 
            "gsp", "vgp")
varlongnames <- c("annual mean monthly average of daily mean temperature in the past water year - mat",
                "mean of monthly average of daily mean temperature from April to August - mtaa",
                "monthly average of daily mean temperature in August - mta",
                "minimum of monthly average of daily minimum temperature between Dec and Feb - ntw",
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
              
csvpath <- "/home2/dongmeic/beetle/csvfiles/"
all_pts <- read.csv(paste0(csvpath,"climatic_variables_longlat_var.csv"))
all_pts <- all_pts[,-1] 
# ordf <- all_pts
# all_pts <- all_pts[, -grep("std", colnames(all_pts))]
nobs <- dim(all_pts)[1]
# user interface
ui <- fluidPage(
  titlePanel("Climate space for mountain pine beetle in North America"),
  sidebarLayout(
    sidebarPanel(
      h4("Filter"),
      sliderInput("year", "Mountain pine beetle outbreak years",
                  2000, 2014, value = c(2014, 2014)),
      sliderInput("lon", "Longtitude range", -180, 180, value = range(all_pts$lon)),
      sliderInput("lat", "Latitude range", -5, 85, value = range(all_pts$lat)),
      checkboxGroupInput("DEM","Select a DEM model", c("ETOPO","SRTM"), inline = TRUE),
      conditionalPanel(
        condition = "input.DEM == 'ETOPO'",
        sliderInput("etopo1", "Elevation from the ETOPO1 Model",
                    -1172, 4740, value = range(all_pts$etopo1))
      ),
      conditionalPanel(
        condition = "input.DEM == 'SRTM'",
        sliderInput("srtm30", "Elevation from the SRTM Model",
                    -1244, 5069, value = range(all_pts$srtm30))
      ),
      selectInput("colors", "Color scheme",
                  rownames(subset(brewer.pal.info, category == "qual")),
                  "Set1"),
      sliderInput("trans", "Transparancy", 0.1, 1, c(0.1,0.7)),
      checkboxGroupInput("data", "Select groups of data",
                         choices=c("Land","Tree","Beetle"), c("Land","Tree","Beetle"),inline = TRUE),
      selectInput("xvar", "X-axis variable", varnames, selected = "ntw"),
      selectInput("yvar", "Y-axis variable", varnames, selected = "xta"),
      span(textOutput("vartext"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Climate space", plotlyOutput("plot1")),
        tabPanel("Boxplot - X", plotlyOutput("plot2")), 
        tabPanel("Boxplot - Y", plotlyOutput("plot3")),
        tabPanel("Map - X", plotlyOutput("plot4")),
        tabPanel("Map - Y", plotlyOutput("plot5"))
      )
    )
  )
)
# server
server <- function(input, output,session){
  fltedpts <- reactive({
    minyr <- input$year[1]
    maxyr <- input$year[2]
    minlon <- input$lon[1]
    maxlon <- input$lon[2]
    minlat <- input$lat[1]
    maxlat <- input$lat[2]
    mineto <- input$etopo1[1]
    maxeto <- input$etopo1[2]
    minsrt <- input$srtm30[1]
    maxsrt <- input$srtm30[2]
    mintra <- input$trans
    xi <- which(varnames==input$xvar)
    yi <- which(varnames==input$yvar)
    # Apply filters
    all_pts %>%
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
    k1 <- which(years==input$year[1]); k2 <- which(years==input$year[2])
    i <- which(varnames==input$xvar)
    j <- which(varnames==input$yvar)
    xyrpts <- fltedpts()[,(7+45*(i-1)+3*(k1-1)+1):(7+45*(i-1)+3*k2)]
    yyrpts <- fltedpts()[,(7+45*(j-1)+3*(k1-1)+1):(7+45*(j-1)+3*k2)]
    xdtpts <- array(data.matrix(xyrpts), dim=c(nobs,3,(dim(xyrpts)[2]/3)))
    d <- length(input$data)
    dtsets <- c(match(input$data,dtnms))
    xdtpts <- xdtpts[,dtsets,]
    if (d ==1){
      ncols <- dim(xdtpts)[2]
    }else{
      ncols <- dim(xdtpts)[2] * dim(xdtpts)[3]
    }
    dim(xdtpts) <- c(nobs, ncols)
    ydtpts <- array(data.matrix(yyrpts), dim=c(nobs,3,(dim(yyrpts)[2]/3)))
    ydtpts <- ydtpts[,dtsets,]
    dim(ydtpts) <- c(nobs, ncols)
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
    return(df)
  }
  
  get_df2 <- function(){
    k1 <- which(years==input$year[1]); k2 <- which(years==input$year[2])
    i <- which(varnames==input$xvar)
    j <- which(varnames==input$yvar)
    xyrpts <- fltedpts()[,(7+45*(i-1)+3*(k1-1)+1):(7+45*(i-1)+3*k2)]
    yyrpts <- fltedpts()[,(7+45*(j-1)+3*(k1-1)+1):(7+45*(j-1)+3*k2)]
    ncols <- dim(xyrpts)[2]
    d <- length(input$data)
    dtsets <- c(match(input$data,dtnms))
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
    return(df)
  }
  
  get_df3 <- function(){
    df <- get_df2()
    df2 <- df[,-7:-1]; d2 <- dim(df2)[2]
    df2.x <- df2[,1:(d2/2)]; df2.y <- df2[,((d2/2)+1):d2]
    d3 <- (dim(df2.x)[2])/(length(input$data))
    df2.x.yr <- df2.x[,1:d3]; df2.y.yr <- df2.y[,1:d3]
    df3.x <- rowMeans(df2.x.yr, na.rm = TRUE, dims = 1)
    df3.y <- rowMeans(df2.y.yr, na.rm = TRUE, dims = 1)
    df3 <- data.frame(xvar=df3.x,yvar=df3.y)
    df3 <- cbind(df[,1:7],df3)
    return(df3)
  }
  
  output$plot1 <- renderPlotly({
    df <- get_df()
    cols <- brewer.pal(length(input$data), input$colors)
    k1 <- which(years==input$year[1]); k2 <- which(years==input$year[2])
    plot1 <- ggplot(data=df, aes(x=xvar, y=yvar, color=factor(key), alpha=factor(key))) + geom_point() +
      labs(x= axislabs[which(varnames==input$xvar)], y = axislabs[which(varnames==input$yvar)], 
           title = paste0("MPB climate space (", years[k1], " - ", years[k2],")")) +
      scale_colour_manual(name="Presencce", values = cols) +
      scale_alpha_discrete(range=c(input$trans[1], input$trans[2]),guide=FALSE) +
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
    print(ggplotly(plot1))
  })
  
  output$vartext <- renderText(
    paste0("Note: ",
           "X-axis variable is the ", varlongnames[which(varnames==input$xvar)],
           ", and Y-axis variable is the ", varlongnames[which(varnames==input$yvar)],
           ".")
  )
  output$plot2 <- renderPlotly({
    df <- get_df()
    cols <- brewer.pal(length(input$data), input$colors)
    plot2 <- ggplot(data=df, aes(x=key, y=xvar, fill=factor(key))) + geom_boxplot() +
      scale_fill_manual(values = cols) + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
      labs(x="Presencce", y=axislabs[which(varnames==input$xvar)]) +theme(legend.position="none")
    print(ggplotly(plot2))
  })
  output$plot3 <- renderPlotly({
    df <- get_df()
    cols <- brewer.pal(length(input$data), input$colors)
    plot3 <- ggplot(data=df, aes(x=key, y=yvar, fill=factor(key)))+geom_boxplot() + 
      scale_fill_manual(values = cols) + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
      labs(x="Presencce", y=axislabs[which(varnames==input$yvar)])+theme(legend.position="none")
    print(ggplotly(plot3))
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
  
  output$plot4 <- renderPlotly({
    df <- get_df2()
    df2 <- get_df3()
    cols <- brewer.pal(length(input$data), input$colors)
    lyr <- input$year[2] - input$year[1] + 1
    plot4 <- df %>% plot_geo(x = ~lon, y = ~lat) %>% 
      add_text(
        x = -106.3468, y = 56.1304, text = 'Canada', showlegend = F
      ) %>%
      add_text(
        x = -95.7129, y = 37.0902, text = 'United States', showlegend = F
      ) %>%
      add_markers(
        data = df2, x = ~lon, y = ~lat, text = ~paste(input$xvar,": ",xvar)
      ) %>%
      layout(title=paste0("A map of the ", lyr, " year(s) mean values for Y-axis variable - ", input$yvar), geo = g)
    print(plot4)
  })
  
  output$plot5 <- renderPlotly({
    df <- get_df2()
    df2 <- get_df3()
    cols <- brewer.pal(length(input$data), input$colors)
    lyr <- input$year[2] - input$year[1] + 1
    plot5 <- df %>% plot_geo(x = ~lon, y = ~lat) %>% 
      add_text(
        x = -106.3468, y = 56.1304, text = 'Canada', showlegend = F
      ) %>%
      add_text(
        x = -95.7129, y = 37.0902, text = 'United States', showlegend = F
      ) %>%
      add_markers(
        data = df2, x = ~lon, y = ~lat, text = ~paste(input$yvar,":",yvar)
      ) %>%
      layout(title=paste0("A map of the ", lyr, " year(s) mean values for Y-axis variable - ", input$yvar), geo = g)
    print(plot5)
  })
}
# create the Shiny App
shinyApp(ui = ui, server = server)