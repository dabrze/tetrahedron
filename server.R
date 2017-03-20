# Must be executed BEFORE rgl is loaded on headless devices.
options(rgl.useNULL=TRUE)
options(shiny.usecairo=T)

library(Cairo)
library(svglite)
library(ggplot2)
library(plot3D)
library(shiny)
library(rgl)
library(htmlwidgets)
library(jsonlite)
library(geometry)
library(RColorBrewer)
library(dplyr)
source("measures.R")

shinyServer(function(input, output, session) {
  getTetrahedronPoints <- reactive({
    resolutions[[as.character(input$resolution)]]
  })
  
  getMeasureValues <- reactive({
    validateInputs()
    points <- getTetrahedronPoints()
    a <- points[,1]
    b <- points[,2]
    c <- points[,3]
    d <- points[,4]
    n <- max(points[1,])
    
    if (input$customMeasure == "") {
      measureList[[input$measure]](a, b, c, d, input$alpha, input$beta)
    } else {
      p1=input$alpha
      p2=input$beta
      result <- eval(parse(text=input$customMeasure))
      
      if (length(result) == 1){
        rep(result, length(a))
      } else {
        result
      }
    }
  })
  
  getCrosssectionMeasureValues <- reactive({
    validateInputs()
    points <- resolutions[["47905"]]
    a <- points[,1]
    b <- points[,2]
    c <- points[,3]
    d <- points[,4]
    n <- max(points[1,])
    
    if (input$customMeasure == "") {
      measureList[[input$measure]](a, b, c, d, input$alpha, input$beta)
    } else {
      p1=input$alpha
      p2=input$beta
      result <- eval(parse(text=input$customMeasure))
      
      if (length(result) == 1){
        rep(result, length(a))
      } else {
        result
      }
    }
  })
  
  getCrossSection <- reactive({
    points <- resolutions[["47905"]]
    crossSectionPoints <- ((points[,1] + points[,3]) == as.integer(input$ratio * max(points[1,])))
    
    a <- points[,1]
    b <- points[,2]
    c <- points[,3]
    d <- points[,4]
    n <- max(points[1,])
    v <- getCrosssectionMeasureValues()
    
    # Order the points to achieve a proper cross-section
    orderedPoints <- v[crossSectionPoints][order(b[crossSectionPoints], a[crossSectionPoints], decreasing = c(T,T))]
    # Sadly, we have to clip inifinite values to plot them correctly
    orderedPoints[orderedPoints == -Inf] <- range(orderedPoints, na.rm = T, finite = T)[1]
    orderedPoints[orderedPoints == Inf] <- range(orderedPoints, na.rm = T, finite = T)[2]

    matrix(orderedPoints, nrow = input$ratio*n+1)
  })
  
  getPlotTitle <- reactive({
    if (input$showTitle) {
      if(input$customMeasure == "") {
        input$measure
      } else {
        input$customMeasure
      }
    } else {
      ""
    }
  })
  
  updateRgl <- reactive({
    deletions <- rgl.ids()$id
    
    points <- resolutions[[as.character(input$resolution)]]
    a <- points[,1]
    b <- points[,2]
    c <- points[,3]
    d <- points[,4]
    n <- max(points[1,])
    v <- getMeasureValues()
    cls <- (isClassificationMeasure(input$measure) && input$customMeasure == "")
    vertices <- t(matrix(c(+1, +1, +1, -1, +1, -1, -1, -1, +1, +1, -1, -1), ncol=4))
    
    x <- (a-b-c+d)/n
    y <- (a+b-c-d)/n
    z <- (a-b+c-d)/n
    
    if (!all(is.na(v))) {
      pal <- colorRampPalette(getPallete(input$palette))(max(dense_rank(v), na.rm = T))
    } else {
      pal <- colorRampPalette(getPallete(input$palette))(1)
    }
    colors <- pal[dense_rank(v)]
    colors[is.na(colors)] <- input$naColor
    layers <- getSkinLayers(points, n, input$layers)

    mesh <- plot3d(x[layers], y[layers], z[layers], col = colors[layers],
                   box = F, axes = F, size = input$pointSize, 
                   main = getPlotTitle(), xlab="", ylab = "", zlab="")
    
    if (input$showLabels) {
      text <- text3d(vertices * 1.2, texts=if(cls) c("TP", "FP", "FN", "TN") else c("A", "B", "C", "D"))
    }
    
    if (input$showSilhouette) {
      lines <- lines3d(vertices[combn(nrow(vertices), 2),]* (1 + 0.005*input$pointSize), col="#444444", lwd = 1)
    }
    
    additions <- rgl.ids()$id
    
    list(add=additions, delete=deletions)
  })
  
  getCrossSectionPlot <- function(){
    cls <- (isClassificationMeasure(input$measure) && input$customMeasure == "")
    crossection <- getCrossSection()
    
    par(mar=c(2,2.5,3,2.5))
    if (input$showContour && length(unique(as.vector(crossection))) > 1) {
      contour <- list(col = "#FFFFFF", labcex = 1, lwd = 2, alpha = 0.75)
    } else {
      contour <- FALSE
    }

    image2D(crossection, col=colorRampPalette(getPallete(input$palette))(256),
            NAcol=input$naColor, resfac = 4, contour=contour,
            colkey = FALSE, xaxt='n', yaxt='n', xlab="", ylab="")
    
    if (input$showLabels) {
      title(main=ifelse(input$showTitle, getPlotTitle(), ""), line=2)
      cls <- (isClassificationMeasure(input$measure) && input$customMeasure == "")
      
      if (cls) {
        mtext(expression(bar("FP")), side=1, line=0.5)
        mtext(expression("TPFP"), side=1, line=0.5, adj=-0.05)
        mtext(expression("FNFP"), side=1, line=0.5, adj=1.05)
        
        mtext(expression(bar("TP")), side=2, line=0.5, las=1)
        
        mtext(expression(bar("TN")), side=3, line=0.5)
        mtext(expression("TPTN"), side=3, line=0.5, adj=-0.05)
        mtext(expression("FNTN"), side=3, line=0.5, adj=1.05)
        
        mtext(expression(bar("FN")), side=4, line=0.5, las=1)
      } else {
        mtext(expression(bar("B")), side=1, line=0.5)
        mtext(expression("AB"), side=1, line=0.5, adj=-0.05)
        mtext(expression("CB"), side=1, line=0.5, adj=1.05)

        mtext(expression(bar("A")), side=2, line=0.5, las=1)

        mtext(expression(bar("D")), side=3, line=0.5)
        mtext(expression("AD"), side=3, line=0.5, adj=-0.05)
        mtext(expression("CD"), side=3, line=0.5, adj=1.05)

        mtext(expression(bar("C")), side=4, line=0.5, las=1)
      }
      
      text(5, par("usr")[4], "X", srt = 30, xpd = TRUE, pos = 3)
    } else if (input$showTitle) {
      title(main=getPlotTitle(), line=1.5)
    }
  }
  
  rglObserver <- observe({
    changes <- updateRgl()
    session$sendCustomMessage("sceneChange",
                              sceneChange("rglPlot",
                                          delete = changes$delete,
                                          add = changes$add,
                                          rootSubscene=TRUE,
                                          skipRedraw = FALSE))
  }, suspended = TRUE)
  
  output$tetrahedronInfo <- renderText({
    validateInputs()
    "Drag to rotate, scroll to zoom"
  })
  
  output$crossSectionInfo <- renderText({
    validateInputs()
    "Hover to display value"
  })
  
  # Periodically get view parameters from browser, if we get them we know that rgl is working
  observe({
    par <- isolate(getBrowserPar3d(input))
    if (!par$userMatrix && !par$zoom) {
      invalidateLater(500, session)
      session$sendInputMessage("ctrlplot3d",list(cmd="getpar3d", rglwidgetId="rglPlot"))
    } else {
      rglObserver$resume()
      tabObserver$resume()
    }
  })
  
  # We don't want to recalculate the tetrahedron when other tabs are active
  # (we have to this manually since we're controlling the rgl scene with an observer)
  tabObserver <- observe({
    activeTab <- input$tabs
    if (activeTab == "Tetrahedron") {
      rglObserver$resume()
    } else {
      rglObserver$suspend()
    }
  }, suspended = TRUE)
  
  # Plot measure visualization
  output$rglPlot <- renderRglwidget({
    isolate(updateRgl())
    rglwidget()
  })
  
  output$colorKeyPlot <- renderPlot({
    v <- getMeasureValues()
    
    if (!all(is.na(v))) {
      pal <- colorRampPalette(getPallete(input$palette))(max(dense_rank(v), na.rm = T))
    } else {
      pal <- colorRampPalette(getPallete(input$palette))(1)
    }
    n = length(pal)
    minValue = min(v, na.rm = T)
    maxValue = max(v, na.rm = T)
    midValue = v[match(floor((max(dense_rank(v), na.rm = T) + min(dense_rank(v), na.rm = T))/2.0), dense_rank(v))]
    decimalPlaces = 2
    
    par(mar = c(2,1,0,1))
    image(1:n, 1, as.matrix(1:n), col = pal, xlab="", ylab = "", xaxt="n", yaxt = "n", bty = "n")
    axis(1, at=c(1,floor((n+1)/2), n), lwd = 1.3, cex.axis=1.15,
         labels=c(format(round(minValue, decimalPlaces), nsmall = decimalPlaces),
                  format(round(midValue, decimalPlaces), nsmall = decimalPlaces),
                  format(round(maxValue, decimalPlaces), nsmall = decimalPlaces)))
  })
  
  # Plot cross-section
  output$crossSectionPlot <- renderPlot({
      getCrossSectionPlot()
    },
    width = reactive({validateInputs(); nrow(getCrossSection())*csMult}),
    height = reactive({validateInputs(); ncol(getCrossSection())*csMult})
  )
  
  # Cross-section tooltip
  output$hoverInfo <- renderUI({
    hover <- input$plotHover
    data <- getCrossSection()
    point <- nearPoints(expand.grid(1:nrow(data)/nrow(data), 1:ncol(data)/ncol(data)), hover, 
                        xvar = "Var1", yvar = "Var2", threshold = csMult, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate position
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # return tooltip
    div(
      id = "hover-tooltip",
      class = "well well-sm",
      style = paste0("left:", left_px + 5, "px; top:", top_px + 5, "px;"),
      p(HTML(paste0("<b> Value: </b>", round(data[as.integer(rownames(point))], 3))))
    )
  })
  
  # Download cross-section image
  output$downloadCrossSection <- downloadHandler(
    filename = function() { 
      paste0(ifelse(input$customMeasure == "", input$measure, "custom_measure"),
             "_p=", input$ratio, "_", format(Sys.time(), "%d-%m-%YT%H-%M-%S"), ".", input$imgType)
    },
    content = function(file) 
    {
      ggsave(file, device = input$imgType, plot = getCrossSectionPlot(),
             width = nrow(getCrossSection())*csMult/2,
             height = ncol(getCrossSection())*csMult/2,
             unit="mm")
    }
  )
  
  output$downloadWidget <- downloadHandler(
    filename = function() { 
      paste0(ifelse(input$customMeasure == "", input$measure, "custom_measure"),
             "_", format(Sys.time(), "%d-%m-%YT%H-%M-%S"), ".html")
    },
    content = function(file)
    {
      htmlwidgets::saveWidget(rglwidget(), file)
    }
  )
  
  output$hasAlpha <- reactive({
    if ("alpha" %in% names(measureParameters[[input$measure]])
        || grepl("p1", input$customMeasure)){
      if(input$customMeasure == ""){
        updateSliderInput(session, "alpha",
                          label = "Measure parameter (alpha)",
                          min = measureParameters[[input$measure]]$alpha$min,
                          max = measureParameters[[input$measure]]$alpha$max,
                          value = measureParameters[[input$measure]]$alpha$default,
                          step = measureParameters[[input$measure]]$alpha$step)
      } else {
        updateSliderInput(session, "alpha",
                          label = "Measure parameter (p1)",
                          min = 0,
                          max = 1,
                          value = 0.5,
                          step = 0.1)
      }
      TRUE
    } else {
      FALSE
    }
  })
  outputOptions(output, 'hasAlpha', suspendWhenHidden = FALSE)
  
  output$hasBeta <- reactive({
    if ("beta" %in% names(measureParameters[[input$measure]])
        || grepl("p2", input$customMeasure)){
      if(input$customMeasure == ""){
        updateSliderInput(session, "beta",
                          label = "Measure parameter (beta)",
                          min = measureParameters[[input$measure]]$beta$min,
                          max = measureParameters[[input$measure]]$beta$max,
                          value = measureParameters[[input$measure]]$beta$default,
                          step = measureParameters[[input$measure]]$beta$step)
      } else {
        updateSliderInput(session, "beta",
                          label = "Measure parameter (p2)",
                          min = 0,
                          max = 1,
                          value = 0.5,
                          step = 0.01)
      }
      TRUE
    } else {
      FALSE
    }
  })
  outputOptions(output, 'hasBeta', suspendWhenHidden = FALSE)
  
  validateInputs <- reactive({
    a=1; b=1; c=1; d=1; n=4; p1=1; p2=1;
    
    shiny::validate(
      need(input$customMeasure == "" ||
             tryCatch({ eval(parse(text=input$customMeasure)); TRUE },
                      error = function(e) { FALSE }), 
           "Invalid expression in custom function."),
      need(input$customMeasure == "" || grepl("^[a-dp0-9 n().^%*/+-]+$", input$customMeasure),
           "In custom functions, please use only: a, b, c, d, n, p1, p2, numbers, and math operators (+, -, *, /, ^). For example, try: a/(b-c)."),
      need(input$ratio >= 0.1 && input$ratio <= 0.9, 
           "The minority ratio has to be between 0.1 and 0.9 to properly display the image.")
    )
  })
})

getPallete <- function(paletteName) {
  if (paletteName == "Jet") { # for Matlab fans ;)
    c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
  } else if (paletteName == "Greys" || paletteName == "YlOrRd"){
    brewer.pal(9,paletteName)
  } else {
    rev(brewer.pal(9,paletteName))
  }
}

getSkinLayers <- function(points, n, layer) {
  which(apply(points, 1, min) >= (n/4 - layer*n/4))
}

getBrowserPar3d <- function(input) {
  umat <- 0
  zoom <- 0
  
  try({
    jsonpar3d <- input$ctrlplot3d
    if (!is.null(jsonpar3d) && jsonlite::validate(jsonpar3d)){
      par3dout <- fromJSON(jsonpar3d)
      umat <- matrix(unlist(par3dout$userMatrix),4,4)
      zoom <- par3dout$zoom
    }
  })
  
  return(list(userMatrix = umat, zoom = zoom))
}

resolutions <- list(
  "969" = readRDS("www/grids/x16.RDS"),
  "6545" = readRDS("www/grids/x32.RDS"),
  "47905" = readRDS("www/grids/x64.RDS"),
  "156849" = readRDS("www/grids/x96.RDS")
)

csMult <- 12
