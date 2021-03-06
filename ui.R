library(shiny)
library(rgl)
library(colourpicker)
source("measures.R")

tetrahedronTab <- "$('li.active a').first().html()==='Tetrahedron'"
crossSectionTab <- "$('li.active a').first().html()==='Cross-sections'"
measureTab <- "$('li.active a').first().html()==='Measure definitions'"
helpTab <- "$('li.active a').first().html()==='Help'"
nonMeasureTab <- "$('li.active a').first().html()!=='Measure definitions'&&$('li.active a').first().html()!=='Help'"

rglwgtctrl <- function(inputId, value="", nrows, ncols) {
  tagList(
    singleton(tags$head(tags$script(src = "js/rglwidgetaux.js"))),
    tags$div(id = inputId, class = "rglWidgetAux", as.character(value))
  )
}

shinyUI(fluidPage(
  registerSceneChange(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")),
  tags$head(tags$link(rel = "icon", type = "image/png", href = "img/favicon.png")),
  tags$head(tags$script(src = "js/custom.js")),
  
  # Header
  fluidRow(
    column(width = 8, includeMarkdown("www/header.md"), class="header"),
    column(width = 4, a(img(src="img/put_logo.png", id="logo"), href="http://www.put.poznan.pl/"))
  ),
  br(),
  sidebarLayout(
    # Sidebar with options
    sidebarPanel(
      conditionalPanel(nonMeasureTab,
          selectInput("measure", label = "Measure", selected = "Accuracy",
                      choices = list(Classification = sort(classificationMeasures),
                                     Interestingness = sort(interestingnessMeasures),
                                     Misc = sort(setdiff(names(measureList),
                                                        c(classificationMeasures, interestingnessMeasures))))),
          textInput("customMeasure", label ="Custom function", value = "", placeholder="Write an expression..."),
          conditionalPanel("output.hasAlpha == true",
            sliderInput("alpha", label="Measure parameter (alpha)", min = 0, max = 1, value = 0.5, 
                        animate=animationOptions(interval = 800, loop = T))),
          conditionalPanel("output.hasBeta == true",
            sliderInput("beta", label="Measure parameter (beta)", min = 0, max = 1, value = 0.5, 
                        animate=animationOptions(interval = 800, loop = T)))),
      conditionalPanel(crossSectionTab,
            sliderInput("ratio", label = "Minority ratio", value = 0.5,
                      min = 0.1, max = 0.9, step = 0.05,
                      animate=animationOptions(interval = 700, loop = T))),
      conditionalPanel(tetrahedronTab,
          selectInput("resolution", label = "Resolution [points]",
                     choices = list(969, 6545, 47905#, 156849
                                    ))),
      conditionalPanel(nonMeasureTab,
          selectInput("palette", label = "Color palette",
                      choices = list("Jet", "Inversed spectral" = "Spectral", "Emerald to brown" = "BrBG",
                                     "Green to pink" = "PiYG", "Green to purple" = "PRGn",
                                     "Purple to orange" ="PuOr", "Blue to red" = "RdBu", "Grey to red" = "RdGy", 
                                     "Blue-yellow-red" = "RdYlBu", "Green-yellow-red" = "RdYlGn", "Greys" = "Greys",
                                     "Reds" = "YlOrRd")),
          colourInput("naColor", "Undefined value color", "magenta")),
      conditionalPanel(tetrahedronTab,
                       sliderInput("pointSize", label = "Point size", min = 1, max = 16, value = 8),
                       sliderInput("layers", label = "Layers (internal view)", min = 0, max = 1, value = 1, step = 0.05, ticks = F, 
                                   animate=animationOptions(interval = 800, loop = T))),
      conditionalPanel(nonMeasureTab,
          checkboxInput("showTitle", label = "Show measure name on plot", value = TRUE),
          checkboxInput("showLabels", label = "Show labels", value = TRUE)),
      conditionalPanel(tetrahedronTab,
                       checkboxInput("showSilhouette", label = "Show tetrahedron wireframe", value = TRUE)),
      conditionalPanel(crossSectionTab,
                       checkboxInput("showContour", label = "Show contour", value = TRUE)),
      conditionalPanel(tetrahedronTab,
                       actionButton("snapshot", label = "Save snapshot", icon = icon("camera"), width = "100%"),
                       downloadButton("downloadWidget", label = "Save to html", class="tetrahedron-download-btn")),
      conditionalPanel(crossSectionTab,
                       fluidRow(
                           div(class="col-sm-7 cross-section-snapshot-btn",
                               downloadButton("downloadCrossSection", label = "Save image")),
                           div(class="col-sm-5 cross-section-snapshot-list",
                               selectInput("imgType", label="", selected = "png",
                                           choices = c("eps", "tex", "pdf", "jpeg", "png", "svg"))))),
      conditionalPanel(measureTab,
                       p("All visualized classification measures are defined based on a two-class confusion matrix."),
                       p("Interestingness measures are defined based on a contingency table summarizing examples
                         satisfying and not satisfying the evidence or hypothesis of a rule."),
                       p("The remaining measures are based on a two-by-two matrix: \\(\\begin{bmatrix}a & c\\\\b & d\\end{bmatrix}\\)."),
                       class="measures"),
      conditionalPanel(helpTab,
                       includeMarkdown("www/helpContents.md"),
                       class="measures"),
      rglwgtctrl('ctrlplot3d')
    ),
    
    # Generated 3d plot
    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel("Tetrahedron", div(class = "plot-container",
                                    tags$img(src = "img/spinner.svg", id="rgl-spinner"),
                                    tags$span(textOutput("tetrahedronInfo"), class="info"),
                                    rglwidgetOutput("rglPlot", width = "550px", height = "550px"),
                                    plotOutput("colorKeyPlot", width = "550px", height = "50px"))),
        tabPanel("Cross-sections",
                 div(class = "hover-plot-wrapper plot-container",
                     tags$span(textOutput("crossSectionInfo"), class="info"),
                     tags$img(src = "img/spinner.svg", id = "cross-section-spinner"),
                     plotOutput("crossSectionPlot", width = "400px", height = "400px",
                                hover = hoverOpts(id ="plotHover", delay = 150, delayType = "debounce")),
                     uiOutput("hoverInfo"))),
        tabPanel("Measure definitions",
                 withMathJax(),
                 div(includeMarkdown("www/measures.md"), class="measures")),
        tabPanel("Help",
                 withMathJax(),
                 div(includeMarkdown("www/help.md"), class="help"))
      )
    )
  ),
  title = "Tetrahedron Measure Visualization"
))