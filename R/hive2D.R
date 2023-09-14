#' @export
hive2D <- function(...) {
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML("
              .fullWidth {
                width: 100%;
              }

              .module-title {
                line-height: 41px;
                color: #ffffff;
                font-size: 24px;
                text-align: center;
                vertical-align: middle;
                background-color: #22478a;
                border-radius: 4px;
              }

              .vrtc-tab-panel-menu.vrtc-tab-panel-menu-right {
                width: 50px;
                float: right;
              }

              .vrtc-tab-panel.tab-content {
                width: calc(100% - 50px);
                float: left;
                padding-right: 20px;
                padding-left: 0;
              }

              #display {
                margin-top: 10px;
                margin-bottom: 10px;
              }
          ")
      ),

      shiny::tags$script(
        shiny::HTML("
          $(function() {
            var dim = [0, 0];
            $(window).resize(function() {
              const element = document.querySelector('#plot-panel');
              dim[0] = element.offsetHeight;
              dim[1] = element.offsetWidth;
              Shiny.setInputValue('plot_panel_dims', dim);
            });
            $(document).ready(function() {
              const element = document.querySelector('#plot-panel');
              dim[0] = element.offsetHeight;
              dim[1] = element.offsetWidth;
              Shiny.setInputValue('plot_panel_dims', dim);
            });
          });"
        )
      )
    ),

    shiny::tags$div(
      style = "width: 100%; height: calc(100vh - 20px);",
      id = "main-panel",

      shiny::tags$div(
        style = "float: left; width: calc(100% - 410px); height: calc(100% - 20px); margin-top: 20px; margin-bottom: 20px;",
        id = "plot-panel",

        shiny::tags$div(
          class = "vrtc-tab-panel-container",
          style = "margin-top: 0px;",
          plotly::plotlyOutput("display", height = "auto"),
          align = "center"
        )
      ),

      shiny::tags$div(
        style = "width: 400px; margin-left: calc(100% - 400px);",

        shinyWidgets::verticalTabsetPanel(
          id = "main",
          contentWidth = 10,
          menuSide = "right",
          selected = "1",

          shinyWidgets::verticalTabPanel(
            title = "1",
            box_height = "100%",

            shiny::p("Load video", class = "module-title"),

            shiny::hr(),

            shiny::tags$div(
              class = "panel panel-default",
              shinyFiles::shinyFilesButton("videoFile_x", "Select video file",
                                           "Please select a video file", FALSE,
                                           class = "fullWidth")
            ),

            shiny::hr(),

            shiny::htmlOutput("rangeSlider"),

            shiny::htmlOutput("stepSetter"),

            shiny::p(style = "padding-bottom: 10px;")
          ),

          shinyWidgets::verticalTabPanel(
            title = "2",
            box_height = "100%",

            shiny::p("Process ROIs", class = "module-title"),

            shiny::hr(),

            shinyFiles::shinySaveButton("saveResults_x", "Save to",
                                        "Save results as...",
                                        filetype = list(R = c("csv")),
                                        class = "fullWidth"),

            shiny::hr(),

            shiny::htmlOutput("processingInputs"),

            shiny::p(style = "padding-bottom: 10px;")
          )
        ),

        shiny::htmlOutput("videoSlider")
      )
    )
  )

  server <- function(input, output, session) {
    # Variables and reactives
    volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
    theVideo <- NULL
    theImage <- NULL
    theMask <- NULL
    theRed <- NULL
    theBW <- NULL
    tmpDir <- tempdir()
    ROI <- NULL

    defaultRoot <- shiny::reactiveVal()
    defaultPath <- shiny::reactiveVal("")
    resultPath <- shiny::reactiveVal()
    reDraw <- shiny::reactiveVal(0)
    refreshImage <- shiny::reactiveVal(0)
    refreshDisplay <- shiny::reactiveVal(0)


    # Loading video
    shinyFiles::shinyFileChoose(input, "videoFile_x", roots = volumes, session = session,
                                defaultRoot = defaultRoot(), defaultPath = defaultPath())

    shiny::observeEvent(input$videoFile_x, {
      path <- shinyFiles::parseFilePaths(volumes, input$videoFile_x)

      toCheck <- tryCatch(Rvision::video(path$datapath),
                          error = function(e) NA)

      if (Rvision::isVideo(toCheck)) {
        if (Rvision::nframes(toCheck) > 0) {
          if (Rvision::isVideo(theVideo))
            Rvision::release(theVideo)

          theVideo <<- toCheck
          theImage <<- Rvision::readFrame(theVideo, 1)
          theMask <<- Rvision::zeros(nrow(theImage), ncol(theImage), 1)
          theRed <<- Rvision::zeros(nrow(theImage), ncol(theImage), 1)
          theBW <<- Rvision::zeros(nrow(theImage), ncol(theImage), 1)
          sx <- c((ncol(theImage) / 2) - (0.4 * nrow(theImage)),
                  (ncol(theImage) / 2) + (0.4 * nrow(theImage)))
          sy <- c(0.1 * nrow(theImage), 0.9 * nrow(theImage))
          ROI <<- cbind(c(sx[1], sx[1] + rnorm(1), sx[2], sx[2]),
                        c(sy[1], sy[2], sy[2], sy[1]))
          reDraw(reDraw() + 1)
        } else {
          theVideo <<- NULL
          theImage <<- NULL
        }
      } else {
        theImage <<- NULL
      }
    })

    output$rangeSlider <- shiny::renderUI({
      if (reDraw() > 0 & Rvision::isVideo(theVideo)) {
        shiny::sliderInput("rangePos_x", "Video range", width = "100%", min = 1,
                           max = Rvision::nframes(theVideo),
                           value = c(1, Rvision::nframes(theVideo)), step = 1)
      }
    })

    output$stepSetter <- shiny::renderUI({
      if (reDraw() > 0 & Rvision::isVideo(theVideo)) {
        shiny::tagList(
          shiny::hr(),
          shiny::numericInput("stepSize_x", "Step size (frames)",
                              round(Rvision::fps(theVideo)),
                              min = 1, max = 60 * round(Rvision::fps(theVideo)))
        )
      }
    })

    output$videoSlider <- shiny::renderUI({
      if (reDraw() > 0 & !is.null(input$rangePos_x) & Rvision::isVideo(theVideo)) {
        if (is.null(input$stepSize_x)) {
          step <- 1
        } else {
          step <- input$stepSize_x
        }

        shiny::isolate({
          if (!is.null(input$videoPos_x)) {
            val <- input$videoPos_x
          } else {
            val <- input$rangePos_x[1]
          }

        })

        shiny::tags$div(
          class = "col-sm-12 vrtc-tab-panel-container tabbable",
          style = "margin-top: 10px;",
          shiny::tags$div(
            style = "margin-top: 20px; margin-left: 20px; margin-right: 70px; margin-bottom: 10px;",
            shiny::sliderInput("videoPos_x", "Frame:", width = "100%", step = step,
                               value = val,
                               min = input$rangePos_x[1],
                               max = input$rangePos_x[2]),
            shiny::tags$div(style = "font-weight: bold;", shiny::textOutput("timer"))
          )
        )
      }
    })


    # Processing ROIs
    shinyFiles::shinyFileSave(input, "saveResults_x", roots = volumes,
                              session = session, defaultRoot = defaultRoot(),
                              defaultPath = defaultPath())

    shiny::observeEvent(input$saveResults_x, {
      if (length(input$saveResults_x) > 1) {
        resultPath(shinyFiles::parseSavePath(volumes, input$saveResults_x)$datapath)
      }
    })

    output$processingInputs <- shiny::renderUI({
      if (!is.null(resultPath()) & !is.null(input$rangePos_x)) {
        shiny::tagList(
          shiny::actionButton("validate_x", "Validate ROI", width = "100%"),
          shiny::p(style = "padding-bottom: 10px;"),
          shinyWidgets::prettySwitch("preview_x", "Preview")
        )
      }
    })

    shiny::observeEvent(input$preview_x, {
      if (!is.null(input$preview_x))
        refreshImage(refreshImage() + 1)
    })


    # Display
    output$display <- plotly::renderPlotly({
      if (reDraw() > -1 & !is.null(input$plot_panel_dims)) {
        if (Rvision::isVideo(theVideo)) {
          imgWidth <- ncol(theVideo)
          imgHeight <- nrow(theVideo)
        } else {
          imgWidth <- 1920
          imgHeight <- 1080
        }

        winWidth <- input$plot_panel_dims[2] - 20
        winHeight <- input$plot_panel_dims[1] - 20
        scaleFactor <- min(winWidth / imgWidth, winHeight / imgHeight)

        fig <- emptyPlotly(width = imgWidth * scaleFactor,
                           height = imgHeight * scaleFactor)

        shiny::isolate(
          if (!is.null(input$videoPos_x)) {
            imgFile <- list.files(tmpDir, "display_*")
            fig <- plotly::layout(
              fig,
              images = list(
                list(
                  source = paste0("imgDir/", imgFile),
                  x = 0,
                  sizex = imgWidth * scaleFactor,
                  y = imgHeight * scaleFactor,
                  sizey = imgHeight * scaleFactor,
                  xref = "x",
                  yref = "y",
                  opacity = 1.0,
                  layer = "below",
                  sizing = "stretch"
                )
              ),

              shapes = list(
                list(
                  type = "path",
                  editable = TRUE,
                  xref = "x",
                  yref = "y",
                  xsizemode = "scaled",
                  ysizemode = "scaled",
                  line = list(color = "white", width = 3, dash = "solid"),
                  name = "ROI",
                  layer = "above",
                  label = list(text = "ROI", textposition = "topleft"),
                  path = paste0("M", ROI[1, 1] * scaleFactor, ",", ROI[1, 2] * scaleFactor, "L",
                                ROI[2, 1] * scaleFactor, ",", ROI[2, 2] * scaleFactor, "L",
                                ROI[3, 1] * scaleFactor, ",", ROI[3, 2] * scaleFactor, "L",
                                ROI[4, 1] * scaleFactor, ",", ROI[4, 2] * scaleFactor, "Z")
                )
              )
            )
          }
        )

        fig <- plotly::event_register(fig, "plotly_relayout")

        fig
      }
    })

    shiny::observeEvent(input$rangePos_x, {
      if (!is.null(input$videoPos_x))
        refreshImage(refreshImage() + 1)
    })

    shiny::observeEvent(input$videoPos_x, {
      refreshImage(refreshImage() + 1)
    })

    output$timer <- shiny::renderText({
      if (!is.null(input$videoPos_x)) {
        fr <- round(input$videoPos_x %% Rvision::fps(theVideo))
        tot_sec <- input$videoPos_x %/% Rvision::fps(theVideo)
        hours <- tot_sec %/% 3600
        mins <- (tot_sec - hours * 3600) %/% 60
        secs <- tot_sec %% 60
        paste0("Time: ", sprintf("%02d", hours), ":", sprintf("%02d", mins), ":",
               sprintf("%02d", secs), ".", sprintf("%02d", fr))
      }
    })

    shiny::observeEvent(plotly::event_data("plotly_relayout"), {
      e <- plotly::event_data("plotly_relayout")

      if (length(e$shapes) > 0) {
        if (Rvision::isVideo(theVideo)) {
          imgWidth <- ncol(theVideo)
          imgHeight <- nrow(theVideo)
        } else {
          imgWidth <- 1920
          imgHeight <- 1080
        }

        winWidth <- input$plot_panel_dims[2] - 20
        winHeight <- input$plot_panel_dims[1] - 20
        scaleFactor <- min(winWidth / imgWidth, winHeight / imgHeight)

        if (inherits(e$shapes, "data.frame")) {
          st <- e$shapes$path[1]
        } else {
          st <- e$shapes
        }

        st <- gsub("M", "", st)
        st <- gsub("Z", "", st)
        st <- strsplit(st, "L")[[1]]
        st <- strsplit(st, ",")
        st <- t(simplify2array(st))
        ROI <<- matrix(as.numeric(st) / scaleFactor, ncol = ncol(st))
      }
    })

    shiny::observeEvent(refreshImage(), {
      if (refreshImage() > 0) {
        Rvision::readFrame(theVideo, input$videoPos_x, theImage)
        unlink(paste0(tmpDir, "/", list.files(tmpDir, "display_*")))

        if (!is.null(input$preview_x)) {
          if (input$preview_x) {
            Rvision::multiply(theMask, 0, theMask)
            Rvision::fillPoly(theMask, ROI)
            Rvision::extractChannel(theImage, 3, theRed)
            Rvision::canny(theRed, 50, 150, target = theBW)
            Rvision::multiply(theBW, theMask, theBW)
            Rvision::morph(theBW, "close", k_shape = "ellipse", target = theBW)
            Rvision::threshold(theRed, method = "Otsu", mask = theBW,
                               threshold_type = "inverse", target = theBW)
            Rvision::multiply(theBW, theMask, theBW)
            Rvision::changeColorSpace(theBW, "BGR", theImage)
          }
        }

        suppressMessages(
          Rvision::write.Image(theImage, paste0(tmpDir, "/display_", refreshImage(), ".jpg"), TRUE)
        )
        refreshDisplay(refreshDisplay() + 1)
      }
    })

    shiny::addResourcePath(prefix = "imgDir", directoryPath = tmpDir)

    shiny::observeEvent(refreshDisplay(), {
      if (refreshDisplay() > 0) {
        imgWidth <- ncol(theVideo)
        imgHeight <- nrow(theVideo)
        winWidth <- input$plot_panel_dims[2] - 20
        winHeight <- input$plot_panel_dims[1] - 20
        scaleFactor <- min(winWidth / imgWidth, winHeight / imgHeight)
        imgFile <- list.files(tmpDir, "display_*")

        plotly::plotlyProxyInvoke(
          plotly::plotlyProxy("display", session),
          "relayout",
          list(
            images = list(
              list(
                source = paste0("imgDir/", imgFile),
                x = 0,
                sizex = imgWidth * scaleFactor,
                y = imgHeight * scaleFactor,
                sizey = imgHeight * scaleFactor,
                xref = "x",
                yref = "y",
                opacity = 1.0,
                layer = "below",
                sizing = "stretch"
              )
            ),

            shapes = list(
              list(
                type = "path",
                editable = TRUE,
                xref = "x",
                yref = "y",
                xsizemode = "scaled",
                ysizemode = "scaled",
                line = list(color = "white", width = 3, dash = "solid"),
                name = "ROI",
                layer = "above",
                label = list(text = "ROI", textposition = "topleft"),
                path = paste0("M", ROI[1, 1] * scaleFactor, ",", ROI[1, 2] * scaleFactor, "L",
                              ROI[2, 1] * scaleFactor, ",", ROI[2, 2] * scaleFactor, "L",
                              ROI[3, 1] * scaleFactor, ",", ROI[3, 2] * scaleFactor, "L",
                              ROI[4, 1] * scaleFactor, ",", ROI[4, 2] * scaleFactor, "Z")
              )
            )
          )
        )
      }
    })


    # Save
    shiny::observeEvent(input$validate_x, {
      out <- c(input$videoPos_x, ROI)
      names(out) <- c("frame", "x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4")

      if (file.exists(resultPath())) {
        write.table(data.frame(as.list(out)), resultPath(), sep = ",",
                    row.names = FALSE, col.names = FALSE, append = TRUE)
      } else {
        write.table(data.frame(as.list(out)), resultPath(), sep = ",",
                    row.names = FALSE)
      }

      shiny::updateSliderInput(session, "videoPos_x", value = input$videoPos_x + input$stepSize_x)
    })


    # Cleanup
    shiny::onSessionEnded(function() {
      if (Rvision::isVideo(theVideo))
        Rvision::release(theVideo)
    })
  }

  shiny::shinyApp(ui, server, ...)
}





