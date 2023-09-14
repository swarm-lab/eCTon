#' @export
prep_stack <- function(width = 1200, height = 1e6) {
  options(rgl.useNULL = TRUE)

  ui <- miniUI::miniPage(
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

          .card-body {
            -webkit-box-shadow: 0 6px 12px rgba(0, 0, 0, 0.175);
            box-shadow: 0 6px 12px rgba(0, 0, 0, 0.175);
            -moz-box-shadow: 0 6px 12px rgba(0, 0, 0, 0.175);
          }

          .main-panel {
            margin-top: 20px;
            text-align: center;
            width: 100%;
          }

          .slicer-panel {
            width: 100%;
          }
      ")
      )
    ),

    shinyjs::useShinyjs(),
    miniUI::gadgetTitleBar("Stack preparation"),
    miniUI::miniContentPanel(
      shiny::fluidRow(
        shiny::column(
          8,

          shiny::div(
            id = "2d-panel",
            class = "main-panel",
            shinyWidgets::panel(
              shiny::plotOutput("display_x", height = "calc(100vh - 285px)", width = "100%")
            )
          ),

          shiny::div(
            id = "3d-panel",
            class = "main-panel",
            shinyWidgets::panel(
              rgl::rglwidgetOutput("display3d_x", height = "calc(100vh - 285px)", width = "100%")
            )
          ),

          shiny::div(
            id = "slicer",
            class = "slicer-panel",
            shinyWidgets::panel(
              shiny::sliderInput("slicer_x", label = NULL, min = 1, max = 2,
                                 value = 1, step = 1, width = "100%")
            )
          ),

          shiny::div(
            id = "slicer2",
            class = "slicer-panel",
            shinyWidgets::panel(
              shiny::sliderInput("slicer2_x", label = NULL, min = 1, max = 2,
                                 value = c(1, 2), step = 1, width = "100%")
            )
          ),

          shiny::div(
            id = "slicer3",
            class = "slicer-panel",
            shinyWidgets::panel(
              shiny::sliderInput("slicer3_x", label = NULL, min = 1, max = 2,
                                 value = 1, step = 1, width = "100%")
            )
          )
        ),

        shiny::column(
          4,
          shinyWidgets::verticalTabsetPanel(
            id = "main",
            contentWidth = 10,
            menuSide = "right",
            selected = "1",

            shinyWidgets::verticalTabPanel(
              title = "1",
              box_height = "100%",
              shiny::p("Load stack", class = "module-title"),
              shiny::hr(),
              shinyFiles::shinyFilesButton("stackFile_x", "Select stack file",
                                           "Please select a stack file", FALSE,
                                           class = "fullWidth"),
              shiny::hr(),
              shiny::p(style = "padding-bottom: 10px;")
            ),

            shinyWidgets::verticalTabPanel(
              title = "2",
              box_height = "100%",
              shiny::p("Set container ROI", class = "module-title"),
              shiny::hr(),
              shiny::sliderInput("containerx_x", "Center x coordinate:",
                                 min = 1, max = 1, value = 1, width = "100%"),
              shiny::sliderInput("containery_x", "Center y coordinate:",
                                 min = 1, max = 1, value = 1, width = "100%"),
              shiny::sliderInput("containerr_x", "Radius:",
                                 min = 1, max = 1, value = 1, width = "100%"),
              shiny::sliderInput("containerc_x", "Cutoff:",
                                 min = 0, max = 1, value = 0.9, width = "100%"),
              shiny::hr(),
              shiny::actionButton("container_x", "Automatically find ROI", width = "100%"),
              shiny::p(style = "padding-bottom: 10px;")
            ),

            shinyWidgets::verticalTabPanel(
              title = "3",
              box_height = "100%",
              shiny::p("Set bivouac ROI", class = "module-title"),
              shiny::hr(),
              shiny::actionButton("bivouac_x", "Automatically find ROI", width = "100%"),
              shiny::hr(),
              shiny::p(style = "padding-bottom: 10px;")
            ),

            shinyWidgets::verticalTabPanel(
              title = "4",
              box_height = "100%",
              shiny::p("Fix container artifact", class = "module-title"),
              shiny::hr(),
              shiny::actionButton("magic_x", "Perform magic", width = "100%"),
              shiny::hr(),
              shiny::p(style = "padding-bottom: 10px;")
            ),

            shinyWidgets::verticalTabPanel(
              title = "5",
              box_height = "100%",
              shiny::p("Set global threshold", class = "module-title"),
              shiny::hr(),
              shiny::numericInput("threshold_x", "Threshold:", min = 0, max = Inf,
                                  value = 0, width = "100%"),
              shiny::hr(),
              shiny::numericInput("sample_x", "Number of samples per slice:", min = 0, max = 20000,
                                  value = 5000, width = "100%"),
              shinyWidgets::pickerInput("methods_x", "Thresholding methods",
                                        multiple = TRUE, selected = c("Otsu","RenyiEntropy"),
                                        c(Otsu = "Otsu", `Renyi entropy` = "RenyiEntropy",
                                          ImageJ = "ImageJ", Huang = "Huang", Huang2 = "Huang2",
                                          Intermodes = "Intermodes", `Iso data` = "IsoData",
                                          Li = "Li", `Maximum entropy` = "MaxEntropy",
                                          Mean = "Mean", `Minimum error I` = "MinErrorI",
                                          Minimum = "Minimum", Moments = "Moments",
                                          Percentile = "Percentile", Shanbhag = "Shanbhag",
                                          Triangle = "Triangle", Yen = "Yen")),
              shiny::hr(),
              shiny::actionButton("autothreshold_x", "Automatically find threshold", width = "100%"),
              shiny::hr(),
              shiny::p(style = "padding-bottom: 10px;")
            ),

            shinyWidgets::verticalTabPanel(
              title = "6",
              box_height = "100%",
              shiny::p("Preview & export", class = "module-title"),
              shiny::hr(),
              shiny::actionButton("threed_x", "Render 3D preview", width = "100%"),
              shiny::hr(),
              shinyFiles::shinySaveButton("export_x", "Export final result", "Export results to...",
                                          filetype = list(stack = c("tif", "tiff")),
                                          class = "fullWidth"),
              shiny::hr(),
              shiny::p(style = "padding-bottom: 10px;")
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    # FUNCTIONS
    toggleAll <- function(state = "OFF") {
      input_list <- shiny::reactiveValuesToList(input)
      to_toggle <- grepl("_x", names(input_list))
      input_list <- input_list[to_toggle]

      for (name in names(input_list)) {
        if (state == "OFF") {
          shinyjs::disable(name)
        } else {
          shinyjs::enable(name)
        }
      }
    }

    # VARIABLES
    volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
    defaultRoot <- shiny::reactiveVal(NULL)
    defaultPath <- shiny::reactiveVal("")
    stackfile <- shiny::reactiveVal(NULL)
    stack <- shiny::reactiveVal(NULL)
    mask <- shiny::reactiveVal(NULL)
    mods <- shiny::reactiveVal(NULL)
    D <- shiny::reactiveVal(NULL)
    preview3d <- shiny::reactiveVal(NULL)

    # LOAD MODULE
    shinyFiles::shinyFileChoose(input, "stackFile_x", roots = volumes, session = session,
                                defaultRoot = defaultRoot(), defaultPath = defaultPath())

    shiny::observeEvent(input$stackFile_x, {
      path <- shinyFiles::parseFilePaths(volumes, input$stackFile_x)
      if (nrow(path) > 0) {
        shiny::showNotification("Loading stack in memory.", id = "load",
                                duration = NULL, type = "error")
        pbapply::pboptions(title = "Converting stack...")
        toggleAll("OFF")
        if (grepl(".mat", path$datapath)) {
          stack(read_mat_stack(path$datapath))
        } else {
          stack(read_stack(path$datapath))
        }
        stackfile(path$datapath)
        toggleAll("ON")
        shiny::removeNotification(id = "load")
      }
    })

    # CONTAINER MODULE
    shiny::observeEvent(input$container_x, {
      if (length(stack()) > 0) {
        pbapply::pboptions(title = "Processing...")
        toggleAll("OFF")
        circ <- find_container(stack(), input$slicer_x - 10, input$slicer_x + 10)
        toggleAll("ON")
        shiny::removeNotification(id = "process")
        shiny::updateSliderInput(session, "containerx_x", value = circ[[1]])
        shiny::updateSliderInput(session,  "containery_x", value = circ[[2]])
        shiny::updateSliderInput(session, "containerr_x", value = circ[[3]])
        mask(make_container_mask(nrow(stack()[[1]]), ncol(stack()[[1]]), circ,
                                 input$containerc_x))
        toggleAll("ON")
      }
    })

    shiny::observeEvent(input$containerx_x, {
      if (length(stack()) > 0) {
        circ <- c(input$containerx_x, input$containery_x, input$containerr_x)
        mask(make_container_mask(nrow(stack()[[1]]), ncol(stack()[[1]]), circ,
                                 input$containerc_x))
      }
    })

    shiny::observeEvent(input$containery_x, {
      if (length(stack()) > 0) {
        circ <- c(input$containerx_x, input$containery_x, input$containerr_x)
        mask(make_container_mask(nrow(stack()[[1]]), ncol(stack()[[1]]), circ,
                                 input$containerc_x))
      }
    })

    shiny::observeEvent(input$containerr_x, {
      if (length(stack()) > 0) {
        circ <- c(input$containerx_x, input$containery_x, input$containerr_x)
        mask(make_container_mask(nrow(stack()[[1]]), ncol(stack()[[1]]), circ,
                                 input$containerc_x))
      }
    })

    shiny::observeEvent(input$containerc_x, {
      if (length(stack()) > 0) {
        circ <- c(input$containerx_x, input$containery_x, input$containerr_x)
        mask(make_container_mask(nrow(stack()[[1]]), ncol(stack()[[1]]), circ,
                                 input$containerc_x))
      }
    })

    # BIVOUAC MODULE
    shiny::observeEvent(input$bivouac_x, {
      if (length(stack()) > 0) {
        mask <- mask()

        if (!Rvision::isImage(mask))
          mask <- (stack()[[1]] * 0) + 1

        pbapply::pboptions(title = "Processing...")
        toggleAll("OFF")
        usable <- find_usable_slices(stack(), mask)
        shiny::updateSliderInput(session, inputId = "slicer2_x",
                                 value = c(usable[[1]], usable[[2]]))
        toggleAll("ON")
      }
    })

    # ARTIFACT MODULE
    shiny::observeEvent(input$slicer2_x, {
      shiny::updateSliderInput(session, inputId = "slicer3_x", min = input$slicer2_x[1],
                               max = input$slicer2_x[2], value = input$slicer2_x[1])
      mods(NULL)
      D(NULL)
    })

    shiny::observeEvent(input$magic_x, {
      if (length(stack()) > 0) {
        pbapply::pboptions(title = "Processing...")
        toggleAll("OFF")
        circ <- c(input$containerx_x, input$containery_x, input$containerr_x)
        mods(unbend_light(stack(), c(input$slicer2_x[1], input$slicer2_x[2]), circ))

        nc <- ncol(stack()[[1]])
        nr <- nrow(stack()[[1]])
        tmp <- circ[3] - sqrt((matrix(1:nc, ncol = nc, nrow = nr, byrow = TRUE) - circ[1]) ^ 2 +
                                (matrix(1:nr, ncol = nc, nrow = nr) - circ[2]) ^ 2)
        dim(tmp) <- c(dim(tmp), 1)
        D(tmp)
        toggleAll("ON")
      }
    })

    # THRESHOLD MODULE
    shiny::observeEvent(input$autothreshold_x, {
      if (length(stack()) > 0) {
        pbapply::pboptions(title = "Processing...")
        toggleAll("OFF")
        circ <- c(input$containerx_x, input$containery_x, input$containerr_x)
        th <- find_global_treshold(stack(), circ, mods(), mask(),
                                   n = input$sample_x,
                                   methods = input$methods_x)
        shiny::updateNumericInput(session, "threshold_x", value = th)
        toggleAll("ON")
      }
    })

    # 3D PREVIEW MODULE
    shiny::observeEvent(input$threed_x, {
      if (length(stack()) > 0 & length(D()) > 0) {
        pbapply::pboptions(title = "Processing...")
        toggleAll("OFF")
        ds <- 4
        slices <- seq(input$slicer2_x[1], input$slicer2_x[2], ds)
        nr <- floor(nrow(stack()[[1]]) / ds)
        nc <- floor(ncol(stack()[[1]]) / ds)
        ns <- length(slices)

        pbapply::pblapply(1:ns, function(j) {
          out <- Rvision::cloneImage(stack()[[slices[j]]])
          i <- which(mods()[, 1] == slices[j])
          BASE <- (mods()[i, 2] * exp(-mods()[i, 3] * D())) + mods()[i, 4]
          Rvision::subtract(out, Rvision::image(BASE), "self")
          Rvision::multiply(out, mask(), "self")
          Rvision::resize(out < input$threshold_x, nr, nc)[][,,1]
        }) -> vol
        vol <- simplify2array(vol)

        dt <- mmand::distanceTransform(vol)
        tmp <- dt == 1
        tmp[1, , ] <- dt[1, , ] > 0
        tmp[nr, , ] <- dt[nr, , ] > 0
        tmp[, 1, ] <- dt[, 1, ] > 0
        tmp[, nc, ] <- dt[, nc, ] > 0
        tmp[, , 1] <- dt[, , 1] > 0
        tmp[, , ns] <- dt[, , ns] > 0
        tmp[tmp == 0] <- NA
        preview3d(tmp)

        toggleAll("ON")
      }
    })

    # SAVE MODULE
    shinyFiles::shinyFileSave(input, "export_x", roots = volumes, session = session,
                              defaultRoot = defaultRoot(), defaultPath = defaultPath())

    shiny::observeEvent(input$export_x, {
      path <- shinyFiles::parseSavePath(volumes, input$export_x)

      if (nrow(path) > 0) {
        shiny::showNotification("Saving processed stack.", id = "save",
                                duration = NULL, type = "error")
        toggleAll("OFF")
        circ <- c(input$containerx_x, input$containery_x, input$containerr_x)
        Rvision::writeMulti(path$datapath,
                            make_clean_stack(stack(), mask(), mods(), circ, input$threshold_x))

        yaml::write_yaml(yaml::as.yaml(list(file = stackfile(),
                                            bottom_slice = input$slicer2_x[1],
                                            top_slice = input$slicer2_x[2],
                                            container_x = input$containerx_x,
                                            container_y = input$containery_x,
                                            container_radius = input$containerr_x,
                                            container_roi_scale = input$containerc_x,
                                            global_threshold = input$threshold_x),
                                       precision = 16),
                         paste0(tools::file_path_sans_ext(path$datapath), ".yml"))

        toggleAll("ON")
        shiny::removeNotification(id = "save")
      }
    })

    # DISPLAY
    shiny::observeEvent(stack(), {
      if (length(stack()) > 0) {
        shiny::updateSliderInput(session, inputId = "slicer_x", max = length(stack()),
                                 value = length(stack()) / 2)
        shiny::updateSliderInput(session, inputId = "slicer2_x", max = length(stack()),
                                 value = c(1, length(stack())))
        shiny::updateSliderInput(session, "containerx_x", max = ncol(stack()[[1]]),
                                 value = ncol(stack()[[1]]) / 2)
        shiny::updateSliderInput(session, "containery_x", max = nrow(stack()[[1]]),
                                 value = nrow(stack()[[1]]) / 2)
        shiny::updateSliderInput(session, "containerr_x", max = max(dim(stack()[[1]])),
                                 value = max(dim(stack()[[1]])) / 2)
        shinyjs::show("slicer", anim = TRUE, animType = "fade")
      }
    })

    output$display_x <- shiny::renderPlot({
      if (length(stack()) > 0) {
        if (input$main == "1") {
          to_display <- Rvision::cloneImage(stack()[[input$slicer_x]])
          plot(to_display)
        } else if (input$main == "2") {
          to_display <- Rvision::cloneImage(stack()[[input$slicer_x]])
          Rvision::changeBitDepth(to_display, "8U", 255 / max(to_display)[1], "self")
          Rvision::changeColorSpace(to_display, "BGR", "self")
          Rvision::drawCircle(to_display, input$containerx_x, input$containery_x,
                              input$containerr_x, "green", 3)
          Rvision::drawCircle(to_display, input$containerx_x, input$containery_x,
                              input$containerr_x * input$containerc_x, "red", 3)
          plot(to_display)
        } else if (input$main == "3") {
          tmp1 <- Rvision::cloneImage(stack()[[input$slicer2_x[1]]])
          if (max(tmp1)[1] > 0) {
            Rvision::divide(tmp1, max(tmp1)[1], "self")
            Rvision::multiply(tmp1, 255, "self")
          }
          Rvision::border(tmp1, 0, 0, 0, 5, border_color = "white", target = "self")

          tmp2 <- Rvision::cloneImage(stack()[[input$slicer2_x[2]]])
          if (max(tmp2)[1] > 0) {
            Rvision::divide(tmp2, max(tmp2)[1], "self")
            Rvision::multiply(tmp2, 255, "self")
          }
          Rvision::border(tmp2, 0, 0, 0, 5, border_color = "white", target = "self")
          to_display <- Rvision::concatenate(tmp1, tmp2, direction = "horizontal")
          plot(to_display)
        } else if (input$main == "4") {
          to_display <- Rvision::cloneImage(stack()[[input$slicer3_x]])

          if (length(D()) > 0) {
            i <- which(mods()[, 1] == input$slicer3_x)
            BASE <- (mods()[i, 2] * exp(-mods()[i, 3] * D())) + mods()[i, 4]
            Rvision::subtract(to_display, Rvision::image(BASE), "self")
            Rvision::multiply(to_display, mask(), "self")
            Rvision::multiply(to_display, (to_display > 0) / 255, "self")
          }

          plot(to_display)
        } else if (input$main == "5") {
          to_display <- Rvision::cloneImage(stack()[[input$slicer3_x]])

          if (length(D()) > 0) {
            i <- which(mods()[, 1] == input$slicer3_x)
            BASE <- (mods()[i, 2] * exp(-mods()[i, 3] * D())) + mods()[i, 4]
            Rvision::subtract(to_display, Rvision::image(BASE), "self")
            Rvision::multiply(to_display, mask(), "self")
            Rvision::multiply(to_display, (to_display > 0) / 255, "self")
            ct <- Rvision::findContours(to_display >= input$threshold_x, mode = "list", method = "none")
            Rvision::changeBitDepth(to_display, "8U", 255 / max(to_display)[1], "self")
            Rvision::changeColorSpace(to_display, "BGR", "self")
            Rvision::drawCircle(to_display, ct$contours[, 2], ct$contours[, 3], 1, "green", -1)
          }

          plot(to_display)
        }
      } else {
        plot(Rvision::zeros(500, 500, 1))
      }
    })

    output$display3d_x <- rgl::renderRglwidget({
      try(close3d(), silent = TRUE)

      if (length(preview3d()) > 0) {
        shiny::showNotification("Rendering preview.", id = "render3d",
                                duration = NULL, type = "error")
        fsbrain::volvis.voxels(preview3d(), voxelcol = "gray50", specular = "black")
        shiny::removeNotification(id = "render3d")
      }

      rgl::box3d()
      rgl::bg3d(color = "#dfe5ed")
      rgl::rgl.viewpoint(theta = 0, phi = 0, fov = 45)
      rglwidget()
    })

    # OTHER
    shiny::observeEvent(input$main, {
      if (input$main == "3") {
        shinyjs::hide("3d-panel")
        shinyjs::hide("slicer")
        shinyjs::hide("slicer3")
        if (length(stack()) == 0) {
          shinyjs::hide("slicer2")
        } else {
          shinyjs::show("slicer2", anim = TRUE, animType = "fade")
        }
        shinyjs::show("2d-panel", anim = TRUE, animType = "fade")
      } else if (input$main == "4" | input$main == "5") {
        shinyjs::hide("3d-panel")
        shinyjs::hide("slicer")
        shinyjs::hide("slicer2")
        if (length(stack()) == 0) {
          shinyjs::hide("slicer3")
        } else {
          shinyjs::show("slicer3", anim = TRUE, animType = "fade")
        }
        shinyjs::show("2d-panel", anim = TRUE, animType = "fade")
      } else if (input$main == "6") {
        shinyjs::hide("2d-panel")
        shinyjs::hide("slicer")
        shinyjs::hide("slicer2")
        shinyjs::hide("slicer3")
        shinyjs::show("3d-panel", anim = TRUE, animType = "fade")
      } else {
        shinyjs::hide("3d-panel")
        shinyjs::hide("slicer2")
        shinyjs::hide("slicer3")
        if (length(stack()) == 0) {
          shinyjs::hide("slicer")
        } else {
          shinyjs::show("slicer", anim = TRUE, animType = "fade")
        }
        shinyjs::show("2d-panel", anim = TRUE, animType = "fade")
      }
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }

  shiny::runGadget(
    ui, server,
    viewer = shiny::dialogViewer("Stack preparation", width = width, height = height)
  )
}
