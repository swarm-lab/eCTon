#" @export
emptyPlotly <- function(width = 1920, height = 1080) {
  # Add invisible scatter trace.
  # This trace is added to help the autoresize logic work.
  fig <- plotly::plot_ly(width = width, height = height)
  fig <- plotly::config(
    fig, displayModeBar = TRUE, displaylogo = FALSE,
    modeBarButtonsToAdd = list("drawclosedpath", "eraseshape", "zoom"),
    modeBarButtonsToRemove = list("pan", "select", "zoomIn", "zoomOut",
                                  "autoScale", "lasso2d", "toImage", "resetScale",
                                  "hoverClosestCartesian", "hoverCompareCartesian"))
  fig <- plotly::add_trace(fig, x = c(0, width), y = c(0, height),
                           type = "scatter",  mode = "markers", alpha = 0)

  # Configure layout
  sx <- c((width / 2) - (0.4 * height), (width / 2) + (0.4 * height))
  sy <- c(0.1 * height, 0.9 * height)

  fig <- plotly::layout(
    fig,

    # Configure axes
    xaxis = list(
      title = NULL,
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(0, width)
    ),
    yaxis = list(
      title = NULL,
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(0, height),
      scaleanchor = "x"
    ),

    # Configure other layout
    margin = list(r = 0, l = 0, b = 0, t = 0),
    plot_bgcolor = "#e5ecf6",
    xaxis = list(
      zerolinecolor = "#ffff",
      zerolinewidth = 2,
      gridcolor = "ffff"),
    yaxis = list(
      zerolinecolor = "#ffff",
      zerolinewidth = 2,
      gridcolor = "ffff")
  )

  fig
}
