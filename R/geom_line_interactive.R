#' @rdname geom_abline_interactive
#' @examples
#' # add horizontal interactive reference lines to a ggplot -------
#' @example examples/geom_hline_interactive.R
#' @seealso [girafe()]
#' @export
geom_hline_interactive <- function(...)
  layer_interactive(geom_hline, ...)

#' @rdname ggiraph-ggproto
#' @format NULL
#' @usage NULL
#' @export

'''GeomInteractiveLine <- ggproto(
  "GeomInteractiveLine",
  GeomLine,
  default_aes = add_default_interactive_aes(GeomLine),
  parameters = interactive_geom_parameters,
  draw_key = interactive_geom_draw_key,
  
  # Customize the panel drawing function for interactivity
  draw_panel = function(data, panel_params, coord, lineend = "butt", ..., .ipar = IPAR_NAMES) {
    ranges <- coord$backtransform_range(panel_params)
    
    # Adjust the line coordinates to ensure proper panel clipping
    data$xend <- data$x
    data$yend <- data$y
    
    # Ensure the line is drawn correctly within the panel range
    GeomInteractiveSegment$draw_panel(unique0(data), panel_params, coord, lineend = lineend, ..., .ipar = .ipar)
  }
)


'''

GeomInteractiveLine <- ggproto(
  "GeomInteractiveLine",
  GeomLine,
  default_aes = add_default_interactive_aes(GeomLine),
  parameters = interactive_geom_parameters,
  draw_key = interactive_geom_draw_key,
  
  draw_panel = function(data, panel_params, coord, lineend = "butt", ..., .ipar = IPAR_NAMES) {
    # Ensure that the data is within the valid range
    ranges <- coord$backtransform_range(panel_params)
    
    # Correct the line endpoints to make sure they stay within the plot bounds
    data$xend <- pmin(pmax(data$x, ranges$x[1]), ranges$x[2])
    data$yend <- pmin(pmax(data$y, ranges$y[1]), ranges$y[2])
    
    # Call GeomInteractiveSegment to draw the interactive segments
    GeomInteractiveSegment$draw_panel(unique0(data), panel_params, coord, lineend = lineend, ..., .ipar = .ipar)
  }
)



