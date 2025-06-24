##-------------------------------------------
## Author  : Izi
## Project :
## Created :dom 19 set 2021 14:04:53 -03
## License : MIT
## Updated :
##-------------------------------------------

##===========================================
##
##===========================================
coord_trans_flip <- function(x = "identity", y = "identity", xlim = NULL, ylim = NULL,
                             clip = "on", expand = TRUE, flip = FALSE) {
  # resolve transformers
  if (is.character(x)) x <- scales::as.trans(x)
  if (is.character(y)) y <- scales::as.trans(y)

  if(flip)
  {
    ggproto(NULL, CoordTransFlip,
            trans = list(x = x, y = y),
            limits = list(x = xlim, y = ylim),
            expand = expand,
            clip = clip)
  } else {
    ggproto(NULL, CoordTrans,
            trans = list(x = x, y = y),
            limits = list(x = xlim, y = ylim),
            expand = expand,
            clip = clip)
  }#end if
}

##===========================================
##
##===========================================
flip_axis_labels <- utils::getFromNamespace("flip_axis_labels", "ggplot2")
# reverse_trans <- utils::getFromNamespace("reverse_trans", "scales")

##===========================================
##
##===========================================
CoordTransFlip <- ggproto("CoordTransFlip", CoordTrans,

  transform = function(self, data, panel_params) {
    ## Need the panel params to be unflipped to correctly transform the data
    panel_params <- flip_axis_labels(panel_params)
    data <- ggproto_parent(CoordTrans, self)$transform(data, panel_params)
     flip_axis_labels(data)
  },

  backtransform_range = function(self, panel_params) {
    self$range(panel_params)
  },

  range = function(self, panel_params) {
    # summarise_layout() expects the original x and y ranges here,
    # not the ones we would get after flipping the axes
    un_flipped_range <- ggproto_parent(CoordTrans, self)$range(panel_params)
    list(x = un_flipped_range$y, y = un_flipped_range$x)
  },

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    parent <- ggproto_parent(CoordTrans, self)
    panel_params <- parent$setup_panel_params(scale_x, scale_y, params)
    flip_axis_labels(panel_params)
  },

  labels = function(labels, panel_params) {
      CoordTrans$labels(flip_axis_labels(labels), panel_params)
  },

  setup_layout = function(layout, params) {
    CoordFlip$setup_layout(layout, params)
  },

  modify_scales = function(scales_x, scales_y) {
    CoordFlip$modify_scales(scales_x, scales_y)
  }
)

