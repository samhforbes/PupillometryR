# Copyright (c) 2018 David Robinson.
#
# The R code in this file (referred to as the Software) is licensed under the
# MIT license.
#
# LICENSE:
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' geom_flat_violin_HELPER1
#'
#' Borrowed from
#' https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R.
#' Original author David Robinson, from https://gist.github.com/dgrtwo/eb7750e74997891d7c20
#'
#' @format NULL
#' @usage NULL
#' @name gfv_helper1
#' @import ggplot2

"%||%" <- function(a, b) {
  if (!is.null(a))
    a
  else
    b
}

#' ggplot Flat Violin
#' @export
#' @details Copy-pasted from https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R
#' somewhat hackish solution to:
#' https://twitter.com/EamonCaddigan/status/646759751242620928
#' based mostly on copy/pasting from ggplot2 geom_violin source:
#' https://github.com/hadley/ggplot2/blob/master/R/geom-violin.r
#' The original seems to be: sourced from: https://gist.github.com/dgrtwo/eb7750e74997891d7c20,
#' Author is David Robinson.
#' A key internal function for the raincloud plots used as a plotting option in this package.
#' For information on raincloud plots see: Allen, M., Poggiali, D., Whitaker, K.,
#' Marshall, T. R., & Kievit, R. A. (2019). Raincloud plots: a multi-platform
#' tool for robust data visualization. Wellcome open research,
#' 4, 63. doi:10.12688/wellcomeopenres.15191.1
#' @param mapping A value
#' @param data A value
#' @param position A value
#' @param show.legend A value
#' @param inherit.aes A value
#' @param stat A value
#' @param trim A value
#' @param scale A value
#' @param ... A value
#' @examples
#' ggplot(diamonds, aes(cut, carat)) +
#'   geom_flat_violin() +
#'   coord_flip()
#' @import ggplot2

geom_flat_violin <-
  function(mapping = NULL,
           data = NULL,
           stat = "ydensity",
           position = "dodge",
           trim = TRUE,
           scale = "area",
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomFlatViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(trim = trim,
                    scale = scale,
                    ...)
    )
  }

#'geom_flat_violin_HELPER2
#'
#' Borrowed from
#' \href{https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R}{Ben Marwick}.
#' Original author David Robinson.
#'
#' @format NULL
#' @usage NULL
GeomFlatViolin <-
  ggplot2::ggproto(
    "GeomFlatViolin",
    ggplot2::Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||% (ggplot2::resolution(data$x, FALSE) * 0.9)

      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      data %>%
        dplyr::group_by(.data = ., group) %>%
        dplyr::mutate(
          .data = .,
          ymin = min(y),
          ymax = max(y),
          xmin = x,
          xmax = x + width / 2
        )
    },

    draw_group = function(data, panel_scales, coord)
    {
      # Find the points for the line to go all the way around
      data <- base::transform(data,
                              xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))

      # Make sure it's sorted properly to draw the outline
      newdata <-
        base::rbind(
          dplyr::arrange(.data = base::transform(data, x = xminv), y),
          dplyr::arrange(.data = base::transform(data, x = xmaxv), -y)
        )

      # Close the polygon: set first and last point the same
      # Needed for coord_polar and such
      newdata <- rbind(newdata, newdata[1,])

      ggplot2:::ggname("geom_flat_violin",
                       GeomPolygon$draw_panel(newdata, panel_scales, coord))
    },

    draw_key = ggplot2::draw_key_polygon,

    default_aes = ggplot2::aes(
      weight = 1,
      colour = "grey20",
      fill = "white",
      size = 0.5,
      alpha = NA,
      linetype = "solid"
    ),

    required_aes = c("x", "y")
  )
