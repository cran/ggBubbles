#' Calculate offsets for a specific point, in a layer, position
#'
#' each side has several layers, with a number of positions in the layer
#'
#' @param side side for offset
#'     1 - top
#'     2 - right
#'     3 - bottom
#'     4 - left
#'
#' @param layer number of layer
#'
#' @param position number for position at the particular side on the layer
#'
#' @param offset_x offset for x axis
#' @param offset_y offset for y axis
#' @return integer vector of length 2
#' position 1 is new x value,
#' position y is new y value
calc_offset <- function(position,
                        layer,
                        side,
                        offset_x = 0.1,
                        offset_y = 0.1) {

    stopifnot(is.numeric(side) && is.numeric(layer) && is.numeric(position))

    position_x = ceiling(position/2) * (-1)^position  * offset_x
    position_y = layer * offset_y

    if(side == 1) { # top
        return(c(position_x, position_y))
    } else if (side == 2) { # right
        return(c(position_y, -position_x))
    } else if (side == 3) { # bottom
        return(c(-position_x, -position_y))
    } else if (side == 4) { # left
        return(c(-position_y, position_x))
    } else {
        stop(paste("parameter side has to be 1-4, currently it is", side))
    }
}