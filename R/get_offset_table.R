#' Calculates offset table for number of maximum overlapping positions
#'
#' @param max_positions number of maximal exact overlaps
#' @param offset_x offset for positon distance
#' @param offset_y offset for in-between layer distance
#'
#' @return data frame with position, offsets_x and offsets_y
get_offset_table <- function(max_positions, offset_x, offset_y) {

    # first position is always 0, 0
    offsets_x <- 0
    offsets_y <- 0

    #edge <- 2 * layer  + 1
    side      = 1
    position  = 0
    old_layer = 0

    if(max_positions > 1) {
        # loop through other positions
        for(i in 2:max_positions) {
            if(side > 4) {
                # start from top side, move position by 1
                side = 1
                position <- position + 1
                # cat(position)
            }

            layer <- ceiling( (sqrt(i) - 1) / 2 )
            if(layer != old_layer ) {
                old_layer = layer
                position  = 0
            }

            #cat(paste0(i, "-pos-", position, "-layer-",
            #                         layer, "-side-", side, "\n"))
            ret <- calc_offset(position = position,
                               layer    = layer,
                               side     = side,
                               offset_x = offset_x,
                               offset_y = offset_y)
            offsets_x <- c(offsets_x, ret[[1]])
            offsets_y <- c(offsets_y, ret[[2]])

            side <- side + 1
        }
    }

    return(data.frame(position  = 1:length(offsets_x),
                      offsets_x = offsets_x,
                      offsets_y = offsets_y))
}