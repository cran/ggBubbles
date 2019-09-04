#' Surrounds exact overlapping points around the center
#'
#' Bubble plots sometimes can be hard to interpret, especially if you want
#' to overlay an additional feature. Instead of having to colour one blob
#' with this function you can plot the individuals contributing to the bubble
#' and colour them accordingly.
#'
#' @family position adjustments
#' @param offset setting offset for x and y axis added to the
#'   points surrounding the exact position.
#'
#'   Default is 0.1
#' @export
#' @return ggproto
#' @examples
#'  library(ggplot2)
#'  library(ggBubbles)
#'  data(MusicianInterestsSmall)
#'
#'  ggplot(data = MusicianInterestsSmall, aes(x = Instrument, y = Genre, col = Level)) +
#'         geom_point(position = position_surround(), size = 4) +
#'         scale_colour_manual(values = c("#333333", "#666666", "#999999", "#CCCCCC")) + theme_bw()
position_surround <- function(offset = .1#,
                              #offset_x = NA,
                              #offset_y = NA
                              ) {
    ggproto(NULL,
            PositionSurround,
            offset          = offset
            #$offset_x        = offset_x,
            #offset_y        = offset_y
    )
}

#' ggproto for position_surround()
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @import dplyr
#' @import ggplot2
#' @export
PositionSurround <- ggplot2::ggproto("PositionSurround",
                            `_inherit` = ggplot2::Position,
                            required_aes = c("x", "y"),

                            # will be calculated up front
                            setup_params = function(self, data) {
                                offset_x = self$offset
                                offset_y = self$offset

                                #if(!is.na(self$offset_x))
                                #    offset_x = self$offset_x
                                #if(!is.na(self$offset_y))
                                #    offset_y = self$offset_y

                                list(
                                    # calculate the max of exact overlaps
                                    max_positions = data %>%
                                        group_by(x, y) %>%
                                        summarize(position = n()) %>%
                                        .[["position"]] %>% max,
                                    offset_x = offset_x,
                                    offset_y = offset_y
                                )
                            },

                            # transforms data before plotting
                            compute_panel = function(self,
                                                     data,
                                                     params,
                                                     scales) {
                                # precalculate the offsets for all possible positions
                                offset_table <- get_offset_table(max_positions = params$max_positions,
                                                                 offset_x      = params$offset_x,
                                                                 offset_y      = params$offset_y)

                                # joins the offsets for the positions
                                # transforming x and y values by adding the offsets

                                data %>%
                                    group_by(x, y) %>%
                                    mutate(tmp_position = row_number()) %>%
                                    left_join(y  = offset_table,
                                              by = c("tmp_position" = "position")) %>%
                                    ungroup() %>%
                                    mutate(x = x + offsets_x,
                                           y = y + offsets_y)
                            }
)
