

globalVariables(c('char', 'start', 'end', 'x', 'label',
                  'segment_y', 'segment_colour', 'segment_size', 'segment_linetype',
                  'text_y', 'text_colour', 'text_size', 'text_angle', 'text_family', 'text_hjust',
                  'tile_y', 'tile_size', 'tile_fill', 'tile_colour'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sanitise an annotation data.frame
#'
#' @param annotation_df data.frame
#' @param base_size base size
#'
#' @return sanitised annotation_df data.frame
#'
#' @import ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sanitise_annotation_df <- function(annotation_df, base_size = 11) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity checks
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(annotation_df)) {
    return(data.frame())
  }

  stopifnot(is.data.frame(annotation_df))

  if (nrow(annotation_df) == 0L) {
    return(annotation_df)
  }

  necessary_columns <- c('start', 'end', 'segment', 'text', 'label')
  missing_columns <- setdiff(necessary_columns, colnames(annotation_df))
  if (length(missing_columns) != 0L) {
    stop("sanitise_annotation_df(): Missing essential columns: ",
         deparse(missing_columns), call. = FALSE)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # What are sane defaults for the annotation?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text_defaults <- ggplot2::calc_element('text', theme_bw(base_size))
  names(text_defaults) <- paste0("text_", names(text_defaults))
  text_defaults$text_margin <- NULL

  segment_defaults <- ggplot2::calc_element('line', theme_bw(base_size))
  names(segment_defaults) <- paste0("segment_", names(segment_defaults))


  annotation_defaults <- c(
    text_defaults,
    segment_defaults
  )

  annotation_defaults$segment_y <- -0.2
  annotation_defaults$text_y    <- -0.5

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply these sane defaults when the user didn't specify a value
  # Also replace NAs with the default
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (ii in seq_along(annotation_defaults)) {
    col <- names(annotation_defaults)[ii]
    if (!col %in% names(annotation_df)) {
      annotation_df[[col]] <- annotation_defaults[[ii]]
    } else {
      na_idx <- is.na(annotation_df[[col]])
      annotation_df[[col]][na_idx] <- annotation_defaults[[ii]]
      class(annotation_df[[col]]) <- class(annotation_defaults[[ii]])
    }
  }

  annotation_df
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sanitise an bits data.frame
#'
#' @param chars_df data.frame
#' @param base_size base size
#'
#' @return sanitised chars_df data.frame
#'
#' @import ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sanitise_chars_df <- function(chars_df, base_size = 11) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity checks
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(is.data.frame(chars_df))
  stopifnot(!is.null(chars_df))
  stopifnot(nrow(chars_df) > 0)

  necessary_columns <- c('char', 'x')
  missing_columns <- setdiff(necessary_columns, colnames(chars_df))
  if (length(missing_columns) != 0L) {
    stop("sanitise_chars_df(): Missing essential columns: ",
         deparse(missing_columns), call. = FALSE)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # What are sane defaults for the MAIN TEXT + TILE ?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text_defaults <- ggplot2::calc_element('text', theme_bw(base_size))
  names(text_defaults) <- paste0("text_", names(text_defaults))
  text_defaults$text_margin <- NULL

  tile_defaults <- ggplot2::calc_element('rect', theme_bw(base_size))
  names(tile_defaults) <- paste0("tile_", names(tile_defaults))


  bit_defaults <- c(
    text_defaults,
    tile_defaults
  )

  bit_defaults$tile_y <- 0.5
  bit_defaults$text_y <- 0.5


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply these sane defaults when the user didn't specify a value
  # Also replace NAs with the default
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (ii in seq_along(bit_defaults)) {
    col <- names(bit_defaults)[ii]
    if (!col %in% names(chars_df)) {
      chars_df[[col]] <- bit_defaults[[ii]]
    } else if (!grepl('tile_colour|tile_fill', col)) {
      na_idx <- is.na(chars_df[[col]])
      chars_df[[col]][na_idx] <- bit_defaults[[ii]]
      class(chars_df[[col]]) <- class(bit_defaults[[ii]])
    }
  }

  chars_df
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Plot bits
#'
#' @param chars_df data.frame with columns 'bit' and 'x'
#' @param annotation_df data.frame
#' @param title title
#' @param base_size base_size
#' @param plot_theme plot_theme
#'
#' @return ggplot object
#'
#' @import ggplot2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_chars <- function(chars_df, annotation_df, title = NULL, base_size = 11,
                       plot_theme = theme_void(base_size)) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanitise the input data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chars_df      <- sanitise_chars_df(chars_df, base_size = base_size)
  annotation_df <- sanitise_annotation_df(annotation_df, base_size = base_size)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Lay the Foundation of the Plot
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <- ggplot() +
    plot_theme +
    coord_equal()


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add the tiling if requested. TRUE by default
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <- p +
    geom_tile(data = chars_df, aes(x      = x,
                                  y      = I(tile_y),
                                  fill   = I(tile_fill),
                                  colour = I(tile_colour),
                                  size   = I(tile_size)))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add the text for each char
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <- p +
    geom_text(data = chars_df, aes(x      = x,
                                   y      = I(text_y),
                                   label  = char,
                                   colour = I(text_colour),
                                   angle  = I(text_angle),
                                   size   = I(text_size)))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add a title if requested
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(title)) {
    p <- p + annotate('text',
                      label = title,
                      x     = 0.5,
                      y     = 1.5,
                      hjust = 'left',
                      size  = base_size)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply the annotations - one-row-at-a-time
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (idx in seq(nrow(annotation_df))) {
    row <- annotation_df[idx, ]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Annotate with a coloured line segment
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(row$segment)) {
      p <- p +
        geom_segment(data = row, aes(x        = start - 0.45,
                                     xend     = end   + 0.45,
                                     y        = segment_y,
                                     yend     = segment_y,
                                     colour   = I(segment_colour),
                                     linetype = I(segment_linetype),
                                     size     = I(segment_size)))
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Annotate with text
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(row$text)) {
      p <- p + geom_text(data = row, aes(x      = 0.5 * (start + end),
                                         y      = text_y,
                                         label  = label,
                                         colour = I(text_colour),
                                         family = I(text_family),
                                         angle  = I(text_angle),
                                         hjust  = I(text_hjust),
                                         size   = I(text_size)))
    }
  }


  p
}











