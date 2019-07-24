


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Plot the individual bits of a double
#'
#' @inheritParams plot_chars
#' @param dbl double
#' @param ... arguments passed to \code{plot_chars}
#'
#' @return return a ggplot object
#'
#' @import dplyr
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_double <- function(dbl, annotation_df = exhibitionist::anno_double_df,
                        base_size = 3, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # User defines a vector of single character of single digits
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bits <- as.integer(dbl_to_bits(dbl))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create plot data.frame
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chars_df <- tibble(
    char        = bits,
    x           = seq_along(bits),
    tile_fill   = if_else(bits==1, 'white', 'grey85')
  )


  plot_chars(chars_df, annotation_df = annotation_df, base_size = base_size, ...) +
    ylim(-3, 1.5)


}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Plot the individual bits of a double
#'
#' @inheritParams plot_chars
#' @param fp floating_point stored in an integer
#' @param float_type one of 'single', 'half', 'bfloat16'
#' @param float_bits 3 element vector with bit allocations for sign, exponent
#'        and mantissa
#' @param text_y,segment_y y position of the annotations for text and line segment
#'
#' @param ... arguments passed to \code{plot_chars}
#'
#' @return return a ggplot object
#'
#' @import dplyr
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_float <- function(fp,
                       float_type = c('single', 'half', 'bfloat16'),
                       float_bits = NULL, base_size = 8,
                       text_y = -1, segment_y = -0.5,
                       ...) {


  float_type <- match.arg(float_type)

  if (is.null(float_bits)) {
    float_bits <- switch(
      float_type,
      single   = c(1, 8, 23),
      half     = c(1, 5, 10),
      bfloat16 = c(1, 8,  7)
    )
  }

  nbits <- sum(float_bits)
  if (nbits < 3 || nbits > 32) {
    stop("plot_flaot(): bad float_bits: ", deparse(float_bits), call. = FALSE)
  }


  stopifnot(is.integer(fp))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # User defines a vector of single character of single digits
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bits <- as.integer(int_to_bits(fp, nbits))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create plot data.frame
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chars_df <- tibble(
    char        = bits,
    x           = seq_along(bits),
    tile_fill   = if_else(bits==1, 'white', 'grey85')
  )


  csum <- cumsum(float_bits)

  if (float_bits[1] > 0) {
    annotation_df <- exhibitionist::anno_double_df %>%
      mutate(
        start     = c(1, csum[1:2] + 1),
        end       = csum,
        text_y    = -1,
        segment_y = segment_y
      )
  } else {
    annotation_df <- exhibitionist::anno_double_df %>%
      slice(2:3) %>%
      mutate(
        start     = c(1, csum[2] + 1),
        end       = csum[2:3],
        text_y    = text_y,
        segment_y = segment_y
      )
  }



  plot_chars(chars_df, annotation_df = annotation_df, base_size = base_size, ...) +
    ylim(-3, 1.5)


}