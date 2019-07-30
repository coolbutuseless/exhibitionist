




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Plot the individual bits of a double in a compact form
#'
#' @inheritParams plot_chars
#' @param dbl double
#' @param legend.text.multiplier tweak size of legend text. default: 3
#' @param ... ignored
#'
#' @return return a ggplot object
#'
#' @import dplyr
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_compact_double <- function(dbl, base_size = 8, legend.text.multiplier = 3, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # User defines a vector of single character of single digits
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bits <- as.integer(dbl_to_bits(dbl))

  plot_compact_core(bits, c(1, 11, 52), base_size = base_size) +
    theme(
      legend.position      = c(1, 0.05),
      legend.justification = c(1, 0.05),
      legend.text          = element_text(size = base_size * legend.text.multiplier)
    )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Plot the individual bits of a low-fidelity float (stored in an integer)
#'
#' @inheritParams plot_chars
#' @param fp floating_point stored in an integer
#' @param float_type one of 'single', 'half', 'bfloat16'
#' @param float_bits 3 element vector with bit allocations for sign, exponent
#'        and mantissa
#'
#' @param ... arguments passed to \code{plot_chars}
#'
#' @return return a ggplot object
#'
#' @import dplyr
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_compact_float <- function(fp,
                               float_type = c('single', 'half', 'bfloat16'),
                               float_bits = NULL, base_size = 8,...) {


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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # User defines a vector of single character of single digits
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bits <- as.integer(int_to_bits(fp, nbits))

  plot_compact_core(bits, float_bits, base_size = base_size)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Core plot function for compact double/float
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_compact_core <- function(bits, float_bits, base_size = base_size) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Dimensions of grid
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nbits  <- sum(float_bits)
  width  <- float_bits[1] + float_bits[2]
  height <- ceiling(sum(float_bits)/width)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # coordinates of each bit
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x    <- rep(seq(width)       , times = height)[seq(nbits)]
  y    <- rep(rev(seq(height)) , each  = width )[seq(nbits)]
  type <- factor(c(rep('sign'    , float_bits[1]),
                  rep('exponent', float_bits[2]),
                  rep('mantissa', float_bits[3])), levels = c('sign', 'exponent', 'mantissa'))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Data.frame for plotting
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  char_df <- data.frame(
    bits = bits,
    x    = x,
    y    = y,
    type = type
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # geom_tile + remove all margins
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ggplot(char_df, aes(x=x, y=y)) +
    geom_tile(aes(fill = type), height = 0.9, width = 0.9, alpha = 0.25) +
    geom_text(aes(label = bits), size = base_size) +
    theme_void(base_size = base_size) +
    coord_equal() +
    theme(
      legend.position   = 'bottom',
      legend.text       = element_text(size = base_size * 3),
      legend.direction  = 'horizontal',
      plot.margin       = unit(c(0, 0, 0, 0), 'null'),
      panel.spacing     = unit(c(0, 0, 0, 0), 'null'),
    ) +
    scale_fill_brewer(palette = 'Set1') +
    guides(fill = guide_legend(title = NULL)) +
    scale_x_continuous(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0))
}








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

