
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a single R 32-bit signed integer value to bits
#'
#' @param int32 integer value
#'
#' @return vector of 32 bits, sign bit and then 31 bits with most-significant bit first.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int32_to_bits <- function(int32) {
  stopifnot(length(int32) == 1L)
  stopifnot(is.integer(int32))
  rev(rawToBits(writeBin(int32, raw(), size = 4)))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert the lower 'nbits' of an int32 into bit representation
#'
#' @param int32 integer value
#' @param nbits number of lower bits to keep
#'
#' @return vector of length nbits
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int_to_bits <- function(int32, nbits) {
  bits <- int32_to_bits(int32)
  rev(rev(bits)[seq(nbits)])
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a single double precision floating point value to bits
#'
#' @param dbl double value
#'
#' @return vector of bits, MSB first
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dbl_to_bits <- function(dbl) {
  stopifnot(length(dbl) == 1L)
  stopifnot(is.double(dbl))

  rev(rawToBits(writeBin(dbl, raw(), size = 8)))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname show_bits
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
show_bits.double <- function(x, ...) {
  dbl <- x

  exponent_bias  <- 2^(11 - 1) - 1

  bits         <- dbl_to_bits(dbl)
  sign_bit     <- as.integer(bits[1])
  exponent     <- bits[2:12]
  exponent_str <- paste(as.integer(exponent), collapse = "")
  mantissa     <- bits[13:64]
  mantissa_str <- paste(as.integer(mantissa), collapse = "")

  exponent_val <- sum(2^(10:0) * as.integer(exponent))
  exponent_val <- exponent_val - exponent_bias


  cat(sign_bit, " ", exponent_str,
      " (", exponent_val, ") ",
      mantissa_str, " : ", dbl, "\n", sep = "")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname show_bits
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
show_bits.integer <- function(x, nbits = 32, ...) {
  bits <- int32_to_bits(x)
  if (nbits != 32) {
    bits <- rev(rev(bits)[seq_len(nbits)])
  }
  cat(paste(as.integer(bits), collapse = ""), "\n")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Show a representation of the bits in a numeric value
#'
#' Show a representation of the bits in a integer or floating point value
#'
#' Bits are always show with the most-significant bit on the left.
#'
#' For floating point numbers (i.e. 'doubles') the display is similar to the
#' \url{seven31}{https://github.com/ThinkR-open/seven31} package.
#'
#' @param x double or integer value
#' @param nbits number of bits to dispaly if showing an integer
#' @param ... other arguments passed to specific implementations
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
show_bits <- function(x, ...) {
  UseMethod('show_bits')
}