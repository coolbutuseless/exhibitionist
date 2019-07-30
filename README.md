
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ![](man/figures/header.png)

<!-- badges: start -->

![](http://img.shields.io/badge/cool-useless-green.svg)
<!-- badges: end -->

The `exhibitionist` package provides functions for showing and
annotating sequences of characters or numbers.

## What’s in the box

  - `show_bit()` for showing bit sequences of integers and doubles in
    the console
  - `plot_char()` for plotting a sequence of characters as defined in a
    `chars_df` data.frame with an `annotation_df` data.frame used to
    control the annotation underneat the characters.

## Installation

You can install from
[GitHub](https://github.com/coolbutuseless/exhibitionist) with:

``` r
# install.packages("devtools")
devtools::install_github("coolbutuseless/exhibitionist")
```

# `show_bit()` for displaying integers and doubles in the terminal

``` r
show_bits(12345.67)
#> 0 10000001100 (13) 1000000111001101010111000010100011110101110000101001 : 12345.67
```

``` r
show_bits(12345L)
#> 00000000000000000011000000111001
```

# Built in annotation for ‘doubles’

``` r
plot_double(1.2345e87)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r
plot_compact_double(1.2345e87, base_size = 5, legend.text.multiplier = 2)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

# Some miscellaneous examples

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

# Annotating the word ‘exhibitionist’ for the header of this page

The header of this page was made with this package.

The two key data.frames are:

  - `chars_df` which must include the columns
      - `x` for the the x-coordinate of the character
      - `char` for the character itself
      - and optional styling columns (see below
  - `annotation_df` which must include the columns
      - `start`/`end` for the start and end of a line segment underneath
        the tiles
      - `segment`/`text` logical values which indicate whether the
        segment and text should be drawn
      - and optional styling columns (see below)

Styling columns:

  - variables prefixed with `text_` are passed to `geom_text()`
  - variables prefixed with `segment_` are passed to `geom_segment()`
  - The characters inside the tiles and the annotations are styled
    independently.

<!-- end list -->

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create the characters data.frame
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chars_df <- tibble(
  char        = strsplit("Exhibitionist", '')[[1]],
  x           = seq_along(char),
  text_colour = 'black'
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the word 'bit' to be BLUE text
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chars_df$text_colour[5:7] <- 'blue'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How should the individual characters be labelled
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
annotation_df <- readr::read_csv(
"start, end,  segment,  text, text_size, label               , segment_colour, segment_size
     1,   4,    TRUE ,  TRUE,         7, A package for       , grey30        , 1
     5,   7,    TRUE ,  TRUE,         7, annotating          , darkgreen     , 2
     8,  13,    TRUE ,  TRUE,         7, bit & char sequences, orange        , 1
")

plot_chars(chars_df, annotation_df) + ylim(-2, 1.5)
```

<img src="man/figures/README-header-1.png" width="100%" />

# Annotating way too much

This is a demonstration of a lot more options you can change to modify
appearance.

In general:

  - variables prefixed with `text_` are passed to `geom_text()`
  - variables prefixed with `segment_` are passed to `geom_segment()`
  - The characters inside the tiles have different styling attributes
    than the annotations which appear
underneath.

<!-- end list -->

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create the characters data.frame
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chars_df <- tibble(
  char        = strsplit("Over the top", '')[[1]],
  x           = seq_along(char),
  text_colour = 'black',
  text_size   = 11,
  text_angle  = 0
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Style a lot of the characters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chars_df <- chars_df %>% 
  mutate(
    text_colour = if_else(x %% 2 == 0, 'red', text_colour),
    text_angle  = if_else(x %% 3 == 0,   45 , text_angle ),
    text_size   = if_else(x %% 6 == 0,   28 , text_size  ),
    tile_fill   = if_else(char == ' ', NA_character_, 'white'),
    tile_colour = if_else(char == ' ', NA_character_, 'black')
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How should the individual characters be labelled underneath?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
annotation_df <- readr::read_csv(
"start, end,  segment,  text, text_size, label               , segment_colour, segment_linetype, segment_size, text_family, text_angle, text_hjust
     1,   4,    TRUE ,  TRUE,        10, Over it!!           , darkgreen     , 1               , 1           , serif      , 0         ,  0.5
     6,   8,    TRUE ,  TRUE,         7, Too Much?           , blue          , 2               , 2           ,            , 45        ,  1
    10,  12,    TRUE ,  TRUE,         7, Or not enough??     , orange        , 3               , 4           ,            , 180       ,  0.5
")

plot_chars(chars_df, annotation_df) + 
  ylim(-2, 1.2)
```

<img src="man/figures/README-too_much-1.png" width="100%" />
