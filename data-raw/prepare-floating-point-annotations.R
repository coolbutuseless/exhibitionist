

suppressPackageStartupMessages({
  library(dplyr)
})



# double   = list(float_bits = c(1, 11, 52)),
# single   = list(float_bits = c(1,  8, 23)),
# half     = list(float_bits = c(1,  5, 10)),
# bfloat16 = list(float_bits = c(1,  8,  7))

anno_double_df <- readr::read_csv(
"start, end,  segment,  text, text_size, label     , text_y, segment_y, segment_size, segment_colour
     1,    1,    TRUE ,  TRUE,         5, sign     ,   -2  ,      -0.5,            2, red
     2,   12,    TRUE ,  TRUE,         5, exponent ,   -2  ,      -0.5,            2, darkgreen
     13,  64,    TRUE ,  TRUE,         5, mantissa ,   -2  ,      -0.5,            2, blue
")




usethis::use_data(anno_double_df  , overwrite = TRUE)
