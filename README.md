# geok

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Overview

geok is a group of functions.

- sumup
- format_number

## Installation

1. Install `geok` from GitHub:

```R
remotes::install_github("izi-i3i/geok")
```

## Usage

```R
library(geok)
```

### summarize

```R
sm <- sumup(iris,
  measure.var = "Sepal.Length",
  group.by = "Species",
  lang = "en")

sm[2]
# $formated
# Key: <Params>
#        Params setosa versicolor virginica
#        <char> <char>     <char>    <char>
#  1:   Total_N    150        150       150
#  2:   Missing      0          0         0
#  3:   Valid_N    150        150       150
#  4:       Min  4.300      4.900     4.900
#  5:       Max  5.800      7.000     7.900
#  6:    Median  5.000      5.900     6.500
#  7:       SAM  5.006      5.936     6.588
#  8:      TM10  5.003      5.938     6.572
#  9:       MAD  0.297      0.519     0.593
# 10:        SD  0.352      0.516     0.636
# 11:        SE  0.050      0.073     0.090
# 12:        CV  0.070      0.087     0.097
# 13:       Q25  4.800      5.600     6.225
# 14:       Q75  5.200      6.300     6.900
# 15:       IQR  0.400      0.700     0.675
# 16:     Range  1.500      2.100     3.000
# 17:  Skewness  0.113      0.099     0.111
# 18:  Kurtosis -0.451     -0.694    -0.203
# 19: Normality  0.460      0.465     0.258

sm[3]
# $mean5numbers
#       Species      SAM (SD) [Min; Max] Valid_N
#        <fctr>                           <char>
# 1:     setosa 5.006 (0.352) [4.300; 5.800] 150
# 2: versicolor 5.936 (0.516) [4.900; 7.000] 150
# 3:  virginica 6.588 (0.636) [4.900; 7.900] 150
```

### format_number

```R
x <- c(
  -0.000123, 0, 0.000123, 0.00123, 0.0123, 0.123, 1.23,
  12.3, 123, 123456.789, NA, NaN, Inf, -Inf, pi
)
format_number(x, nsmall = 0)
```
