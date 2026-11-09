# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

# CRAN policy: at most 2 threads. Polars reads this when rquasar is loaded.
Sys.setenv(POLARS_MAX_THREADS = 2)
options(rquasar.threads = 2L)

library(testthat)
library(rquasar)

test_check("rquasar")
