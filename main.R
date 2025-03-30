# LoadLibrary
library(tidyverse)
library(magrittr)
library(ggdark)
library(expss)
library(lubridate)
library(haven)
library(pacman)

# .lintr
text <- c(
  "linters: linters_with_defaults(",
  "  assignment_linter(allow_pipe_assign = TRUE)",
  "  )"
)

pathfile <- file.path(getwd(), ".lintr")

writeLines(text, pathfile)

options("width" = 10000)
getOption("width")

# Directorios
file.create("test.R")
file.create("readme.md")
file.create("inform.qmd")
file.create(".gitignore")
dir.create("Files")

# Procesamiento
options("width" = 10000)
