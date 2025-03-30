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

mpg %>% glimpse()
mpg %>% str()

mm <- mpg

mm %<>%
  mutate(
    across(where(is.character), as.factor)
  )

mm %>% str()

mm %>%
  .$manufacturer %>%
  fct_count()

mm %<>%
  mutate(
    mnf2 =
      fct_collapse(manufacturer,
        "europe" = c("audi", "volkswagen"),
        "usa" = c(
          "dodge", "ford",
          "jeep", "land rover", "lincoln", "mercury", "pontiac"
        ),
        "asian" = c("honda", "hyundai", "nissan", "subaru", "toyota")
      )
  )

mm %>%
  .$mnf2 %>%
  fct_count()
mm %>%
  .$drv %>%
  fct_count()
mm %>%
  .$fl %>%
  fct_count()

mm %>%
  .$model %>%
  fct_count()

mm %>%
  .$trans %>%
  fct_count()

mm %>%
  cross_cases(trans, nfl)

mm %<>%
  mutate(
    ntrans = fct_collapse(trans,
      "manual" = c("manual(m5)", "manual(m6)"),
      "auto" = setdiff(levels(mm$trans), c("manual(m5)", "manual(m6)"))
    )
  )

mm %>%
  str() %>%
  capture.output() %>%
  write(., "mm.txt")

mm %>%
  .$cyl %>%
  table()

mm %>%
  mutate(ncyl = cyl) %>%
  filter(ncyl == 5) %>%
  select(cyl)

mm %>% cross_cases(fl, mnf)
mm %>% cross_cases(fl, nfl)

mm %<>% mutate(
  nfl = fct_collapse(fl,
    cdep = c("c", "d", "e", "p")
  )
)


mm %>% glimpse()

levels(mm$trans) %in% c("manual(m5)", "manual(m6)")

setdiff(levels(mm$trans), c("manual(m5)", "manual(m6)"))

mm %>% cross_cases(trans, ntrans)

mm %>%
  .$class %>%
  fct_count()

mm %>% cross_cases(model, mnf)

mm %>%
  .$model %>%
  fct_count() %>%
  sheet()

mm %>% glimpse()

mm %<>%
  mutate(nyear = as.factor(year))

mm %>%
  .$cyl %>%
  table()

mm %<>%
  mutate(
    cyl = case_when(
      cyl == 5 ~ 4,
      TRUE ~ cyl
    )
  )

mm %>% sapply(is.numeric)  %>% class

nmm<-
mm %>% select(is.numeric)