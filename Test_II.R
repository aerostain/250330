# Cargar Data
data(mpg)
mpg %>% str()

# Formatear Variables
mpg$year %<>% as.factor()

mpg %<>%
  mutate(
    across(where(is.character), as.factor)
  )

# Tablas preliminares
tmp <-
  mpg %>%
  select(where(is.factor)) %>%
  names()

# Opción 1
tmp %>%
  walk(function(coln) {
    mpg %>%
      pull(coln) %>%
      fct_count() %>%
      print()
  })

# Opción 2
for (x in tmp) {
  cat("\n")
  print(paste0("Variable: ", x))
  print(
    mpg %>%
      pull(x) %>%
      fct_count() %>%
      sheet()
  )
  cat("\n")
}

# Opción 3
for (x in a) {
  cat("\n")
  print(paste0("Variable: ", x))
  print(
    fct_count(mpg[[x]]) %>%
      sheet()
  )
  cat("\n")
}

# Cambiar niveles de factores
mpg %<>%
  mutate(
    n_manuf =
      fct_collapse(manufacturer,
        "europe" = c("audi", "volkswagen"),
        "usa" = c(
          "dodge", "ford","chevrolet",
          "jeep", "land rover", "lincoln", "mercury", "pontiac"
        ),
        "asian" = c("honda", "hyundai", "nissan", "subaru", "toyota")
      )
  )

mpg %<>%
  mutate(
    n_trans = fct_collapse(trans,
      "manual" = c("manual(m5)", "manual(m6)"),
      "auto" = setdiff(levels(mpg$trans), c("manual(m5)", "manual(m6)"))
    )
  )

mpg %<>% mutate(
  n_fl = fct_collapse(fl,
    cdep = c("c", "d", "e", "p")
  )
)

mpg %<>% mutate(
  n_drv = fct_collapse(drv,
    "4r" = c("4", "r")
  )
)

mpg %<>%
  mutate(
    n_cyl = case_when(
      cyl == 5 ~ 4,
      TRUE ~ cyl
    )
  )

# Tablas
tmp <- mpg %>% names()
tmp <- setdiff(tmp, c("manufacturer", "model", "cyl", "trans", "drv", "fl"))
mpg <- mpg[, tmp]

mpg %>% glimpse()

tmp <- mpg %>% sapply(is.factor)
fct <- mpg[, tmp] %>% names()

tmp <- mpg %>% sapply(is.numeric)
nume <- mpg[, tmp] %>% names()

band <- paste(fct[4:6], collapse = ",")

for (j in fct) {
  eval(parse(text = paste0(
    "mpg %>%
    tab_cells(", j, ") %>%
    tab_cols(total(),", band, ") %>%
    tab_stat_cpct() %>%
    tab_pivot() %>%
    print()"
  )))
  cat("\n")
  cat("\n")
}

for (j in nume) {
  eval(parse(text = paste0(
    "suppressWarnings(mpg %>%
    tab_cells(", j, ") %>%
    tab_cols(total(),", band, ") %>%
    tab_stat_mean_sd_n() %>%
    tab_last_sig_means(subtable_marks = 'both') %>%
    tab_pivot()) %>%
    print()"
  )))
  cat("\n")
  cat("\n")
}

getwd()
