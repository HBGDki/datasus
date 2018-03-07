## some universal utility functions and packages to load
##---------------------------------------------------------

library(dplyr)
library(readr)
library(read.dbc)

get_tb_uf <- function() {
  tf <- "data/datasus/territory/tb_uf.csv"
  tb_uf <- tbl_df(suppressMessages(readr::read_delim(tf, delim = ";")))
  tb_uf %>% filter(CO_STATUS == "ATIVO") %>%
    select(-c(CO_STATUS, DS_NOME, CO_SUCESS, CO_ALTER, NU_ORDEM)) %>%
    rename(state_code = CO_UF, state = DS_SIGLA,
      state_name = DS_NOMEPAD, region_code = CO_REGIAO, area = NU_AREA)
}

read_and_transform <- function(f, dict, keep) {
  if (grepl("\\.csv$", f)) {
    b <- readr::read_csv(f)
  } else if (grepl("\\.dbf$", f)) {
    b <- foreign::read.dbf(f)
  } else {
    b <- read.dbc::read.dbc(f)
  }

  # sapply(b[keep], function(x) paste0(class(x)[1], "_", length(unique(x))))
  b <- b[, names(b) %in% keep]

  for (nm in keep) {
    tmp <- b[[nm]]
    dct <- dict[[nm]]

    if (is.factor(tmp))
      tmp <- as.character(tmp)

    if (!is.null(dct$map_en)) {
      tmp <- unname(dct$map_en[match(tmp, names(dct$map_en))])
      tmp <- factor(tmp, levels = unique(dct$map_en))
    }

    if (dct$type == "numeric") {
      tmp <- as.numeric(tmp)
    } else if (dct$type == "date") {
      tmp <- as.Date(tmp, format = "%d%m%Y")
    }

    b[[nm]] <- tmp
    names(b)[which(names(b) == nm)] <- dct$name_en
  }

  b
}

## functions to summarize a variable by another and plot
##---------------------------------------------------------

summarize_var_by <- function(dat, x, by) {
  x <- enquo(x)
  by <- enquo(by)

  snsc %>%
    group_by(!!by) %>%
    summarise(
      n = n(),
      med  = median(!!x, na.rm = TRUE),
      q1 = quantile(!!x, 0.25, na.rm = TRUE),
      q3 = quantile(!!x, 0.75, na.rm = TRUE),
      mad = mad(!!x, na.rm = TRUE),
      mean = mean(!!x, na.rm = TRUE),
      sd = sd(!!x, na.rm = TRUE),
      se = sd / sqrt(n()))
}

plot_var_by <- function(dat, x, by, se = FALSE, xlab = NULL, ylab = NULL) {
  dat <- dat[!is.na(dat[[by]]), ]

  dat$by <- dat[[by]]
  if (is.null(xlab))
    xlab <- by
  if (is.null(ylab))
    ylab <- x

  p <- ggplot(dat, aes(by, med)) +
    geom_point(size = 2) +
    theme_bw() +
    labs(x = xlab, y = ylab)

  if (se) {
    p <- p + geom_errorbar(aes(ymin = mean - 2 * se, ymax = mean + 2 * se), width = 0.2)
  } else {
    p <- p + geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2)
  }

  p
}

## functions to summarize and plot by state and year
##---------------------------------------------------------

summarize_st_yr <- function(dat, var) {
  var <- enquo(var)

  res <- dat %>%
    group_by(m_state_code, birth_year, !!var) %>%
    tally()

  res <- res %>%
    filter(!is.na(!!var) & !is.na(m_state_code)) %>%
    group_by(birth_year, m_state_code) %>%
    mutate(
      n_yst = sum(n),
      pct = n / n_yst * 100)
}

plot_st_yr <- function(dat, var, ylab = NULL, llab = NULL) {
  if (is.null(ylab))
    ylab <- paste0("Percentage of Births")

  if (is.null(llab))
    llab <- var

  dat$fill_var <- dat[[var]]

  ggplot(dat, aes(birth_year, pct, fill = fill_var)) +
    geom_col(position = position_stack(), width = 1, alpha = 0.7) +
    geom_abline(slope = 0, intercept = 50, alpha = 0.25) +
    theme_bw() +
    scale_fill_tableau(name = llab) +
    scale_x_continuous(expand = c(0, 0), labels = function(x) paste0("'", substr(x, 3, 4))) +
    scale_y_continuous(expand = c(0, 0)) +
    facet_geo(~ m_state_code, grid = "br_states_grid2", label = "name") +
    theme(strip.text.x = element_text(margin = margin(0.1, 0, 0.1, 0, "cm"), size = 7)) +
    labs(x = "Year", y = ylab)
}
