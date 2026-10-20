#' Text, table and figure helpers for Hawaiʻi water supply reports
#'
#' Formatting functions used for inline numbers in R Markdown, a flextable
#' wrapper with fixed column widths, and a figure saver. None are park-specific.

num <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ",")
int <- function(x) formatC(round(x), format = "d", big.mark = ",")
sgn <- function(x, d = 1) paste0(ifelse(round(x, d) > 0, "+", ""), num(x, d))
pc <- function(x, d = 1) paste0(sgn(x, d), "%")
sig <- function(x, n = 2) int(signif(x, n))

#' "-2.1 to +1.4%" for a range of signed percentages
pc_range <- function(x, d = 1) {
  r <- sort(range(x, na.rm = TRUE))
  if (round(r[1], d) == round(r[2], d)) pc(r[1], d) else paste0(sgn(r[1], d), " to ", sgn(r[2], d), "%")
}
#' "24 to 31" for a range of values
num_range <- function(x, d = 1) {
  r <- sort(range(x, na.rm = TRUE))
  if (round(r[1], d) == round(r[2], d)) num(r[1], d) else paste0(num(r[1], d), " to ", num(r[2], d))
}
#' "15.3 to 17.2%" (magnitudes) for "declines ..." phrasing
mag_range <- function(x, d = 1) {
  r <- sort(abs(range(x, na.rm = TRUE)))
  if (round(r[1], d) == round(r[2], d)) paste0(num(r[1], d), "%") else paste0(num(r[1], d), " to ", num(r[2], d), "%")
}
#' "13–16" for "from A to B" phrasing
dash_range <- function(x, d = 0) {
  r <- sort(range(x, na.rm = TRUE))
  if (round(r[1], d) == round(r[2], d)) num(r[1], d) else paste0(num(r[1], d), "–", num(r[2], d))
}

#' flextable with fixed column widths (relative `weights` scaled to `total` inches)
ft_make <- function(df, caption, weights = rep(1, ncol(df)), size = 9, total = 6.8) {
  flextable::flextable(df) %>%
    flextable::theme_booktabs() %>%
    flextable::fontsize(size = size, part = "all") %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::valign(valign = "top", part = "body") %>%
    flextable::set_caption(caption) %>%
    flextable::width(width = total * weights / sum(weights)) %>%
    flextable::set_table_properties(layout = "fixed")
}

#' x axis for monthly plots in water-year order (Nov ... Oct); plot `x = hi_wy_month(month)`
hi_scale_x_wy_month <- function(...) {
  ggplot2::scale_x_continuous(breaks = 1:12, labels = substr(month.abb[c(11:12, 1:10)], 1, 1), ...)
}

#' x axis for dated series with a tick at the start (November 1) of every `every`-th
#' water year, labeled with the water year
hi_scale_x_wy_date <- function(every = 2, ...) {
  ggplot2::scale_x_date(
    breaks = function(lim) {
      wy <- seq(hi_water_year(as.integer(format(lim[1], "%Y")), as.integer(format(lim[1], "%m"))),
                hi_water_year(as.integer(format(lim[2], "%Y")), as.integer(format(lim[2], "%m"))))
      as.Date(paste0(wy[wy %% every == 0] - 1L, "-11-01"))
    },
    labels = function(d) as.integer(format(d, "%Y")) + 1L, ...)
}

#' Save a ggplot to `dir` as PNG and return it with its size for knitr chunk options
save_fig <- function(p, name, w = 6.5, h = 4, dir = getOption("hi.fig_dir", tempdir())) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(file.path(dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
  list(p = p, w = w, h = h)
}
