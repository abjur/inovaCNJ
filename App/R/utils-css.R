create_theme_css <- function() {
  fresh::create_theme(
    fresh::bs4dash_yiq(
      contrasted_threshold = 10,
      text_dark = "#ffffff"
    ),
    fresh::bs4dash_status(
      info = status_para_cor("info"),
      secondary = status_para_cor("secondary"),
      primary = status_para_cor("primary"),
      success = status_para_cor("success"),
      warning = status_para_cor("warning"),
      danger = status_para_cor("danger")
    ),
    fresh::bs4dash_color(
      lightblue = status_para_cor("info"),
      gray_800 = "#495961",
      blue = status_para_cor("primary"),
      green = status_para_cor("success"),
      yellow = status_para_cor("warning")
    )
  )
}

status_para_cor <- function(status) {
  switch (status,
          info = "#0f7cbf",
          secondary = "#495961",
          primary = "#003366",
          success = "#caab5d",
          warning = "#ffcd37",
          danger = "#BF616A")
}