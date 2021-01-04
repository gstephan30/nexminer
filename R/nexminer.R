#' Import Item
#'
#' @param item_string any correct written string of an item
#' @param server string of server, eg Patchwerk
#' @param faction string of faction, horde or alliance
#' @return raw json of api request
#' @examples
#'
#' import_item("black lotus", server, faction)
import_item <- function(item_string, server, faction) {
  require(dplyr)
  item <- item_string %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(., " ", "-")

  server <- stringr::str_to_lower(server)
  faction <- stringr::str_to_lower(faction)

  jsonlite::read_json(
    glue::glue(
      "https://api.nexushub.co/wow-classic/v1/items/{server}-{faction}/{item}/prices?timerange=1000"
    )
  )
}

#' Clean raw imported item
#'
#' @param json raw json item of api request - generated with import_item function
#' @return clean tibble of item with value, date, server, faction and item string
#'
#' @examples
#' bl_raw <- import_item("black lotus", server, faction)
#' clean_json(bl_raw)
clean_json <- function(json) {
  item_name <- json$name
  server <- json$slug

  bl_clean <- tibble(json = json) %>%
    dplyr::mutate(test = lengths(json)) %>%
    dplyr::filter(test != 1) %>%
    dplyr::select(-test) %>%
    tidyr::unnest(json) %>%
    tidyr::unnest_wider(json)

  df <- bl_clean %>%
    dplyr::mutate(
      scannedAt = stringr::str_sub(scannedAt, 1, 10),
      scannedAt = lubridate::ymd(scannedAt),
      marketValue = marketValue / 10000
    ) %>%
    dplyr::select(ds = scannedAt, y = marketValue) %>%
    dplyr::mutate(item = item_name,
                  server = server)

  return(df)
}

#' creates smooth plot for price development
#'
#' @param item_clean raw json item of api request - generated with import_item function
#' @return plot of item value including smooth curve
#' @examples
#' bl_raw <- import_item("black lotus", server, fraction)
#' bl_clean <- clean_json(bl_raw)
#' smooth_item(bl_clean)
smooth_item <- function(item_clean) {
  item <- dplyr::pull(item_clean, item) %>%
    unique() %>%
    stringr::str_replace_all(., "-", " ") %>%
    stringr::str_to_title(.)

  server <- dplyr::pull(item_clean, server) %>%
    unique() %>%
    stringr::str_replace_all(., "-", " ") %>%
    stringr::str_to_title(.)

  item_clean %>%
    ggplot2::ggplot(ggplot2::aes(ds, y)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_smooth() +
    ggplot2::labs(title = paste0("Price development for ", item, " on ", server))
}

#' explore linear relation between price and weekday
#'
#' @param item_clean raw json item of api request - generated with import_item function
#' @return tie-fighter plot of item value compared to the weekday
#' @examples
#' bl_raw <- import_item("black lotus", server, fraction)
#' bl_clean <- clean_json(bl_raw)
#' lm_item_wday(bl_clean)
lm_item_wday <- function(item_clean) {
  item_clean %>%
    dplyr::mutate(wday = lubridate::wday(ds, label = TRUE),
                  weekend = ifelse(grepl("Fri|Sat|Sun", wday), 1, 0)) %>%
    mutate(wday = as.character(wday)) %>%
    lm(y ~ wday, data = .) %>%
    broom.helpers::tidy_and_attach(conf.int = TRUE) %>%
    broom.helpers::tidy_remove_intercept() %>%
    broom.helpers::tidy_add_reference_rows() %>%
    broom.helpers::tidy_add_estimate_to_reference_rows() %>%
    broom.helpers::tidy_add_term_labels() %>%
    ggplot2::ggplot(ggplot2::aes(estimate, label)) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low, xmax = conf.high), height = 0.2)
}
