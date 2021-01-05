utils::globalVariables(c("test", "scannedAt", "marketValue", "ds", "y",
                         "estimate", "label", "conf.low", "conf.high", ".",
                         "datum", "mean_y", "quantity"))
#' Import Item
#'
#' This function is a basic import of the raw json from the nexhyub API
#'
#' @param item_string any correct written string of an item
#' @param server string of server, eg Patchwerk
#' @param faction string of faction, horde or alliance
#' @return raw json of api request
#' @import dplyr stringr jsonlite
#' @examples
#'
#' import_item("black lotus", "patchwerk", "horde")
#'
#' @export
import_item <- function(item_string, server, faction) {
  if (length(item_string) != 1) {
    stop("Please provide just one item.")
  }
  if (length(server) != 1) {
    stop("Please provide just one server.")
  }
  if (length(faction) != 1) {
    stop("Please provide just one faction.")
  }

  item <- item_string %>%
    str_to_lower() %>%
    str_replace_all(., " ", "-")

  server <- str_to_lower(server)
  faction <- str_to_lower(faction)

  print(paste0("Reading data for ", item, " on ", server, " ", faction))

  json_raw <- read_json(
    glue::glue(
      "https://api.nexushub.co/wow-classic/v1/items/{server}-{faction}/{item}/prices?timerange=1000"
    )
  )

  return(json_raw)
}

#' Clean raw imported item
#'
#' @param json raw json item of api request - generated with import_item function
#' @return clean tibble of item with value, date, server, faction, item ID and item name
#' @import dplyr tidyr lubridate stringr
#'
#' @examples
#'
#' bl_raw <- import_item("black lotus", "patchwerk", "horde")
#' clean_json(bl_raw)
#'
#' @export
clean_json <- function(json) {
  item_name <- json$name
  server <- json$slug
  itemid <- json$itemId

  bl_clean <- tibble(json = json) %>%
    mutate(test = lengths(json)) %>%
    filter(test != 1) %>%
    select(-test) %>%
    unnest(json) %>%
    unnest_wider(json)

  print("Cleaning raw json data")

  df <- bl_clean %>%
    mutate(
      scannedAt = str_replace_all(scannedAt, "T", " "),
      scannedAt = str_remove_all(scannedAt, "Z"),
      scannedAt = ymd_hms(scannedAt),
      marketValue = marketValue / 10000
    ) %>%
    select(ds = scannedAt, y = marketValue, quantity) %>%
    mutate(item = item_name,
           server = server,
           itemid = itemid)

  return(df)
}

#' creates smooth plot for price development
#'
#' @param item_clean raw json item of api request - generated with import_item function
#' @return plot of item value including smooth curve
#' @import dplyr stringr ggplot2 lubridate
#' @examples
#'
#' bl_raw <- import_item("black lotus", "patchwerk", "horde")
#' bl_clean <- clean_json(bl_raw)
#' smooth_item(bl_clean)
#'
#' @export
smooth_item <- function(item_clean) {
  item <- pull(item_clean, item) %>%
    unique() %>%
    str_replace_all(., "-", " ") %>%
    str_to_title(.)

  if(length(item) != 1) {
    stop("More then one item provided, split items into groups. See ?group_split()")
  } else {
    server <- pull(item_clean, server) %>%
      unique() %>%
      str_replace_all(., "-", " ") %>%
      str_to_title(.)

    df_clean <- item_clean %>%
      mutate(datum = as_date(ds)) %>%
      group_by(datum) %>%
      summarise(mean_y = mean(y, na.rm = TRUE)) %>%
      ggplot(aes(datum, mean_y)) +
      geom_line(size = 1) +
      geom_smooth() +
      labs(title = paste0("Price development for ", item, " on ", server),
           y = "Avg. price in Gold",
           x = "Date") +
      theme_light()
  }

  return(df_clean)
}

#' explore linear relation between price and weekday
#'
#' @param item_clean raw json item of api request - generated with import_item function
#' @return tie-fighter plot of item value compared to the weekday
#' @import dplyr lubridate broom.helpers ggplot2
#' @examples
#'
#' bl_raw <- import_item("black lotus", "patchwerk", "horde")
#' bl_clean <- clean_json(bl_raw)
#' lm_item_wday(bl_clean)
#'
#' @export
#' @importFrom stats lm
lm_item_wday <- function(item_clean) {
  item <- pull(item_clean, item) %>%
    unique() %>%
    str_replace_all(., "-", " ") %>%
    str_to_title(.)

  server <- pull(item_clean, server) %>%
    unique() %>%
    str_replace_all(., "-", " ") %>%
    str_to_title(.)

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
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    ggplot2::theme_light() +
    ggplot2::labs(title = paste0("Linear Regression between ", item, " value to the day of the week"),
                  subtitle = paste0("for Server ", server),
                  y = "Day of the week")

}
