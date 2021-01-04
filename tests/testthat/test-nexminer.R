library(dplyr)
library(ggplot2)

server <- "patchwerk"
fraction <- "horde"

bl_raw <- import_item("black lotus", server, faction)
bl_clean <- clean_json(bl_raw)
smooth_item(bl_clean)
lm_item_wday(bl_clean)
