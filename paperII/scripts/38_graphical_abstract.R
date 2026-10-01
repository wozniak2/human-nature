# ============================================================
# 38_graphical_abstract.R
# Graphical abstract for the Journal of Environmental Psychology.
#
# The six countries shaded by the seen-wanted gap (percentage points seeing
# Master minus wanting it), with the two headline results in words.
#
# Journal requirements (Guide for Authors): 531 x 1328 pixels (h x w) or
# proportionally more, readable at 5 x 13 cm; TIFF, EPS, PDF or MS Office.
# Elsevier's map policy: a map may not show a larger area than the bounding
# box of the study area, and carries the note that map lines delineate study
# areas and do not necessarily depict accepted national boundaries. So the
# map is two panels, each cropped to its own study area: Canada and Panama,
# and the four European countries.
#
# Output: figures/manuscript/graphical_abstract.png (600 dpi) and .pdf,
# drawn at the final size of 13 x 5.2 cm.
#
# Requires: 01 already run. Packages: ggplot2, sf, rnaturalearth,
# rnaturalearthdata, patchwork.
# ============================================================

suppressMessages({ library(ggplot2); library(sf); library(rnaturalearth); library(patchwork) })
sf_use_s2(FALSE)

# --- the gap per country, from the data -------------------------------------
dat <- readRDS("hnr_data.rds")
gap <- do.call(rbind, lapply(split(dat, dat$country), function(d) data.frame(
  country = as.character(d$country[1]),
  seen = 100 * mean(d$typ_now == 1), wanted = 100 * mean(d$typ_should == 1))))
gap$gap <- gap$seen - gap$wanted
pooled <- c(seen = 100 * mean(dat$typ_now == 1), wanted = 100 * mean(dat$typ_should == 1))
print(transform(gap, seen = round(seen, 1), wanted = round(wanted, 1), gap = round(gap, 1)), row.names = FALSE)

# --- shapes -------------------------------------------------------------------
world <- ne_countries(scale = 50, returnclass = "sf")[, c("admin", "geometry")]
world$study <- world$admin %in% gap$country
world$gap <- gap$gap[match(world$admin, gap$country)]

INK <- "#1A1A1A"; MUTE <- "#5E5E5E"; LAND <- "#E9E7E2"; SEA <- "#FFFFFF"
fill_scale <- scale_fill_gradient(low = "#FBE1CC", high = "#B23A00", limits = c(0, 22), na.value = LAND, guide = "none")

panel <- function(countries, crs, pad, labels, clip = NULL) {
  w <- st_transform(world, crs)
  # the study area: the countries' own territory in this region (clip drops, e.g., the Canary Islands and the
  # Caribbean Netherlands, which would otherwise stretch the box far beyond the mainland)
  sa <- world[world$admin %in% countries, ]
  if (!is.null(clip)) sa <- suppressWarnings(st_crop(sa, clip))
  box <- st_bbox(st_transform(sa, crs))
  dx <- (box$xmax - box$xmin) * pad; dy <- (box$ymax - box$ymin) * pad
  lab <- labels                                     # already projected: text at (Xt, Yt), the country at (Xend, Yend)
  ggplot() +
    geom_sf(data = w, aes(fill = gap), colour = "white", linewidth = 0.15) +
    geom_sf(data = w[w$study, ], aes(fill = gap), colour = "#7A2A00", linewidth = 0.25) +
    { if (any(lab$line)) geom_segment(data = lab[lab$line, ], aes(x = Xt, y = Yt, xend = Xend, yend = Yend), colour = MUTE, linewidth = 0.2) } +
    geom_label(data = lab, aes(x = Xt, y = Yt, label = text), size = 1.95, lineheight = 0.85, colour = INK, fontface = "bold",
               fill = "white", label.size = 0, label.padding = unit(0.06, "lines"), alpha = 0.85) +
    fill_scale +
    coord_sf(crs = crs, xlim = c(box$xmin - dx, box$xmax + dx), ylim = c(box$ymin - dy, box$ymax + dy), expand = FALSE, datum = NA) +
    theme_void() +
    theme(panel.background = element_rect(fill = SEA, colour = NA), plot.margin = margin(1, 1, 1, 1))
}

lbl <- function(country, lon, lat, tlon = lon, tlat = lat) {
  g <- gap$gap[gap$country == country]
  data.frame(country = country, lon = lon, lat = lat, tlon = tlon, tlat = tlat,
             text = sprintf("%s\n+%.1f", country, g), line = tlon != lon | tlat != lat)
}
place_text <- function(l, crs) {   # project the label anchor (text) and the point it refers to
  p <- st_coordinates(st_transform(st_as_sf(l, coords = c("tlon", "tlat"), crs = 4326), crs))
  q <- st_coordinates(st_transform(st_as_sf(l, coords = c("lon", "lat"), crs = 4326), crs))
  l$Xt <- p[, 1]; l$Yt <- p[, 2]; l$Xend <- q[, 1]; l$Yend <- q[, 2]; l
}

crs_am <- "+proj=laea +lat_0=45 +lon_0=-95 +datum=WGS84"
crs_eu <- "+proj=laea +lat_0=52 +lon_0=10 +datum=WGS84"
lab_am <- place_text(rbind(lbl("Canada", -105, 58), lbl("Panama", -80.0, 8.7, -97, 15)), crs_am)
lab_eu <- place_text(rbind(lbl("Spain", -3.7, 40.2), lbl("Sweden", 15.0, 62.0, 4.5, 63.5),
                           lbl("Poland", 19.3, 52.1), lbl("Netherlands", 5.6, 52.3, -1.5, 55.3)), crs_eu)
mk <- function(l) data.frame(text = l$text, line = l$line, Xt = l$Xt, Yt = l$Yt, Xend = l$Xend, Yend = l$Yend)
eu_box <- st_bbox(c(xmin = -12, ymin = 34, xmax = 30, ymax = 72), crs = st_crs(4326))
p_am <- panel(c("Canada", "Panama"), crs_am, 0.03, mk(lab_am))
p_eu <- panel(c("Spain", "Sweden", "Poland", "Netherlands"), crs_eu, 0.05, mk(lab_eu), clip = eu_box)

# --- text panel -----------------------------------------------------------------
txt <- ggplot() + xlim(0, 1) + ylim(0, 1) + theme_void() +
  annotate("text", x = 0, y = 0.97, hjust = 0, vjust = 1, size = 2.9, fontface = "bold", colour = INK, lineheight = 0.9,
           label = "Seeing mastery,\nwanting less") +
  annotate("text", x = 0, y = 0.70, hjust = 0, vjust = 1, size = 2.15, colour = INK, lineheight = 0.95,
           label = sprintf("%.0f%% see humans as masters of\nnature; %.0f%% want them to be.\nShading: the gap in each country,\nin percentage points.", pooled[["seen"]], pooled[["wanted"]])) +
  annotate("text", x = 0, y = 0.30, hjust = 0, vjust = 1, size = 2.15, colour = INK, lineheight = 0.95,
           label = "The more agency people grant a\nplace they know, the more of those\nwho see mastery reject it:\n40% to 83%.") +
  theme(plot.margin = margin(2, 2, 2, 4))

note <- "Map lines delineate study areas and do not necessarily depict accepted national boundaries. Six online panel samples, N = 2,513."
ga <- (txt | p_am | p_eu) + plot_layout(widths = c(1.0, 1.0, 1.0)) +
  plot_annotation(caption = note, theme = theme(plot.caption = element_text(size = 4.6, colour = MUTE, hjust = 1, margin = margin(1, 0, 0, 0)),
                                                plot.background = element_rect(fill = "white", colour = NA)))

dir.create(file.path("figures", "manuscript"), showWarnings = FALSE, recursive = TRUE)
ggsave(file.path("figures", "manuscript", "graphical_abstract.png"), ga, width = 13, height = 5.2, units = "cm", dpi = 600, bg = "white")
ggsave(file.path("figures", "manuscript", "graphical_abstract.pdf"), ga, width = 13, height = 5.2, units = "cm", bg = "white")
cat("wrote figures/manuscript/graphical_abstract.png (600 dpi, 13 x 5.2 cm) and .pdf\n")
