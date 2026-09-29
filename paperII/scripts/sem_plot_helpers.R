# ============================================================
# sem_plot_helpers.R
# A small ggplot2 engine for path diagrams, used by 10_figures.R.
#
# It draws standardised estimates read straight from a fitted lavaan model;
# it does not estimate anything. Each diagram is described by two tables:
#   nodes: id, label, x, y, shape ("ellipse" | "box"), w, h, fill, border,
#          size (text, mm), face, lwd
#   edges: from, to, label, kind ("path" | "load"), beta, sig, t (label
#          position, 0 = source, 1 = target), end_side ("auto" | "left" |
#          "right" | "top" | "bottom"), end_off (shift along the target side),
#          start_side, start_off
# Coordinates are in inches, so node sizes and text can be judged directly.
# Lines are clipped at the node outlines, widths grow with |beta|, arrowheads
# have one fixed size, and every number sits on a white chip so that no line
# strikes through it.
# ============================================================

suppressMessages(library(ggplot2))

sem_col <- list(up = "#0072B2", down = "#D55E00", ns = "#BDBDBD", load = "#7A7A7A",
                ink = "#1A1A1A", mute = "#5E5E5E")

# outline point of a node in the direction of (px, py)
sem_boundary <- function(nd, px, py) {
  dx <- px - nd$x; dy <- py - nd$y
  if (dx == 0 && dy == 0) return(c(nd$x, nd$y))
  if (nd$shape == "ellipse") {
    s <- 1 / sqrt((dx / (nd$w / 2))^2 + (dy / (nd$h / 2))^2)
  } else {
    s <- min((nd$w / 2) / abs(dx), (nd$h / 2) / abs(dy), na.rm = TRUE)
  }
  c(nd$x + s * dx, nd$y + s * dy)
}

sem_port <- function(nd, side, off) {
  switch(side,
    left   = c(nd$x - nd$w / 2, nd$y + off),
    right  = c(nd$x + nd$w / 2, nd$y + off),
    top    = c(nd$x + off, nd$y + nd$h / 2),
    bottom = c(nd$x + off, nd$y - nd$h / 2))
}

sem_polygon <- function(nd, id) {
  if (nd$shape == "ellipse") {
    a <- seq(0, 2 * pi, length.out = 121)
    data.frame(id = id, x = nd$x + nd$w / 2 * cos(a), y = nd$y + nd$h / 2 * sin(a))
  } else {                                   # rounded rectangle
    r <- min(0.09, nd$h / 2, nd$w / 2); hw <- nd$w / 2; hh <- nd$h / 2
    corner <- function(cx, cy, a0) { a <- seq(a0, a0 + pi / 2, length.out = 9)
      data.frame(x = cx + r * cos(a), y = cy + r * sin(a)) }
    pts <- rbind(corner(nd$x + hw - r, nd$y + hh - r, 0),
                 corner(nd$x - hw + r, nd$y + hh - r, pi / 2),
                 corner(nd$x - hw + r, nd$y - hh + r, pi),
                 corner(nd$x + hw - r, nd$y - hh + r, 3 * pi / 2))
    data.frame(id = id, pts)
  }
}

sem_plot <- function(nodes, edges, xlim, ylim, title = NULL, subtitle = NULL, caption = NULL,
                     bands = NULL, headers = NULL, texts = NULL, legend_at = NULL,
                     arrow_mm = 2.6, base_size = 11) {
  nodes$shape <- ifelse(is.na(nodes$shape), "box", nodes$shape)
  nodes$fill   <- ifelse(is.na(nodes$fill), "#F4F4F4", nodes$fill)
  nodes$border <- ifelse(is.na(nodes$border), "#8A8A8A", nodes$border)
  nodes$size   <- ifelse(is.na(nodes$size), 3.3, nodes$size)
  nodes$lwd    <- ifelse(is.na(nodes$lwd), 0.5, nodes$lwd)
  nodes$face   <- ifelse(is.na(nodes$face), "plain", nodes$face)
  rownames(nodes) <- nodes$id

  polys <- do.call(rbind, lapply(seq_len(nrow(nodes)), function(i) sem_polygon(nodes[i, ], nodes$id[i])))
  polys <- merge(polys, nodes[, c("id", "fill", "border", "lwd")], by = "id", sort = FALSE)
  polys <- polys[order(match(polys$id, nodes$id)), ]

  # edge geometry
  E <- edges
  for (col in c("t", "end_off", "start_off")) if (is.null(E[[col]])) E[[col]] <- NA_real_
  if (is.null(E$end_side))   E$end_side   <- NA_character_
  if (is.null(E$start_side)) E$start_side <- NA_character_
  if (is.null(E$kind))       E$kind       <- "path"
  if (is.null(E$sig))        E$sig        <- TRUE
  geo <- lapply(seq_len(nrow(E)), function(i) {
    e <- E[i, ]; a <- nodes[e$from, ]; b <- nodes[e$to, ]
    es <- if (is.na(e$end_side)) "auto" else e$end_side
    ss <- if (is.na(e$start_side)) "auto" else e$start_side
    eo <- if (is.na(e$end_off)) 0 else e$end_off
    so <- if (is.na(e$start_off)) 0 else e$start_off
    pe <- if (es == "auto") NULL else sem_port(b, es, eo)
    ps <- if (ss == "auto") NULL else sem_port(a, ss, so)
    tgt <- if (is.null(pe)) c(b$x, b$y) else pe
    src <- if (is.null(ps)) c(a$x, a$y) else ps
    if (is.null(ps)) ps <- sem_boundary(a, tgt[1], tgt[2])
    if (is.null(pe)) pe <- sem_boundary(b, src[1], src[2])
    data.frame(x = ps[1], y = ps[2], xend = pe[1], yend = pe[2])
  })
  geo <- do.call(rbind, geo)
  E <- cbind(E, geo)
  E$t <- ifelse(is.na(E$t), 0.5, E$t)
  E$lx <- E$x + E$t * (E$xend - E$x); E$ly <- E$y + E$t * (E$yend - E$y)
  E$col <- ifelse(E$kind == "load", sem_col$load,
           ifelse(!E$sig, sem_col$ns, ifelse(E$beta < 0, sem_col$down, sem_col$up)))
  E$lwd <- ifelse(E$kind == "load", 0.45, ifelse(!E$sig, 0.5, 0.55 + 2.6 * pmin(abs(E$beta), 0.7)))
  E$lcol <- ifelse(E$kind == "path" & !E$sig, "#8C8C8C", E$col)
  E$bold <- E$kind == "path" & E$sig
  E$lsize <- ifelse(E$kind == "load", 2.6, 3.1)

  g <- ggplot() + coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) + theme_void(base_size = base_size)
  if (!is.null(bands))
    g <- g + geom_polygon(data = do.call(rbind, lapply(seq_len(nrow(bands)), function(i)
             cbind(sem_polygon(list(x = bands$x[i], y = bands$y[i], w = bands$w[i], h = bands$h[i], shape = "box"), i),
                   fill = bands$fill[i]))),
             aes(x, y, group = id, fill = I(fill)), colour = NA)
  if (!is.null(headers))
    g <- g + geom_text(data = headers, aes(x, y, label = label), size = 3.3, colour = sem_col$mute,
                       fontface = "bold", hjust = 0.5)
  # edges: paths first, loadings beneath
  for (k in c("load", "path")) {
    Ek <- E[E$kind == k, ]
    if (nrow(Ek))
      g <- g + geom_segment(data = Ek, aes(x = x, y = y, xend = xend, yend = yend, colour = I(col),
                                           linewidth = I(lwd)),
                            arrow = arrow(length = unit(arrow_mm, "mm"), type = "closed", angle = 22),
                            lineend = "butt", linejoin = "mitre")
  }
  g <- g + geom_polygon(data = polys, aes(x, y, group = id, fill = I(fill), colour = I(border),
                                           linewidth = I(lwd)))
  g <- g + geom_text(data = nodes, aes(x, y, label = label, size = I(size), fontface = face),
                     colour = sem_col$ink, lineheight = 0.9)
  El <- E[!is.na(E$label) & nzchar(E$label), ]
  if (nrow(El))
    g <- g + geom_label(data = El, aes(lx, ly, label = label, colour = I(lcol), size = I(lsize),
                                       fontface = ifelse(bold, "bold", "plain")),
                        fill = "white", label.size = 0, label.padding = unit(0.11, "lines"),
                        label.r = unit(0.1, "lines"))
  if (!is.null(texts))
    g <- g + geom_text(data = texts, aes(x, y, label = label, size = I(size), colour = I(colour),
                                         hjust = hjust, fontface = face), lineheight = 0.95)
  if (!is.null(legend_at)) {
    lx <- legend_at[1]; ly <- legend_at[2]
    lg <- data.frame(x = lx + c(0, 1.9, 3.8), y = ly, xe = lx + c(0.55, 2.45, 4.35),
                     col = c(sem_col$up, sem_col$down, sem_col$ns),
                     lab = c("raises", "lowers", "not significant"))
    g <- g + geom_segment(data = lg, aes(x = x, y = y, xend = xe, yend = y, colour = I(col)),
                          linewidth = 1.1, arrow = arrow(length = unit(2.2, "mm"), type = "closed", angle = 22)) +
      geom_text(data = lg, aes(x = xe + 0.08, y = y, label = lab), size = 2.9, hjust = 0, colour = sem_col$mute)
  }
  g + labs(title = title, subtitle = subtitle, caption = caption) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.title = element_text(colour = sem_col$ink, size = 14, margin = margin(b = 3)),
          plot.subtitle = element_text(colour = sem_col$mute, size = 10, margin = margin(b = 6)),
          plot.caption = element_text(colour = sem_col$mute, size = 8.5, hjust = 0, lineheight = 1.15,
                                      margin = margin(t = 8)),
          plot.title.position = "plot", plot.caption.position = "plot",
          plot.margin = margin(12, 14, 10, 14), legend.position = "none")
}

# pull standardised structural paths out of a lavaan fit: tidy table
sem_paths <- function(fit) {
  ps <- lavaan::standardizedSolution(fit)
  ps[ps$op == "~", c("lhs", "rhs", "est.std", "pvalue")]
}
sem_loads <- function(fit) {
  ps <- lavaan::standardizedSolution(fit)
  ps[ps$op == "=~", c("lhs", "rhs", "est.std", "pvalue")]
}
fmt_b <- function(b) {
  s <- sub("^(-?)0[.]", "\\1.", sprintf("%.2f", b))    # drop the leading zero
  ifelse(s == "-.00", ".00", s)
}
