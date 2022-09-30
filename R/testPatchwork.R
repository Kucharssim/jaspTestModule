testPatchwork <- function(jaspResults, dataset, options) {
  .patchworkFromData  (jaspResults, dataset, options)
  .patchworkSimulation(jaspResults,          options)
}

.patchworkFromData <- function(jaspResults, dataset, options) {
  if(length(options[["variables"]]) <= 1 || options[["variables"]] == "") return()
  if(!is.null(jaspResults[["patchworkPlot"]])) return()

  if(is.null(dataset)) {
    dataset <- jaspBase::.readDataSetToEnd(columns.as.numeric = options[["variables"]])
  }
  cn <- colnames(dataset)

  jaspResults[["patchworkPlot"]] <- jaspBase::createJaspPlot(
    title  = gettext("Patchwork"),
    width  = 250 * length(options[["variables"]]) + 20,
    height = 250 * length(options[["variables"]]) + 20,
    dependencies = "variables"
  )

  direction <- "right"
  pp <- ggplot2::ggplot() + ggplot2::theme_void()
  for(col in cn) {
    p <- jaspGraphs::jaspHistogram(x = dataset[[col]]) +
      ggplot2::ggtitle(col) +
      ggplot2::scale_x_continuous() +
      ggplot2::xlab(col)

    pp <- switch(
      direction,
      right = patchwork::wrap_plots(pp, p),
      down  = patchwork::wrap_plots(pp, p, nrow = 2),
      left  = patchwork::wrap_plots(p, pp),
      up    = patchwork::wrap_plots(p, pp, nrow = 2)
    )

    direction <- .updateDirection(direction)
  }

  pp <- pp + patchwork::plot_annotation(
    title = gettextf("Pretty plot"),
    subtitle = paste(cn, collapse = ", "),
    caption = gettextf("Notice how e.g., '%s' is twice as large as '%s'", cn[2], cn[1]),
    theme = ggplot2::theme(title         = ggplot2::element_text(size = 24),
                           plot.subtitle = ggplot2::element_text(size = 21),
                           plot.caption  = ggplot2::element_text(size = 18))
  )
  jaspResults[["patchworkPlot"]]$plotObject <- pp
}

.patchworkSimulation <- function(jaspResults, options) {
  if(!options[["runSimulation"]]) return()

  plot <- jaspBase::createJaspPlot(title = "Simulated plot")
  jaspResults[["simulatedPlot"]] <- plot

  if(options[["layout"]] == "grid") {
    nPlots <- options[["gridRows"]] * options[["gridColumns"]]
    plot$width  <- 20 + 150 * options[["gridColumns"]]
    plot$height <- 20 + 150 * options[["gridRows"]]
  } else {
    nPlots <- options[["spiralSize"]]
    layout <- .gridExtraSpiralLayout(options)
    plot$width  <- 20 + 150 * ncol(layout)
    plot$height <- 20 + 150 * nrow(layout)
  }

  plots <- replicate(n = nPlots, expr = .makeRandomPlot(options), simplify = FALSE)

  plot$plotObject <- switch(
    options[["engine"]],
    gridExtra = .gridExtraPlot(jaspResults, plots, options),
    patchwork = .patchworkPlot(jaspResults, plots, options)
  )
}

.makeRandomPlot <- function(options) {
  x <- rnorm(n = options[["sampleSize"]], mean = rnorm(1), sd = rexp(1, 0.2))
  y <- rnorm(n = options[["sampleSize"]], mean = rnorm(1), sd = rexp(1, 0.2))
  p <- ggplot2::qplot(x, y) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}

.gridExtraPlot <- function(jaspResults, plots, options) {
  if(options[["layout"]] == "grid") {
    layout <- matrix(seq_along(plots), nrow = options[["gridRows"]], ncol = options[["gridColumns"]])
    p <- jaspGraphs:::jaspGraphsPlot$new(plots, layout = layout)
  } else {
    layout <- .gridExtraSpiralLayout(options)
    p <- jaspGraphs:::jaspGraphsPlot$new(plots, layout = layout)
  }

  return(p)
}

.gridExtraSpiralLayout <- function(options) {
  layout <- NULL
  direction <- "up"
  for(i in seq_len(options[["spiralSize"]])) {
    rr <- if(is.null(layout)) 1 else nrow(layout)
    cc <- if(is.null(layout)) 1 else ncol(layout)
    ll <- matrix(i, nrow = rr, ncol = cc)

    layout <- switch(
      direction,
      right  = cbind(layout, ll),
      down   = rbind(layout, ll),
      left   = cbind(ll, layout),
      up     = rbind(ll, layout)
    )

    direction <- .updateDirection(direction)
  }

  return(layout)
}

.patchworkPlot <- function(jaspResults, plots, options) {
  if(options[["layout"]] == "grid") {
    p <- patchwork::wrap_plots(
      plots,
      ncol  = options[["gridRows"]],
      nrow  = options[["gridColumns"]],
      byrow = FALSE
    )
  } else {
    if(options[["spiralNested"]]) {
      p <- .patchworkNestedSpiral(plots)
    } else {
      layout <- .patchworkSpiralLayout(options)
      p <- patchwork::wrap_plots(plots, design = layout)
    }
  }

  return(p)
}

.patchworkSpiralLayout <- function(options) {
  layout <- .gridExtraSpiralLayout(options)
  if(max(layout) > length(LETTERS)) stop("Cannot draw plot bigger than that.")

  design <- matrix(NA, ncol = ncol(layout), nrow = nrow(layout))
  for(rr in seq_len(nrow(layout))) {
    for(cc in seq_len(ncol(layout)))
      design[rr,cc] <- LETTERS[layout[rr, cc]]
  }

  design <- apply(design, 1, paste, collapse = "")
  design <- paste(design, collapse = "\n")

  return(design)
}

.patchworkNestedSpiral <- function(plots) {
  pp <- plots[[1]]
  plots <- plots[-1]
  direction <- "right"
  for(p in plots) {
    pp <- switch(
      direction,
      right = patchwork::wrap_plots(pp, p),
      down  = patchwork::wrap_plots(pp, p, nrow = 2),
      left  = patchwork::wrap_plots(p, pp),
      up    = patchwork::wrap_plots(p, pp, nrow = 2)
    )

    direction <- .updateDirection(direction)
  }

  return(pp)
}

.updateDirection <- function(direction) {
  switch(
    direction,
    right = "down",
    down  = "left",
    left  = "up",
    up    = "right"
  )
}
