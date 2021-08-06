speciesBiomassPlot <- function(df, cols, y, ylab = "y", plotTitle = NULL) {
  gg <- ggplot(data = df, aes_string(x = "year", y = y, fill = "species", group = "species")) +
    geom_area(position = "stack") +
    scale_fill_manual(values = cols) +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    labs(x = "Year", y = ylab, title = plotTitle) +
    theme(legend.text = element_text(size = 12), legend.title = element_blank()) +
    theme_bw(base_size = 16)
  return(gg)
}

speciesLeadingPlot <- function(df, cols, maxNpixels) {
  gg <- ggplot(data = df, aes_string(x = "year", y = "counts", fill = "leadingType")) +
    geom_hline(yintercept = maxNpixels, linetype = "dashed", color = "darkgrey", size = 1) +
    geom_area() +
    scale_fill_manual(values = cols) +
    labs(x = "Year", y = "Count", title = "Number of pixels by leading type") +
    theme(legend.text = element_text(size = 12), legend.title = element_blank()) +
    theme_bw(base_size = 16)
  return(gg)
}

speciesAgeANPPPlot <- function(df, y, cols, ylab = "y", plotTitle = NULL) {
  gg <- ggplot(data = df,
               aes_string(x = "year", y = y, colour = "species", group = "species")) +
    geom_line(size = 1) +
    scale_colour_manual(values = cols) +
    labs(x = "Year", y = ylab, title = plotTitle) +
    theme(legend.text = element_text(size = 12), legend.title = element_blank()) +
    theme_bw(base_size = 16)
  return(gg)
}

landscapeAttributesPlot <- function(df, varLabels) {
  gg <- ggplot(data = df, aes_string(x = "year", y = "value", colour = "variable")) +
    geom_line(size = 1) +
    scale_colour_brewer(labels = varLabels, type = "qual", palette = "Dark2") +
    theme_bw() +
    theme(legend.text = element_text(size = 12), legend.title = element_blank(),
          legend.position = "bottom") +
    theme_bw(base_size = 16) +
    facet_wrap(~ variable, scales = "free_y",
               labeller = labeller(variable = varLabels)) +
    labs(x = "Year", y = "Value", colour = "")
  return(gg)
}
