#' Creates time series plot of biomass measurements for each species in a simulation
#'
#' @param df summary data of simulation measurements by species
#' @param y variable for plotting. Here should be `BiomassBySpecies`
#' @param species categorical species variable for plotting
#' @param cols a named character vector of species and colours for labeling
#' @param ylab character string to use as y axis label
#' @param title character string to use as plot title
#' @param subtitle character string to use as plot subtitle
#'
speciesBiomassPlot <- function(df, cols, y, species, ylab = "y",
                               plotTitle = NULL, plotSubtitle = NULL) {
  gg <- ggplot(data = df, aes_string(x = "year", y = y, fill = species, group = species)) +
    geom_area(position = "stack") +
    scale_fill_manual(values = cols) +
    # scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    scale_y_continuous() +
    labs(x = "Year", y = ylab, fill ="Species", title = plotTitle, subtitle = plotSubtitle) +
    theme(legend.text = element_text(size = 12), legend.title = element_blank()) +
    theme_bw(base_size = 16)
  return(gg)
}

#' Creates time series plot of relative biomass measurements for each species
#' in a simulation
#'
#' @param df summary data of simulation measurements by species
#' @param y variable for plotting. Here should be `RelativeBiomassBySpecies`
#' @param species categorical species variable for plotting
#' @param cols a named character vector of species and colours for labeling
#' @param ylab character string to use as y axis label
#' @param title character string to use as plot title
#' @param subtitle character string to use as plot subtitle
#'
speciesRelativeBiomassPlot <- function(df, cols, y, species, ylab = "y",
                                       plotTitle = NULL, plotSubtitle = NULL) {
  gg <- ggplot(data = df, aes_string(x = "year", y = y, fill = species, group = species)) +
    geom_area(position = "stack") +
    scale_fill_manual(values = cols) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    labs(x = "Year", y = ylab, fill ="Species", title = plotTitle, subtitle = plotSubtitle) +
    theme(legend.text = element_text(size = 12), legend.title = element_blank()) +
    theme_bw(base_size = 16)
  return(gg)
}

#' Creates time series plot of the proportion of leading pixels by each species
#' in a simulation
#'
#' @param df summary data of leading pixel counts by species. Here should
#'    be `LeadingPixelsSummary`, data summarized to account for empty pixels
#' @param cols a named character vector of species and colours for labeling
#' @param title character string to use as plot title
#' @param subtitle character string to use as plot subtitle
#'
speciesLeadingPlot <- function(df, cols, plotTitle = NULL, plotSubtitle = NULL) {
  gg <- ggplot(data = df, aes_string(x = "year", y = "counts", fill = "leadingType")) +
    geom_area(position = "fill") +
    scale_fill_manual(values = cols, , breaks = ~ .x[.x != "1. Empty"], na.value = "#00000000") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey", size = 1) +
    labs(x = "Year", y = "Proportion", fill = "Species", title = plotTitle, subtitle = plotSubtitle) +
    theme(legend.text = element_text(size = 12), legend.title = element_blank()) +
    theme_bw(base_size = 16)
  return(gg)
}

#' Creates time series line plot of oldest cohort age or total aNPP by
#' each species in a simulation
#'
#' @param df summary data of simulation measurements (biomass, age, etc) by species
#' @param y variable for plotting. Either Age or aNPP
#' @param species categorical species variable for plotting
#' @param cols a named character vector of species and colours for labeling
#' @param ylab character string to use as y axis label
#' @param title character string to use as plot title
#' @param subtitle character string to use as plot subtitle
#'
speciesAgeANPPPlot <- function(df, y, species, cols, ylab = "y",
                               plotTitle = NULL, plotSubtitle = NULL) {
  gg <- ggplot(data = df,
               aes_string(x = "year", y = y, colour = species, group = species)) +
    geom_line(size = 1) +
    scale_colour_manual(values = cols) +
    labs(x = "Year", y = ylab, title = plotTitle, , subtitle = plotSubtitle, colour = "Species") +
    theme(legend.text = element_text(size = 12), legend.title = element_blank()) +
    theme_bw(base_size = 16)
  return(gg)
}



#' Create a map of an attribute (e.g. biomass, mortality,
#' reproduction) for a given year in a simulation
#'
#' @param x a single layer `SpatRaster`.
#' @param title character string to use as plot title
#' @param subtitle character string to use as plot subtitle
#'
gg_vegAttrMap <- function(x, title, subtitle = NULL) {
  if (terra::is.factor(x)) {
    gg <- ggplot() +
      tidyterra::geom_spatraster(data = x) +
      tidyterra::scale_fill_coltab(data = x, na.value = "transparent")
  } else {
    ## need to convert integer rasters to float to get colours (won't use coltab b/c continuous)
    gg <- ggplot() +
      tidyterra::geom_spatraster(data = x * 10 / 10) +
      viridis::scale_fill_viridis(na.value = "transparent")
  }

  gg <- gg + ggtitle(title) + labs(subtitle = subtitle) + theme_bw()

  return(gg)
}

#' Creates time series plots of multiple measurements (e.g. biomass, age,
#' and aNPP) for a simulation
#'
#' @param df summary data of simulation measurements (biomass, age, etc) by species
#' @param varLabels a named character vector for labeling plot panels
#' @param unitLabels a named character vector for labeling the units of plot panels
#' @param title character string to use as plot title
#' @param subtitle character string to use as plot subtitle
#'
landscapeAttributesPlot <- function(df, varLabels, plotTitle = NULL, plotSubtitle = NULL) {
  gg <- ggplot(data = df, aes_string(x = "year", y = "value", colour = "variable")) +
    geom_line(size = 1) +
    scale_colour_brewer(type = "qual", palette = "Dark2") +
    facet_wrap(~ variable, scales = "free_y",
               labeller = labeller(variable = varLabels)) +
    theme_bw(base_size = 16) +
    theme(legend.position = "none") +
    labs(x = "Year", y = "Value", colour = "", title = plotTitle, subtitle = plotSubtitle)
  return(gg)
}
