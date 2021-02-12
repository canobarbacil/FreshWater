#' @title mycols
#' @description  Extract the hex codes from a list of colours.
#' @author Carlos Cano-Barbacil
#' @examples
#' mycols("green")
#' mycols("green", "blue")
#' normalHist(u,dens=TRUE)
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#' geom_point(color = mycols("red"),
#'            size = 1.5, alpha =1)+
#'   theme_test()
#'
mycols <- function(...) {
  mycolors <- c(
    `green`= "#00B358",
    `light green`= "#62D99C",
    `dark green` = "#007439",
    `blue` = "#0969A2",
    `light blue` = "#64A8D1",
    `dark blue` = "#03436A",
    `yellowgreen` = "#74E600",
    `light yellowgreen` = "#B0F26D",
    `dark yellowgreen` = "#4B9500",
    `red` = "#FF3D00",
    `light red` = "#FF9473",
    `dark red` = "#A62800",
    `orange` = "#FF8C00",
    `ligth orange` = "#FFC073",
    `dark orange` = "#A65B00",
    `gold` = "#CFA127",
    `dark brown`= "#454416",
    `black` = "#000000",
    `light grey` = "#E6E6E6")
  cols <- c(...)

  if (is.null(cols))
    return (mycolors)

  mycolors[cols]
}

#' @title freshwater_pal
#' @description This function allows to get a pallete by name from the list ("Tajo" by default).
#' It also has a boolean condition determining whether to reverse the color order or not,
#' and additional arguments to pass on to colorRampPallete()ular
#' @param palette c("Tajo", "Jarama", "Manzanares", "Culebro", "Aphanius", "Lepomis", "Cyprinus")
#' @param reverse TRUE/FALSE
#' @author Carlos Cano-Barbacil
#' @examples
#' freshwater_pal("Tajo")
#' colors <- freshwater_pal("Tajo")(12)
#' colors
#' show_col(colors)
#'
freshwater_pal <- function(palette = "main", reverse = FALSE, ...) {

  mypalettes <- list(
    `Tajo` = mycols("dark blue","green","light yellowgreen"),
    `Jarama` = mycols("dark brown", "green", "light yellowgreen"),
    `Manzanares` = mycols("blue","green","yellowgreen","orange","red"),
    `Culebro` = mycols("dark brown","orange", "light yellowgreen"),
    `Aphanius` = mycols("light blue", "gold", "dark brown", "black"),
    `Lepomis` = mycols("light blue","green","orange","black"),
    `Cyprinus` = mycols("black","dark brown","gold","light grey"))
  pal <- mypalettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' @title scale_color_freshwater
#' @description Colour your ggplot2 graphs with FreshWater palettes.
#' @param palette c("Tajo", "Jarama", "Manzanares", "Culebro", "Aphanius", "Lepomis", "Cyprinus")
#' @param discrete TRUE/FALSE
#' @param reverse TRUE/FALSE
#' @author Carlos Cano-Barbacil
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#'   geom_point(size = 2, alpha = 1) +
#'   scale_color_freshwater(discrete = FALSE, palette = "Jarama")+
#'   ggtitle("Jarama")+theme_test()+ theme(legend.position = "none")
#'
scale_color_freshwater <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- freshwater_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("freshwater_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @title scale_fill_freshwater
#' @description Colour your ggplot2 graphs with FreshWater palettes.
#' @param palette c("Tajo", "Jarama", "Manzanares", "Culebro", "Aphanius", "Lepomis", "Cyprinus")
#' @param discrete TRUE/FALSE
#' @param reverse TRUE/FALSE
#' @author Carlos Cano-Barbacil
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#'   geom_point(shape = 21, size = 2, alpha = 1) +
#'   scale_fill_freshwater(discrete = FALSE, palette = "Jarama")+
#'   ggtitle("Jarama")+theme_test()+ theme(legend.position = "none")
#'
scale_fill_freshwater <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- freshwater_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("freshwater_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
