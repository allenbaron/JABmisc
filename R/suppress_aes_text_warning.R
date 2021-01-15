#' Suppress ggplot2 warning when using text aesthetic for plotly
#'
#' Suppresses unknown aesthetic warning when providing a `text` aesthetic.
#' Intended for cosmetic purposes when printing as a ggplot object.
#' Unnecessary, if converting to plotly for use as hovertext with `ggplotly()`.
#'
#' @examples
#' g <- ggplot(iris) +
#'     geom_point(aes(x = Sepal.Length, y = Sepal.Width, text = Species))
#' g
#'
#' suppress_aes_text_warning(g)
#'
#' @export
suppress_aes_text_warning <- function(ggplot2_expr) {
    withCallingHandlers(
        {ggplot2_expr},
        warning = function(w) {
            if (conditionMessage(w) == "Ignoring unknown aesthetics: text") {
                invokeRestart("muffleWarning")
            }
        }
    )
}
