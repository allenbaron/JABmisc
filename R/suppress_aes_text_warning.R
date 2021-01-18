#' Suppress ggplot2 warning when using text aesthetic for plotly
#'
#' Suppresses unknown aesthetic warning when providing a `text` aesthetic.
#' Intended for cosmetic purposes when printing as a ggplot object.
#' Unnecessary, if converting to plotly for use as hovertext with `ggplotly()`.
#'
#' @examples
#' requireNamespace("ggplot2", quietly = TRUE)
#'
#' g <- ggplot2::ggplot(iris) +
#'     ggplot2::geom_point(
#'         ggplot2::aes(x = Sepal.Length, y = Sepal.Width, text = Species)
#'     )
#' g
#'
#' suppress_aes_text_warning(g)
#'
#' @export
suppress_aes_text_warning <- function(ggplot2_expr) {

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        err_msg <- paste(
            strwrap("This function is designed to work with the \"ggplot2\" package. Please install it before use."),
            collapse = "\n"
        )

        stop(err_msg, call. = FALSE)
    }

    withCallingHandlers(
        {ggplot2_expr},
        warning = function(w) {
            if (conditionMessage(w) == "Ignoring unknown aesthetics: text") {
                invokeRestart("muffleWarning")
            }
        }
    )
}
