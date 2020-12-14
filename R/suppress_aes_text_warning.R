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
