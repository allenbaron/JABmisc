filter_list <- function(list, name, value) {

    if(all(missing(name), missing(value))) {
        stop("'name' or 'value' must be specified")
    }

    if(isFALSE(missing(name))) {
        return(list[names(list) == name])
    }

    if(isFALSE(missing(value))) {
        return(list[list == value])
    }
}
