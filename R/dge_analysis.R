ds2_pca <- function(.object, .metadata_df, .by = "sample") {
    # Returns useful data from principal components analysis on
    # DESeq2 transform (e.g. vst())
    #
    # .object = DESeqTransform object, see DESeq2::plotPCA
    # .metadata_df = sample information to be added
    # .by = either "sample" or a join by statement c(<.metadata variable
    #   to join by> = "sample"), see dplyr::full_join for syntax

    pca_res <- assay(.object) %>%
        t() %>%
        prcomp()

    pc_md <- tibble(
        PC = as.integer(
            str_extract(
                dimnames(pca_res$x)[[2]],
                "[0-9]+"
            )
        ),
        sdev = pca_res$sdev,
        percentVar = sdev^2/sum(sdev^2)
    )

    pc_df <- full_join(
        x = .metadata_df,
        y = as_tibble(pca_res$x, rownames = "sample"),
        by = .by
    )

    list(prcomp_res = pca_res, metadata = pc_md, sample_pc = pc_df)
}


# Comparison of DE-gene LFCs

plot_LFC_scatter <- function(df, entrezgene_df, compare = c("contrast", "group"),
                               contrast_var, .contrast, group_var, .group,
                               .colors, title, breaks_every, x_breaks_every,
                               y_breaks_every) {

    # Validate args required every execution
    stopifnot(
        is.data.frame(df),
        is.data.frame(entrezgene_df),
        "entrezgene_accession" %in% names(entrezgene_df)
    )
    compare <- match.arg(compare, choices = c("contrast", "group"))

    contrast_sym <- ensym(contrast_var)
    contrast_str <- rlang::as_string(contrast_sym)
    if (isFALSE(contrast_str %in% names(df))) {
        stop("contrast_var not found in data.frame")
    }

    .contrast_lgl <- .contrast %in% unique(df[[contrast_str]])
    if (isFALSE(all(.contrast_lgl))) {
        stop(
            paste0(
                ".contrast value(s) not found in data.frame$",
                contrast_str, ": ",
                paste0(.contrast[!.contrast_lgl], collapse = ", ")
            )
        )
    }

    # to ensure ensembl_gene_id remains unique identifier (not multiples
    # because entrezgene info added)
    entrezgene_df <- entrezgene_df %>%
        group_by(ensembl_gene_id) %>%
        mutate(
            entrezgene_accession = if_else(
                is.na(entrezgene_accession),
                NA_character_,
                paste0(entrezgene_accession, collapse = ", ")
            )
        ) %>%
        unique()

    # Skip group subset if missing or error for compare = "group"
    if (missing(group_var) || missing(.group)) {

        if (compare == "group") {
            stop("Both group_var and .group must be specified")
        } else {
            if (isFALSE(missing(group_var) && missing(.group))) {
                stop(
                    strwrap(
                        "One of group_var and .group is missing. Did you
                        intend to compare contrasts within a specific group?",
                        prefix = "\n"
                    )
                )
            }

            stopifnot(length(.contrast) == 2)

            df_filter <- df %>%
                filter(!!contrast_sym %in% .contrast)

            compare_sym <- contrast_sym
            compare_str <- contrast_str
            .compare <- .contrast
        }

    } else {

        # Validate group args
        group_sym <- ensym(group_var)
        group_str <- rlang::as_string(group_sym)
        if (isFALSE(group_str %in% names(df))) {
            stop("group_var not found in data.frame")
        }

        .group_lgl <- .group %in% unique(df[[group_str]])
        if (isFALSE(all(.group_lgl))) {
            stop(
                paste0(
                    ".group value(s) not found in data.frame$",
                    group_str, ": ",
                    paste0(.group[!.group_lgl], collapse = ", ")
                )
            )
        }

        # Validate conditions & reduce code by setting new vars
        if (compare == "contrast") {
            stopifnot(
                length(.contrast) == 2,
                length(.group) == 1
            )

            compare_sym <- contrast_sym
            compare_str <- contrast_str
            .compare <- .contrast
        }

        if (compare == "group") {
            stopifnot(
                length(.contrast) == 1,
                length(.group) == 2
            )

            compare_sym <- group_sym
            compare_str <- group_str
            .compare <- .group
        }

        df_filter <- df %>%
            filter(!!contrast_sym %in% .contrast, !!group_sym %in% .group)

    }

    lfc_vars <- paste0(.compare, ".lfc")
    padj_vars <- paste0(.compare, ".padj")

    # Identify gene significance and prepare df for plotting
    df_wide <- df_filter %>%
        dplyr::select(ensembl_gene_id, !!compare_sym, lfc, padj) %>%
        group_by(ensembl_gene_id) %>%
        mutate(sig_any = any(padj < 0.1)) %>%
        ungroup() %>%
        filter(sig_any) %>%
        pivot_wider(
            id_cols = ensembl_gene_id,
            names_from = compare_str,
            values_from = c(lfc, padj),
            names_sep = "."
        ) %>%
        rename_at(
            .vars = vars(dplyr::matches("lfc|padj")),
            .funs = ~str_replace(., "(lfc|padj)\\.(.*)", "\\2\\.\\1")
        ) %>%
        mutate(
            significant = case_when(
                UQ(sym(padj_vars[[1]])) < 0.1 &
                    UQ(sym(padj_vars[[2]])) < 0.1 ~ "Both",
                UQ(sym(padj_vars[[1]])) < 0.1 ~ .compare[[1]],
                UQ(sym(padj_vars[[2]])) < 0.1 ~ .compare[[2]]
            )
        ) %>%
        left_join(
            y = entrezgene_df,
            by = "ensembl_gene_id"
        )

    # create ggplot w/hovertext & lines for reference
    g <- suppress_aes_text_warning(
        ggplot(df_wide) +
            # Add grey boxes to plot at regions of NO significance
            #   Bug in gggplotly preventing boxes with Inf from appearing = not
            #   currently implemented (https://github.com/ropensci/plotly/issues/1559)
            #geom_rect(
            #    data = tibble(
            #        xmin = c(-Inf, -0.585), xmax = c(Inf, 0.585),
            #        ymin = c(-0.585, -Inf), ymax = c(0.585, Inf)
            #    ),
            #    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            #    color = "grey", alpha = 0.2
            #) +
        # Add 0 lines (because can't use boxes)
        geom_hline(yintercept = 0, color = "grey", size = 1) +
            geom_vline(xintercept = 0, color = "grey", size = 1) +
            # Add LFC equal line
            geom_abline(slope = 1, intercept = 0, size = 1) +
            geom_point(
                aes(
                    x = !!sym(lfc_vars[[1]]),
                    y = !!sym(lfc_vars[[2]]),
                    color = significant,
                    text = paste0(
                        "<b>", entrezgene_accession, "</b><br>",
                        lfc_vars[[1]], ": ",
                        round(eval(sym(lfc_vars[[1]])), 2), "<br>",
                        lfc_vars[[2]], ": ",
                        round(eval(sym(lfc_vars[[2]])), 2), "<br>",
                        padj_vars[[1]], ": ",
                        round(eval(sym(padj_vars[[1]])), 3), "<br>",
                        padj_vars[[2]], ": ",
                        round(eval(sym(padj_vars[[2]])), 3), "<br>",
                        ensembl_gene_id
                    )
                )
            )
    )


    if (!missing(.colors)) {
        g <- g + scale_color_manual(values = .colors)
    }


    if (!missing(title)) {
        g <- g + ggtitle(title)
    }

    #   centered_breaks() NOT WORKING!! Fix.
    #    if (!missing(breaks_every)) {
    #        if (!missing(x_breaks_every) || !missing(y_breaks_every)) {
    #            warning(
    #                strwrap("breaks_every and x_breaks_every|y_breaks_every
    #                both specified, using breaks_every", prefix = "\n"),
    #                strwrap("If separate breaks are desired for x & y, use ONLY
    #                x_breaks_every and/or y_breaks_every", prefix = "\n")
    #            )
    #        }
    #
    #        g <- g +
    #            scale_x_continuous(
    #                breaks = ~centered_breaks(., by = breaks_every)
    #            ) +
    #            scale_x_continuous(
    #                breaks = ~centered_breaks(., by = breaks_every)
    #            )
    #
    #    } else {
    #
    #        if (!missing(x_breaks_every)) {
    #            g <- g +
    #                scale_x_continuous(
    #                    breaks = ~centered_breaks(., by = x_breaks_every)
    #                )
    #        }
    #
    #        if (!missing(y_breaks_every)) {
    #            g <- g +
    #                scale_x_continuous(
    #                    breaks = ~centered_breaks(., by = y_breaks_every)
    #                )
    #        }
    #
    #    }

    ggplotly(g, tooltip = "text")

}


# Gene Set

gsc_count <- function(df_info, type = "kegg_pathway") {
    # Count DE-genes by gene set
    #
    # type = one of "kegg_pathway", "kegg_generic" (consolidated
    #   version added as extra column to KEGG pathway df), or
    #   "go_term_name"

    stopifnot(
        is.character(type),
        length(type) == 1,
        type %in% c("kegg_pathway", "kegg_generic", "GO")
    )

    if (type == "kegg_pathway" & any(names(df_info) == "kegg_generic")) {
        groups <- syms("kegg_generic")
    } else if (type == "GO") {
        var <- sym("go_term_name")
        groups <- syms(c("go_term_accession", "go_domain"))
    } else {
        var <- sym(type)
        groups <- NULL
    }

    df_info %>%
        group_by(!!var) %>%
        transmute(n_genes = n_distinct(ensembl_gene_id), !!!groups) %>%
        unique() %>%
        arrange(dplyr::desc(n_genes)) %>%
        ungroup()
}

gsc_plot <- function(gsc_count, type = "kegg_pathway", ..., show = 150, ggplot_labs = list()) {
    # Plot from gsc_count object
    #
    # For gene ontology use type = "GO"

    stopifnot(
        is.character(type),
        length(type) == 1,
        type %in% c("kegg_pathway", "kegg_generic", "GO")
    )

    filter_args <- enquos(...)

    if (type == "GO") {
        go_domains <- c("biological_process", "molecular_function", "cellular_component")
        gsc_count <- filter(gsc_count, !go_term_name %in% go_domains)
        str_len <- str_length(gsc_count$go_term_name)
        max_str_len <- max(str_len, na.rm = TRUE)
        if(isTRUE(max_str_len > 80)) {
            gsc_count <- gsc_count %>%
                mutate(go_term_name = if_else(
                    str_len > 80,
                    paste(go_term_accession, str_trunc(go_term_name, 60), sep = "-"),
                    go_term_name
                )
                )
        }

        map(
            go_domains,
            function(domain) {

                g_tmp <- gsc_count %>%
                    arrange(dplyr::desc(n_genes)) %>%
                    filter(!is.na(go_term_name), go_domain == !!quo(domain), !!!filter_args) %>%
                    mutate(go_term_name = fct_reorder(go_term_name, n_genes, .desc = FALSE)) %>%
                    filter(row_number(dplyr::desc(n_genes)) <= show) %>%
                    ggplot() +
                    geom_col(aes(x = go_term_name, y = n_genes)) +
                    theme(axis.text.y = element_text(size = 8)) +
                    coord_flip() +
                    scale_y_continuous(sec.axis = dup_axis())
                if ("subtitle" %in% names(ggplot_labs)) {
                    g_tmp <- g_tmp +
                        labs(title = type, caption = domain, !!!ggplot_labs)
                } else {
                    g_tmp <- g_tmp +
                        labs(title = type, subtitle = domain, !!!ggplot_labs)
                }

                print(g_tmp)
            }
        )
    } else {
        var <- sym(type)

        gsc_count %>%
            arrange(dplyr::desc(n_genes)) %>%
            filter(!is.na(!!var), !!!filter_args) %>%
            mutate(!!var := fct_reorder(!!var, n_genes, .desc = FALSE)) %>%
            filter(row_number(dplyr::desc(n_genes)) <= show) %>%
            ggplot() +
            geom_col(aes(x = !!var, y = n_genes)) +
            theme(axis.text.y = element_text(size = 8)) +
            coord_flip() +
            scale_y_continuous(sec.axis = dup_axis()) +
            labs(title = type, !!!ggplot_labs)
    }
}
