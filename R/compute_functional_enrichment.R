PIP_BIN_BREAKS <- c(-Inf, 0.01, 0.1, 0.5, 0.9, 1.0)

#' Compute functional enrichment of variants by PIP bins
#'
#' Calculates enrichment of functional consequences in high vs low posterior
#' inclusion probability (PIP) bins using risk ratio estimates.
#'
#' @param data A data frame containing variant data with columns for max_pip,
#'   consequence (or column specified by consequence_col), and optionally max_maf
#' @param annot_levels Character vector of ordered consequence annotation levels
#' @param pip_bin_breaks Numeric vector of PIP bin breakpoints (default: c(-Inf, 0.01, 0.1, 0.5, 0.9, 1.0))
#' @param consequence_col Character string specifying the column name containing consequence annotations (default: "consequence")
#' @param maf_match Logical indicating whether to match variants by minor allele frequency (default: FALSE)
#' @param seed Random seed for MAF matching (default: 12345)
#'
#' @return A data frame with columns:
#'   \item{consequence}{Functional consequence category}
#'   \item{enrichment}{Risk ratio estimate}
#'   \item{lower}{Lower confidence interval}
#'   \item{upper}{Upper confidence interval}
#'   \item{n_bottom}{Count in bottom PIP bin}
#'   \item{total_bottom}{Total in bottom PIP bin}
#'   \item{n_top}{Count in top PIP bin}
#'   \item{total_top}{Total in top PIP bin}
#'
#' @importFrom dplyr mutate filter count group_by summarize ungroup bind_rows sample_n rename group_split
#' @importFrom stringr str_replace str_c
#' @importFrom tidyr drop_na pivot_wider
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @importFrom epitools riskratio
#' @importFrom rlang sym !!
#'
#' @export
compute_functional_enrichment <-
  function(data,
           annot_levels,
           pip_bin_breaks = PIP_BIN_BREAKS,
           consequence_col = "consequence",
           maf_match = FALSE,
           seed = 12345) {
    pip_levels <- levels(cut(0, pip_bin_breaks))
    pip_bottom_level <- pip_levels[1]
    pip_bottom_level2 <- stringr::str_replace(pip_bottom_level, "^\\(-Inf", "[0")
    pip_bottom_mapping <- setNames(pip_bottom_level2, pip_bottom_level)
    pip_top_level <- pip_levels[length(pip_levels)]

    data <-
      dplyr::mutate(data, max_pip_bin = cut(max_pip, pip_bin_breaks)) %>%
      dplyr::filter(max_pip_bin %in% c(pip_bottom_level, pip_top_level))

    if (maf_match) {
      set.seed(seed)
      data <- dplyr::mutate(data, max_maf_bin = Hmisc::cut2(max_maf, g = 5))

      data.k <-
        dplyr::group_by(data, max_pip_bin, max_maf_bin) %>%
        dplyr::count() %>%
        dplyr::group_by(max_maf_bin) %>%
        dplyr::filter(length(max_pip_bin) == 2) %>%
        dplyr::summarize(k = max(n) %/% min(n)) %>%
        dplyr::ungroup()
      k <- min(data.k$k)

      data <- dplyr::group_split(data, max_maf_bin) %>%
        purrr::map_dfr(~ {
          top <- dplyr::filter(.x, max_pip_bin == !!pip_top_level)
          bottom <- dplyr::filter(.x, max_pip_bin == !!pip_bottom_level)
          if (nrow(bottom) < k * nrow(top)) {
            stop("Error")
          }
          return(dplyr::bind_rows(top, dplyr::sample_n(bottom, k * nrow(top))))
        })
    }

    data <-
      dplyr::count(data, max_pip_bin, !!rlang::sym(consequence_col)) %>%
      dplyr::group_by(max_pip_bin) %>%
      dplyr::mutate(total = sum(n), frac = n / total) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        max_pip_bin = dplyr::recode_factor(max_pip_bin, !!!pip_bottom_mapping), !!consequence_col := ordered(!!rlang::sym(consequence_col), levels = .env$annot_levels)
      ) %>%
      tidyr::drop_na(!!rlang::sym(consequence_col)) %>%
      dplyr::rename(consequence = !!consequence_col) %>%
      tidyr::pivot_wider(
        id_cols = "consequence",
        names_from = "max_pip_bin",
        values_from = c("n", "total")
      ) %>%
      dplyr::group_split(consequence) %>%
      purrr::map_dfr(function(data) {
        total_bottom <- stringr::str_c("total_", pip_bottom_level2)
        n_bottom <- stringr::str_c("n_", pip_bottom_level2)
        total_top <- stringr::str_c("total_", pip_top_level)
        n_top <- stringr::str_c("n_", pip_top_level)
        null_result <- tibble::tibble(
          consequence = data$consequence,
          enrichment = NA,
          lower = NA,
          upper = NA
        )

        if (!all(c(total_bottom, n_bottom, total_top, n_top) %in% colnames(data))) {
          return(null_result)
        }

        m <- with(data, matrix(
          c(
            get(total_bottom) - get(n_bottom),
            get(n_bottom),
            get(total_top) - get(n_top),
            get(n_top)
          ),
          nrow = 2,
          byrow = T
        ))

        if (!all(is.finite(m)) || !all(m > 0)) {
          return(null_result)
        }
        measure <- epitools::riskratio(m, method = "boot")$measure
        tibble::tibble(
          consequence = data$consequence,
          enrichment = measure[2, "estimate"],
          lower = measure[2, "lower"],
          upper = measure[2, "upper"],
          n_bottom = data[[n_bottom]],
          total_bottom = data[[total_bottom]],
          n_top = data[[n_top]],
          total_top = data[[total_top]]
        )
      })
    return(data)
  }
