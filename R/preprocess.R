#' Title
#'
#' @param data
#' @param lead_variant
#' @param variant_col
#' @param beta_col
#' @param se_col
#' @param pip_col
#' @param cs_id_col
#' @param r2_col
#'
#' @return
#' @export
preprocess <-
  function(data,
           lead_variant = NULL,
           chromosome_col = "chromosome",
           position_col = "position",
           variant_col = "variant",
           beta_col = "beta",
           se_col = "se",
           pvalue_col = "pvalue",
           pip_col = "pip",
           cs_id_col = "cs_id",
           r2_col = "r2") {
    required_cols = c(chromosome_col, position_col, variant_col, beta_col, se_col, pip_col, cs_id_col)
    if (!all(required_cols %in% colnames(data))) {
      stop(paste("Required columns are missing:", paste(
        setdiff(required_cols, colnames(data)), collapse = ","
      )))
    }

    data = dplyr::rename(
      data,
      chromosome = chromosome_col,
      position = position_col,
      variant = variant_col,
      beta = beta_col,
      se = se_col,
      pip = pip_col,
      cs_id = cs_id_col,
      or_missing(pvalue_col %in% colnames(data), c(pvalue = pvalue_col)),
      or_missing(r2_col %in% colnames(data), c(r2 = r2_col))
    ) %>%
      dplyr::mutate(cs_id = factor(ifelse(is.na(cs_id) | cs_id < 0, NA, cs_id)))

    if (!('nlog10p' %in% colnames(data))) {
      if ("pvalue" %in% colnames(data) & min(data$pvalue, na.rm = TRUE) > 0) {
        data <- dplyr::mutate(data, nlog10p = -log10(pvalue))
      } else {
        data <-
          dplyr::mutate(data,
                        nlog10p = pchisq((beta / se) ** 2,
                                         1,
                                         log.p = TRUE,
                                         lower.tail =
                                           F
                        ) / -log(10))
      }
    }

    if (!is.null(lead_variant)) {
      lead_variant_idx = which(data$variant == lead_variant)
    } else {
      lead_variant_idx = which.max(data$nlog10p)
    }
    data$lead_variant = FALSE
    data$lead_variant[lead_variant_idx] = TRUE

    return(data)
  }
