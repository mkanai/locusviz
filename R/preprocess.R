#' Preprocess GWAS data for visualization
#'
#' This function preprocesses GWAS summary statistics data for use with locusviz
#' plotting functions. It standardizes column names, calculates -log10(p) values,
#' and identifies the lead variant.
#'
#' @param data Data frame containing GWAS summary statistics
#' @param lead_variant Character string specifying the lead variant ID. If NULL,
#'   the variant with the highest -log10(p) value will be selected
#' @param chromosome_col Name of the chromosome column (default: "chromosome")
#' @param position_col Name of the position column (default: "position")
#' @param variant_col Name of the variant ID column (default: "variant")
#' @param beta_col Name of the beta/effect size column (default: "beta")
#' @param se_col Name of the standard error column (default: "se")
#' @param pvalue_col Name of the p-value column (default: "pvalue")
#' @param pip_col Name of the PIP column for fine-mapping (default: "pip")
#' @param cs_id_col Name of the credible set ID column (default: "cs_id")
#' @param r2_col Name of the r² column (default: "r2")
#'
#' @return A standardized data frame with columns: chromosome, position, variant,
#'   beta, se, pip, cs_id, nlog10p, lead_variant, and optionally pvalue and r2
#'
#' @importFrom dplyr rename mutate
#' @importFrom stats pchisq
#'
#' @examples
#' \dontrun{
#' # Basic preprocessing
#' processed_data <- preprocess(gwas_data)
#'
#' # With custom column names
#' processed_data <- preprocess(
#'   gwas_data,
#'   chromosome_col = "chr",
#'   position_col = "pos",
#'   lead_variant = "rs123456"
#' )
#' }
#'
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
    required_cols <- c(chromosome_col, position_col, variant_col, beta_col, se_col, pip_col, cs_id_col)
    if (!all(required_cols %in% colnames(data))) {
      stop(paste("Required columns are missing:", paste(
        setdiff(required_cols, colnames(data)),
        collapse = ","
      )))
    }

    data <- dplyr::rename(
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

    if (!("nlog10p" %in% colnames(data))) {
      if ("pvalue" %in% colnames(data) && any(!is.na(data$pvalue)) && min(data$pvalue, na.rm = TRUE) > 0) {
        data <- dplyr::mutate(data, nlog10p = -log10(pvalue))
      } else {
        data <-
          dplyr::mutate(data,
            nlog10p = pchisq((beta / se)**2,
              1,
              log.p = TRUE,
              lower.tail =
                F
            ) / -log(10)
          )
      }
    }

    if (!is.null(lead_variant)) {
      lead_variant_idx <- which(data$variant == lead_variant)
    } else {
      lead_variant_idx <- which.max(data$nlog10p)
    }
    data$lead_variant <- FALSE
    data$lead_variant[lead_variant_idx] <- TRUE

    return(data)
  }
