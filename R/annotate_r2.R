#' Annotate variants with linkage disequilibrium (r2) values
#'
#' @param df Data frame containing variant information with a 'variant' column
#' @param lead_variant Lead variant in chr:pos:ref:alt or chr_pos_ref_alt format.
#'   If NULL, will attempt to use the variant marked as TRUE in the column
#'   specified by \code{lead_variant_col}
#' @param lead_variant_col Name of the logical column in df that indicates the
#'   lead variant (default: "lead_variant"). Used only when \code{lead_variant}
#'   is NULL
#' @param reference_panel Reference panel to use ("1000G" or "sisu42")
#' @param window Window size around lead variant in base pairs (default: 500000)
#' @param population For 1000G panel, the population code (e.g., "EUR", "AFR", "EAS", "SAS", "AMR")
#' @return Data frame with r2 values annotated
#' @export
annotate_r2 <- function(df,
                        lead_variant = NULL,
                        lead_variant_col = "lead_variant",
                        reference_panel = c("1000G", "sisu42"),
                        window = 500000,
                        population = "EUR") {
  # Check if df has 'variant' column
  if (!"variant" %in% names(df)) {
    stop("Input dataframe must have a 'variant' column")
  }

  # Spot check first 10 variants in df follow expected format
  variant_pattern <- "^(chr)?[0-9XY]+[_:]([0-9]+)[_:]([ATGC]+)[_:]([ATGC]+)$"
  variants_to_check <- head(df$variant, 10)
  invalid_variants <- variants_to_check[!stringr::str_detect(variants_to_check, variant_pattern)]
  if (length(invalid_variants) > 0) {
    stop(sprintf(
      "Invalid variant format(s) found in first 10 rows. Expected format: 'chr1:123:A:T' or 'chr1_123_A_T'. Invalid entries: %s",
      paste(invalid_variants, collapse = ", ")
    ))
  }

  # Create normalized variant column for internal use (preserve original)
  df$variant_normalized <- stringr::str_replace_all(df$variant, "_", ":")

  # Add chr prefix to normalized variants if missing
  needs_chr <- !stringr::str_starts(df$variant_normalized, "chr")
  if (any(needs_chr)) {
    warning(sprintf(
      "%d variant(s) lack 'chr' prefix. Adding 'chr' prefix for GRCh38 format. Make sure your coordinates are in GRCh38.",
      sum(needs_chr)
    ))
    df$variant_normalized[needs_chr] <- paste0("chr", df$variant_normalized[needs_chr])
  }

  reference_panel <- match.arg(reference_panel)

  # Determine lead variant
  if (is.null(lead_variant)) {
    # Check if lead_variant_col exists and has TRUE values
    if (lead_variant_col %in% names(df)) {
      lead_variants <- df$variant_normalized[df[[lead_variant_col]] == TRUE]
      if (length(lead_variants) == 0) {
        stop(sprintf("No lead variant found. Column '%s' exists but has no TRUE values.", lead_variant_col))
      } else if (length(lead_variants) > 1) {
        warning(sprintf(
          "Multiple lead variants found in column '%s'. Using the first one: %s",
          lead_variant_col, lead_variants[1]
        ))
      }
      lead_variant <- lead_variants[1]
    } else {
      stop(sprintf("No lead variant specified and column '%s' not found in dataframe.", lead_variant_col))
    }
  }

  # Normalize lead variant format to chr:pos:ref:alt
  lead_variant <- stringr::str_replace_all(lead_variant, "_", ":")

  # Check lead_variant format and add chr prefix if missing
  if (!stringr::str_detect(lead_variant, "^(chr)?[0-9X]+:[0-9]+:[ATGC]+:[ATGC]+$")) {
    stop("lead_variant must be in format 'chr1:123:A:T' or '1:123:A:T' (chromosome:position:ref:alt)")
  }

  if (!stringr::str_starts(lead_variant, "chr")) {
    warning("lead_variant lacks 'chr' prefix. Adding 'chr' prefix for GRCh38 format. Make sure your coordinates are in GRCh38.")
    lead_variant <- paste0("chr", lead_variant)
  }

  # Parse variant components
  parsed <- locusviz::parse_variant(lead_variant)
  chrom <- parsed$chromosome
  pos <- parsed$position

  if (reference_panel == "sisu42") {
    # FinnGen/SISU42 panel
    api_url <- sprintf(
      "http://api.finngen.fi/api/ld?variant=%s&window=%d&panel=sisu42&r2_thresh=0",
      lead_variant,
      window
    )

    ld_data <- jsonlite::fromJSON(api_url)$ld %>%
      dplyr::transmute(
        variant_normalized = paste0("chr", variation2),
        r2 = r2
      )
  } else if (reference_panel == "1000G") {
    # 1000 Genomes panel
    # Convert variant format for API: chr:pos:ref:alt to chr_pos_ref/alt
    variant_api <- stringr::str_replace(lead_variant, ":([ATGC]+):([ATGC]+)$", "_\\1/\\2")

    start <- max(1, pos - window)
    end <- pos + window

    api_url <- sprintf(
      "https://portaldev.sph.umich.edu/ld/genome_builds/GRCh38/references/1000G/populations/%s/variants?correlation=rsquare&variant=%s&chrom=%s&start=%d&stop=%d",
      population,
      variant_api,
      chrom,
      start,
      end
    )

    message(sprintf("Fetching LD from 1000G %s panel: %s", population, api_url))

    ld_data <- as.data.frame(jsonlite::fromJSON(api_url)$data) %>%
      dplyr::transmute(
        variant_normalized = stringr::str_replace_all(variant2, "[_/]", ":"),
        r2 = correlation
      )
  }

  # Remove existing r2 column if present and join new data
  if ("r2" %in% names(df)) {
    df <- dplyr::select(df, -r2)
  }

  # Join LD data using normalized variant format
  df <- dplyr::left_join(df, ld_data, by = "variant_normalized") %>%
    dplyr::select(-variant_normalized)

  return(df)
}
