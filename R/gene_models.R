#' Internal helpers for gene-track layout
#'
#' These helpers extract gene models from a TxDb and pack them into rows so
#' gene bodies AND their labels share horizontal space without collisions.
#' They are not exported.
#'
#' @keywords internal
#' @name gene_models
NULL

#' Extract per-gene features from a TxDb for a genomic region
#'
#' Returns a list with `$genes` (one row per gene) and `$features` (one row
#' per exon/CDS/UTR/intron). Gene identity comes from `tx_name` in the TxDb
#' — for the TxDbs shipped with locusviz this is the gene symbol (see
#' [gencode_txdb()]). When a TxDb has multiple transcripts per symbol they
#' are unioned per symbol.
#'
#' @param txdb A TxDb object
#' @param gr A length-1 GRanges describing the view region
#'
#' @return A list with two data frames: `genes` and `features`. Both are
#'   empty data frames with the right columns when nothing overlaps `gr`.
#'
#' @importFrom biovizBase crunch
#' @importFrom GenomicRanges as.data.frame
#' @importFrom dplyr group_by summarize filter select rename mutate %>%
#' @keywords internal
get_gene_models <- function(txdb, gr) {
  empty <- list(
    genes = data.frame(
      gene_id = character(),
      symbol = character(),
      chromosome = character(),
      strand = character(),
      start = integer(),
      end = integer(),
      tss = integer(),
      stringsAsFactors = FALSE
    ),
    features = data.frame(
      gene_id = character(),
      type = character(),
      start = integer(),
      end = integer(),
      stringsAsFactors = FALSE
    )
  )

  cr <- tryCatch(biovizBase::crunch(txdb, which = gr),
    error = function(e) {
      message(conditionMessage(e))
      NULL
    }
  )
  if (is.null(cr) || length(cr) == 0L) {
    return(empty)
  }

  df <- as.data.frame(cr, stringsAsFactors = FALSE)
  if (nrow(df) == 0L) {
    return(empty)
  }

  if (!"tx_name" %in% colnames(df)) df$tx_name <- as.character(df$tx_id)
  df$tx_name <- as.character(df$tx_name)
  df$type <- as.character(df$type)
  df$strand <- as.character(df$strand)
  df$seqnames <- as.character(df$seqnames)

  genes <- df %>%
    dplyr::group_by(tx_name) %>%
    dplyr::summarize(
      chromosome = seqnames[1],
      strand = strand[1],
      start = min(start),
      end = max(end),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      gene_id = tx_name,
      symbol = tx_name,
      tss = ifelse(strand == "-", end, start)
    ) %>%
    dplyr::select(gene_id, symbol, chromosome, strand, start, end, tss) %>%
    as.data.frame()

  features <- df %>%
    dplyr::filter(type %in% c("cds", "utr", "exon", "gap")) %>%
    dplyr::select(gene_id = tx_name, type, start, end) %>%
    as.data.frame()

  list(genes = genes, features = features)
}

#' Greedy in-order row packing for intervals
#'
#' For each input interval in the order given, assigns the lowest row in
#' which it does not overlap any already-placed interval. Unlike
#' `IRanges::disjointBins`, this respects the caller's order — which lets
#' the caller bias certain intervals toward low row numbers (e.g., priority
#' genes packed before others).
#'
#' @param starts Numeric vector of interval starts (left-closed).
#' @param ends   Numeric vector of interval ends (right-closed).
#' @param pad    Numeric scalar added to both sides of each interval.
#'
#' @return Integer vector of row indices (1-based), same length as `starts`.
#'   Intervals on the same row are guaranteed non-overlapping (after padding).
#'
#' @keywords internal
#' @noRd
pack_rows <- function(starts, ends, pad = 0) {
  n <- length(starts)
  if (n == 0L) {
    return(integer())
  }
  s <- starts - pad
  e <- ends + pad
  rows <- integer(n)
  row_s <- list()
  row_e <- list()
  for (i in seq_len(n)) {
    placed <- FALSE
    for (r in seq_along(row_s)) {
      if (!any(row_s[[r]] <= e[i] & row_e[[r]] >= s[i])) {
        rows[i] <- r
        row_s[[r]] <- c(row_s[[r]], s[i])
        row_e[[r]] <- c(row_e[[r]], e[i])
        placed <- TRUE
        break
      }
    }
    if (!placed) {
      r <- length(row_s) + 1L
      rows[i] <- r
      row_s[[r]] <- s[i]
      row_e[[r]] <- e[i]
    }
  }
  rows
}

#' Estimate label width in genome coordinates
#'
#' Approximates `geom_text` label width in bp assuming the panel renders
#' roughly `chars_per_panel` monospace characters across the x-axis.
#' This is a deliberately simple heuristic — the user can tune
#' `chars_per_panel` per their figure aspect ratio.
#'
#' @param symbols Character vector of label strings.
#' @param x_range Numeric length-2 vector: panel x limits in bp.
#' @param chars_per_panel Approximate number of characters that fit across
#'   the panel at the active font size.
#' @param char_pad Extra characters per label as horizontal padding.
#'
#' @return Numeric vector of estimated label widths (in bp).
#' @keywords internal
estimate_label_width_bp <- function(symbols, x_range, chars_per_panel = 100,
                                    char_pad = 1.5) {
  if (length(symbols) == 0L) {
    return(numeric())
  }
  span <- diff(x_range)
  bp_per_char <- span / chars_per_panel
  (nchar(symbols) + char_pad) * bp_per_char
}
