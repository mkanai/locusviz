#' Conditionally return value or NULL
#'
#' This utility function returns a value if a predicate is TRUE, otherwise NULL.
#' Useful for conditionally including ggplot2 layers.
#'
#' @param predicate Logical value determining whether to return the value
#' @param value Any R object to return if predicate is TRUE
#'
#' @return The value if predicate is TRUE, otherwise NULL
#' @export
#'
or_missing <- function(predicate, value) {
  if (predicate) {
    value
  } else {
    NULL
  }
}

#' Return first non-NA value
#'
#' This utility function returns the first argument if it's not NA, otherwise
#' returns the second argument.
#'
#' @param a First value to check
#' @param b Alternative value to return if a is NA
#'
#' @return a if not NA, otherwise b
#' @export
#'
or_else <- function(a, b) {
  ifelse(!is.na(a), a, b)
}

#' NA-safe AND operation
#'
#' Performs logical AND operation treating NA values as FALSE.
#'
#' @param ... Logical vectors to combine with AND
#'
#' @return Logical vector with NA-safe AND operation
#' @export
#'
na_and <- function(...) {
  purrr::reduce(list(...), function(x, y) {
    or_else(x, FALSE) & or_else(y, FALSE)
  })
}

#' NA-safe OR operation
#'
#' Performs logical OR operation treating NA values as FALSE.
#'
#' @param ... Logical vectors to combine with OR
#'
#' @return Logical vector with NA-safe OR operation
#' @export
#'
na_or <- function(...) {
  purrr::reduce(list(...), function(x, y) {
    or_else(x, FALSE) | or_else(y, FALSE)
  })
}

#' Parse variant string into components
#'
#' This function parses variant identifiers in the format
#' "chromosome:position:ref:alt" into separate columns.
#'
#' @param variant Character vector of variant identifiers
#' @param sep Character separator used in variant string (default: ":")
#'
#' @return A tibble with columns: chromosome, position (numeric), ref, alt
#'
#' @importFrom stringr str_split_fixed
#' @importFrom tibble tibble
#'
#' @examples
#' parse_variant(c("1:1000:A:G", "2:2000:C:T"))
#'
#' @export
#'
parse_variant <- function(variant, sep = ":") {
  v <- stringr::str_split_fixed(variant, sep, 4)
  return(tibble::tibble(
    chromosome = v[, 1],
    position = as.numeric(v[, 2]),
    ref = v[, 3],
    alt = v[, 4]
  ))
}


#' Create variant string from components
#'
#' This function creates variant identifiers in the format
#' "chromosome:position:ref:alt" from separate components.
#'
#' @param chromosome Character vector of chromosome identifiers
#' @param position Numeric vector of positions
#' @param ref Character vector of reference alleles
#' @param alt Character vector of alternative alleles
#'
#' @return Character vector of variant identifiers
#'
#' @importFrom stringr str_c
#'
#' @examples
#' variant_str("1", 1000, "A", "G")
#'
#' @export
#'
variant_str <- function(chromosome, position, ref, alt) {
  stringr::str_c(chromosome, position, ref, alt, sep = ":")
}

#' Create variant string from locus and alleles
#'
#' This function creates variant identifiers from locus (chromosome:position)
#' and alleles string, handling various formatting.
#'
#' @param locus Character vector of locus strings (chromosome:position)
#' @param alleles Character vector of allele strings (may contain brackets/quotes)
#'
#' @return Character vector of variant identifiers
#'
#' @importFrom stringr str_replace str_remove_all str_c
#'
#' @examples
#' variant_str2("1:1000", "[\"A\",\"G\"]")
#'
#' @export
#'
variant_str2 <- function(locus, alleles) {
  alleles <- stringr::str_replace(stringr::str_remove_all(alleles, '[\\[\\]\\"]'), ",", ":")
  stringr::str_c(locus, alleles, sep = ":")
}
