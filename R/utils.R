#' Title
#'
#' @param predicate
#' @param value
#'
#' @return
#' @export
#'
or_missing = function(predicate, value) {
  if (predicate) {
    value
  } else{
    NULL
  }
}

#' Title
#'
#' @param a
#' @param b
#'
#' @return
#' @export
#'
or_else = function(a, b) {
  ifelse(!is.na(a), a, b)
}

#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
na_and = function(...) {
  purrr::reduce(list(...), function(x, y) {
    or_else(x, FALSE) & or_else(y, FALSE)
  })
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
na_or = function(...) {
  purrr::reduce(list(...), function(x, y) {
    or_else(x, FALSE) | or_else(y, FALSE)
  })
}

#' Title
#'
#' @param variant
#' @param sep
#'
#' @return
#' @export
#'
parse_variant = function(variant, sep = ':') {
  v = stringr::str_split_fixed(variant, sep, 4)
  return(tibble::tibble(
    chromosome = v[, 1],
    position = as.numeric(v[, 2]),
    ref = v[, 3],
    alt = v[, 4]
  ))
}


#' Title
#'
#' @param chromosome
#' @param position
#' @param ref
#' @param alt
#'
#' @return
#' @export
#'
variant_str = function(chromosome, position, ref, alt) {
  stringr::str_c(chromosome, position, ref, alt, sep = ":")
}

#' Title
#'
#' @param locus
#' @param alleles
#'
#' @return
#' @export
#'
variant_str2 = function(locus, alleles) {
  alleles = stringr::str_replace(stringr::str_remove_all(alleles, '[\\[\\]\\"]'), ",", ":")
  stringr::str_c(locus, alleles, sep = ":")
}

