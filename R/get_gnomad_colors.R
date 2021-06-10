#' Title
#'
#' @return
#' @export
get_gnomad_colors = function() {
  # https://github.com/macarthur-lab/gnomad_lof/blob/master/R/constants.R
  color_amr = k_amr = '#ED1E24'
  color_eur = k_eur = '#6AA5CD'
  color_afr = k_afr = '#941494'
  color_sas = k_sas = '#FF9912'
  color_eas = k_eas = '#108C44'
  color_oth = k_oth = '#ABB9B9'
  color_mde = k_mde = '#33CC33'
  color_asj = k_asj = 'coral'

  color_nfe = k_nfe = color_eur
  color_fin = k_fin = '#002F6C'

  pop_colors = c('afr' = color_afr,
                 'amr' = color_amr,
                 'eas' = color_eas,
                 'fin' = color_fin,
                 'eur' = color_nfe,
                 'nfe' = color_nfe,
                 'oth' = color_oth,
                 'sas' = color_sas,
                 'mde' = color_mde,
                 'asj' = color_asj,
                 'uniform' = 'pink',
                 'consanguineous' = 'pink',
                 'sas_non_consang' = 'orange',
                 'exac' = 'gray',
                 'est' = 'black',
                 'bgr' = '#66C2A5',
                 'est' = '#4891D9',
                 'nwe' = '#C60C30',
                 'seu' = '#3ca021',
                 'swe' = 'purple',
                 'onf' = color_nfe,
                 'kor' = '#4891D9',
                 'jpn' = '#BC002D',
                 'oea' = color_eas,
                 'unk' = color_oth)

  pop_colors_uc = pop_colors
  names(pop_colors_uc) = stringr::str_to_upper(names(pop_colors_uc))
  pop_colors = c(pop_colors, pop_colors_uc)

  return(pop_colors)
}

