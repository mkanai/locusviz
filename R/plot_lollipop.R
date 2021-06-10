plot_lollipop = function(df,
                         gene_symbol,
                         point_colors,
                         clinvar,
                         point_shapes = cohort_shapes,
                         trait_idx = NULL,
                         gene_col = 'grey90',
                         color_by_cohort = FALSE,
                         plot.domains = TRUE,
                         remove.unknown.domains = TRUE,
                         omit_spacer = FALSE,
                         extend.size = NULL,
                         plot.extra.genes = FALSE,
                         genome_build = c('hg19', 'hg38'),
                         txdb = NULL) {
  genome_build = match.arg(genome_build)
  if (is.null(txdb)) {
    txdb = load_txdb(genome_build)
  }

  my_theme =
    BuenColors::pretty_plot(fontsize = 8) +
    theme(
      legend.position = c(1, 1),
      legend.justification = c(1, 1),
      legend.title = element_text(margin = margin(0, 0, 0, 0)),
      legend.background = element_blank(),
      legend.key.size = unit(0.3, "cm"),
      panel.border = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    )

  gr <-
    range(subset(GenomicFeatures::transcripts(txdb), tx_name == gene_symbol),
          ignore.strand = TRUE)
  chrom = stringr::str_remove(seqnames(gr), "^chr")
  start = start(gr)
  end = end(gr)
  if (!is.null(extend.size)) {
    if (length(extend.size) == 1) {
      extend.size = rep(extend.size, 2)
    }
    start = start - extend.size[1]
    end = end + extend.size[2]
    if (plot.extra.genes) {
      gr <- GRanges(seqnames = paste0("chr", chrom),
                    ranges = IRanges(start, end))
    }
  }
  print(c(start, end))

  # df = df %>%
  #   filter(pip > 0.1 & !is.na(susie.beta_posterior) & vep.consequence_category != "Synonymous" & vep.gene_most_severe == gene_symbol) %>%
  #   mutate(
  #     vep.hgvsp = stringr::str_remove(vep.hgvsp, "^ENSP.*:"),
  #     chromosome = parse_chromosome(variant),
  #     position = parse_position(variant),
  #     signed_pip = sign(susie.beta_posterior) * pip,
  #   ) %>%
  #   dplyr::filter(chromosome == chrom & start <= position & position <= end) %>%
  #   arrange(position,-pip, domain, trait)

  assign_trait_idx = function(df) {
    trait_idx = dplyr::select(df, trait) %>%
      dplyr::distinct() %>%
      dplyr::arrange(trait) %>%
      dplyr::mutate(idx = 1:n()) %>%
      as.data.frame()
    rownames(trait_idx) = trait_idx$trait

    return(trait_idx)
  }

  if (is.null(trait_idx)) {
    trait_idx = assign_trait_idx(df)
  }

  df = dplyr::mutate(df, signed_pip = sign(susie.beta_posterior) * pip) %>%
    dplyr::filter(position >= start & position <= end) %>%
    dplyr::mutate(idx = trait_idx[trait, "idx"]) %>%
    dplyr::group_by(sign(susie.beta_posterior)) %>%
    # mutate(label = coalesce(vep.hgvsp, stringr::str_remove(vep.most_severe, "_variant"))) %>%
    dplyr::mutate(label = replace(label, duplicated(label), NA)) %>%
    dplyr::ungroup()

  if (color_by_cohort) {
    s_color = scale_color_manual(values = point_colors, na.value = "grey20")
    s_shape = scale_shape_manual(values = point_shapes)
  } else {
    s_color = scale_color_manual(values = point_colors, na.value = "grey20")
    s_shape = scale_shape_manual(values = point_shapes)
  }

  plot_pip = function(df,
                      sign,
                      xscale,
                      label.y = 1.1,
                      ymax = 1.8) {
    if (sign > 0) {
      df = df %>%
        dplyr::filter(susie.beta_posterior > 0)
      yinf = -Inf
      ylim = c(0, ymax)
      my_scale_y = scale_y_continuous(breaks = seq(0, 1, by = 0.2))
      hjust = 0
      p_theme = my_theme +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.length.x = unit(0, "pt"),
          axis.line.y = element_line(),
          legend.position = "none"
        )
    } else {
      df = df %>%
        dplyr::filter(susie.beta_posterior < 0)
      yinf = Inf
      ylim = c(-ymax, 0)
      label.y = -label.y
      my_scale_y = scale_y_continuous(breaks = -seq(0, 1, by = 0.2),
                                      labels = sprintf("%.1f", seq(0, 1, by = 0.2)))
      hjust = 1
      p_theme = my_theme +
        theme(axis.line.y = element_line(),
              legend.position = "none")
    }

    if (nrow(df) == 0) {
      return(plot_spacer())
    }

    df = dplyr::arrange(df, position) %>%
      dplyr::mutate(position2 = jitter_labels(position, xscale = xscale))
    v.pos = dplyr::pull(df, position) %>%
      unique()

    label.path =
      dplyr::select(df, cohort, trait, variant, position, position2, signed_pip) %>%
      tidyr::nest(-cohort,-trait,-variant) %>%
      dplyr::mutate(data = purrr::map(data, function(data) {
        tibble::tibble(
          x = c(data$position, data$position2, data$position2),
          y = c(yinf, 0, data$signed_pip)
        )
      })) %>%
      tidyr::unnest(data)

    if (color_by_cohort) {
      g_point = geom_point(aes(position2, signed_pip, col = cohort, shape = cohort),
                           size = 4)
    } else {
      g_point = geom_point(aes(position2, signed_pip, col = trait, shape = cohort),
                           size = 4)
    }

    df %>%
      ggplot() +
      geom_path(
        aes(
          x = x,
          y = y,
          group = interaction(cohort, trait, variant)
        ),
        data = label.path,
        size = 0.2,
        color = 'grey50'
      ) +
      geom_segment(
        aes(
          x = position2,
          xend = position2,
          y = signed_pip,
          yend = label.y
        ),
        data = df %>% filter(!is.na(label)),
        size = 0.2,
        color = 'grey50',
        linetype = 'dotted'
      ) +
      g_point +
      geom_text(aes(position2, signed_pip, label = idx),
                size = 2,
                color = "white") +
      geom_text(
        aes(x = position2, y = label.y, label = label),
        data = df %>% filter(!is.na(label)),
        angle = 90,
        vjust = 0.5,
        hjust = hjust,
        size = 2
      ) +
      p_theme +
      my_scale_y +
      s_color +
      s_shape +
      labs(x = "Position", y = "PIP") +
      coord_cartesian(xlim = xscale,
                      ylim = ylim,
                      clip = 'off')
  }

  gof_pos = df %>% dplyr::filter(susie.beta_posterior > 0) %>% .$position %>% unique()
  lof_pos = df %>% dplyr::filter(susie.beta_posterior < 0) %>% .$position %>% unique()

  pfam.domains = get_pfam_domains(
    gene_symbol,
    remove.unknown.domains = remove.unknown.domains,
    genome_build = genome_build,
    txdb = txdb
  )

  clinvar.snv = clinvar %>%
    filter(
      GeneSymbol == gene_symbol &
        ClinicalSignificance %in% c("Pathogenic", "Likely pathogenic")
    ) %>%
    filter(Type == "single nucleotide variant") %>%
    mutate(
      parse_variant(locus),
      gof_overlap = position %in% gof_pos,
      lof_overlap = position %in% lof_pos
    )
  print(clinvar.snv)

  clinvar.sv = clinvar %>%
    filter(
      GeneSymbol == gene_symbol &
        ClinicalSignificance %in% c("Pathogenic", "Likely pathogenic")
    ) %>%
    # filter(Type != "single nucleotide variant") %>%
    mutate(
      position = as.numeric(stringr::str_split_fixed(locus, ":", 2)[, 2]),
      start = as.numeric(stringr::str_split_fixed(interval, ":|-|\\)", 4)[, 2]),
      end = as.numeric(stringr::str_split_fixed(interval, ":|-|\\)", 4)[, 3]),
      gof_overlap = position %in% gof_pos,
      lof_overlap = position %in% lof_pos,
      large_sv = (end - start) > 50
    )
  print(clinvar.sv)

  g_domains = or_missing(nrow(pfam.domains) > 0, geom_rect(
    aes(
      xmin = start,
      xmax = end,
      ymin = 0.89,
      ymax = 1.11,
      fill = Pfam_description
    ),
    data = pfam.domains
  ))
  # g_snv = or_missing(nrow(clinvar.snv) > 0, geom_point(aes(x = position, y = 1), data = clinvar.snv %>% filter(!gof_overlap | !lof_overlap), shape = 18, color = "grey50", size = 3))
  g_snv = NULL
  g_sv = or_missing(
    nrow(clinvar.sv) > 0,
    geom_segment(
      aes(
        x = start,
        xend = start,
        y = 0.95,
        yend = 1.05
      ),
      data = clinvar.sv %>% filter(!large_sv),
      color = "grey50",
      size = 0.3
    )
  )
  g_large_deletion = or_missing(
    any(clinvar.sv$large_sv),
    geom_segment(
      aes(
        x = start,
        xend = end,
        y = 1.05,
        yend = 1.05
      ),
      data = clinvar.sv %>% filter(large_sv),
      color = "grey50",
      size = 0.3
    )
  )# BuenColors::jdb_palette("brewer_red")[5]))
  # g_gof_overlap = or_missing(any(clinvar.snv$gof_overlap), geom_point(aes(x = position, y = 1), data = clinvar.snv %>% filter(gof_overlap), shape = 18, color = "grey20", size = 3))
  # g_lof_overlap = or_missing(any(clinvar.snv$lof_overlap), geom_point(aes(x = position, y = 1), data = clinvar.snv %>% filter(lof_overlap), shape = 18, color = "grey20", size = 3))
  g_gof_overlap = or_missing(
    any(clinvar.snv$gof_overlap |
          clinvar.snv$lof_overlap),
    geom_segment(
      aes(
        x = start,
        xend = start,
        y = 0.95,
        yend = 1.05
      ),
      data = clinvar.sv %>% filter(gof_overlap |
                                     lof_overlap),
      color = "grey20",
      size = 0.3
    )
  )
  g_lof_overlap = NULL
  g_gof = or_missing(length(gof_pos) > 0,
                     geom_point(
                       aes(x = gof_pos, y = 1.11),
                       shape = 18,
                       color = ifelse(gof_pos %in% clinvar.sv$position, "grey20", "grey50"),
                       #BuenColors::jdb_palette("solar_extra")[1],
                       size = 3
                     ))
  g_gof_path = or_missing(length(gof_pos) > 0,
                          geom_path(
                            aes(x, y, group = i),
                            data = data.frame(
                              x = rep(gof_pos, each = 2),
                              y = rep(c(1.1, Inf), length(gof_pos)),
                              i = rep(1:length(gof_pos), each = 2)
                            ),
                            size = 0.2,
                            color = 'grey50'
                          ))
  g_lof = or_missing(length(lof_pos) > 0,
                     geom_point(
                       aes(x = lof_pos, y = 0.89),
                       shape = 18,
                       color = ifelse(lof_pos %in% clinvar.sv$position, "grey20", "grey50"),
                       #BuenColors::jdb_palette("solar_extra")[9],
                       size = 3
                     ))
  g_lof_path = or_missing(length(lof_pos) > 0,
                          geom_path(
                            aes(x, y, group = i),
                            data = data.frame(
                              x = rep(lof_pos, each = 2),
                              y = rep(c(0.9,-Inf), length(lof_pos)),
                              i = rep(1:length(lof_pos), each = 2)
                            ),
                            size = 0.2,
                            color = 'grey50'
                          ))

  p_gene = ggplot() +
    ggbio::geom_alignment(
      txdb,
      which = gr,
      cds.rect.h = 0.1,
      color = gene_col,
      fill = gene_col,
      label = plot.extra.genes,
      label.size = 2,
      subset.tx_name = locusviz::or_missing(!plot.extra.genes, gene_symbol)
    ) +
    geom_text(
      aes(x = start, y = 1, label = gene_symbol),
      hjust = 1,
      nudge_x = -500,
      size = 2
    ) +
    g_domains +
    g_large_deletion +
    # SV
    g_sv +
    # SNV
    g_snv + g_gof_overlap + g_lof_overlap +
    # GoF
    g_gof_path + g_gof +
    # LoF
    g_lof_path + g_lof +
    my_theme +
    theme(
      panel.border = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    scale_fill_manual(values = BuenColors::jdb_palette('corona')[16:30]) +
    scale_y_discrete(expand = c(0, 0.1)) +
    labs(x = "Position", fill = "Domain") + coord_cartesian(xlim = c(start, end), clip = "off")

  p1 = plot_pip(df, sign = 1, xscale = c(start, end))
  p2 = plot_pip(df, sign = -1, xscale = c(start, end))


  cohort_idx = df %>% distinct(cohort) %>% mutate(idx = 1:n())
  if (color_by_cohort) {
    g_legend_trait_point = geom_point(
      aes(idx, 0),
      data = trait_idx,
      color = "grey50",
      shape = 18,
      size = 2.5
    )
    g_legend_cohort_point = geom_point(
      aes(idx, 0, col = cohort),
      data = cohort_idx,
      shape = 18,
      size = 2.5
    )
  } else {
    g_legend_trait_point = geom_point(
      aes(idx, 0, col = trait),
      data = trait_idx,
      shape = 18,
      size = 2.5
    )
    g_legend_cohort_point = geom_point(
      aes(idx, 0, shape = cohort),
      data = cohort_idx,
      color = "grey50",
      size = 2.5
    )
  }

  p_legend_trait = ggplot() +
    geom_text(aes(x = 0, y = 0, label = "Trait"),
              size = 2,
              hjust = 0) +
    g_legend_trait_point +
    geom_text(
      aes(idx, 0, label = idx),
      data = trait_idx,
      size = 1.5,
      color = "white"
    ) +
    geom_text(
      aes(idx, 0, label = trait),
      data = trait_idx,
      size = 2,
      hjust = 0,
      nudge_x = 0.1
    ) +
    s_color +
    s_shape +
    cowplot::theme_nothing() +
    coord_cartesian(xlim = c(0, max(max(trait_idx$idx), 12)))

  p_legend_cohort = ggplot() +
    geom_text(aes(x = 0, y = 0, label = "Cohort"),
              size = 2,
              hjust = 0) +
    g_legend_cohort_point +
    geom_text(
      aes(idx, 0, label = cohort),
      data = cohort_idx,
      size = 2,
      hjust = 0,
      nudge_x = 0.1
    ) +
    s_color +
    s_shape +
    cowplot::theme_nothing() +
    coord_cartesian(xlim = c(0, max(max(trait_idx$idx), 12)))

  if (omit_spacer & "spacer" %in% c(class(p1), class(p2))) {
    if ("spacer" %in% class(p1)) {
      p = p2
    } else if ("spacer" %in% class(p2)) {
      p = p1
    }
    plt_combined = p + p_gene + p_legend_trait + p_legend_cohort + guide_area() + plot_layout(
      nrow = 5,
      guides = 'collect',
      heights = c(1, 0.25, 0.1, 0.1, 0.1)
    )
    return(plt_combined)
  }

  plt_combined = p1 + p_gene + p2 + p_legend_trait + p_legend_cohort + guide_area() + plot_layout(
    nrow = 6,
    guides = 'collect',
    heights = c(1, 0.25, 1, 0.1, 0.1, 0.1)
  )
  plt_combined
  # return(list(plt_combined, p1, p_gene, p2, p_legend))
}
