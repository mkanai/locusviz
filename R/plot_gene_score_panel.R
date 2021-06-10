plot_gene_score_panel = function(chromosome,
                                 start,
                                 end,
                                 gene_score.data,
                                 genome_build = c('hg19', 'hg38'),
                                 txdb = NULL,
                                 highlight_pos = NULL,
                                 append.distance = TRUE,
                                 distance.type = c("GB", "TSS"),
                                 method.levels = NULL,
                                 colors = NULL,
                                 area.max_size = 4) {
  genome_build = match.arg(genome_build)
  distance.type = match.arg(distance.type)

  txdb = load_txdb(genome_build, txdb)

  gr = GenomicRanges::GRanges(seqnames = chromosome, ranges = IRanges(start, end))
  gr.txdb <- biovizBase::crunch(txdb, which = gr)
  plotted.genes <- unique(gr.txdb$tx_name)

  distance.df = or_missing(
    append.distance,
    compute_distance_to_gene(txdb, chromosome, start, end, highlight_pos, distance.type)
  )

  if (is.null(method.levels)) {
    method.levels = c(or_missing(append.distance, "Distance"),
                      unique(as.character(gene_score.data$method)))
  }

  gene_score.data =
    dplyr::bind_rows(distance.df, gene_score.data) %>%
    dplyr::filter(gene %in% plotted.genes) %>%
    dplyr::group_by(method) %>%
    dplyr::mutate(score = normalize_rank(score))  %>%
    dplyr::mutate(gene = factor(gene, levels = levels(distance.df$gene)),
                  method = factor(method, levels = rev(method.levels)))

  p_gene_score =
    ggplot(gene_score.data, aes(gene, method)) +
    geom_point(aes(
      size = score,
      color = method,
      alpha = score
    )) +
    get_default_theme(hide.xtitle = TRUE, hide.ytitle = TRUE) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      panel.grid.major.x = element_line(),
      legend.position = "none"
    ) +
    scale_size_area(max_size = area.max_size) +
    or_missing(!is.null(colors), scale_color_manual(values = colors))

  return(p_gene_score)
}
