# locusviz

A comprehensive R package for creating publication-quality visualizations of GWAS (Genome-Wide Association Study) data at specific genomic loci. The package provides LocusZoom-style plots using ggplot2 with support for multiple data types including GWAS summary statistics, fine-mapping results, and gene annotations.

**This package is still WIP.**

## Installation

```r
# Install from GitHub
remotes::install_github("mkanai/locusviz")
```

## Features

- **Multi-panel visualizations**: Combine Manhattan plots, fine-mapping results, LD patterns, and gene tracks in a single figure
- **Flexible data input**: Works with standard GWAS summary statistics and fine-mapping outputs
- **Genome build support**: Compatible with both hg19/GRCh37 and hg38/GRCh38
- **Publication-ready**: Clean, customizable themes designed for academic publications
- **Performance optimized**: Rasterization support for large datasets
- **Extensive customization**: Control colors, sizes, labels, and panel arrangement

## Main Functions

### Core Plotting Functions

- `plot_locuszoom()` - Create comprehensive multi-panel locus plots
- `plot_manhattan_panel()` - GWAS p-value visualization with LD coloring
- `plot_fm_panel()` - Fine-mapping posterior inclusion probabilities
- `plot_r2_panel()` - Linkage disequilibrium patterns by population
- `plot_gene_panel()` - Gene annotations with exon/intron structure
- `plot_gene_score_panel()` - Gene-level scores and prioritization

### Data Processing

- `preprocess()` - Standardize input data format
- `liftover_variant()` - Convert between genome builds
- `compute_distance_to_gene()` - Calculate variant-gene distances

### Additional Features

- `plot_lollipop()` - Variant effect visualization
- `UpSet2()` - Set intersection plots
- Custom scales and transformations for extreme p-values

## Quick Start

```r
library(locusviz)

# Preprocess your GWAS data
data <- preprocess(gwas_summary_stats)

# Create a basic LocusZoom plot
plot_locuszoom(
  data,
  highlight_pos = 123456789,  # Position to highlight
  window = 500000             # Window size around lead variant
)

# Customize panels
plot_locuszoom(
  data,
  plot.manhattan = TRUE,
  plot.fm = TRUE,
  plot.r2 = TRUE,
  plot.gene = TRUE,
  manhattan.title = "GWAS results for trait X",
  fm.legend_title = "95% Credible Set"
)
```

## Data Requirements

### Basic GWAS visualization

- `chromosome`: Chromosome identifier
- `position`: Base pair position
- `variant`: Variant identifier
- `beta`: Effect size
- `se`: Standard error
- `pvalue` or ability to compute from beta/se

### Fine-mapping visualization

- `pip`: Posterior inclusion probability
- `cs_id`: Credible set membership

### LD visualization

- `gnomad_lead_r2_*`: LD r² values by population

## Dependencies

The package relies on:

- ggplot2 (core plotting)
- patchwork (panel composition)
- GenomicRanges (genomic operations)
- BuenColors (color palettes)
- ComplexHeatmap (UpSet plots)
- dplyr (data manipulation)

## License

MIT License

## Author

Masahiro Kanai <mkanai@broadinstitute.org>
