import hail as hl
import pandas as pd

# cf. https://github.com/broadinstitute/gnomad-browser/blob/main/data-pipeline/src/data_pipeline/data_types/canonical_transcript.py
def get_canonical_transcripts(**sites_table_paths):
    canonical_transcripts = set()
    for path in sites_table_paths.values():
        sites_table = hl.read_table(path)
        table_canonical_transcripts = sites_table.aggregate(
            hl.agg.explode(
                lambda csq: hl.agg.collect_as_set((csq.gene_id, csq.transcript_id)),
                sites_table.vep.transcript_consequences.filter(lambda csq: csq.canonical == 1),
            )
        )
        canonical_transcripts = canonical_transcripts.union(table_canonical_transcripts)

    canonical_transcripts = hl.Table.from_pandas(
        pd.DataFrame(
            {"gene_id": gene_id, "canonical_transcript_id": canonical_transcript_id}
            for gene_id, canonical_transcript_id in canonical_transcripts
        ),
        key="gene_id",
    )

    canonical_transcripts = canonical_transcripts.repartition(32, shuffle=True)

    return canonical_transcripts


# gnomAD v4.1.1 uses VEP v105 / GENCODE v39 on GRCh38.
# The genomes sites HT covers the whole genome, so canonical transcript
# annotations are captured for essentially every gene (matches the prior
# v3.1.2 genomes-only approach). Add an `exomes=...` kwarg here if you also
# want to union in the v4.1.1 exomes sites HT.
ht = get_canonical_transcripts(
    genomes="gs://gcp-public-data--gnomad/release/4.1.1/ht/genomes/gnomad.genomes.v4.1.1.sites.ht"
)
ht = ht.filter(ht.gene_id.startswith("ENSG"))
ht.export("gs://finngen-xavier-tmp-7days/canonical_transcripts_grch38_v39.tsv.bgz")
