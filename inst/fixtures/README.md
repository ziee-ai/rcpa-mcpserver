# Test fixtures

Small canonical inputs used by Tier-3 template smoke tests and
Tier-4 dispatch tests. Kept under `inst/` so they ship with the
installed package and `system.file()` can locate them in CI.

| File | Purpose |
|---|---|
| `small_expr.csv` | 20 genes × 10 samples expression matrix, balanced 2-group design (suitable for limma) |
| `small_expr_counts.csv` | 20 genes × 10 samples integer counts (suitable for DESeq2/edgeR) |
| `small_design.csv` | matching design with `sample`, `group` columns |
| `small_design_paired.csv` | same samples with `pair` column |
| `small_design_3group.csv` | 3-group design needing explicit `contrast` |
| `small_gene_stats.csv` | 30 genes × {id, logFC, pvalue} suitable for ORA/fgsea |
| `small_gene_stats_alt.csv` | second gene-stats file (different logFC distribution) for meta-analysis |
