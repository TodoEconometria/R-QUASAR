---
title: 'rquasar: A query-driven framework for reproducible survey analysis in R with a Rust backend'
tags:
  - R
  - reproducible research
  - survey microdata
  - official statistics
  - Rust
  - data engineering
authors:
  - name: Juan Marcelo Gutierrez Miranda
    orcid: 0000-0002-4452-7934
    affiliation: 1
affiliations:
  - name: TodoEconometria, Spain
    index: 1
date: 24 August 2026
bibliography: paper.bib
---

# Summary

`rquasar` is an R [@RCoreTeam] framework that removes the repetitive glue code
between raw survey microdata and publication-ready output. From a single context
—data, model, and options declared once— it produces formatted tables, plots and
reports without the user rewriting the same scaffolding for every project. For
datasets too large to hold comfortably in memory, file-level operations (reading,
grouping, filtering, sorting) are delegated to a Rust backend built on `extendr`
[@extendr] and Polars [@polars], so the same workflow scales from a few hundred
rows to files of several gigabytes. The reader targets the messiness of real-world
microdata directly —arbitrary field delimiters and non-UTF-8 encodings— and pushes
column projection down to the scan, so wide files cost only the columns actually
used. Inputs larger than memory can be streamed straight to Parquet in bounded
memory, giving a reproducible columnar staging layer without a database server.

The package ships with `qsr_survey`, a fully synthetic household-survey dataset
shaped like national statistical office microdata but containing no real
observations, so every function can be demonstrated on runnable, self-contained
data. Optional AI helpers assist with survey-specific tasks —comparing codebooks
across waves, classifying free-text fields, flagging data-quality issues— and
support both a hosted provider and a fully local backend (Ollama), so that
confidential microdata never has to leave the machine.

# Statement of need

Analysts of survey and official-statistics microdata spend a disproportionate
share of their time on boilerplate: importing heterogeneous files, harmonising
labels, wiring the same table-and-figure code into each new project, and moving
large files through memory-bound pipelines. That work is rarely reusable and is a
frequent source of silent, hard-to-audit errors.

`rquasar` addresses this by organising the workflow into four explicit stages
—scaffold, context, connectors and an output engine— and by fixing the
error-prone parts once. Metrics and outputs are deterministic and reproducible
(fixed seeds, no hidden network calls in the defaults, escaped user input in
generated reports), which matters when the results feed publications. The Rust
backend targets the specific bottleneck of file-level operations on large tabular
data, where base R and memory-resident data frames become impractical; the design
is explicit that the backend buys *memory headroom*, not merely speed.

Existing tools cover parts of this space —the `survey` package [@lumley2004] for
design-based estimation, and general-purpose reporting and data-manipulation
packages— but the repetitive integration between raw microdata and
publication-ready output, together with a transparent path to out-of-memory
scale, is left to each analyst to reassemble. `rquasar` packages that integration
as a coherent, reproducible framework: it reads the native formats of survey
microdata directly (SPSS, Stata, SAS, fixed-width record layouts, Excel, and the
columnar formats), produces weighted, design-based standard errors, confidence
intervals and design effects in a single call by building on `survey` rather than
reimplementing it, and writes results back out to any of those formats. It is
aimed at researchers and public-statistics practitioners who need auditable
output at scale.

# Acknowledgements

The synthetic `qsr_survey` dataset and a companion open benchmarks repository are
released so that every example stays runnable and every performance claim
reproducible.

# References
