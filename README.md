# Replication of Mesoudi, Whiten & Dunbar (2006), Experiment 2

This repository holds my preregistered replication of Experiment 2 from Mesoudi, Whiten and Dunbar (2006), "A bias for social information in human cultural transmission," *British Journal of Psychology*, 97, 405–423 (https://doi.org/10.1348/000712605X85871). Participants pass four short paragraphs along transmission chains of four people, and asks whether social information survives retelling better than non-social information.

## Files

- `writeup/Replication Report Template.qmd`: the report, rendered to `writeup/Replication-Report-Template.pdf`
- `writeup/power/power_sim.R`: the power simulation. It saves its results (`results.rds`) and figures (`.png`) beside itself, and the report reads them
- `original_paper/`: the original article
- `data/`: empty until data collection. Participant data stay out of the repository

## Rendering the report

The report needs R with tidyverse, and Quarto. From `writeup/`, run:

    quarto render "Replication Report Template.qmd"

The report loads Mesoudi's published data from his repository (https://github.com/amesoudi/mesoudi_whiten_dunbar_2006), pinned to one version, so rendering needs an internet connection. The slow part of the power simulation does not re-run during rendering. To rebuild it, run `Rscript power/power_sim.R` from `writeup/`, which takes about 15 minutes.
