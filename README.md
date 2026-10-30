# A global review of functional vegetation paleoecology and the integration of plant traits with fossil pollen

| | |
| --- | --- |
| **Project** | ![Manuscript](https://img.shields.io/badge/manuscript-in%20review-E58606?style=flat-square&logo=readthedocs&logoColor=white) [![License: CC BY 4.0](https://img.shields.io/badge/license-CC%20BY%204.0-52BCA3?style=flat-square&logo=creativecommons&logoColor=white)](LICENSE) [![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.23806443-1682D4?style=flat-square&logo=zenodo&logoColor=white)](https://doi.org/10.5281/zenodo.23806443) |
| **Data** | [![Case studies](https://img.shields.io/badge/case%20studies-62-8E9F7F?style=flat-square&logo=leaflet&logoColor=white)](Data/Input/case_studies_table.csv) [![Variables](https://img.shields.io/badge/variables-65-8E9F7F?style=flat-square)](Data/Input/case_studies_table.csv) [![Years covered](https://img.shields.io/badge/published-1996%E2%80%932025-CC79A7?style=flat-square)](Data/Input/case_studies_table.csv) |
| **Code** | [![R 4.5.1](https://img.shields.io/badge/R-4.5.1-276DC3?style=flat-square&logo=r&logoColor=white)](https://www.r-project.org/) [![renv](https://img.shields.io/badge/packages-renv%20locked-276DC3?style=flat-square&logo=rstudioide&logoColor=white)](renv.lock) [![Last commit](https://img.shields.io/github/last-commit/NatalieNamesna/FVP_review?style=flat-square&color=56B4E9)](https://github.com/NatalieNamesna/FVP_review/commits) |

Data and R code accompanying the review manuscript:

> Námešná N., Flantua S. G. A., Kuneš P., Nogué S., Šímová I., Williams J. W. & Mottl O. *A global review of functional vegetation paleoecology and the integration of plant traits with fossil pollen.* Manuscript in review.

The review is a quantitative bibliometric and methodological synthesis of 62 case studies (published 1996–2025) that link pollen data (fossil or modern) to plant functional information, either as plant functional traits or as plant functional types (PFTs). This repository contains the dataset extracted from those studies, the scripts used to clean and analyse it, and the scripts that build Figures 1–4 of the manuscript.

## Dataset

`Data/Input/case_studies_table.csv` has one row per case study (62) and 65 variables, grouped into:

- **Publication metadata**: title, first author, year, journal, DOI and keywords.
- **Geography**: spatial scale, region (continent) and country.
- **Pollen data**: number and selection of records, time coverage, data sources (e.g. Neotoma) and how the pollen data were processed.
- **Functional data**: whether PFTs or plant traits were used, the trait data source (e.g. TRY, BIEN) and which traits were used.
- **Linking methodology**: how functional information was linked to pollen taxa (e.g. linking tables, assigning traits at higher taxonomic levels, probabilistic models) and how the combined data were analysed.

Most variables are Boolean (`TRUE` / `FALSE` / `NA` for not reported). The rest are categorical with controlled vocabularies, such as database names and regions. `Data/Processed/case_studies_clean.csv` is the same table with standardised column names, and it is the input to all figure scripts.

### How studies were selected

Case studies were identified by:

1. A structured Web of Science search (1 June 2026) combining pollen, paleoecology and functional-ecology terms. It returned 174 records, of which 46 were kept after relevance filtering.
2. Screening the references of the selected studies and of relevant reviews (+9 studies).
3. Searching Research Rabbit from the resulting list (+7 studies).

The inclusion criterion was that a study linked pollen data with information on plant functional attributes. The lead author extracted the variables for every study. A second round used an AI assistant (Google Gemini) to flag possible discrepancies, which the authors then checked by hand, and the senior author resolved any remaining uncertain cases. The full search query is given in the manuscript.

## Repository structure

```text
FVP_review/
├── Data/
│   ├── Input/
│   │   ├── case_studies_table.csv     # raw dataset of reviewed case studies
│   │   └── fonts/                     # Font Awesome icon font used in exploratory waffle plots
│   └── Processed/
│       └── case_studies_clean.csv     # cleaned dataset (standardised column names)
├── R/
│   ├── 00_Config_file.R               # packages, paths, figure sizes, palettes, theme
│   ├── 01_Data_processing/
│   │   └── table_processing.R         # Input -> Processed
│   ├── 02_Main_analyses/              # exploratory analyses and individual plots (01-07)
│   ├── 03_Figures/
│   │   ├── Figure_01.R                # publication metadata
│   │   ├── Figure_02.R                # pollen data
│   │   ├── Figure_03.R                # functional (PFT and trait) data
│   │   ├── Figure_04.R                # linking methodology
│   │   └── Run_03.R                   # builds all manuscript figures
│   └── Functions/                     # helper functions sourced by the config file
├── Outputs/
│   └── Figures/                       # final Figures 1-4 (PDF and PNG)
├── renv/                              # renv activation script and settings
├── renv.lock                          # exact package versions
└── FVP_review.Rproj
```

## Reproducing the figures

The analyses were run in R 4.5 (figures verified to be identical under R 4.5.0 and 4.5.1).

1. Open `FVP_review.Rproj` in RStudio (or start R in the repository root), so that paths resolved by [`here`](https://here.r-lib.org/) point to the project.
2. Restore the package versions recorded in `renv.lock` with [`renv`](https://rstudio.github.io/renv/). renv activates automatically when the project is opened.

   ```r
   renv::restore()
   ```

3. (Optional) Regenerate the cleaned dataset from the raw input:

   ```r
   source("R/01_Data_processing/table_processing.R")
   ```

4. Build all manuscript figures:

   ```r
   source("R/03_Figures/Run_03.R")
   ```

   The composites are written to `Outputs/Figures/` as `Figure_01`–`Figure_04` in PDF and PNG, sized for a full page width of 166 mm. Each figure script can also be run on its own.

The scripts in `R/02_Main_analyses/` record the exploratory analyses that came before the final figures. They are kept for transparency but are not needed to reproduce Figures 1–4.

## Data availability

Releases of this repository are archived on Zenodo: [https://doi.org/10.5281/zenodo.23806443](https://doi.org/10.5281/zenodo.23806443). This DOI always resolves to the latest release, and each release also has its own version-specific DOI on Zenodo.

## License

This work is licensed under the [Creative Commons Attribution 4.0 International License (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/); see [LICENSE](LICENSE). If you use the data or code, please cite the manuscript above.

The bundled Font Awesome Free font (`Data/Input/fonts/`) is a third-party file distributed under the [SIL Open Font License 1.1](https://fontawesome.com/license/free). It is not covered by the CC BY 4.0 license.

## Funding

OM was funded by the Czech Science Foundation PIF grant (GN23-06386I), the Charles University Research Centre program (UNCE/24/SCI/006), and the Institutional Support for Science and Research of the Ministry of Education, Youth and Sports of the Czech Republic. SGAF was supported by the Trond Mohn Foundation and the University of Bergen (startup grant TMS2022STG03). SN acknowledges support from the European Research Council grant TIME-LINES (ERC-CoG-2021.101045309). IS was supported by the Czech Science Foundation (25-18055S).

## Contact

Natálie Námešná (Department of Botany, Faculty of Science, Charles University, Prague). For questions about the data or code, please open an [issue](https://github.com/NatalieNamesna/FVP_review/issues).
