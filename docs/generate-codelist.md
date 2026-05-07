# Generating a Phenotype Codelist

## Overview

This page provides an example R script that can be adapted to generate a phenotype codelist for this repository. This codelist-generating R script template is for CPRD Gold and Aurum. The script is intended as a starting point for contributors and should not be used to generate codelists without careful review and adaptation.

> 💡 **Tip:** Search 🔴 in the script to jump directly to sections that need changing for your phenotype.

---

## Download the Template

Download the R script here:

- [**Depression_codelist_script_example.R**](https://github.com/Janeh19/Code-list-generating-template/raw/main/Depression_codelist_script_example.R)

---

## What This Script Is For

This R script helps contributors:

- Identify relevant clinical codes for a phenotype
- Document inclusion and exclusion logic
- Generate a structured codelist for review
- Adapt the workflow for a new phenotype

---

## How to Use It

 1. Download the template R script and open it in R or RStudio.
 2. Set up and load the required data (see [Inputs](#inputs) below).
 3. Replace the example phenotype content with your own phenotype definition.
 4. Update the inclusion and exclusion logic as needed.
 5. Search for new relevant medical codes and compare with multiple reference lists.
 6. Create and update the codelists, then review the output carefully.
 7. Save the resulting codelist and associated metadata.
 8. Upload your codelist and supporting files to your repository.

---

---

## What You Should Customise

Before using the script, review and adapt:

- The phenotype name
- The code search terms
- The inclusion criteria
- The exclusion criteria
- Any reviewer notes
- Metadata describing dataset, coding system, and intended use

---

## Inputs

| # | File | Description |
|---|------|-------------|
| 1 | `CPRDAurumMedical.txt` | CPRD Aurum medical dictionary |
| 2 | `CPRDGoldMedical.txt` | CPRD Gold medical dictionary |
| 3 | `Aurum_Depression_codelist_20250725.txt` | Depression codelist from GitHub |
| 4 | `Gold_Depression_codelist_20250725.txt` | Depression codelist from GitHub |
| 5 | `hdrukcodelist.csv` | Depression phenotype from HDRUK library |
| 6 | `qof.xlsm` | Quality and Outcomes Framework clusters list |

---

## Outputs

### Intermediate Outputs (for clinical review)

| File | Description |
|------|-------------|
| `aurum_depression_codelist_15042026_tobereviewed.csv` | Aurum codelist to be clinically reviewed |
| `gold_depression_codelist_15042026_tobereviewed.csv` | Gold codelist to be clinically reviewed |

### Final Outputs

| File | Description |
|------|-------------|
| `aurum_depression_codelist_15042026.txt` | Final depression codelist from Aurum |
| `gold_depression_codelist_15042026.txt` | Final depression codelist from Gold |
| `Depression_metadata_15042026.txt` | Metadata text file |

---

## Recommended Supporting Files

When contributing a codelist, include:

- The generated codelist file
- A metadata file describing the phenotype
- The codelist-generating script
- A citation file (`CITATION.cff`) where possible

---

## Notes for Contributors

Please treat this script as a template rather than a final validated definition. All generated codelists should be checked for:

- Correctness
- Completeness
- Reproducibility
- Clinical and research appropriateness where relevant
