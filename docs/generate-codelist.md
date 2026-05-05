# Generate a codelist

This page provides an example R script that can be adapted to generate a phenotype codelist for this repository.

## Download the template

Download the script here:

- [**1a_Depression_codelist_script_r.R**](downloads/1a_Depression_codelist_script_r.R)

## What this script is for

This R script is intended as a starting point for contributors who want to:

- identify relevant clinical codes for a phenotype
- document inclusion and exclusion logic
- generate a structured codelist for review
- adapt the workflow for a new phenotype

## How to use it

1. Download the template R script.
2. Open it in R or RStudio.
3. Replace the example phenotype content with your own phenotype definition.
4. Update the inclusion and exclusion logic as needed.
5. Run the script and review the output carefully.
6. Save the resulting codelist and associated metadata.
7. Upload your codelist and supporting files to your repository.

## What you should customise

Before using the script, you should review and adapt:

- the phenotype name
- the code search terms
- the inclusion criteria
- the exclusion criteria
- any reviewer notes
- metadata describing dataset, coding system, and intended use

## Recommended supporting files

When contributing a codelist, it is helpful to include:

- the generated codelist file
- a metadata file describing the phenotype
- the codelist-generating script
- a citation file (`CITATION.cff`) where possible

## Notes for contributors

Please treat this script as a template rather than a final validated definition.

All generated codelists should still be checked for:

- correctness
- completeness
- reproducibility
- clinical and research appropriateness where relevant