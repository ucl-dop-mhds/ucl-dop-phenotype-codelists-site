# UCL DoP Phenotype Codelists

Welcome to the UCL Division of Psychiatry’s Mental Health Data Science team’s GitHub.
This repository stores phenotype codelists used by our team members.

## 🗂️ Repository layout and codelist browsing

Use the [**Catalog**](catalog.md) to browse codelists by dataset type and phenotype grouping.

You can also:

- open an individual phenotype page
- download the codelist as CSV
- download the R code used to generate the codelist when available
  
## ✍️ How to contribute to the repository

You must have a GitHub account to contribute to the repository.

Your repository must include a codelist file such as a `.txt` file or CSV-derived phenotype output.  
Please make sure that your file name or folder name contains:

- a **non-abbreviated term** for the phenotype  
  for example: `antidepressant`, not `AD`, unless it is a standard standalone abbreviation such as `SMI`
- and the words **`codelist`** or **`code list`**

This allows the GitHub ingestion workflow to automatically scrape the codelist from your repository.

## ✅ Documentation / metadata criteria checklist

Any codelist submitted to the MHDS GitHub will be assessed for **metadata completeness** using the checklist below.

You can still contribute a codelist even if not all metadata items are currently available.

### Metadata criteria

1. Information on dataset used (for example, CPRD Aurum vs CPRD Gold)
2. Version information
3. Inclusion and exclusion criteria documented in the code-generating script
4. Information about a pre-print, published paper, or protocol
5. A field flagging the codelist as an exposure, outcome, or covariate
6. A named clinical reviewer, or an explanation of why the codelist was not clinically reviewed or was reviewed by an alternative reviewer
7. A citation file (`.cff`)
8. Information on coding system
9. A brief description of the phenotype and how it should be used

### Recommended files to include

- 📄 Recommended phenotype metadata file
- 💻 Codelist-generating script

### Metadata stars

This website uses a **9-star metadata completeness system**.

Each colored star corresponds to one metadata criterion. A codelist only displays the stars for criteria that it satisfies.  
This is **not** an assessment of phenotype validity or codelist quality. It is only an assessment of how well the codelist is documented.

<div class="metadata-legend">
  <div><span class="meta-star meta-star-1">★</span> Dataset used</div>
  <div><span class="meta-star meta-star-2">★</span> Version information</div>
  <div><span class="meta-star meta-star-3">★</span> Inclusion / exclusion criteria in script</div>
  <div><span class="meta-star meta-star-4">★</span> Pre-print / publication / protocol</div>
  <div><span class="meta-star meta-star-5">★</span> Exposure / outcome / covariate flag</div>
  <div><span class="meta-star meta-star-6">★</span> Clinical or alternative reviewer information</div>
  <div><span class="meta-star meta-star-7">★</span> Citation file (.cff)</div>
  <div><span class="meta-star meta-star-8">★</span> Coding system information</div>
  <div><span class="meta-star meta-star-9">★</span> Brief phenotype description and usage</div>
</div>

## 🧪 How to use the code lists in the repository

Our codelist-generating script template generally follows current standard practice for primary care codelist creation, with additional steps commonly used by researchers contributing to this GitHub.

Instructions are available within the script.  
You may re-upload your customised codelist-generating script alongside your repository.

## 📚 Recommendations for reference code lists

When choosing a previously generated code list to compare your newly generated code list with, make sure that:

1. the older code list has been clinically reviewed, and
2. its inclusion and exclusion criteria are clear.

Useful reference sources include:

- HDR UK Phenotype Library
- CALIBER Phenotype Library
- CEHRAM
- DATAMIND GitHub
- other collaborator GitHub repositories as appropriate

## 📝 Citations

### For contributors

Adding a citation file (`CITATION.cff`) to your repository allows other researchers to download citation metadata directly from GitHub.

### For users

If you use one of our team member’s codelists to create your own, best practice is to credit both:

- the GitHub repository
- and the associated paper or preprint, where available

If a `.cff` file is not available, example repository citation formats are below.

#### APA (7th edition)

Author/Organization. (Year of release or last update). *Title of repository* (Version if applicable) [Computer software]. URL

#### Vancouver

Author/Organization. *Title of repository* [Internet]. Place of publication: Publisher; Year [cited Year Month Day]. Available from: URL

#### AMA

Author/Organization. *Title*. Year of release or last update. Accessed Month Day, Year. URL
