# UCL Division of Psychiatry (DoP) Phenotype Codelists

Welcome to the UCL Division of Psychiatry’s Mental Health Data Science team’s GitHub! This repository stores phenotype code lists used by our team members.

## 🗂️ Repository layout and code list browsing

Use the [**Catalogue**](catalog.md) to browse code lists by dataset type and phenotype grouping.

You can also:

- open an individual phenotype page
- download the code list as CSV
- download the R code used to generate the code list when available

## 💻 Generate a codelist using our R template

We provide an example R script that can be adapted to generate a phenotype codelist.

You can:

- download the [**R codelist generation template**](https://github.com/Janeh19/Code-list-generating-template/raw/main/Depression_codelist_script_example.R)
- read the [**step-by-step guide for generating a codelist**](generate-codelist.md)
  
## ✍️ How to contribute to the repository

You must have a GitHub account to contribute to the repository. To contribute codelists to this repository, please email your GitHub repository name to **dop.mhds@ucl.ac.uk**.

Your repository must include a code list file such as a `.txt` file or CSV-derived phenotype output, and a code list generating script (R, STATA, or otherwise).  Please make sure that your file name or folder name contains:

- a **non-abbreviated term** for the phenotype  
  for example: `antidepressant`, not `AD`, unless it is a standard standalone abbreviation such as `SMI`
- and the words `codelist` or `code list`

This allows the GitHub ingestion workflow to automatically scrape the code list from your repository.

## ✅ Documentation / metadata criteria checklist

Any code list submitted to the MHDS GitHub will be assessed for **metadata quality** using the checklist below. **You will still be able to contribute to the MHDS Github repository even if you do not include all the items from the checklist in your metadata.**

### Metadata checklist

1. Information on database used (e.g., CPRD Aurum vs CPRD Gold, SNOMED, HES)
2. Code list version information
3. Inclusion and exclusion criteria documented in the code list generating script
4. Information about a pre-print, published paper, or protocol
5. A field flagging the code list as an exposure, outcome, or covariate
6. A named clinical reviewer, or an explanation of why the code list was not clinically reviewed or was reviewed by an alternative reviewer
7. A citation file (`.cff`) - *please refer to the citation section below to see how to generate a .cff file*
8. Information on coding system
9. A brief description of the phenotype and how it should be used

OR 

1. Recommended phenotype metadata file containing the information above (.txt) – *please refer to the [Codelist Generation page and script]( generate-codelist.md) to see how to generate a metadata file*
2. A citation file (`.cff`) - *please refer to the citation section below to see how to generate a .cff file*
3. Code list generating script


### Metadata stars

This website uses a **9-star metadata completeness system**, which indicate the quality of documentation based on the criteria listed below. For example, repositories that fulfil **4 out of 9 criteria **will receive a metadata score of 4/9 so that users can check what type of information is readily available. Please also note that better data documentation will increase the reproducibility of your research and help others readily cite your GitHub or publication in future studies (refer to the section on citation below). 

**Please note that the 5-start system is not an assessment of data quality (e.g., how “valid” the code lists are) but an assessment on how well the data is documented.** 

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

Our code list generating script template generally follows [current standard practice for primary care code list creation](https://bmjopen.bmj.com/content/bmjopen/7/11/e019637.full.pdf), with some additional steps commonly used within researchers in our team and other teams in UCL (see CEHRAM GitHub). 

Instructions are available within the script. You may re-upload your customised code list generating script alongside your repository. You can access the standard code list script here. 

## 📚 Recommendations for reference code lists

When choosing a previously generated code list to compare your newly generated code list with, make sure that:

1. the reference code list has been clinically reviewed, and
2. its inclusion and exclusion criteria are clear.

You can find these code lists in our MHDS GitHub, or from other reputable sources such as the following: 

- HDRUK Phenotype Library
- CALIBER Phenotype Library
  
You can also search the GitHub repos of our collaborator to see if there is a code list better suited for your research needs: 

- CEHRAM
- DATAMIND GitHub

## 📝 Citations

### For contributors

Adding a citation file (`CITATION.cff`) to your repository will automatically generate a sidebar where other researchers can download information for their citation indices (E.g., EndNote). For further instructions on how to create a citation file, please refer to [this guideline](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-citation-files) published by GitHub.  

### For users

If you use one of our team member’s code lists to create your own, **it is best practice credit to their GitHub repository and their published paper by citing it in your own manuscript or publication**. When citing a researcher’s repository, you can either download the .cff file provided by the author of the repository. If these files are not available, please refer to examples on how to cite a GitHub repository are listed below:

#### APA (7th edition)

Author/Organization. (Year of release or last update). *Title of repository* (Version if applicable) [Computer software]. URL

#### Vancouver

Author/Organization. *Title of repository* [Internet]. Place of publication: Publisher; Year [cited Year Month Day]. Available from: URL

#### AMA

Author/Organization. *Title*. Year of release or last update. Accessed Month Day, Year. URL
