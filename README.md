# ucl-dop-phenotype-codelists-site (MVP)

Static site (MkDocs) that renders phenotype pages from the hub repo:
`dop-mhds/ucl-dop-phenotype-codelists`

## How it works
The GitHub Actions workflow:
1) checks out this repo
2) checks out the hub repo into `_data`
3) runs `scripts/render_site.py` to generate markdown pages and copy CSVs
4) builds and deploys to GitHub Pages

## Local preview
```bash
python -m pip install -r requirements.txt
python scripts/render_site.py --data-repo ../ucl-dop-phenotype-codelists --docs docs
mkdocs serve
```
