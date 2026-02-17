import argparse
import json
import pathlib
import shutil
from collections import defaultdict

import yaml


def slug_anchor(text: str) -> str:
    return text.strip().lower().replace(" ", "-").replace("/", "-")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--data-repo", required=True, help="Path to checked out hub repo")
    ap.add_argument("--docs", required=True, help="Path to mkdocs docs/ directory")
    args = ap.parse_args()

    data_repo = pathlib.Path(args.data_repo).resolve()
    docs = pathlib.Path(args.docs).resolve()

    catalog_path = data_repo / "catalog" / "catalog.json"
    if not catalog_path.exists():
        raise FileNotFoundError(
            f"Missing catalog at {catalog_path}. Run hub catalog/build_catalog.py first."
        )

    catalog = json.loads(catalog_path.read_text(encoding="utf-8"))

    # Ensure folders exist
    (docs / "phenotypes").mkdir(parents=True, exist_ok=True)
    (docs / "csv").mkdir(parents=True, exist_ok=True)
    (docs / "r").mkdir(parents=True, exist_ok=True)

    # Group items
    groups = defaultdict(list)
    for item in catalog.get("items", []):
        g = item.get("group") or "Ungrouped"
        groups[g].append(item)

    # Sort groups and items
    group_names = sorted(groups.keys(), key=lambda x: x.lower())
    for g in group_names:
        groups[g] = sorted(groups[g], key=lambda it: (it.get("title", "").lower(), it.get("id", "")))

    # Build Catalog page
    catalog_lines = [
        "# Catalog",
        "",
        f"Total phenotypes: **{catalog.get('count', 0)}**",
        "",
        "## Groups",
    ]
    for g in group_names:
        catalog_lines.append(f"- [{g}](#{slug_anchor(g)})")
    catalog_lines.append("")

    # Build Phenotypes index page
    phen_index_lines = [
        "# Phenotypes",
        "",
        "Browse phenotype pages by group.",
        "",
    ]

    for g in group_names:
        catalog_lines.append(f"## {g}")
        catalog_lines.append("")
        catalog_lines.append("| ID | Title | Status | Version | Downloads |")
        catalog_lines.append("|---|---|---|---|---|")

        phen_index_lines.append(f"## {g}")

        for item in groups[g]:
            pid = item["id"]
            title = item.get("title", pid)
            status = item.get("status", "draft")
            version = item.get("version", "0.0.0")

            phen_path = data_repo / item["path"]
            meta_path = phen_path / "metadata.yml"
            csv_path = phen_path / "codelist.csv"
            code_dir = phen_path / "code"

            # Copy CSV for stable download URLs
            out_csv = docs / "csv" / f"{pid}.csv"
            shutil.copy2(csv_path, out_csv)

            # Copy R scripts (if present) into docs/r/<pid>/
            r_links = []
            out_r_dir = docs / "r" / pid
            if code_dir.exists() and code_dir.is_dir():
                out_r_dir.mkdir(parents=True, exist_ok=True)
                for rp in sorted(code_dir.glob("*.R")):
                    dest_r = out_r_dir / rp.name
                    shutil.copy2(rp, dest_r)
                    r_links.append(f"../r/{pid}/{rp.name}")

            # Render phenotype page
            meta = yaml.safe_load(meta_path.read_text(encoding="utf-8")) or {}
            page = []
            page.append(f"# {title}")
            page.append("")
            page.append(f"**Group:** `{g}`  ")
            page.append(f"**ID:** `{pid}`  ")
            page.append(f"**Status:** `{status}`  ")
            page.append(f"**Version:** `{version}`  ")
            if meta.get("created"):
                page.append(f"**Created:** `{meta.get('created')}`  ")
            if meta.get("imported"):
                page.append(f"**Imported:** `{meta.get('imported')}`  ")
            if meta.get("updated"):
                page.append(f"**Updated:** `{meta.get('updated')}`  ")
            page.append("")

            page.append("## Description")
            page.append(meta.get("description", ""))
            page.append("")

            page.append("## Downloads")
            page.append(f"- [Download codelist CSV](../csv/{pid}.csv)")

            page.append("## Source")
            if r_links:
                page.append("R code used to generate this codelist:")
                for rl in r_links:
                    fname = rl.split("/")[-1]
                    page.append(f"  [{fname}]({rl})")
            else:
                page.append("R code used to generate this codelist: *(not available)*")
            page.append("")

            page.append("## Metadata")
            page.append("```yaml")
            page.append(yaml.safe_dump(meta, sort_keys=False, allow_unicode=True).strip())
            page.append("```")

            out_page = docs / "phenotypes" / f"{pid}.md"
            out_page.write_text("\n".join(page) + "\n", encoding="utf-8")

            # Table downloads column
            downloads_cell = f"[csv](csv/{pid}.csv)"
            if r_links:
                # link to the folder (MkDocs will list file if clicked, or users can click from phenotype page)
                downloads_cell += f", [R](r/{pid}/)"

            catalog_lines.append(
                f"| [`{pid}`](phenotypes/{pid}.md) | {title} | {status} | {version} | {downloads_cell} |"
            )
            phen_index_lines.append(f"- [`{pid}`](./{pid}.md) — {title}")

        catalog_lines.append("")
        phen_index_lines.append("")

    (docs / "catalog.md").write_text("\n".join(catalog_lines) + "\n", encoding="utf-8")
    (docs / "phenotypes" / "index.md").write_text("\n".join(phen_index_lines) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
