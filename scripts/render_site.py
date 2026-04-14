#!/usr/bin/env python3
import argparse
import html
import json
import pathlib
import re
import shutil
from collections import defaultdict

import yaml


def slug_anchor(text: str) -> str:
    text = text.strip().lower()
    text = text.replace("/", "-")
    text = text.replace(" ", "-")
    return text


def load_yaml_dict(path: pathlib.Path) -> dict:
    try:
        data = yaml.safe_load(path.read_text(encoding="utf-8")) or {}
        return data if isinstance(data, dict) else {}
    except Exception:
        return {}


def ensure_dir(path: pathlib.Path) -> None:
    path.mkdir(parents=True, exist_ok=True)


def reset_generated_dirs(docs: pathlib.Path) -> None:
    for subdir in ["phenotypes", "csv", "r", "cff"]:
        target = docs / subdir
        if target.exists():
            shutil.rmtree(target)
        target.mkdir(parents=True, exist_ok=True)


def read_text_if_exists(path: pathlib.Path) -> str:
    if not path.exists():
        return ""
    try:
        return path.read_text(encoding="utf-8", errors="ignore")
    except Exception:
        return ""


def nonempty(value) -> bool:
    return isinstance(value, str) and value.strip() and value.strip().lower() != "unknown"


def any_nonempty_str(values) -> bool:
    return any(nonempty(v) for v in values if isinstance(v, str))


STAR_RULES = [
    (1, "Dataset used", "dataset_used"),
    (2, "Version information", "version_info"),
    (3, "Inclusion / exclusion criteria in generating script", "incl_excl_script"),
    (4, "Pre-print / publication / protocol information", "publication_info"),
    (5, "Exposure / outcome / covariate flag", "phenotype_role"),
    (6, "Clinical or alternative reviewer information", "reviewer_info"),
    (7, "Citation file (.cff)", "citation_file"),
    (8, "Coding system information", "coding_system_info"),
    (9, "Brief phenotype description and usage", "description_usage"),
]


def file_contains_keywords(path: pathlib.Path, keywords: list[str]) -> bool:
    text = read_text_if_exists(path).lower()
    return any(k.lower() in text for k in keywords)


def directory_has_cff(phen_path: pathlib.Path) -> pathlib.Path | None:
    for p in sorted(phen_path.iterdir()):
        if p.is_file() and p.suffix.lower() == ".cff":
            return p
    cff = phen_path / "CITATION.cff"
    if cff.exists():
        return cff
    return None


def infer_star_flags(meta: dict, phen_path: pathlib.Path) -> dict[str, bool]:
    """
    Hybrid approach:
    - prefer explicit metadata fields when they exist
    - fall back to heuristics using existing files so current phenotypes get credit where possible
    """
    documentation = meta.get("documentation", {}) if isinstance(meta.get("documentation"), dict) else {}
    evidence = documentation.get("evidence", {}) if isinstance(documentation.get("evidence"), dict) else {}
    review = documentation.get("review", {}) if isinstance(documentation.get("review"), dict) else {}

    supplemental_text = "\n".join(
        [
            read_text_if_exists(phen_path / "SUPPLEMENTAL.md"),
            read_text_if_exists(phen_path / "README.md"),
            read_text_if_exists(phen_path / "readme.md"),
            read_text_if_exists(phen_path / "SOURCE.txt"),
        ]
    ).lower()

    # Criterion 1: dataset used
    dataset_used = False
    dataset_context = meta.get("dataset_context", [])
    if isinstance(dataset_context, list):
        for row in dataset_context:
            if isinstance(row, dict) and nonempty(row.get("dataset")):
                dataset_used = True
                break
    if not dataset_used and nonempty(meta.get("dataset_type")):
        dataset_used = True

    # Criterion 2: version info
    version_info = nonempty(meta.get("version"))

    # Criterion 3: inclusion/exclusion criteria in generating script
    incl_excl_script = documentation.get("inclusion_exclusion_in_script") is True
    code_dir = phen_path / "code"
    if not incl_excl_script and code_dir.exists():
        script_keywords = [
            "inclusion criteria",
            "exclusion criteria",
            "include",
            "exclude",
            "included",
            "excluded",
        ]
        for script_path in list(code_dir.glob("*.R")) + list(code_dir.glob("*.py")):
            if file_contains_keywords(script_path, script_keywords):
                incl_excl_script = True
                break

    # Criterion 4: preprint / publication / protocol information
    publication_info = False
    if nonempty(evidence.get("kind")) or nonempty(evidence.get("title")) or nonempty(evidence.get("url")) or nonempty(evidence.get("doi")):
        publication_info = True
    elif nonempty(meta.get("provenance", {}).get("source_citation") if isinstance(meta.get("provenance"), dict) else None):
        publication_info = True
    else:
        publication_keywords = ["preprint", "published", "publication", "protocol", "medrxiv", "biorxiv", "doi"]
        publication_info = any(k in supplemental_text for k in publication_keywords)

    # Criterion 5: exposure / outcome / covariate flag
    phenotype_role = False
    if documentation.get("phenotype_role") in {"exposure", "outcome", "covariate"}:
        phenotype_role = True
    else:
        tags = [t.strip().lower() for t in meta.get("tags", []) if isinstance(t, str)]
        phenotype_role = any(t in {"exposure", "outcome", "covariate", "covariates"} for t in tags)

    # Criterion 6: reviewer info
    reviewer_info = False
    if nonempty(review.get("status")) and (
        nonempty(review.get("reviewer_name")) or nonempty(review.get("explanation"))
    ):
        reviewer_info = True
    else:
        review_keywords = [
            "clinical review",
            "clinically reviewed",
            "clinical reviewer",
            "reviewed by clinician",
            "not reviewed by a clinician",
            "alternative reviewer",
        ]
        reviewer_info = any(k in supplemental_text for k in review_keywords)

    # Criterion 7: CFF file
    citation_file = directory_has_cff(phen_path) is not None

    # Criterion 8: coding system info
    coding_system_info = False
    coding_systems = meta.get("coding_systems", [])
    if isinstance(coding_systems, list):
        for row in coding_systems:
            if isinstance(row, dict) and nonempty(row.get("system")):
                coding_system_info = True
                break
    if not coding_system_info and nonempty(meta.get("coding_system")):
        coding_system_info = True

    # Criterion 9: brief description and usage
    description_usage = nonempty(meta.get("description"))
    if not description_usage:
        description_usage = nonempty(documentation.get("usage_notes"))

    return {
        "dataset_used": dataset_used,
        "version_info": version_info,
        "incl_excl_script": incl_excl_script,
        "publication_info": publication_info,
        "phenotype_role": phenotype_role,
        "reviewer_info": reviewer_info,
        "citation_file": citation_file,
        "coding_system_info": coding_system_info,
        "description_usage": description_usage,
    }


def render_star_html(flags: dict[str, bool]) -> str:
    parts = []
    score = sum(bool(flags.get(key)) for _, _, key in STAR_RULES)

    for number, label, key in STAR_RULES:
        if flags.get(key):
            title = html.escape(f"{number}. {label}")
            parts.append(
                f'<span class="meta-star meta-star-{number}" title="{title}" aria-label="{title}">★</span>'
            )

    star_markup = "".join(parts) if parts else '<span class="meta-note">No metadata stars yet</span>'
    return (
        '<div class="metadata-summary">'
        f'<span class="metadata-score">{score}/9</span>'
        f'<span class="metadata-stars">{star_markup}</span>'
        "</div>"
    )


def render_star_legend_html() -> list[str]:
    lines = [
        "## Metadata star legend",
        "",
        '<div class="metadata-legend">',
    ]
    for number, label, _ in STAR_RULES:
        lines.append(f'<div><span class="meta-star meta-star-{number}">★</span> {number}. {label}</div>')
    lines.extend(["</div>", ""])
    return lines


def main() -> None:
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
    items = catalog.get("items", [])
    if not isinstance(items, list):
        items = []

    ensure_dir(docs)
    reset_generated_dirs(docs)

    dataset_code_groups = defaultdict(lambda: defaultdict(list))
    code_display_names = {}

    for item in items:
        if not isinstance(item, dict):
            continue

        dataset_type = item.get("dataset_type") or "Unknown"
        code_name = item.get("code_name") or item.get("group") or "ungrouped"
        display_name = (
            item.get("display_name")
            or item.get("title")
            or item.get("group")
            or code_name
            or "Ungrouped"
        )

        dataset_code_groups[dataset_type][code_name].append(item)

        if code_name not in code_display_names or not code_display_names[code_name]:
            code_display_names[code_name] = display_name

    dataset_type_names = sorted(dataset_code_groups.keys(), key=lambda x: str(x).lower())

    for dataset_type in dataset_type_names:
        code_names = sorted(dataset_code_groups[dataset_type].keys(), key=lambda x: str(x).lower())
        for code_name in code_names:
            dataset_code_groups[dataset_type][code_name] = sorted(
                dataset_code_groups[dataset_type][code_name],
                key=lambda it: (
                    str(it.get("display_name") or "").lower(),
                    str(it.get("group") or "").lower(),
                    str(it.get("title") or it.get("id") or "").lower(),
                    str(it.get("id") or ""),
                ),
            )

    catalog_lines = [
        "# Catalog",
        "",
        f"Total phenotypes: **{catalog.get('count', len(items))}**",
        "",
        "This catalog shows a colored metadata-star summary for each codelist.",
        "",
    ]
    catalog_lines.extend(render_star_legend_html())

    catalog_lines.extend(
        [
            "## Browse",
            "",
        ]
    )

    for dataset_type in dataset_type_names:
        dataset_anchor = slug_anchor(dataset_type)
        catalog_lines.append(f"- [{dataset_type}](#{dataset_anchor})")

        code_names = sorted(dataset_code_groups[dataset_type].keys(), key=lambda x: str(x).lower())
        for code_name in code_names:
            display_name = code_display_names.get(code_name, code_name)
            code_anchor = slug_anchor(display_name)
            catalog_lines.append(f"    - [{display_name}](#{code_anchor})")

    catalog_lines.append("")

    for dataset_type in dataset_type_names:
        catalog_lines.append(f"## {dataset_type}")
        catalog_lines.append("")

        code_names = sorted(dataset_code_groups[dataset_type].keys(), key=lambda x: str(x).lower())

        for code_name in code_names:
            display_name = code_display_names.get(code_name, code_name)

            catalog_lines.append(f"### {display_name}")
            catalog_lines.append("")
            catalog_lines.append("| ID | Title | Group | Coding System | Status | Version | Metadata | Downloads |")
            catalog_lines.append("|---|---|---|---|---|---|---|---|")

            for item in dataset_code_groups[dataset_type][code_name]:
                pid = item.get("id")
                if not pid:
                    continue

                title = item.get("title") or item.get("display_name") or pid
                status = item.get("status", "draft")
                version = item.get("version", "0.0.0")
                group = item.get("group", "Ungrouped")
                coding_system = item.get("coding_system", "Unknown")

                phen_rel_path = item.get("path")
                if not phen_rel_path:
                    continue

                phen_path = data_repo / phen_rel_path
                meta_path = phen_path / "metadata.yml"
                csv_path = phen_path / "codelist.csv"
                code_dir = phen_path / "code"

                if not meta_path.exists() or not csv_path.exists():
                    continue

                out_csv = docs / "csv" / f"{pid}.csv"
                shutil.copy2(csv_path, out_csv)

                r_links = []
                out_r_dir = docs / "r" / pid
                if code_dir.exists() and code_dir.is_dir():
                    out_r_dir.mkdir(parents=True, exist_ok=True)
                    for rp in sorted(code_dir.glob("*.R")):
                        dest_r = out_r_dir / rp.name
                        shutil.copy2(rp, dest_r)
                        r_links.append(f"../r/{pid}/{rp.name}")

                cff_link = None
                cff_path = directory_has_cff(phen_path)
                if cff_path:
                    out_cff = docs / "cff" / f"{pid}.cff"
                    shutil.copy2(cff_path, out_cff)
                    cff_link = f"../cff/{pid}.cff"

                meta = load_yaml_dict(meta_path)
                star_flags = infer_star_flags(meta, phen_path)
                star_html = render_star_html(star_flags)

                display_name = item.get("display_name") or group or code_name
                page_title = title if title and title != pid else display_name
                page = [
                    f"# {page_title}",
                    "",
                    star_html,
                    "",
                    f"**Display name:** `{display_name}`  ",
                    f"**Dataset type:** `{dataset_type}`  ",
                    f"**Code name:** `{code_name}`  ",
                    f"**Group:** `{group}`  ",
                    f"**ID:** `{pid}`  ",
                    f"**Status:** `{status}`  ",
                    f"**Version:** `{version}`  ",
                    f"**Coding system:** `{coding_system}`  ",
                ]

                documentation = meta.get("documentation", {}) if isinstance(meta.get("documentation"), dict) else {}

                if documentation.get("phenotype_role"):
                    page.append(f"**Phenotype role:** `{documentation.get('phenotype_role')}`  ")

                if meta.get("created"):
                    page.append(f"**Created:** `{meta.get('created')}`  ")
                if meta.get("imported"):
                    page.append(f"**Imported:** `{meta.get('imported')}`  ")
                if meta.get("updated"):
                    page.append(f"**Updated:** `{meta.get('updated')}`  ")

                page.extend(
                    [
                        "",
                        "## Description",
                        meta.get("description", ""),
                        "",
                    ]
                )

                if documentation.get("usage_notes"):
                    page.extend(
                        [
                            "## Usage notes",
                            str(documentation.get("usage_notes")),
                            "",
                        ]
                    )

                page.extend(
                    [
                        "## Downloads",
                        f"- [Download codelist CSV](../csv/{pid}.csv)",
                    ]
                )

                if r_links:
                    for rl in r_links:
                        fname = rl.split("/")[-1]
                        page.append(f"- [Download R script: {fname}]({rl})")

                if cff_link:
                    page.append(f"- [Download citation file (.cff)]({cff_link})")

                page.extend(render_star_legend_html())

                page.extend(
                    [
                        "## Metadata",
                        "```yaml",
                        yaml.safe_dump(meta, sort_keys=False, allow_unicode=True).strip(),
                        "```",
                        "",
                    ]
                )

                out_page = docs / "phenotypes" / f"{pid}.md"
                out_page.write_text("\n".join(page) + "\n", encoding="utf-8")

                downloads = [f"[csv](csv/{pid}.csv)"]
                if r_links:
                    downloads.append(f"[R](r/{pid}/)")
                if cff_link:
                    downloads.append(f"[cff](cff/{pid}.cff)")

                catalog_lines.append(
                    f"| [`{pid}`](phenotypes/{pid}.md) | {title} | {group} | {coding_system} | {status} | {version} | <div class=\"catalog-meta-cell\">{star_html}</div> | {', '.join(downloads)} |"
                )

            catalog_lines.append("")

    (docs / "catalog.md").write_text("\n".join(catalog_lines) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()