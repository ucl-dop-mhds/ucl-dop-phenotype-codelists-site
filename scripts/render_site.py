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
    for subdir in ["phenotypes", "csv", "source", "r", "cff"]:
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


def render_star_html(flags: dict[str, bool], label_score: bool = False) -> str:
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
        f'<span class="metadata-score">{"Metadata score: " if label_score else ""}{score}/9</span>'
        f'<span class="metadata-stars">{star_markup}</span>'
        "</div>"
    )


def render_star_legend_html() -> list[str]:
    lines = [
        "### Metadata star legend",
        "",
        '<div class="metadata-legend">',
    ]
    for number, label, _ in STAR_RULES:
        lines.append(f'<div><span class="meta-star meta-star-{number}">★</span> {number}. {label}</div>')
    lines.extend(["</div>", ""])
    return lines



def clean_text(value) -> str | None:
    if value is None:
        return None
    if isinstance(value, str):
        text = value.strip()
        if not text or text.lower() == "unknown":
            return None
        return text
    text = str(value).strip()
    return text or None


def render_dl_rows(rows: list[tuple[str, str | None]]) -> str:
    items = []
    for label, value in rows:
        cleaned = clean_text(value)
        if cleaned is None:
            cleaned = "NA"
        items.append(
            '<div class="phenotype-meta-row">'
            f'<dt>{html.escape(label)}</dt>'
            f'<dd>{cleaned}</dd>'
            '</div>'
        )
    return '<div class="phenotype-meta-grid">' + ''.join(items) + '</div>'



def source_repo_from_dicts(*dicts: dict) -> tuple[str, str, str, str] | None:
    """Return (owner, repo, first_repo_word, owner/repo). Prefer PROVENANCE.yml."""
    candidates = []

    for d in dicts:
        if not isinstance(d, dict):
            continue

        provenance = d.get("provenance", {}) if isinstance(d.get("provenance"), dict) else {}

        candidates.extend([
            d.get("source_repository"),
            provenance.get("source_repository"),
            d.get("description"),
        ])

    for value in candidates:
        text = clean_text(value)
        if not text:
            continue

        match = re.search(r"(?:github\.com/)?([A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+)", text)
        if not match:
            match = re.search(r"Auto-ingested from\s+([A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+)\s*\(", text)

        if match:
            full = match.group(1).rstrip("/")
            owner, repo = full.split("/", 1)
            first_word = re.split(r"[-_\s]+", repo.strip())[0] or repo
            return owner, repo, first_word, full

    return None


def format_source_repo_link(*sources: dict) -> str:
    repo = extract_source_repository(*sources)

    if not repo:
        return "NA"

    url = f"https://github.com/{repo}"
    return f'<a href="{html.escape(url, quote=True)}">{html.escape(repo)}</a>'


def original_source_extension(provenance: dict) -> str:
    """
    Use the original source path extension first, because this is the true
    file extension users expect to download.
    """
    source_path = clean_text(provenance.get("source_path"))
    if source_path:
        suffix = pathlib.Path(source_path).suffix
        if suffix:
            return suffix.lower()

    fmt = clean_text(provenance.get("source_format"))
    if fmt:
        fmt = fmt.lower().lstrip(".")
        if fmt in {"txt", "csv", "tsv", "xlsx", "xls", "json", "r", "md"}:
            return f".{fmt}"

    return ".txt"

def format_catalog_short_id(pid: str, code_name: str, meta: dict) -> str:
    info = source_repo_from_metadata(meta)
    if not info:
        return code_name or pid
    owner, _, first_word, _ = info
    return f"{code_name}-{owner}-{first_word}"

def render_section(title: str, rows: list[tuple[str, str | None]]) -> list[str]:
    return [
        f"## {title}",
        "",
        render_dl_rows(rows),
        "",
    ]


def render_download_list(downloads: list[tuple[str, str] | tuple[str, str, str]]) -> str:
    items = []

    for item in downloads:
        if len(item) == 3:
            label, href, download_name = item
            download_attr = f' download="{html.escape(download_name, quote=True)}"'
        else:
            label, href = item
            download_attr = ""

        items.append(
            '<li>'
            f'<a href="{html.escape(href, quote=True)}"{download_attr}>'
            f'{html.escape(label)}'
            '</a>'
            '</li>'
        )

    return '<ul class="phenotype-download-list">' + ''.join(items) + '</ul>'


def format_contacts(contacts) -> str | None:
    if not isinstance(contacts, list):
        return None
    parts = []
    for contact in contacts:
        if not isinstance(contact, dict):
            continue
        name = clean_text(contact.get("name"))
        if not name:
            continue
        extras = []
        team = clean_text(contact.get("team"))
        email = clean_text(contact.get("email"))
        if team:
            extras.append(team)
        if email:
            extras.append(email)
        if extras:
            parts.append(f"{name} ({', '.join(extras)})")
        else:
            parts.append(name)
    return '; '.join(parts) if parts else None


def format_dataset_context(dataset_context) -> str | None:
    if not isinstance(dataset_context, list):
        return None
    parts = []
    for row in dataset_context:
        if not isinstance(row, dict):
            continue
        dataset = clean_text(row.get("dataset")) or "Unknown dataset"
        setting = clean_text(row.get("setting"))
        population_notes = clean_text(row.get("population_notes"))
        segment = dataset
        if setting:
            segment += f" ({setting})"
        if population_notes:
            segment += f" - {population_notes}"
        parts.append(segment)
    return '; '.join(parts) if parts else None


def format_coding_systems(coding_systems) -> str | None:
    if not isinstance(coding_systems, list):
        return None
    parts = []
    for row in coding_systems:
        if not isinstance(row, dict):
            continue
        system = clean_text(row.get("system")) or "Unknown"
        version = clean_text(row.get("version"))
        notes = clean_text(row.get("notes"))
        segment = system
        if version:
            segment += f" (version: {version})"
        if notes:
            segment += f" - {notes}"
        parts.append(segment)
    return '; '.join(parts) if parts else None


def format_evidence(evidence: dict) -> str | None:
    if not isinstance(evidence, dict):
        return None
    bits = []
    kind = clean_text(evidence.get("kind"))
    title = clean_text(evidence.get("title"))
    doi = clean_text(evidence.get("doi"))
    url = clean_text(evidence.get("url"))
    if kind:
        bits.append(kind.replace("_", " ").title())
    if title:
        bits.append(title)
    if doi:
        bits.append(f"DOI: {doi}")
    if url:
        bits.append(f'<a href="{html.escape(url, quote=True)}">{html.escape(url)}</a>')
    return ' | '.join(bits) if bits else None


def format_review(review: dict) -> str | None:
    if not isinstance(review, dict):
        return None
    status = clean_text(review.get("status"))
    reviewer_name = clean_text(review.get("reviewer_name"))
    reviewer_role = clean_text(review.get("reviewer_role"))
    explanation = clean_text(review.get("explanation"))
    bits = []
    if status:
        bits.append(status.replace("_", " ").title())
    if reviewer_name and reviewer_role:
        bits.append(f"{reviewer_name} ({reviewer_role})")
    elif reviewer_name:
        bits.append(reviewer_name)
    elif reviewer_role:
        bits.append(reviewer_role)
    if explanation:
        bits.append(explanation)
    return ' | '.join(bits) if bits else None

def render_metadata_detail_html(
    flags: dict[str, bool],
    meta: dict,
    phen_path: pathlib.Path,
    provenance: dict,
    documentation: dict,
    evidence: dict,
    review: dict,
) -> str:
    phenotype_role = clean_text(documentation.get("phenotype_role"))
    formatted_role = phenotype_role.replace("_", " ").title() if phenotype_role else None

    usage_notes = clean_text(documentation.get("usage_notes"))
    description = clean_text(meta.get("description"))

    cff_path = directory_has_cff(phen_path)

    dataset_value = (
        format_dataset_context(meta.get("dataset_context"))
        or clean_text(meta.get("dataset_type"))
        or clean_text(provenance.get("dataset_type"))
    )

    coding_value = (
        format_coding_systems(meta.get("coding_systems"))
        or clean_text(meta.get("coding_system"))
        or clean_text(provenance.get("coding_system"))
    )

    evidence_value = (
        format_evidence(evidence)
        or clean_text(provenance.get("source_citation"))
    )

    review_value = format_review(review)

    role_value = formatted_role
    if not role_value:
        tags = [t.strip() for t in meta.get("tags", []) if isinstance(t, str) and t.strip()]
        role_tags = [t for t in tags if t.lower() in {"exposure", "outcome", "covariate", "covariates"}]
        role_value = ", ".join(role_tags) if role_tags else None

    desc_usage = []
    if description:
        desc_usage.append(description)
    if usage_notes:
        desc_usage.append(f"Usage notes: {usage_notes}")

    value_by_key = {
        "dataset_used": dataset_value,
        "version_info": clean_text(meta.get("version")),
        "incl_excl_script": "Found in generating script" if flags.get("incl_excl_script") else None,
        "publication_info": evidence_value,
        "phenotype_role": role_value,
        "reviewer_info": review_value,
        "citation_file": cff_path.name if cff_path else None,
        "coding_system_info": coding_value,
        "description_usage": " | ".join(desc_usage) if desc_usage else None,
    }

    lines = ['<div class="metadata-detail-list">']

    for number, label, key in STAR_RULES:
        achieved = bool(flags.get(key))
        star = "★" if achieved else "☆"
        value = value_by_key.get(key) or "NA"

        lines.append(
            '<div class="metadata-detail-row">'
            f'<span class="meta-star meta-star-{number}">{star}</span> '
            f'<strong>{html.escape(label)}:</strong> '
            f'{value if "<a " in str(value) else html.escape(str(value))}'
            '</div>'
        )

    lines.append("</div>")
    return "\n".join(lines)


def render_metadata_section(
    flags: dict[str, bool],
    meta: dict,
    phen_path: pathlib.Path,
    provenance: dict,
    documentation: dict,
    evidence: dict,
    review: dict,
) -> list[str]:
    return [
        "## Metadata",
        "",
        render_star_html(flags, label_score=True),
        "",
        "<br>",
        "",
        render_metadata_detail_html(flags, meta, phen_path, provenance, documentation, evidence, review),
        "",
    ]


def phenotype_anchor(dataset_type: str, display_name: str) -> str:
    return slug_anchor(f"{dataset_type}--{display_name}")


def extract_source_repository(*sources) -> str | None:
    """
    Find owner/repo from PROVENANCE.yml, metadata.yml, catalogue item,
    or any text field containing a GitHub repo.
    """
    candidates = []

    for source in sources:
        if not isinstance(source, dict):
            continue

        candidates.extend(
            [
                source.get("source_repository"),
                source.get("repository"),
                source.get("repo"),
                source.get("description"),
            ]
        )

        nested_provenance = source.get("provenance")
        if isinstance(nested_provenance, dict):
            candidates.extend(
                [
                    nested_provenance.get("source_repository"),
                    nested_provenance.get("repository"),
                    nested_provenance.get("repo"),
                    nested_provenance.get("description"),
                ]
            )

    for value in candidates:
        text = clean_text(value)
        if not text:
            continue

        text = text.strip().rstrip("/")

        # Full GitHub URL
        match = re.search(
            r"github\.com/([A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+)",
            text,
        )
        if match:
            return match.group(1).rstrip("/")

        # Plain owner/repo
        match = re.search(
            r"\b([A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+)\b",
            text,
        )
        if match:
            return match.group(1).rstrip("/")

    return None


def github_owner_from_repo(repo: str | None) -> str | None:
    repo = clean_text(repo)
    if not repo or "/" not in repo:
        return None
    return repo.split("/", 1)[0]


def github_first_repo_word(repo: str | None) -> str | None:
    repo = clean_text(repo)
    if not repo or "/" not in repo:
        return None

    repo_name = repo.split("/", 1)[1]
    repo_name = re.sub(r"[^A-Za-z0-9]+", "-", repo_name).strip("-")

    if not repo_name:
        return None

    return repo_name.split("-")[0]


def short_catalog_id(item: dict, provenance: dict, meta: dict | None = None) -> str:
    code_name = (
        clean_text(item.get("code_name"))
        or clean_text(item.get("display_name"))
        or clean_text(item.get("title"))
        or clean_text(item.get("group"))
        or clean_text(item.get("id"))
        or "phenotype"
    )

    repo = extract_source_repository(provenance, meta or {}, item)

    owner = github_owner_from_repo(repo)
    first_word = github_first_repo_word(repo)

    parts = [slug_anchor(code_name)]

    if owner:
        parts.append(slug_anchor(owner))

    if first_word:
        parts.append(slug_anchor(first_word))

    return "-".join(parts)


def render_codelist_preview_table(raw_path: pathlib.Path, max_rows: int = 100) -> str:
    """
    Render the original SOURCE.txt/SOURCE.* file as the preview.
    The first line of the raw file determines the column names.
    """
    import csv

    if not raw_path.exists():
        return "<p>No codelist preview available.</p>"

    try:
        sample = raw_path.read_text(encoding="utf-8", errors="ignore")
    except Exception:
        return "<p>No codelist preview available.</p>"

    if not sample.strip():
        return "<p>No codelist preview available.</p>"

    try:
        dialect = csv.Sniffer().sniff(sample[:4096], delimiters="\t,;|")
    except Exception:
        # Most SOURCE.txt files in this repo are tab-delimited.
        dialect = csv.excel_tab

    try:
        rows = list(csv.reader(sample.splitlines(), dialect))
    except Exception:
        return "<p>No codelist preview available.</p>"

    if not rows:
        return "<p>No codelist preview available.</p>"

    header = rows[0]
    body = rows[1:max_rows + 1]

    lines = [
        '<div class="codelist-preview">',
        '<table>',
        '<thead>',
        '<tr>',
    ]

    for col in header:
        lines.append(f"<th>{html.escape(str(col))}</th>")

    lines.extend([
        "</tr>",
        "</thead>",
        "<tbody>",
    ])

    for row in body:
        lines.append("<tr>")
        padded = row + [""] * max(0, len(header) - len(row))
        for value in padded[:len(header)]:
            lines.append(f"<td>{html.escape(str(value))}</td>")
        lines.append("</tr>")

    lines.extend([
        "</tbody>",
        "</table>",
        "</div>",
    ])

    if len(rows) - 1 > max_rows:
        lines.append(f"<p><em>Preview shows first {max_rows} rows from the original codelist file.</em></p>")

    return "\n".join(lines)


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
                key=lambda it: str(
                    it.get("display_name")
                    or it.get("title")
                    or it.get("id")
                    or ""
                ).lower(),
            )

    catalog_lines = [
        "# Catalogue",
        "",
        f"Total phenotypes: **{catalog.get('count', len(items))}**",
        "",
        "This catalogue shows a colored metadata-star summary for each codelist.",
        "",
        "!!! note \"Phenotype ID format\"",
        "    Catalogue IDs are shortened for readability. They are derived as: `phenotype-GitHubID-firstRepoWord`, where `GitHubID` is the owner of the spoke repository and `firstRepoWord` is the first word of the spoke repository name.",
        "",
    ]
    catalog_lines.extend(render_star_legend_html())

    catalog_lines.extend(
        [
            "## Browse",
            "",
            '<div class="browse-accordion">',
            "",
        ]
    )

    for dataset_type in dataset_type_names:
        dataset_anchor = slug_anchor(dataset_type)

        code_names = sorted(
            dataset_code_groups[dataset_type].keys(),
            key=lambda code_name: str(code_display_names.get(code_name, code_name)).lower(),
        )

        catalog_lines.append('<details class="browse-dataset">')
        catalog_lines.append(
            f'<summary>{html.escape(str(dataset_type))}</summary>'
        )
        catalog_lines.append('<ul class="browse-phenotypes">')

        for code_name in code_names:
            display_name = code_display_names.get(code_name, code_name)
            code_anchor = phenotype_anchor(dataset_type, display_name)
            catalog_lines.append(
                f'  <li><a href="#{code_anchor}">{html.escape(str(display_name))}</a></li>'
            )

        catalog_lines.append("</ul>")
        catalog_lines.append("</details>")
        catalog_lines.append("")

    catalog_lines.append("</div>")
    catalog_lines.append("")

    for dataset_type in dataset_type_names:
        catalog_lines.append(f"## {dataset_type}")
        catalog_lines.append("")

        code_names = sorted(
            dataset_code_groups[dataset_type].keys(),
            key=lambda x: str(code_display_names.get(x, x)).lower(),
        )

        for code_name in code_names:
            display_name = code_display_names.get(code_name, code_name)
            section_anchor = phenotype_anchor(dataset_type, display_name)

            catalog_lines.append(f'<a id="{section_anchor}"></a>')
            catalog_lines.append(f"### {display_name}")
            catalog_lines.append("")
            catalog_lines.append("| ID | Title | Database | Source | Metadata | Downloads |")
            catalog_lines.append("|---|---|---|---|---|---|")

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

                provenance_path = phen_path / "PROVENANCE.yml"
                provenance_file = load_yaml_dict(provenance_path)
                source_txt = phen_path / "SOURCE.txt"

                source_link = None
                source_ext = original_source_extension(provenance_file)
                if source_txt.exists():
                    out_source = docs / "source" / f"{pid}{source_ext}"
                    shutil.copy2(source_txt, out_source)
                    source_link = f"../../source/{pid}{source_ext}"

                r_links = []
                out_r_dir = docs / "r" / pid
                if code_dir.exists() and code_dir.is_dir():
                    out_r_dir.mkdir(parents=True, exist_ok=True)
                    for rp in sorted(code_dir.glob("*.R")):
                        dest_r = out_r_dir / rp.name
                        shutil.copy2(rp, dest_r)
                        r_links.append(f"../../r/{pid}/{rp.name}")

                cff_link = None
                cff_path = directory_has_cff(phen_path)
                if cff_path:
                    out_cff = docs / "cff" / f"{pid}.cff"
                    shutil.copy2(cff_path, out_cff)
                    cff_link = f"../../cff/{pid}.cff"

                meta = load_yaml_dict(meta_path)
                star_flags = infer_star_flags(meta, phen_path)
                star_html = render_star_html(star_flags)

                display_name = item.get("display_name") or group or code_name
                page_title = title if title and title != pid else display_name
                documentation = meta.get("documentation", {}) if isinstance(meta.get("documentation"), dict) else {}
                evidence = documentation.get("evidence", {}) if isinstance(documentation.get("evidence"), dict) else {}
                review = documentation.get("review", {}) if isinstance(documentation.get("review"), dict) else {}
                provenance = meta.get("provenance", {}) if isinstance(meta.get("provenance"), dict) else {}
                if not provenance:
                    provenance = provenance_file

                phenotype_role = clean_text(documentation.get("phenotype_role"))
                formatted_role = phenotype_role.replace("_", " ").title() if phenotype_role else None
                tag_values = [t.strip() for t in meta.get("tags", []) if isinstance(t, str) and t.strip()]

                download_items = []
                if source_link:
                    source_download_name = f"{pid}{source_ext}"
                    download_items.append(
                        (
                            f"Download original codelist ({source_ext})",
                            source_link,
                            source_download_name,
                        )
                    )

                for rl in r_links:
                    fname = rl.split("/")[-1]
                    download_items.append((f"Download R script: {fname}", rl))

                if cff_link:
                    download_items.append(("Download citation file (.cff)", cff_link))

                repository_rows = [
                    ("Name", html.escape(display_name)),
                    ("Version", html.escape(version)),
                    ("ID", f"<code>{html.escape(pid)}</code>"),
                    ("Status", html.escape(status)),
                    ("Dataset", html.escape(dataset_type)),
                    ("Code name", f"<code>{html.escape(code_name)}</code>"),
                    ("Phenotype group", html.escape(str(group))),
                    ("Coding system", html.escape(coding_system)),
                    ("Coding systems", html.escape(format_coding_systems(meta.get("coding_systems")) or "NA")),
                    ("Phenotype role", html.escape(formatted_role) if formatted_role else None),
                    ("Created", html.escape(str(meta.get("created"))) if meta.get("created") else None),
                    ("Imported", html.escape(str(meta.get("imported"))) if meta.get("imported") else None),
                    ("Updated", html.escape(str(meta.get("updated"))) if meta.get("updated") else None),
                ]

                background_rows = [
                    ("Summary", html.escape(meta.get("description", "")) if clean_text(meta.get("description")) else None),
                    ("Usage notes", html.escape(str(documentation.get("usage_notes"))) if clean_text(documentation.get("usage_notes")) else None),
                    ("Dataset context", html.escape(format_dataset_context(meta.get("dataset_context")) or "NA")),
                    ("Provenance", html.escape(str(provenance.get("source_type"))).replace("_", " ").title() if clean_text(provenance.get("source_type")) else None),
                    ("Source citation", html.escape(str(provenance.get("source_citation"))) if clean_text(provenance.get("source_citation")) else None),
                    ("Source repository", html.escape(str(provenance.get("source_repository"))) if clean_text(provenance.get("source_repository")) else None),
                    ("Source path", f"<code>{html.escape(str(provenance.get('source_path')))}</code>" if clean_text(provenance.get("source_path")) else None),
                    ("Evidence", format_evidence(evidence)),
                    ("Review", html.escape(format_review(review)) if format_review(review) else None),
                    ("Contacts", html.escape(format_contacts(meta.get("contacts")) or "NA")),
                    ("License", html.escape(str(meta.get("license"))) if clean_text(meta.get("license")) else None),
                    ("Tags", html.escape(", ".join(tag_values)) if tag_values else None),
                ]

                short_pid = short_catalog_id(item, provenance_file, meta)
                source_repo = format_source_repo_link(provenance_file, meta)

                license_value = clean_text(
                    meta.get("license")
                    or provenance_file.get("license")
                    or item.get("license")
                ) or "NA"

                date_created = clean_text(
                    meta.get("date_created")
                    or meta.get("created")
                    or provenance_file.get("date_created")
                    or provenance_file.get("created")
                    or item.get("date_created")
                    or item.get("created")
                ) or "NA"

                date_imported = clean_text(
                    provenance_file.get("ingested_at")
                    or provenance_file.get("date_imported")
                    or provenance_file.get("imported_at")
                    or provenance_file.get("import_date")
                    or item.get("ingested_at")
                    or item.get("date_imported")
                    or item.get("imported_at")
                    or item.get("import_date")
                ) or "NA"

                date_updated = clean_text(
                    meta.get("date_updated")
                    or meta.get("updated")
                    or provenance_file.get("date_updated")
                    or provenance_file.get("updated")
                    or provenance_file.get("last_updated")
                    or item.get("date_updated")
                    or item.get("updated")
                    or item.get("last_updated")
                ) or "NA"

                page = [
                    f"# {page_title}",
                    "",
                    '<div class="phenotype-header-card">',
                    #f'<p class="phenotype-kicker">{html.escape(dataset_type)} phenotype</p>',
                    #f'<h2>{html.escape(page_title)}</h2>',
                    '<div class="phenotype-header-fields">',
                    f'<div><strong>ID:</strong> <code>{html.escape(short_pid)}</code></div>',
                    f'<div><strong>Source:</strong> {source_repo}</div>',
                    f'<div><strong>Dataset:</strong> {html.escape(dataset_type)}</div>',
                    f'<div><strong>License:</strong> {html.escape(license_value)}</div>',
                    f'<div><strong>Date Created:</strong> {html.escape(date_created)}</div>',
                    f'<div><strong>Date Imported:</strong> {html.escape(date_imported)}</div>',
                    f'<div><strong>Date Updated:</strong> {html.escape(date_updated)}</div>',
                    '</div>',
                    '</div>',
                    '',
                ]

                # page.extend(render_section("Repository details", repository_rows))
                # page.extend(render_section("Background information", background_rows))
                page.extend(
                render_metadata_section(
                    star_flags,
                    meta,
                    phen_path,
                    provenance,
                    documentation,
                    evidence,
                    review,
                )
            )
                page.extend([
                    "## Downloads",
                    "",
                    render_download_list(download_items),
                    "",
                    "## Codelist preview",
                    "",
                    render_codelist_preview_table(source_txt),
                    "",
                ])
                # page.extend(render_star_legend_html())

                out_page = docs / "phenotypes" / f"{pid}.md"
                out_page.write_text("\n".join(page) + "\n", encoding="utf-8")

                downloads = []
                if source_link:
                    downloads.append(f"[{source_ext.lstrip('.')}](source/{pid}{source_ext})")
                if r_links:
                    downloads.append(f"[R](r/{pid}/)")
                if cff_link:
                    downloads.append(f"[cff](cff/{pid}.cff)")

                short_pid = short_catalog_id(item, provenance_file, meta)
                source_repo = format_source_repo_link(provenance_file, meta)
                catalog_lines.append(
                    f"| [`{short_pid}`](phenotypes/{pid}.md) | {title} | {dataset_type} | {source_repo} | <div class=\"catalog-meta-cell\">{star_html}</div> | {', '.join(downloads)} |"
                )

            catalog_lines.append("")

    (docs / "catalog.md").write_text("\n".join(catalog_lines) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()