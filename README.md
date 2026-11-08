# citation_meta

R scripts for describing the chapters of a thesis (Word with Zotero citations, or PDF).

| Script | What it does |
|---|---|
| `citation_meta.R` | Age of the literature cited in each chapter and of the literature those papers cite (Crossref); redundancy between chapters, citation lag, self-citations, how often each reference is cited in the text. Figures as SVG. |
| `wordclouds.R` | Word clouds of the chapters (text without the reference list), one per chapter plus a compound figure (PNG and SVG). |

## Use

1. Install the packages loaded at the top of each script.
2. Edit the paths at the top (`chapters_dir` / `in_dir`). `citation_meta.R` also has the surnames for self-citations (`self_rx`, section F) and the PhD period.
3. Run the script top to bottom. `citation_meta.R` queries Crossref (a few minutes the first time; results are cached in `.rds` files).

## Rules and limitations of `citation_meta.R`

- **Years of the references of cited papers** are used only if Crossref returns a publication year for the entry's DOI. Entries without a DOI (books, theses, reports) are dropped, so older and non-journal literature is underrepresented and the cited literature may look younger than it is. Years from the entry's own `year` field or from free text are not used (page numbers and title years get mistaken for years).
- **Identity of a work:** same DOI, or same first-author surname plus the first 40 letters of the title. Initials are not used.
- **In-text citation counts** come from Zotero fields where available (DOI match), otherwise from text matching, and were checked only in part.
- **Ages** are measured up to the date in `chapter_date` (default: today). For unpublished manuscripts this underestimates the age at final publication.
- Self-citation matching is on surnames in the author part of an entry; check the printed list, since common surnames can match other people.

## Citation

See `CITATION.cff`. Please cite the version you used (Zenodo DOI of the release).

## Licence

MIT.
