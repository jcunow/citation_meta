# citation_years.R

R script that analyses **how old the literature is** that a manuscript or thesis chapter cites, and how old the literature is that *those* papers cite.

For each document (Word with Zotero citations, or PDF) it:

1. reads the reference list and the in-text citations and compares them (publication-year histograms, median age, median citation age);
2. looks up the reference list of every cited paper in [Crossref](https://www.crossref.org/documentation/retrieve-metadata/rest-api/) (free, no key) and takes the publication year registered for each DOI;
3. pools all documents, counting each work once, and overlays the document references on the references of the cited papers;
4. reports the citation lag per cited paper, self-citations, and how often each reference is cited in the text.

Figures are saved as SVG.

## Use

1. Install: `officer`, `pdftools`, `stringr`, `dplyr`, `ggplot2`, `jsonlite`, `stringi` (optional: `svglite`).
2. Edit section **0. INPUT** (paths to your documents, PhD period, optionally the document dates) and the surnames in `self_rx` (section F).
3. Run the script top to bottom. Section C queries Crossref and can take several minutes; results are cached in `.rds` files and reused on reruns.

## Rules and limitations

- **Years of the references of cited papers** are used only if Crossref returns a publication year for the entry's DOI. Entries without a DOI (books, theses, reports) are dropped, so older and non-journal literature is underrepresented and the cited literature may look younger than it is. Years from the entry's own `year` field or from free text are not used (page numbers and title years get mistaken for years).
- **Identity of a work:** same DOI, or same first-author surname plus the first 40 letters of the title. Initials are not used.
- **In-text citation counts** come from Zotero fields where available (DOI match), otherwise from text matching, and were checked only in part.
- **Ages** are measured up to the date in `chapter_date` (default: today). For unpublished manuscripts this underestimates the age at final publication.
- Self-citation matching is on surnames in the author part of an entry; check the printed list, since common surnames can match other people.

## Citation

See `CITATION.cff`. Please cite the version you used (Zenodo DOI of the release).

## Licence

MIT.
