# Generative AI, LLMs, and Coding Agents

This repository is the canonical source for Davut Ayan's generative-AI study
book. It combines the original `9_GenAI` LLM notes with the Claude and coding
agent material formerly developed in `9a_Claude_Mastery`.

Published book: <https://davutemrah.github.io/GenAI_Repo/>

## Repository structure

- `index.Rmd` — book title, scope, and reader guidance.
- `__repo/*.Rmd` — LLM foundations, pre-training, fine-tuning, and evaluation.
- `__repo/claude/*.Rmd` — Claude, coding-agent, verification, and notebook chapters.
- `_bookdown.yml` — explicit chapter order and `docs/` publication target.
- `_output.yml` — GitBook presentation and navigation.
- `style.css` — book-specific styling.
- `docs/` — generated GitHub Pages output; do not edit by hand.
- `CONTENT_REVIEW.md` — editorial findings, standards, and remaining review work.

The standalone `9a_Claude_Mastery` folder is a recovery source only. New Claude
material belongs in this repository.

## Build locally

From this repository root:

```bash
Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'
```

Or run:

```bash
sh _build.sh
```

The build writes the complete static site to `docs/`. Review source changes and
generated changes separately before committing.

## Safe editing workflow

1. Edit an `.Rmd` source file, never its generated HTML counterpart.
2. Keep `_bookdown.yml` explicit when adding, removing, or reordering chapters.
3. For product-specific claims, check current primary documentation and link it
   near the claim.
4. Distinguish general LLM concepts from behavior supplied by a particular app,
   agent environment, tool, or permission setting.
5. Build the entire book and inspect navigation, search, code blocks, and links.
6. Commit the source and required `docs/` output together.
7. Push only after user approval and verify the GitHub Pages result.

## GitHub Pages setup

This repository commits the rendered book to `docs/`. In GitHub repository
settings, configure **Pages → Build and deployment** as:

- Source: **Deploy from a branch**
- Branch: **main**
- Folder: **/docs**

GitHub Pages is configured to publish from `main` and `/docs`. After each push,
verify the homepage and at least one affected chapter at the published URL.

## Content standards

- Treat generated text and code as unverified until supported by evidence.
- Avoid universal modeling recipes; state assumptions and decision context.
- Prevent train/test leakage and compare models with appropriate baselines.
- Prefer primary documentation and original papers for technical claims.
- Label time-sensitive product behavior and avoid fixed model specifications
  unless they are necessary and dated.
- Never publish tokens, credentials, proprietary data, or unintended personal
  information.
