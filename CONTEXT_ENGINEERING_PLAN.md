# Context Engineering Course Integration Plan

Source: DeepLearning.AI *Context Engineering* (built with Chroma). Course files
are kept outside the repo, in a local `context engineering/` folder, while
Davut is still collecting them.

Decisions (2026-11-14):

- Integrate into the existing chapter
  `__repo/claude/11_Context_Harness_Engineering.Rmd`
  `{#context-harness-engineering}`. No new chapter unless the course turns out
  to be too large for one chapter; revisit after the inventory is complete.
- Wait until Davut has saved more modules before writing anything.
- Same rules as the other course plans: no slide screenshots (rebuild as
  original tables/diagrams); lab notebooks become small explained code
  sketches in our own code, not course code; graded assignments get a concept
  paragraph only, no solutions (public repo, honor code).

Course structure (from vid1/vid3/vid10): the course is organized around the
three-part definition "right information, right format, right time".
M1 overview, failure modes, tools and hooks, traces (ticket-refund agent);
M2 right information: agentic search (coding assistant that parses and chunks
code); M3 right format: pruning, compaction, scratchpads, deduplication,
metadata (research assistant); M4 right time: just-in-time retrieval and
memory (multi-agent email/personal assistant). A Drew Breunig interview
excerpt closes each module.

Status: Phase 1 (Module 1, chapter + labs appendix) done, committed and
pushed as `2f9a1d8` (2026-11-14); GitHub Pages deploy verified live.

Phase 1 changes in `claude/11`:
- Course attribution paragraph and definition bullet.
- New sections `{#right-info-format-time}` (three axes mapped to the four
  families) and `{#context-components}` (five components; context resent in
  full every call; growth arithmetic; token breakdown finding).
- Attention budget: LongMemEval focused vs full (verified in Chroma report:
  ~300 vs ~113k tokens, thinking does not close gap), advertised vs effective
  context length; the course's "20-40%" figure is marked [Unverified] (not in
  the report); rot (size) vs failure modes (quality).
- Four failures: refund-conversation table, course vs Breunig definitions,
  lab 1 lesson (evidence in context is not enough; small local models).
- New sections `{#tools-and-hooks}` (tool's four parts, four tool-failure
  points, pre/post hooks, own marketing sketch `keep_current_versions`,
  tested), `{#prompt-debt}` (Breunig interview, labeled opinion), and
  `{#look-at-traces}` (traces/spans, logging, `reason` field sketch, two
  cautions).
- Explicit IDs added: `four-families`, `context-cost`, `metadata-context`;
  fixed the one existing ref in `agentic/05` to use `four-families`.
- Exercise items 1-2 and key takeaways updated; LongMemEval and course added to
  references.
- Full render clean (no unresolved refs).

Labs (decided 2026-11-14): new appendix `__repo/C_Context_Labs.Rmd`
`{#context-labs}`, same pattern as `B_Agentic_Labs.Rmd` (goal, setup, steps,
outcome "as described in the lab", own-code sketch, lessons), one section per
module. Module 1 done: `{#context-labs-m1}` with lab 1 failure modes (test
case / pass-rate sketch), lab 2 inside the agent (tool responsibilities table,
token-resend arithmetic sketch, tested), lab 3 hook (policy version table,
`with_post_hooks` pipeline sketch, tested). Added to `_bookdown.yml` after
`B_Agentic_Labs.Rmd`; "Hands-on" pointer added at the end of `claude/11`
before the exercise; `index.Rmd` appendix sentence updated to name all three
lab appendices; `{#four-failures}` ID added. Full render clean.

Phase 2 (Module 2, agentic search) done 2026-11-15, not yet committed:
- `claude/11` (Chapter 22): new `{#agentic-search}` section after the four
  families, with subsections `{#search-methods}` (method table, RRF formula,
  own weighted-RRF sketch showing a 3:1 weight silences the keyword leg,
  tested), tool descriptions (three questions), `{#tool-registries}` (course
  >40% claim and Codex ~5,500-token diagram labeled as course claims;
  cache interaction as author's judgment; Anthropic tool search tool,
  verified, [Vendor]), `{#search-failures}` (failure ladder table, eval
  habits), `{#data-preparation}` (chunking, AST, metadata, own
  precision/recall@k sketch, tested; lever trade-off table), and
  `{#recursive-lms}` (Breunig interview + Zhang & Khattab blog, verified).
  Also: attribution names both scenarios, pointer from the three-axes
  section, tool search added to platform features, exercise item 5, three
  takeaways, three references; course reference now says Modules 1 and 2.
- Cross-refs use existing auto IDs `retrieval-augmented-generation-rag` and
  `designing-tools-a-model-can-use-well` (no renames elsewhere).
- `book.bib`: `cormack2009rrf` (title, authors, k = 60 verified from the PDF).
- Appendix C: `{#context-labs-m2}` with setup table; lab 1 agentic code
  search (seven-tool table, four questions, own read-only SQL sketch, tested);
  lab 2 tool registry (steps, decision table paraphrased, own registry
  hit-rate sketch showing a vocabulary-gap miss, tested); graded assignment
  mentioned only, no solution. Intro and setup wording generalized beyond
  Module 1.
- Full render clean; no unresolved refs; citation resolves.

Next: Davut reviews Phase 2, then commit/push. Module 3 (right format:
pruning, compaction, scratchpads, dedup) when its materials are in; it will
likely extend the Compress family and `{#context-cost}`.

## Inventory (as of 2026-11-14)

Module 1 (customer-service ticket-refund agent as the running example).
Transcripts complete (vid1-vid10, all in `module 1/transcript/`), checked
2026-11-14: each video's closing hands off to the next, and vid10 is the
module recap ("join me in the next module"). Davut to confirm the count of 10
against the course sidebar.

| File | Content |
|---|---|
| `vid1` | Course intro: what context engineering is |
| `vid2` | Why capable agents still fail with bad context |
| `vid3` | Example agent: customer service refund flow |
| `vid4` | Naming the types of context an agent handles |
| `vid5` | How agents fail when context is not managed; ends on context rot |
| `vid6` | Context rot: long-running tasks filling the window |
| `vid7` | Agent architecture: model, tools, data, harness, hooks |
| `vid8` | Spotting context issues by looking at traces; evals |
| `vid9` | Guest interview: Drew Breunig on looking at your data |
| `vid10` | Module 1 recap |
| `lab1` | Failure modes notebook |
| `lab2` | Customer support agent notebook |
| `lab3` | Shaping agent context (post-hooks, policy v1-v3) |

Leftover empty files in `module 1/` (`vid11`, `vid13`-`vid16`) are unused
placeholders, not missing videos.

Module 2 "Agentic Search" (coding agent over a customer-feedback web app:
vector store of code/docs, text log files, SQLite feedback tickets). Checked
2026-11-15 via each transcript's opening/closing handoffs and the sidebar
durations visible for the last four videos. Ten videos, matching the course
sidebar count of 10 (confirmed by Davut 2026-11-15). Two duplicate partial
copies of the tool-registry transcript were deleted and the rest renumbered
vid1-vid10 in course order (2026-11-15).

| File | Content | Check |
|---|---|---|
| `vid1` | Module intro: right information; hard-coded retrieval vs agent choosing | ok |
| `vid2` | Agentic search concept (iterative, agent picks tools); ends on context filling up | ok |
| `vid3` | Worked example: why some tickets are escalated; agent searches DB, vector store, logs | ok |
| `vid4` | How humans search; lexical vs semantic search, embeddings, limits | ok (re-copied 2026-11-15; complete to 8:16, hands off to vid5) |
| `vid5` | Implementing the search tools; writing tool descriptions (provider guidance) | ok |
| `vid6` | Tool registries: tool descriptions as system-prompt cost; search for tools; when a registry is not worth it | ok (3:36) |
| `vid7` | Evaluating agentic search: traces, tool-call vs retrieval errors | ok (sidebar 6m) |
| `vid8` | Improving retrieval with data preparation (chunking, AST parsing, metadata, precision/recall@k) | ok (sidebar 9m) |
| `vid9` | Drew Breunig part 2: recursive language models (test-time strategy) | ok (sidebar 5m) |
| `vid10` | Module recap; hands off to M3 (context window filling up) | ok (sidebar 1m) |
| `lab1` | `C1_M2_Lab_1_agentic_search.ipynb`: seven tools (semantic, symbol lexical, pattern lexical, ranked lexical, hybrid with RRF, log search, DB query), then single, hybrid and multi-hop queries | no saved outputs |
| `lab2` | `C1_M2_Lab_2_Tool_Registry.ipynb`: baseline all-tools vs tools indexed in Chroma and retrieved per query; comparison; when to use registries | no saved outputs |

Lab notebooks were saved unexecuted, so any lab "outcome" must come from the
notebook prose or videos, not from outputs. `lab2/embedding_model` is a
verified byte-identical copy of `lab1/embedding_model` (not used by the book).
Slide PDFs for M1-M4 are also in the course folder (reference only; no
screenshots in the book).

Module 1 topics were confirmed during Phase 1; Module 2 transcripts and lab
notebooks were read in full during Phase 2.

## Existing chapter 11 sections (for mapping)

- Context is a finite attention budget
- Four ways context fails
- Four families of technique: write, select, compress, isolate
- Cost: the overlooked dimension
- Harness engineering: what is in a harness, guides and sensors, patterns per
  failure, thin-harness debate, evaluation as a harness
- Applying both to Databricks analytics apps
- Number-check sensor sketch, exercise, key takeaways

Early read: module 1 overlaps "four ways context fails" and the harness
sections; its trace-based diagnosis and hook examples look like the main
additions.
