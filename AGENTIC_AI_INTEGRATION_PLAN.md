# Agentic AI Course Integration Plan

Source: DeepLearning.AI *Agentic AI* (Andrew Ng), transcripts for modules 1–5
(~38,000 words) plus Module 1 quiz screenshots. Moved to
`__repo/Agentic AI coursera/` (git-ignored). There are no slides, notebooks,
or lab transcripts.

Decisions (2026-10-22): a new "Agentic AI" part before the Claude part; 5
chapters; course files kept in the repo but git-ignored.

Status: Phases 0–4 done and accepted: A1 `{#agentic-workflows}`, A2
`{#reflection}`, A3 `{#tool-use}`, A4 `{#evaluating-agentic-systems}`, A5
`{#planning-multi-agent}`.

Slide and quiz coverage pass (2026-10-25): quizzes for modules 2–4 and ~110
slide screenshots were added later and checked read-only against A1–A5 (and
the LLM course against Ch 1–5). Manifests with gaps per image are in
`reports/slide_manifest/` (git-ignored). Result: nothing missing; a few
partial gaps to fill before archiving the course folders. Slides are rebuilt
as tables/code blocks; the diagram tool is still undecided. Later (decided
2026-10-25): break each course lab notebook down into small explained code
blocks (not runnable) that show the concept the lab teaches. No separate
notebook repo and no course notebooks. Labs: M1 UGL1, M2 UGL2, M3 UGL1/UGL2,
M4 UGL1; the LLM-course labs are already in `A_Course_Labs.Rmd`.

Phase 5 (2026-10-25): source edits done. By-name references converted to
`\@ref()` (explicit IDs added: `agentic-error-analysis`,
`agentic-cost-latency`, `code-as-plan`); see-also lines added to Ch 3, Ch 5,
`claude/10`, `claude/11`, `claude/14`; `index.Rmd`, `README.md`, and
`PROMPT_ENGINEERING_PLAN.md` updated. The two leftover module 1 files need no
action: the screenshot is the degrees-of-autonomy slide (already in A1), and
the text file is the lab's package list.

Full render + book-wide link check done (2026-10-29): clean, no broken
refs, citations, links, or images. Committed and pushed as `e648258`;
GitHub Pages deploy verified.

Gap 4 (2026-10-29): done, awaiting Davut's review. New section
`{#system-prompt-altitude}` in `prompting/01_Prompt_Anatomy.Rmd`
(three-level comparison table plus the full "about right" prompt for a
paid-media reporting assistant), one key takeaway, Anthropic context
engineering reference, link from `claude/11`. Full render clean.

Lab breakdowns (decided 2026-10-29): new appendix `B_Agentic_Labs.Rmd`
`{#agentic-labs}`, one section per chapter, done one chapter at a time.
Ungraded labs get explained sketches (own code, not course code); graded
assignments get a concept paragraph only, no solutions (public repo, honor
code). Corrected lab list (the earlier one was incomplete; the notebook in
`module 1/` is the M2 chart lab):

| Chapter | Ungraded labs | Graded |
|---|---|---|
| A2 Reflection | M2 UGL1 chart generation, M2 UGL2 SQL | C1M2 reflective essay |
| A3 Tool use | M3 UGL1 functions to tools, M3 UGL2 email assistant | C1M3 research report with tools |
| A4 Evaluation | M4 UGL1 component-level eval | - |
| A5 Planning / multi-agent | M5 UGL1 customer service (code as plan), M5 UGL2 market research team (`.md`) | C1M5 planner/research/writer/editor |

- A2 labs (2026-10-29): done, approved and committed `0310c25` (2026-11-02). Chart lab, SQL lab
  (text-only vs. result-grounded critique table; noted that an `ABS()`-only
  fix would count insert events as sales), C1M2 concept paragraph.
  "Hands-on" link added to A2. Full render clean.

Loose screenshots (2026-10-29): 12 moved, logged in
`reports/slide_manifest/move_log_2026-10-29.tsv` (undo script beside it).
Five root Agentic images were module 4 component-eval slides (to
`module 4/module 4 sc/`); already covered by A4 except the team benefit,
now one sentence in "Component-level evals". Three Udemy slides moved to
`Gen AI -LLM coursera/udemy (off-course)/` (context-engineering Venn and
tool-call loop already covered by `claude/11` and A3). Manifest paths
updated.

- A3 labs (2026-11-02): done, approved and pushed with A2. Functions-to-tools
  lab (automatic vs manual loop table; `write_note` sketch with path
  restriction, tested), email assistant lab (runs table with and without
  `delete_email`; approval-gate sketch; lethal-trifecta link), C1M3 concept
  paragraph. "Hands-on" link added to A3.

Next: A4 labs.

## Phase 6: step-by-step chapter review (planned 2026-10-25)

Goal: turn each chapter into a reference Davut can learn from and revise
quickly for interviews. One chapter per step; Davut reviews each before the
next starts. Order: the five Agentic AI chapters (A1–A5) first, then the
LLM-course chapters (Ch 1–5).

For each chapter:

1. **Fill the manifest gaps** for that chapter (`reports/slide_manifest/`).
2. **Review section by section:** correctness, clarity, flow, and dated
   claims. Fix or mark anything unverified.
3. **Add a simple example** to each major section that lacks one: a small
   table, code block, or worked case (marketing examples preferred).
   Illustrative only, not runnable.
4. **Add a short "In short" summary** (2–4 bullets) at the end of each major
   section (`##` level). Keep the existing end-of-chapter "Key takeaways"
   and "Interview checkpoints"; tighten them so they don't just repeat the
   section summaries.
5. **Render that chapter**, check it, then hand it to Davut for review.

Agentic AI is newer than the LLM course (2023), so where the two overlap
(tool use, ReAct vs. native tool calling, evaluation, multi-agent) the
Agentic AI framing is the primary one. The LLM chapters keep the history and
link forward, with a dated "Current practice" note where the course is
superseded. Do this carefully, not as bulk edits: small, reviewable changes
per chapter.

Later: the lab breakdowns (see the status note above) go in after this
review.

Format: section summaries are blockquotes, `> **In short.**` followed by a
bullet list, matching the existing `> **Current practice (...)**` notes.

Progress:

- A1 Agentic Workflows (2026-10-25): done, awaiting Davut's review. No
  manifest gaps. Added 4 examples (workflow vs. agent code, paid-media
  anomaly decomposition table, competitor-mention check, $p^n$ reliability
  table), 9 "In short" boxes, a five-line chapter summary in place of the
  long takeaways, dated the computer-use and agentic-coding claims, and
  stated the independence assumption. Full render clean.
  Davut approved A1.
- A2 Reflection (2026-10-25): done, awaiting review. Filled all 3 manifest
  gaps (reflection as filter; external-feedback loop as SQL code block, since
  diagrams are on hold; JSON rubric-judge code plus an axis-range criterion).
  Added a studies comparison table, a paired 2x2 with/without example, 7
  "In short" boxes, and a five-line chapter summary. Cited figures checked.
  Full render clean.
  Davut approved A2.
- A3 Tool Use (2026-10-25): done, awaiting review. Filled the manifest gap
  (Python function next to its generated schema). Added a before/after tool
  design table, a code-execution example (tags + extraction), a Docker
  sandbox command, a lethal-trifecta inbox example, a Responses API note in
  the Current practice box, 7 "In short" boxes, and a five-line summary.
  Full render clean.
  Davut approved A3. Moved the 45 loose course screenshots into module
  folders (log and undo script in `reports/slide_manifest/`).
- A4 Evaluation and error analysis (2026-10-25): done, awaiting review.
  Filled both manifest gaps (billing basis per step; illustrative cost table
  where a paid data API costs more than all LLM steps). Added a Cohen's kappa
  worked example, a sample trace with spans, a precision/recall/F1 worked
  example, a research-agent sentence in error analysis, 8 "In short" boxes,
  and a five-line summary. Numbers checked. Full render clean.
  Davut approved A4.
- A5 Planning and multi-agent (2026-10-25): done, awaiting review. Added a
  replanning example, explicit step-reference syntax, a plan-validator code
  block, a manager-with-agents-as-tools sketch, a text drawing of the four
  communication patterns, a CodeAct per-model sentence (checked against the
  paper: the manifest's claim was backwards; gains were larger for open
  models on API-Bank), 3 "In short" boxes, and a five-line summary. Full
  render clean.
  Davut approved A5. Agentic AI part complete.
- Ch 4 Alignment (`03_00_Alignment.Rmd`, 2026-10-25): done, awaiting
  review. Filled manifest gaps 1, 2, 6: plain-words KL with a 3-token worked
  example, four commonly confused KL facts (frozen reference, not in reward
  model training, always restrains, differs from clipping), "weights only"
  line, text drawing of one PPO iteration (trained vs. frozen models), TRL
  mention (hedged). Added a preference-record example, Bradley–Terry
  numbers, a clipping example, GRPO advantage examples, 7 "In short" boxes,
  a five-line summary, and 2 interview checkpoints. Numbers checked. Full
  render clean.
  Davut approved the alignment chapter.
- Ch 3 Fine-tuning (`02_00_DL_genAI.Rmd`, 2026-10-25): done, awaiting
  review. Filled gap 3 from the Flan 2022 paper itself (dataset / task
  category / task definitions; four-collection mixture table; held-out
  benchmark table; 0.2% compute; 44 MMLU tasks removed). Added a
  fine-tune vs. retrieve vs. prompt table, a loss-masking illustration, a
  BLEU brevity-penalty example, 7 "In short" boxes, a five-line summary, and
  2 interview checkpoints. Existing LoRA and ROUGE arithmetic rechecked.
  Full render clean.
  Davut approved the fine-tuning chapter.
- Ch 5 LLM applications (`04_00_LLM_Applications.Rmd`, 2026-10-25): done,
  awaiting review. Filled gap 5 (what orchestration frameworks are for, plus
  an interview checkpoint). Added a distillation temperature example,
  quantized memory sizes, a KV-cache size calculation, an augmented RAG
  prompt, 6 "In short" boxes, and a five-line summary. Cited figures
  checked. Full render clean.
  Davut approved the applications chapter.
- Ch 1 Foundations and Ch 2 Pre-training (`00_DL_genAI.Rmd`,
  `01_DL_genAI.Rmd`, 2026-10-25): done, awaiting review. No manifest gaps;
  existing arithmetic rechecked by hand (temperature table, top-k/top-p,
  ZeRO table, GPT-3 compute, Llama 3 tokens/param, INT8 example). Ch 1:
  tokenization example and words-per-token rule of thumb, quadratic-cost
  numbers, a worked attention step, a few-shot tagging prompt, 7 "In short"
  boxes, five-line summary. Ch 2: one sentence under three objectives, a
  parallelism decision table, a compute-optimal sizing example, 7 "In
  short" boxes, five-line summary. Full render clean.
  Davut approved Ch 1 and Ch 2 (2026-10-29).
- LLM part complete; all ten chapters approved. Remaining Phase 6 items:
  gap 4 (too-specific / right / too-vague system-prompt table in
  `prompting/01_Prompt_Anatomy.Rmd`); then the lab breakdowns; then archive
  the course folders (Davut's call).

## Rules (carried over from the Coursera LLM integration)

- Textbook voice, not lecture. Transcripts are source material, never quoted
  as speech.
- Each concept follows the same order: intuition, then mechanism, then when it
  helps or fails, then how to evaluate it, then interview checkpoints.
- Cite primary papers and official docs in `book.bib`. Check numbers before
  use. Drop or mark claims that cannot be verified.
- Illustrative code blocks are fine. No runnable Python chunks.
- Course materials stay local and git-ignored.
- Quizzes (`module N/quiz N/` screenshots) are a coverage check, not
  content. After each chapter, confirm every quiz concept is taught in the
  chapter. Fill any gaps in our own words, and add tricky items as interview
  checkpoints. Never reproduce quiz questions or answer options.
  - Module 1 quiz checked (2026-10-22). Added "when you don't need an
    agentic workflow", which steps need tools, and the
    reflection-vs-tool-use question.

## What the course covers vs. what the book already has

| Module | Course topics | Existing coverage | Action |
|---|---|---|---|
| 1 Introduction | Agentic vs. direct generation; degrees of autonomy; benefits (quality, parallelism, modularity); application difficulty spectrum; task decomposition; building blocks; four design patterns | None at a conceptual level. The Claude part is tool-specific. | **New chapter** |
| 2 Reflection | Self-critique vs. external feedback; multimodal reflection (charts); reflection prompts; measuring whether reflection helps | One verification-loops chapter (`claude/08`) covers human-driven loops, not LLM reflection | **New chapter** |
| 3 Tool use | How function calling works (manual protocol, then native); JSON schemas; tool loops; code execution as a tool; sandboxing; MCP | Ch 5 (ReAct, native tool calling, one note); `claude/10` MCP (deep, Databricks-focused) | **New chapter**. Brief MCP section that links to `claude/10`; shorten the Ch 5 note to a link. |
| 4 Evals and error analysis | Build-analyze loop; end-to-end evals (2×2: code vs. LLM judge × per-example ground truth or not); rubric judges; traces and spans; error analysis by component; component evals (precision/recall/F1); improving components; model choice; cost and latency | Ch 3 (ROUGE, BLEU, LLM-as-judge basics); `claude/11` ("evaluation is a harness too"); the planned prompt-evaluation chapter | **New chapter** (the most important one for interviews) |
| 5 Planning and multi-agent | Planning with tools; JSON/XML plans; code as plan (CodeAct); multi-agent roles; communication patterns (linear, hierarchical, deep hierarchy, all-to-all) | `claude/14` Supervisor agents (Databricks-specific, has "shapes" and "when multi-agent helps") | **New chapter**: vendor-neutral theory; `claude/14` keeps the Databricks implementation and links back |

## Proposed structure

A new **Part: Agentic AI**, placed between Prompt Engineering and Claude and
Coding Agents. The book then flows from model foundations, to prompting, to
agent design, to applied tooling.

| # | File | Title | Main sources to cite |
|---|---|---|---|
| A1 | `__repo/agentic/01_Agentic_Workflows.Rmd` | Agentic Workflows: Concepts and Design | Anthropic "Building effective agents" (workflows vs. agents); HumanEval (Chen et al. 2021); GPT-4 report; HuggingGPT; ChatDev |
| A2 | `__repo/agentic/02_Reflection.Rmd` | Reflection | Self-Refine (Madaan et al. 2023); Reflexion (Shinn et al. 2023); CRITIC (Gou et al. 2024) for tool-grounded critique; Huang et al. 2024 on the limits of self-correction |
| A3 | `__repo/agentic/03_Tool_Use.Rmd` | Tool Use and Code Execution | Toolformer (Schick et al. 2023); ReAct; vendor tool-calling docs; MCP spec; sandboxing guidance |
| A4 | `__repo/agentic/04_Evals_Error_Analysis.Rmd` | Evaluating and Improving Agentic Systems | Zheng et al. 2023 (judge biases); IR metrics; OpenTelemetry (traces/spans); practitioner sources marked as such |
| A5 | `__repo/agentic/05_Planning_Multi_Agent.Rmd` | Planning and Multi-Agent Systems | CodeAct (Wang et al. 2024); Plan-and-Solve; HuggingGPT; ChatDev; AutoGen; multi-agent debate (Du et al. 2024); Cemri et al. 2025 on multi-agent failure modes |

Each chapter ends with key takeaways and interview checkpoints.

## Course claims to verify or correct

- **HumanEval numbers:** the course quotes GPT-3.5 zero-shot at 40%. The
  GPT-4 technical report gives 48.1% (GPT-3.5) and 67.0% (GPT-4), both pass@1.
  Use the report's figures; cite the "agentic wrapper" result as Ng's
  reported experiment, not a peer-reviewed result.
- **Taj Mahal example:** construction dates vary by source (commissioned
  c.1631–32, main mausoleum c.1643–48, complex c.1653). Use a different
  fact-check example.
- **LLM-judge position bias:** the course says most models prefer the first
  option. The literature shows position bias whose direction varies by model
  and setting (Zheng et al. 2023).
- **Binary rubrics beat 1–5 scales:** this is practitioner guidance. Present
  it that way unless a primary source is found.
- **Self-correction:** reflection *without* external feedback can fail or hurt
  on reasoning tasks (Huang et al. 2024). The course says this only softly; the
  chapter will state it plainly.
- **aisuite library:** the course's own package. Mention it as one option, not
  a standard. Show the vendor-neutral JSON-schema form.

## Cross-reference updates (small edits to existing chapters)

- Ch 5 (`04_00_LLM_Applications.Rmd`): ReAct and tool-calling note links to A3
  and A5.
- Ch 3 (`02_00_DL_genAI.Rmd`): LLM-as-a-judge paragraph links to A4.
- `claude/10` MCP, `claude/11` harness, `claude/14` supervisor: one "see also"
  line each.
- `PROMPT_ENGINEERING_PLAN.md`: note that system-level evals live in A4, and
  the planned prompt-evaluation chapter covers prompt-level test sets only.
- `index.Rmd`, `_bookdown.yml`, `README.md`: add the new part.

## Phasing (review after each)

0. Setup: decide the material location; `.gitignore`; add the part header and
   files to `_bookdown.yml`.
1. A1 Agentic workflows.
2. A2 Reflection + A3 Tool use.
3. A4 Evals and error analysis.
4. A5 Planning and multi-agent.
5. Cross-references, index/README, full render, commit (on request).

## Not included

- **Labs appendix:** no lab notebooks or lab transcripts were provided. If you
  have them, add them and I'll write walkthroughs as before.
