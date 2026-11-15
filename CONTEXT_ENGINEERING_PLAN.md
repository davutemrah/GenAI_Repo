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

Next: Davut reviews Phase 1. Then Module 2 (right information) when its
materials are in; likely extends Select / just-in-time retrieval with an
agentic search section.

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

The topic descriptions above come from each transcript's opening and closing
lines only; confirm them during the full inventory.

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
