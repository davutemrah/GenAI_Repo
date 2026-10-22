# Prompt Engineering Part Plan

Source files live in `__repo/prompting/`. Add each chapter to `_bookdown.yml`
only when it is written.

## Decisions (October 2026)

- Vendor-neutral principles; cite Anthropic, OpenAI, and Google documentation
  as evidence of current practice and verify time-sensitive claims when writing.
- Code examples are offline Python in fenced blocks, run locally before
  publishing with their real output pasted. API snippets are optional, marked
  as not executed, and use a `MODEL_ID` placeholder.
- The Claude-part chapter `__repo/claude/02_Prompting.Rmd` (`{#prompting}`) is
  absorbed into chapters 1-3. Remove it, and update the `\@ref(prompting)`
  references in Chapter 1 of this part and in `12_AI_Marketing_Analytics.Rmd`,
  once chapters 2 and 3 cover its content.
- Chapter 1 refers to later chapters by name (core techniques, evaluation,
  robustness); replace these with `\@ref()` links as the chapters are added.
- The short Prompt Engineering section in `00_DL_genAI.Rmd` stays as a pointer.

## Chapters

| # | File | Topic | Status |
|---|------|-------|--------|
| 1 | `01_Prompt_Anatomy.Rmd` | Roles, six building blocks, layout | Drafted |
| 2 | `02_Core_Techniques.Rmd` | Clarity, few-shot, structure, structured output, reasoning, chaining; prefill as historical (unsupported on Claude 4.6+) | Planned |
| 3 | `03_Strategy_Cards.Rmd` | Use-case cards: extraction, classification, summarization, coding/SQL, grounded QA, agents and tool descriptions, marketing copy, LLM-as-judge | Planned |
| 4 | `04_Guideline_First.Rmd` | Gather standards, draft, test, revise; metaprompts and optimizers; failure modes | Planned |
| 5 | `05_Prompt_Evaluation.Rmd` | Success criteria, test sets, graders, regression, versioning | Planned |
| 6 | `06_Prompt_Safety.Rmd` | Prompt injection, data leakage, defences and limits | Planned |

Optional: a one-page checklist appendix and an interactive prompt-anatomy lab
in `labs/`.
