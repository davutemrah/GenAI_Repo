# Content Review

## Review status

- Review date: July 19, 2026
- Scope: structural merge of `9a_Claude_Mastery` into `9_GenAI`, high-risk
  factual review, methodological review, and repository cleanup.
- Canonical repository: `9_GenAI` / `davutemrah/GenAI_Repo`

## Completed in this pass

- Established one explicit chapter order and one published book.
- Removed a duplicate fine-tuning chapter and retained the stronger combined
  chapter.
- Replaced outdated fixed claims about Claude's knowledge cutoff, web access,
  file access, code execution, memory, and context size with an environment-aware
  explanation.
- Replaced an unrelated React-hooks chapter with a Claude Code lifecycle-hooks
  chapter grounded in official documentation.
- Replaced informal saved-prompt folders with the current `SKILL.md`-based Claude
  Code skills structure.
- Revised prompting guidance so verbosity, role prompting, and mnemonic
  frameworks are not presented as guarantees.
- Reframed agent loops around observable evidence rather than fixed timelines.
- Corrected the notebook modeling example to use a held-out split, a baseline,
  held-out metrics, explicit schema checks, and leakage warnings.
- Qualified universal fine-tuning sample-size claims, benchmark interpretations,
  model-scaling claims, and the historical Chinchilla token/parameter estimate.

## Editorial standards applied

Every technical chapter should distinguish:

1. Stable concepts from changing product behavior.
2. Model capability from tools supplied by the host application.
3. Illustrative examples from recommended production practice.
4. Training evidence from held-out or real-world validation.
5. Automatic metrics from structured human judgment.
6. A fluent answer from a verified result.

## Remaining review backlog

The LLM foundation chapters (Part 1) were rewritten in October 2026 (see
`COURSERA_INTEGRATION_PLAN.md`). They now use textbook prose with primary
citations from `book.bib`, hand-worked numeric examples, dated "Current
practice" notes, and interview checkpoints. Two new chapters cover alignment
and LLM applications, and an appendix covers the course labs. Course claims
that conflicted with the primary papers were corrected; the plan file lists
them. By decision, these chapters contain no runnable Python.

Remaining items:

- re-check the dated "Current practice (October 2026)" notes periodically;
- add diagrams (attention, LoRA, the RLHF pipeline, RAG) with provenance and
  accessible alternative text; and
- review whether PDF and EPUB output should be restored after the HTML edition
  is stable.

The applied chapters should later receive executable tests for all substantial
Python examples. Until then, code blocks are instructional examples and should
not be described as tested production implementations.

## Primary product references used

- [Claude Code best practices](https://code.claude.com/docs/en/best-practices)
- [Claude Code hooks](https://code.claude.com/docs/en/hooks-guide)
- [Claude Code skills](https://code.claude.com/docs/en/skills)
- [Claude tool use](https://platform.claude.com/docs/en/agents-and-tools/tool-use/overview)
- [Claude code execution tool](https://platform.claude.com/docs/en/agents-and-tools/tool-use/code-execution-tool)

- [Model Context Protocol architecture](https://modelcontextprotocol.io/docs/learn/architecture)
- [Claude Code MCP](https://code.claude.com/docs/en/mcp)
- [Databricks MCP](https://docs.databricks.com/aws/en/agents/mcp-tools/)
