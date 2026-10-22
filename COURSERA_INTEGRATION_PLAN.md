# Coursera "Generative AI with LLMs" Integration Plan

Source material: `__repo/Gen AI -LLM coursera/` (W1-W3 slide PDFs, video
transcripts, Lab 1 notebook, screenshots) and `__repo/Scaling instruct models.docx`.
Status: **all phases complete (2026-10-21)**; not yet committed.

## Outcome

| Chapter | File | Result |
|---|---|---|
| 1 Foundations | `00_DL_genAI.Rmd` | Rewritten; attention equation, sampling worked example, lifecycle map, sources note |
| 2 Pre-training and scaling | `01_DL_genAI.Rmd` | Rewritten; memory accounting, number formats, DDP/ZeRO/FSDP, scaling laws, BloombergGPT |
| 3 Fine-tuning and evaluation | `02_00_DL_genAI.Rmd` | Rewritten; PEFT sub-chapter (LoRA, QLoRA, prompt tuning), worked ROUGE/BLEU |
| 4 Alignment | `03_00_Alignment.Rmd` | New; RLHF, reward model, PPO, reward hacking, Constitutional AI, DPO, GRPO |
| 5 From model to application | `04_00_LLM_Applications.Rmd` | New; distillation, PTQ, pruning, serving, RAG, CoT, PAL, ReAct, responsible AI |
| Appendix | `A_Course_Labs.Rmd` | New; walkthroughs of the three labs, results marked "reported, not reproduced" |

Course claims corrected against primary sources:

- ROUGE-L uses the longest common *subsequence* (the course used a contiguous match).
- BLEU is a geometric mean of clipped precisions times a brevity penalty, not an average.
- Feed-forward layers, not attention, hold most transformer parameters; QLoRA found that LoRA on all linear layers matches full fine-tuning.
- LoRA typically trains well under 1% of parameters (not "15–20%").
- The PPO ratio uses the previous-iteration policy; the frozen SFT model is the separate KL reference.
- The KL penalty is estimated from sampled tokens, not computed over the full vocabulary.
- InstructGPT used about 40 labelers (not "thousands"); preference reward models output a scalar.
- Distillation works for decoder LLMs (Gemma 2, DeepSeek-R1 distilled models); LoRA is not a pruning method.
- RAG reduces hallucination but does not avoid it; context windows are no longer "a few thousand tokens".
- Dropped unverifiable figures: "1–3% of tokens survive curation", the V100 80 GB benchmark, the 0.495 BLEU example.
- Translation example corrected to "J'aime l'apprentissage automatique".

## Decisions (2026-10-19)

- Purpose: a personal study reference for interview preparation. It must be
  correct, concise and detailed enough, without being complicated. The site is
  still public on GitHub Pages, so citations must be professional.
- Slides, transcripts and lab notebooks are not published.
- PEFT is a sub-chapter of the fine-tuning chapter, not a chapter of its own.
- No runnable Python. Equations, tables and worked numeric examples only.
- The labs become an appendix, written as my own walkthroughs (no copied code).

## Accuracy rules

- Check every number and definition against the primary paper before it goes
  in. If a course claim can't be verified, mark it or drop it.
- Lab results are labeled "reported in the course lab, not reproduced".
- Separate stable concepts from fast-moving practice, and date the
  "Current practice" notes.

## Writing rules for this work

- Use transcripts and slides to understand a topic, then explain it from
  scratch in my own words. Never copy spoken phrasing such as "in this video",
  "as Mike said", or "this week's lab".
- Each concept section follows the same order: intuition, then mechanism
  (with an equation where it helps), then assumptions and failure modes, then
  how to interpret it, then interview takeaways.
- Turn the bullet notes into connected prose. Keep tables and short lists for
  comparisons.
- Cite primary papers, not the course, for each method (list below). Rebuild
  diagrams as original tables or diagrams; don't use slide screenshots
  (consistent with commit `b4a3c5f`).
- Label 2023 course content that has been superseded with a short
  "Current practice" note (see the last section).

## Coverage matrix

Depth: none / brief / solid. File names are relative to `__repo/`.

| Course topic | Where it is in the book now | Depth | Action |
|---|---|---|---|
| **W1** GenAI overview, prompt/completion, use cases | `00` Generative AI Overview, Use Cases | brief | Expand into prose |
| RNN limits, need for attention | `00` Text Generation before Transformers | brief | Expand |
| Transformer components (tokens, embeddings, positional encoding, multi-head attention, FFN, softmax) | `00` Transformers + Transformer Architecture | brief, **duplicated** | Merge the two sections; add the scaled dot-product attention equation (Q, K, V) |
| Encoder-only / decoder-only / seq2seq and their pre-training objectives | `00` and `01` intro (overlap) | solid | Keep one copy in `01`; `00` points to it |
| In-context learning (zero/one/few-shot), the effect of scale | `00` Prompt Engineering, plus a repeat at the start of `02_00` | solid | Keep the `00` pointer (as `PROMPT_ENGINEERING_PLAN.md` says); cut the repeat in `02_00` |
| Generation settings (greedy, sampling, top-k, top-p, temperature) | `00` Configuring Generative AI Models | solid | Light prose rewrite; add a top-p worked example |
| GenAI project lifecycle | `00` lifecycle | solid | Keep; add the effort and time per stage from the W3 cheat sheet |
| Model hubs, choosing a model | `01` intro | brief | Expand slightly |
| GPU memory accounting (4 GB to store vs ~24 GB to train 1B params, FP32) | `01` Memory Challenges | brief | Add the bytes-per-parameter breakdown (weights, Adam states, gradients, activations) |
| Quantization data types (FP32/FP16/BF16/INT8) | `01` Quantization | solid | Keep; add the bit layout of each format |
| Multi-GPU training: DDP, FSDP, ZeRO stages 1-3 | `01` Distributed Computing (3 generic bullets) | none | **Add**: what each strategy shards, and when to use it |
| Scaling laws, compute budget (petaflop/s-days), Chinchilla | `01` Scaling Laws | solid | Add the compute-budget comparison; add a current-practice note |
| Pre-training for domain adaptation (BloombergGPT) | `01` (one sentence) | brief | **Add** a section on when domain pre-training pays off |
| **W2** Limits of in-context learning; instruction fine-tuning process | `02_00` intro | solid | Rewrite as prose |
| Single-task fine-tuning, catastrophic forgetting | `02_00` | solid | Light edit |
| Multi-task instruction tuning, FLAN, SAMSum, prompt templates | `02_00` + `Scaling instruct models.docx` | solid | Add the FLAN scaling result (1,836 tasks, CoT data) from the docx, citing the paper |
| ROUGE-1/2/L, BLEU, ROUGE hacking and clipping | `02_00` Evaluating Model Performance | solid | Add a small ROUGE example worked by hand (no code) |
| Benchmarks (GLUE, SuperGLUE, MMLU, BIG-bench, HELM) | `02_00` Benchmarks | solid | Add a current-practice note on saturation and contamination |
| PEFT motivation, trade-offs, taxonomy (selective / reparameterization / additive) | `02_00` (one bullet) | none | **New sub-chapter** in `02_00` |
| LoRA (low-rank $W + BA$, parameter-count example, rank choice), QLoRA | none (grep hits for "LoRA" are false matches) | none | **New sub-chapter** in `02_00` |
| Prompt tuning / soft prompts vs prompt engineering | none | none | **New sub-chapter** in `02_00` |
| **W3** Why alignment: helpful, honest, harmless | `00` lifecycle (one line) | none | **New chapter** |
| RL basics; RLHF data collection; reward model training | none | none | **New chapter** |
| PPO (value loss, clipped policy loss, entropy), KL penalty and reference model | none | none | **New chapter** |
| Reward hacking, evaluating the aligned model | none | none | **New chapter** |
| Scaling feedback: Constitutional AI / RLAIF | none | none | **New chapter** |
| Inference optimization: distillation, post-training quantization, pruning | `01` covers quantization for training only | none | **New chapter** (applications) |
| Knowledge cutoff, hallucination, weak math | `00` Additional Infrastructure (2 bullets) | brief | **New chapter** (applications) |
| RAG: chunking, embeddings, vector store, retrieval | mentioned in `claude/11`, `claude/01` | brief | **New chapter**; cross-link to `claude/11` |
| Chain-of-thought, PAL, ReAct, LangChain, app architecture | ReAct/agents covered in a modern way in `claude/10`, `claude/11`, `claude/14` | brief | **New chapter**: the concepts, then point to the agent chapters |
| Responsible AI: toxicity, hallucination, IP, ongoing research | none (slides only, no transcript) | none | **New section** in the applications chapter |
| SageMaker JumpStart demo | none | none | **Skip** (vendor demo, no lasting concept) |
| Labs (summarization, PEFT, RLHF with PPO) | none | none | Appendix walkthroughs (see Assets) |

## Proposed outline

Part "LLM Foundations" grows from 3 to 5 chapters. File names follow the
existing `NN_MM` pattern.

1. `00_DL_genAI.Rmd`: Foundations of Generative AI and LLMs (**rewrite**).
   Merge the duplicated transformer sections, add the attention equation, keep
   the prompting section as a pointer, and write in textbook prose.
2. `01_DL_genAI.Rmd`: Pre-training and Scaling (**expand**). Memory accounting
   table, DDP/FSDP/ZeRO, compute budgets, domain-adaptation pre-training.
3. `02_00_DL_genAI.Rmd`: Fine-Tuning and Evaluation (**rewrite and extend**).
   Instruction fine-tuning, then catastrophic forgetting, then multi-task
   fine-tuning and FLAN, then a new **Parameter-Efficient Fine-Tuning**
   sub-chapter: why full fine-tuning is costly, the PEFT taxonomy, LoRA
   mathematics and rank choice, QLoRA, prompt tuning and soft prompts, and a
   comparison table. Evaluation (with a worked ROUGE example) and benchmarks
   come last. Remove the repeated ICL intro.
4. `03_00_Alignment.Rmd` (**new**): Aligning LLMs with Human Feedback. RL
   framing for language models, preference data, the reward model with its
   pairwise loss, PPO step by step, the KL penalty, reward hacking, aligned
   model evaluation, Constitutional AI, and current methods (DPO, RL with
   verifiable rewards).
5. `04_00_LLM_Applications.Rmd` (**new**): From Model to Application.
   Inference optimization (distillation, PTQ, pruning), model limitations,
   RAG, reasoning prompts (CoT), PAL, ReAct, application architecture, and
   responsible generative AI. Short sections that link forward to the Prompt
   Engineering part and to the MCP, harness-engineering and supervisor-agent
   chapters, so they aren't duplicated.

Appendix, under `# (APPENDIX) Appendix {-}`:

- `A_Course_Labs.Rmd`: one section per lab, covering the goal, the workflow
  steps, the key generation or training settings, the result labeled
  "reported, not reproduced", and the lesson. The labs are dialogue
  summarization with prompting, LoRA fine-tuning of FLAN-T5, and PPO
  detoxification with a reward model. Labs 2-3 are written from the
  transcripts because only Lab 1's notebook is available locally.
- References: generated by bookdown from `book.bib`.

Citation style: a short "Sources" note at the start of the LLM Foundations
part says the chapters follow the DeepLearning.AI / AWS course *Generative
AI with Large Language Models* (Coursera) as a learning path. Primary papers
are cited inline where a method is defined or a number is quoted.

`_bookdown.yml`: insert the two new chapters after `02_00_DL_genAI.Rmd` and
the appendix at the end. `index.Rmd`: update the Part 1 description.

## Primary sources to cite

Vaswani et al. 2017 (Transformer); Kaplan et al. 2020 and Hoffmann et al.
2022 (scaling, Chinchilla); Rajbhandari et al. 2020 (ZeRO); Wu et al. 2023
(BloombergGPT); Chung et al. 2022 (FLAN scaling); Lin 2004 (ROUGE);
Papineni et al. 2002 (BLEU); Liang et al. 2022 (HELM); Hu et al. 2021 (LoRA);
Dettmers et al. 2023 (QLoRA); Lester et al. 2021 (prompt tuning); Ouyang et
al. 2022 (InstructGPT); Schulman et al. 2017 (PPO); Bai et al. 2022
(Constitutional AI); Rafailov et al. 2023 (DPO); Hinton et al. 2015
(distillation); Lewis et al. 2020 (RAG); Wei et al. 2022 (CoT); Gao et al.
2022 (PAL); Yao et al. 2022 (ReAct). Check every reference before adding it.

Default: add a `book.bib` and `bibliography: book.bib` to `index.Rmd` so I can
cite with `[@key]`. The book has no bibliography today.

## Slides, transcripts, screenshots and labs

Decided: none of these are published. Add `__repo/Gen AI -LLM coursera/` to
`.gitignore`, as was already done for `Scaling instruct models.docx`. The
materials stay local as study sources. Diagrams are rebuilt as original
tables, never slide screenshots.

## Course content that is likely out of date (verify when writing)

| Course content (2023) | What to add or note |
|---|---|
| PPO-based RLHF as *the* alignment method | DPO and other preference-optimization methods; RL with verifiable rewards for reasoning models |
| CoT as a prompting trick | Reasoning models and test-time compute; tie to the prompting chapters |
| ReAct + LangChain for tools | Native tool-use APIs and MCP (already in `claude/10`) |
| PAL with a Python interpreter | Code-execution tools offered by model hosts |
| RAG with a single vector store | Hybrid search, reranking, long-context trade-offs; link `claude/11` |
| Chinchilla-optimal training | Smaller models now trained well past the Chinchilla ratio because inference cost dominates |
| INT8 and BF16 quantization | FP8 training; 4-bit inference quantization |
| GLUE/SuperGLUE/MMLU | Benchmark saturation and contamination; LLM-as-judge evaluation |
| FLAN-T5 as the example model | Keep for teaching; note it is a small 2022 model |

## Phasing (one reviewable commit per phase)

0. **Setup**: `book.bib`, `bibliography:` in `index.Rmd`, `.gitignore`.
1. **Fine-tuning chapter**: rewrite `02_00` and add the PEFT sub-chapter.
2. **Alignment chapter** (`03_00`).
3. **Applications chapter** (`04_00`), including responsible AI.
4. **Expand chapter 2**: DDP/FSDP/ZeRO, memory accounting, domain pre-training.
5. **Rewrite chapter 1** in textbook voice and remove the duplication.
6. **Labs appendix** and the "Sources" note.

After each phase: render with
`Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'`, check
cross-references, and review source and `docs/` diffs separately. I review
each phase before the next one starts.
