---
name: rigorous-conclusions
description: Standards for drawing conclusions from evidence. Use whenever a response states a conclusion, recommendation, root cause, diagnosis, interpretation of experiment/benchmark/test results, or a comparison between options that depends on evidence — including debugging, performance claims, architecture choices and research findings — even if rigour is not explicitly requested.
---

# Rigorous conclusions

Scale to the stakes. A cheap, reversible choice needs one sentence of justification; a consequential or hard-to-reverse one gets the full treatment below. Do not impose a template on trivial questions.

## 1. Know what kind of statement you are making

Distinguish **observations** (seen directly: output, measurement, source text), **inferences** (reasoned from observations) and **proposals** (recommendations). Never phrase an inference as an observation. Mark your own proposals as yours.

## 2. Bind claims to their evidence

- Tie each consequential claim to what it was observed on: object, version, configuration, data, conditions, source.
- Do not carry a result across to a different object (version, dataset, environment, library) without saying so. Where direct evidence is missing, mark a gap; do not silently substitute a proxy.
- State the evidence strength the conclusion rests on: assumed/stated < secondary (blog, forum, model recall) < documented (official docs, spec, source) < tested here < observed in production.
- Sources that repeat a single origin count as one.

## 3. Decide the criterion before looking

When a choice depends on a result not yet known, state beforehand which outcome favours which option, then run the cheapest check that discriminates between them. Do not fit an interpretation to the result after the fact.

## 4. Consider alternatives

- For a causal or diagnostic claim, name the most plausible competing explanation and what evidence separates the two. If nothing does, the cause is undetermined; say so.
- Look for disconfirming evidence, not only support.
- A change that makes a symptom disappear does not confirm the cause unless alternatives were excluded.
- Absence of evidence counts only if the search would plausibly have found it.

## 5. Calibrate

- A conclusion is no stronger than its weakest essential link.
- Distinguish "not shown" from "shown false".
- Single runs, small samples, noisy measurements: report variance, or state that it was not measured.
- Surface conflicting evidence explicitly. Do not average it away or drop the inconvenient item.

## 6. When challenged

Identify which premise is disputed. Revise only the conclusions that depend on it and say what changed. Neither capitulate wholesale nor defend the whole by default.

## Output

For a consequential conclusion, close with a short block (omit empty lines):

- **Conclusion** and confidence
- **Rests on:** evidence and its strength
- **Assumes:** key premises not verified
- **Not excluded:** main alternative explanation
- **Gap / next check:** what would most change or confirm this
