# Data dictionary

| Role | Fields | Definition |
|---|---|---|
| Respondent ID | `AID` | Merge key; must never be published in diagnostics. |
| Survey design | `GSWGT1`, `REGION`, `PSUSCID` | Wave-I weight, stratum and school PSU. |
| Primary exposure | `H2FS1`–`H2FS19` | Complete Wave-II scale; reverse H2FS4/8/11/15; CES-D >=22. |
| Protected baseline severity | `MHSum_W1` | Default protected representation derived from Wave-I Feelings Scale. |
| Current hours | `H4LM6`, `H4LM11`, `H4LM12`, `H4LM13`, `H4LM19` | Route-specific qualifying work, job count and hours; one-job H4LM19 or multiple/unknown-job H4LM13; support 0–120. |
| Participation | `H4LM6`, `H4LM11`, `H4LM14` | Supported Wave-IV questionnaire definition; not a BLS cross-sectional population estimate. |
| Compensation | `H4EC2`, `H4EC3` | Exact annual amount or configured bracket midpoint, with the configured transform/cap. |
| Education III | `H3ED1`, `H3ED2`, `H3ED3`, `H3ED5` | Nested high-school/some-college/college thresholds defined by the constructor. |
| Education IV | `H4ED2` | Nested thresholds >=3, >=6 and >=7. |
| Health | `H3GH1`, `H4GH1` | At least good health versus fair/poor. |
| Mortality | `NDIDD19Y`, `NDIDD19M` | Dated death linkage; 997 or absent death month is unknown, not a valid month. |
| Timing | `IYEAR4`, `IMONTH4` and wave-specific equivalents | Interview reference month; no-interview fallback uses the latest complete wave interview month. |
| Wave-IV disposition | configured `wave4_disposition` input | Source for verified civilian/noninstitutional/nonresident status rules. |

Same-interview-month death is not assumed to precede the observed outcome.
Documented contribution zeros use their stated source evidence. Unknown positive
contributions remain missing. No respondent-linked diagnostic table is supplied.

The missing-code dictionary uses exact source-informed codes and explicit
overrides, not numeric suffix matching. The file source registry and candidate
denylist prevent birth-record predictors/derivatives entering W. Source names and
public examples are not respondent data.
