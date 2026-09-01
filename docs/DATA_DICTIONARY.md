# Key data roles and provenance

This code-facing dictionary does not replace the Add Health codebooks. Variable
labels, source access, and permissible disclosures remain governed by the
restricted-use documentation.

| Role | Variable(s) | Wave/source | Treatment in the pipeline |
|---|---|---|---|
| Respondent key | `AID` | Linked sources | Required complete and unique where specified; malformed keys and unintended many-to-many joins stop the run. |
| PSU / school cluster | `PSUSCID` | Wave I design sources | Whole cluster retained within folds and used for survey variance. |
| Geographic stratum | `REGION` | Wave I design sources | Excluded from candidate W and used for stratified survey inference. |
| Sampling weight | `GSWGT1` | Wave I weights | Must be finite and positive; raw weights define the primary target population. |
| Exposure items | `H2FS1`-`H2FS19` | Wave II in-home | Items 4, 8, 11, and 15 reverse-scored; codes 6/8/9 are nonresponse. |
| Exposure | `Depressed` | Derived | One at CES-D sum >=22; incomplete item sets are excluded. |
| Protected baseline block | `H1FS1`-`H1FS19` | Wave I in-home | Mandatory in final nuisance models and protected from ordinary screening/capping. |
| LFP sources | `H4LM6`, `H4LM11`, `H4LM14` | Wave IV in-home | Route-aware binary participation outcome. |
| Hours sources | `H4LM6`, `H4LM11`, `H4LM12`, `H4LM13`, `H4LM19` | Wave IV in-home | Unconditional weekly hours; nonworkers 0; route-specific total or primary-job hours; capped at 120. |
| Compensation sources | `H4EC2`, `H4EC3` | Wave IV in-home | Exact amount or configured bracket midpoint; valid zeros retained. |
| Education sources | `H3ED1`, `H3ED2`, `H3ED3`, `H3ED5`; `H4ED2` | Waves III-IV in-home | Verified nested attainment thresholds. |
| Health source | `H3GH1`, `H4GH1` | Waves III-IV in-home | Binary at-least-good health outcome. |
| Analysis outcome | `Y` | Derived | Selected family/member/wave after support rules and mortality composite. |
| Outcome observed | `delta_Y` | Derived | One when `Y` is observed after the composite; modeled as `pi(A,W)`. |
| Death year/month | `NDIDD19Y`, `NDIDD19M` | NDI 2019 linkage | Year 99997 means no recorded death; native missing year follows the configured no-death rule; month 997/native missing is unknown, not evidence of no death. |
| Wave III interview timing | `IYEAR3`, `IMONTH3` | Wave III in-home | Orders death against interview; latest complete date defines fallback endpoint for noninterviewed respondents. |
| Wave IV interview timing | `IYEAR4`, `IMONTH4` | Wave IV in-home | Same role for Wave IV. |
| Bedtime | `H1GH50` | Wave I in-home | Converted to circular sine/cosine terms. |

The full candidate registry, missing-code dictionary, sample audit, and mortality
linkage audit are sensitive run artifacts and should not be committed.
