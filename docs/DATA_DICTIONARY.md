# Key data roles and provenance

This is a code-facing dictionary, not a substitute for the Add Health
codebooks. Variable labels and permissible disclosures remain governed by the
restricted-use documentation.

| Role | Variable(s) | Wave/source | Treatment in the pipeline |
|---|---|---|---|
| Respondent key | `AID` | All linked sources | Required complete and unique on master respondent data; malformed keys and unintended many-to-many joins stop the run. |
| PSU / school cluster | `PSUSCID` (with approved aliases canonicalized) | Wave I design and school sources | Whole PSU retained within cross-fitting folds; used for survey variance. |
| Geographic stratum | `REGION` | Wave I design sources | Canonicalized across merges; excluded from W; used for stratified survey inference. |
| Sampling weight | `GSWGT1` | Wave I weights | Must be finite and positive. Raw weights define the primary target population; winsorization is sensitivity-only. |
| Exposure items | `H2FS1`-`H2FS19` | Wave II in-home | Items 4, 8, 11, and 15 reverse-scored; codes 6/8/9 treated as nonresponse. |
| Exposure | `Depressed` | Derived | One at CES-D sum >=22; zero otherwise; incomplete CES-D excluded. |
| Protected baseline block | `H1FS1`-`H1FS19` | Wave I in-home | Mandatory in every final nuisance model and protected from ordinary screening/capping. |
| Primary earnings exact value | `H4EC2` | Wave IV in-home | Valid 0-999995 used directly; documented exact missing codes excluded. |
| Earnings bracket | `H4EC3` | Wave IV in-home | Valid categories mapped to configured midpoints only when an exact value is unavailable. |
| Primary outcome | `Y` | Derived | Mortality-inclusive Wave IV compensation on the configured scale and upper bound. |
| Outcome observation | `delta_Y` | Derived | One when `Y` is observed after the mortality composite; modeled as `pi(A,W)`. |
| Mortality year | `NDIDD19Y` | NDI 2019 linkage | Defines death in the inclusive 1997-2007 window after explicit no-death and special-code handling. |
| Wave IV interview year | `IYEAR4` | Wave IV in-home | Audit-only timing field; does not define the mortality window. |
| Bedtime | `H1GH50` | Wave I in-home | Converted to `H1GH50__tsin` and `H1GH50__tcos`. |

The full candidate registry and missing-code dictionary are generated during a
restricted run and are sensitive derived artifacts; they should not be
committed. Disclosure-reviewed summaries may be documented separately.
