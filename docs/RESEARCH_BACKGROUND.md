# Research background

> Scope note: This Markdown version retains the supplied Introduction, Literature Review, and references. The supplied Methods and Results were identified as placeholders and are intentionally excluded. The executable analysis is documented in [METHODS_CODE_MAP.md](METHODS_CODE_MAP.md).

## Introduction

Adolescent mental health is declining in the U.S. The proportion of
adolescents reporting a past-year major depressive episode almost
doubled between 2008 and 2024.[^1] At the same time, rates of self-harm,
suicide attempts, and suicide deaths also increased, accompanied by
markers of related chronic diseases.[^2] This builds on a steady decline
in health and human capital among younger cohorts for decades.[^3] While
the current trend shows some evidence of improving since 2021,
adolescent mental health may not return to pre-2008 levels absent
further intervention.[^4] This will likely require substantial public
investment to fundamentally bend the long-term trajectory of adolescent
mental health.

Unfortunately, substantial public investment is rarely politically
feasible, especially given rising federal deficits.[^5] Federal budget
analysts support policymaking by estimating the 10-year budget impact of
proposed policies. If they estimate that the policy would increase the
federal deficit, budget rules come into force that challenge any new
spending.[^6] Currently, policymakers are generally only able to account
for the immediate behavioral responses of policies that improve
adolescent mental health, such as increased spending. However, there is
evidence that improving adolescent mental health may produce downstream
economic benefits that could partially offset policy
costs.[^7]<sup>,</sup>[^8]

Federal budget analysts can consider a policy’s full downstream budget
effects – including at the level of the macroeconomy – but they rarely
have the resources to do so.[^9] Budget analysts do not have a way to
individually estimate the impacts of the hundreds of proposed policies
each year, within the analysts’ limited resources. Analysts could more
likely integrate an elasticity for how changes in adolescent mental
health condition prevalence would affect inputs into their existing
models, such as labor force participation. For each policy proposal,
federal budget analysts could simulate the policy impacts on mental
health and then use the elasticities to estimate how that would alter
the inputs into their models. Analysts could then run their existing
models to determine how the policy ultimately affects the economy and
federal budget. This would not comprehensively estimate policy impacts,
but would simplify estimation to fit within current resources by relying
on a single channel of policy effects on model inputs. With this
streamlined approach, federal budget analysts could more quickly
estimate the budget impacts of policies that improve adolescent mental
health.

If improving adolescent mental health reduces the federal deficit, then
this may partially or even completely offset the costs of a proposed
policy. If the policy costs are offset, then, for the first time,
policymakers will face reduced or even eliminated budget hurdles to
enacting investments on the scale likely needed to shape the long-term
trajectory of adolescent mental health in the U.S. In this paper, we
seek to provide federal budget analysts with evidence for an elasticity
that would enable more comprehensive budget modeling of proposed
policies within their existing resources to enable this opportunity to
improve population-level adolescent mental health.

Specifically, in a nationally representative sample of adolescents that
completed a mental health screening instrument at the end of one year,
we estimate how those that screen above a cut-off or not – indicating
potential depression and other mental health needs, such as anxiety –
differ in labor force participation, usual hours worked, and annual
earnings approximately 6, 13, 22, and 29 years later. Our approach seeks
to simulate the average treatment effect on the treated (ATT) among
adolescents who received a hypothetical mental health intervention over
the course of a year which completely prevents or remits mental health
challenges related to screening above the cut-off. While no intervention
perfectly prevents or remits mental health challenges, with our
estimate, policymakers can implement the streamlined strategy for
estimating policy impacts on inputs into federal budget models, based on
the intervention’s relative risk of preventing or remitting relevant
mental health challenges.

As an extension, we share an open-source policy modeling tool that
demonstrates how our estimates could be used to simulate the impacts of
proposed policies on the federal budget. With the tool, users can
explore the potential budget effects of different example policies,
based on their adolescent mental health impacts. Users can also vary the
underlying assumptions or specify their own policy based on annual
costs, penetration, and effectiveness. With this tool, we support
policymakers in designing and proposing the most effective policies
possible for improving adolescent mental health, within the realities of
existing budget constraints.

## Literature review

Adolescent mental health impacts later labor market outcomes through
several mechanisms. Some mechanisms are internal, such as differences in
memory, attention, and response to reward, while others are external,
such stigma and healthcare costs.[^10] Adolescence, in particular,
heightens this relationship as the brain undergoes the rapid development
of neural circuitry.[^11] On a large enough scale, the labor market
changes can then aggregate to influence the overall macroeconomy.[^12]

Randomized-controlled trials (RCTs) have provided strong evidence that
interventions that address mental health can cause improved labor market
outcomes. A meta-analysis of trials of psychosocial interventions
addressing common mental health conditions, such as depression and
anxiety, in low- and middle-income countries found that treatment
improved aggregate labor market outcomes by 0.16 standard
deviations.[^13] In the U.S., adult employees provided a digital mental
health treatment through an employee assistance program demonstrated
improved workplace productivity and absenteeism.[^14]

Other studies find potentially causal relationships or at least strong
associations between adolescent mental health and later labor market
outcomes using observational data from representative datasets.[^15] For
example, one study used the Add Health dataset and parametric
regressions, school-level and family-level fixed effects, and sibling
comparisons to find that adolescent depression reduced employment by
approximately 5 percentage points 13 years later.[^16] Another study
used the National Longitudinal Survey of Youth and parametric regression
of adolescent depression on later wages, accounting for a set of
covariates indicated by theory, to find a wage penalty between
10-15%.[^17]

A smaller set of studies examine the relationship between mental health
and the macroeconomy. One study specified a structural macroeconomic
model with mechanisms particular to mental health conditions, such as
pessimism and rumination, to estimate the economic gains of different
policy interventions.[^18] Another study used the association between
disability burden and economic outcomes to estimate that averting 10% of
the mental health conditions in the U.S. would be associated with almost
\$2 trillion in GDP growth over 35 years.[^19]

This prior research was conducted for different purposes than estimating
an elasticity for a federal budget model and cannot be easily used for
this purpose. RCTs do not collect the necessary data over the relevant
timeframe, and they test effects with small populations that may not
transport to the general U.S. population. Further, the interventions
RCTs test necessarily have effects beyond just mental health, so the
evaluations may include other impacts as well. The studies with
observational data use parametric methods and account for a small sample
of confounders, posing a substantial risk of mis-specified regressions
and unmeasured confounding. Other strategies, such as sibling
fixed-effects, may also not generalize beyond the population of those
with siblings.

In this study, we build on this literature by using observational data
from a large nationally-representative dataset and use non-parametric
and doubly-robust regression methods, leveraging machine learning
techniques to account for thousands of potential confounders, to
estimate how adolescent mental health challenges relate to later labor
market outcomes most relevant to federal budget model inputs. Through
our approach, we seek to address gaps in the literature most critical
for federal budget analysts in modeling policies that improve adolescent
mental health. Our approach can also serve as a model for producing
estimates that can serve as budget model elasticities beyond adolescent
mental health, supporting researchers to build the capacity of
policymakers to invest across the most pressing public health needs.

[^1]: **References**

    Substance Abuse and Mental Health Services Administration. 2024
    NSDUH Detailed Tables \[Internet\]. Rockville (USA): Substance Abuse
    and Mental Health Services Administration; 2025 \[cited 2026 Mar
    23\]. Available from:
    https://www.samhsa.gov/data/report/2024-nsduh-detailed-tables

[^2]: Forrest CB, Koenigsberg LJ, Eddy Harvey F, Maltenfort MG, Halfon
    N. Trends in US children’s mortality, chronic conditions, obesity,
    functional status, and symptoms. JAMA. 2025 Aug 12;334(6):509-16.

[^3]: Reynolds N. The broad decline in health and human capital of
    Americans born after 1947. American Economic Review: Insights. 2025
    Jun 1;7(2):141-59.

[^4]: Ormel J, Hollon SD, Kessler RC, Cuijpers P, Monroe SM. More
    treatment but no less depression: The treatment-prevalence paradox.
    Clinical psychology review. 2022 Feb 1;91:102111.

[^5]: Auerbach AJ, Gale W. Then and Now: A Look Back and Ahead at the
    Federal Budget. Tax Policy and the Economy. 2026 Jan 1;40(1):47-108.

[^6]: Congressional Research Service. Dynamic Scoring in the
    Congressional Budget Process \[Online\]. Washington (USA):
    Congressional Research Service; 2023 Mar 6 \[cited 2025 Nov 27\].
    Available from: https://www.congress.gov/crs-product/R46233

[^7]: Knapp M, Wong G. Economics and mental health: the current
    scenario. World Psychiatry. 2020 Feb;19(1):3-14.

[^8]: McDaid D, Park AL, Wahlbeck K. The economic case for the
    prevention of mental illness. Annual review of public health. 2019
    Apr 1;40(1):373-89.

[^9]: Elmendorf D, Hubbard G, Williams H. Dynamic Scoring: A Progress
    Report on Why, When, and How. Brookings Papers on Economic Activity.
    Washington (USA): The Brookings Institution; 2024 Sep 26.

[^10]: Ridley M, Rao G, Schilbach F, Patel V. Poverty, depression, and
    anxiety: Causal evidence and mechanisms. Science. 2020 Dec
    11;370(6522):eaay0214.

[^11]: Blakemore SJ. Adolescence and mental health. The Lancet. 2019 May
    18;393(10185):2030-1.

[^12]: Bloom DE, Canning D, Kotschy R, Prettner K, Schünemann J.
    Health and economic growth: Reconciling the micro and macro
    evidence. World Development. 2024 Jun 1;178:106575.

[^13]: Lund C, Orkin K, Witte M, Walker JH, Davies T, Haushofer J,
    Murray S, Bass J, Murray L, Tol W, Patel VH. The effects of mental
    health interventions on labor market outcomes in low-and
    middle-income countries. National Bureau of Economic Research; 2024
    May 13.

[^14]: Birney AJ, Gunn R, Russell JK, Ary DV. MoodHacker mobile web app
    with email for adults to self-manage mild-to-moderate depression:
    randomized controlled trial. JMIR mHealth and uHealth. 2016 Jan
    26;4(1):e8.

[^15]: Evensen M, Lyngstad TH, Melkevik O, Reneflot A, Mykletun A.
    Adolescent mental health and earnings inequalities in adulthood:
    evidence from the Young-HUNT Study. J Epidemiol Community Health.
    2017 Feb 1;71(2):201-6.

[^16]: Fletcher J. Adolescent Depression and Adult Labor Market
    Outcomes. South Econ J. 2013 Jul;80(1):26–49.

[^17]: Johar M, Truong J. Direct and Indirect Effect of Depression in
    Adolescence on Adult Wages. Appl Econ. 2014 Dec 22;46(36):4431–44.

[^18]: Abramson B, Boerma J, Tsyvinski A. Macroeconomics of mental
    health. National Bureau of Economic Research; 2024 Apr 22.

[^19]: Chen S, Kuhn M, Prettner K, Bloom DE. The Macroeconomic Burden
    of Noncommunicable Diseases in the United States: Estimates and
    Projections. PloS One. 2018 Nov 1;13(11):e0206702.
