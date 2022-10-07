---
format: 
    revealjs:
        scrollable: true
        width: 1440
        height: 1080
        toc: true
        slide-number: true
        theme: [default, custom.scss]

title: "Mainstreaming Scientific Racism"
author: "Dan Hicks & Emilio Lobato"
institute: "UC Merced"
---

# Race science

## Some definitions

scientific racism
 ~ (purportedly) justifying racial inequality and colonialism by appealing to the epistemic authority of science
 
race science - wide sense
 ~ scientific research that is amenable to scientific racism

race science - narrow sense
 ~ scientific research that itself engages in scientific racism

- Ex: genotype clustering studies [@RosenbergGeneticStructureHuman2002] or polygenic scores for educational attainment [@HardenGeneticAssociationsMathematics2019] 
can be wide-sense race science without being narrow-sense race science [@CarlsonQuantifyingContextualizingImpact2020; WillsAreClustersRaces2017]

- This talk will focus on narrow-sense race science



## Race science in the 20th century: Pre-*Brown* {.smaller}

:::: columns 
::: {.column width="30%"}
![[Charles Davenport (1866-1944)](https://en.wikipedia.org/wiki/Charles_Davenport), circa 1929. Source: <https://en.wikipedia.org/wiki/File:Charles_Benedict_Davenport.jpg>](img/Charles_Benedict_Davenport.jpg){width="100%"}
:::

::: {.column width="30%"}
![[Franz Boas (1858-1942)](https://en.wikipedia.org/wiki/Franz_Boas), circa 1915. Source: <https://en.wikipedia.org/wiki/File:FranzBoas.jpg>](img/FranzBoas.jpg)
:::

::: {.column width="30%"}
![[Margaret Mead (1901-1978)](https://en.wikipedia.org/wiki/Margaret_Mead), 1948. Source: <https://en.wikipedia.org/wiki/File:Margaret_Mead_(1901-1978).jpg>](img/Margaret_Mead_(1901-1978).jpg)
:::
::::

- 1910: Davenport founds Eugenics Records Office at Cold Spring Harbor
- 1926: NRC "Committee on the Negro" includes both Davenport and Boas: "neither was influential enough to veto his rival's participation" [@BarkanRetreatScientificRacism1992 113]
- 1930s: *Coming of Age in Samoa* *[cite]* popularizes cultural anthropology, "free[ing] anthropology from the shackles of biology" [@BarkanRetreatScientificRacism1992 134]
- 1940: Carnegie Institute defunds Eugenics Records Office
- 1950: "For all practical social purposes 'race' is not so much a biological phenomenon as a social myth." *[https://unesdoc.unesco.org/ark:/48223/pf0000128291]*

## Pioneer Fund

*[todo]*



## *Brown v Board* and *Mankind Quarterly*

:::: columns
::: {.column width="30%"}
![[Reginald Ruggles Gates (1882-1962)](https://en.wikipedia.org/wiki/Reginald_Ruggles_Gates), 1921. Source: <https://en.wikipedia.org/wiki/File:Reginald_Ruggles_Gates_Portrait.jpg>](img/Reginald_Ruggles_Gates_Portrait.jpg)
:::

::: {.column width="30%"}
![[Henry Garrett (1894-1973)](https://en.wikipedia.org/wiki/Henry_Garrett_(psychologist)). Source: <https://www.psychometricsociety.org/post/past-present-and-incoming-presidents>, citing American Psychologist (1946), 1(9), 371](img/henry_e_garrett.png)
:::

::: {.column width="30%"}
![[George Robert Gayre of Gayre and Nigg (1907-1996)](https://en.wikipedia.org/wiki/Robert_Gayre). Source: <https://www.patrickcomerford.com/2020/07/robert-gair-racist-from-rathmines-who.html>, original uncredited](img/Robert Gayre profile.jpg)
:::
::::

> Ultimately, anti-'racial' science played a defining role in the Supreme Court’s dismissal of the 'separate-but-equal' principle in education .... [@SchafferScientificRacismAgain2007 261]


> [E]veryone agrees that races or ethnic groups exist and that they are in fact the raw material by means of which human evolution has taken place. (Gates, *MQ* 1:1 [1960], 11)




# Race science as agnotology and echo chamber

# Data and methods

## Pioneer-funded researchers

:::: columns
::: {.column} 
| |
|:-----|
| [Thomas J. Bouchard, Jr.](https://en.wikipedia.org/wiki/Thomas_J._Bouchard_Jr.) |
| Brunetto Chiarelli |
| [Hans Eysenck](https://en.wikipedia.org/wiki/Hans_Eysenck) |
| [Robert Gordon](https://en.wikipedia.org/wiki/Robert_A._Gordon) |
| [Linda Gottfredson](https://en.wikipedia.org/wiki/Linda_Gottfredson) |
| [Garrett Hardin](https://en.wikipedia.org/wiki/Garrett_Hardin) |
| [Joseph M. Horn](https://en.wikipedia.org/wiki/Joseph_M._Horn) |
| [Lloyd Humphreys](https://en.wikipedia.org/wiki/Lloyd_Humphreys) |
| [Arthur Jensen](https://en.wikipedia.org/wiki/Arthur_Jensen) |
| [Michael Levin](https://en.wikipedia.org/wiki/Michael_Levin) |
| [Richard Lynn](https://en.wikipedia.org/wiki/Richard_Lynn) |
| [R. Travis Osborne](https://en.wikipedia.org/wiki/R._Travis_Osborne) |
| [J. Phillippe Rushton](https://en.wikipedia.org/wiki/J._Phillippe_Rushton) |
| [Audrey M. Shuey](https://en.wikipedia.org/wiki/Audrey_M._Shuey) |
| [Philip A. Vernon](https://en.wikipedia.org/wiki/Philip_A._Vernon) |
| [Daniel Vining, Jr.](https://en.wikipedia.org/wiki/Daniel_Vining_Jr.) |
:::

::: column
- @MillerPioneerFundBankrolling1994
- [List of Grantees from PF website](https://web.archive.org/web/20130103005545/http://www.pioneerfund.org/Grantees.html)

\

- WOS author search results for 14 authors (Sept/Oct 2021)
- Identify journals that published more than 5/14
:::
::::


## Corpus

|                                |       |
|:-------------------------------|------:|
| Behavior Genetics              |  2268 |
| Behavioral & Brain Sciences    |   898 |
| Intelligence                   |  1237 |
| **Mankind Quarterly**          |  1821 |
| Person. & Individ. Differences |  7274 |
| Psychological Reports          | 21398 |

- 34k journal articles
- Published 1960-2010
- 3 journals published by American Psychological Association not available
- PID is missing many relevant book reviews and essays
	- eg, [Rushton's review of Lynn's book on Pioneer](https://www.sciencedirect.com/science/article/pii/S0191886901001337) *[confirm not included]*

\
- Extract article full text from PDF or HTML
- Extract noun phrases using spaCy NLP package


## Topic modeling

![Diachronic evolution of topic categories in *Philosophy of Science*, 1934–2015. @MalaterreWhatThisThing2019 fig. 2.](img/MalaterreWhatThisThing2019.jpeg){width="80%"}

- Statistical method for clustering documents and words simultaneously
- Documents are composed of topics; topics are composed of words
- Custom R package [`tmfast`](https://github.com/dhicks/tmfast)


## Gould's critique of factor analysis

> As a tool for simplification, [PCA/factor analysis] has proved its great value in many disciplines. But many factorists have gone beyond simplification, and tried to define factors as causal entities. This error of reification has plagued the technique since its inception .... [F]actors, by themselves, are neither things nor causes; they are mathematical abstractions. [@GouldMismeasureManRevised1996 284-5]

- `tmfast` uses methods from PCA and factor analysis [@RoheVintageFactorAnalysis2020]
	- (partial) PCA
	- varimax rotation

- Need to avoid hasty reification of topics


## Topic modeling as phenomena construction

Topics as phenomena [@BogenSavingPhenomena1988; @WoodwardDataPhenomenaRestatement2009]

- simplified patterns extracted from word occurrence data
- may or may not be stable:\
  will we see the same topics in a different corpus? [@MalaterreEarlyDaysContemporary2022]
- constructed with minimal role for substantive theory
- **explananda, not explanans**
	
These words tend to be used together; **but why?**\
These test items tend to be answered in the same way; **but why?**


