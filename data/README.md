# Data

## Why the data is not included

All documents were retrieved from **Nexis Uni** under an institutional licence. Nexis content cannot be redistributed, so the raw files are excluded from this repository (see `.gitignore`). This file documents how to rebuild the corpus.

## Search query (Nexis Uni, Boolean)

```
("Arctic" OR "High North" OR "Svalbard" OR "Northern Sea Route" OR "Ny-Ålesund") W/50 (India OR Indian)
AND ("India" W/10 "Arctic" OR "Indian Arctic" OR "Arctic policy" OR "Himadri" OR "NCPOR" OR "Arctic observer"
  OR "observer state" OR "India's Arctic" OR "Indian presence" OR "Arctic strategy" OR "polar policy"
  OR "Arctic engagement" OR "Arctic research" OR "Arctic Council" W/15 India)
AND ("Arctic Council" OR "observer status" OR "observer state" OR "Arctic policy" OR "Himadri" OR "NCPOR"
  OR "Ny-Ålesund" OR strateg! OR geopolit! OR secur! OR militar! OR naval OR Russia OR China OR NATO
  OR shipping OR "Arctic shipping" OR "Arctic corridor" OR "Arctic sea lanes" OR "NSR" OR energy
  OR diplomacy OR cooperation OR governance OR climate OR environment! OR Himalaya! OR monsoon
  OR "scientific research" OR "polar research" OR "polar expedition" OR "Indo-Pacific" OR sovereignty
  OR "sea route" OR resource! OR "polar silk road" OR "great power competition" OR multipolar
  OR "logistics agreement" OR connectivity OR Greenland)
AND NOT (tourism OR travel OR wildlife OR seafood OR cruise OR fashion OR entertainment OR sports
  OR cricket OR recipe OR hotel OR resort OR festival)
```

## Filters

- Source location: India
- Language: English
- Timeline: 1 January 2008 to 31 December 2012 (pre-observer baseline); 1 January 2013 to 31 March 2026 (primary period)
- Duplicate suppression: high similarity
- Source type: newsletters and blogs excluded
- Source name: government sources only for the government set; all sources except government sources for public discourse; all sources for 2008-2012

Government sources retained: Indian Government News; IPR; Government of India Ministry of External Affairs (Press Releases, Speeches & Statements, Media Briefings); Government Initiated Campaigns; Indian Parliament Q&A. Government documents were cross-checked against the original Ministry of External Affairs and government websites.

## Expected folder layout

Export documents from Nexis Uni as individual text files and place them as follows:

```
data/raw/
├── gov_2013_2026/      # government documents, 2013-2026
├── full_2013_2026/     # all documents, 2013-2026 (government duplicates removed in code)
└── full_2008_2012/     # all documents, 2008-2012
```
