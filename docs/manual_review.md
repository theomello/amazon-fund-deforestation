# Manual review of the treatment inputs (2026-10-29)

Two inputs need human judgement. This note records what was checked and decided, so the owner can spot-check it.

## 1. Which municipalities each Fund project covers

**What was reviewed:** all 82 projects whose coverage text names municipalities or indigenous lands, or mentions "municípios". For each, the automatic links (`R/fund_coverage.R`) were compared with the project's *Abrangência territorial* text from its page.

**Fixed in code (with tests):**
- Real municipalities whose names start with "Rio" or "Alto" were being blanked out by the rule that removes river and region names (Rio Branco, Rio Preto da Eva, Rio Crespo, Alto Paraíso, Alto Alegre do Maranhão, ...).
- A shorter name listed on its own was dropped when a longer name containing it was also listed (Grajaú next to Itaipava do Grajaú; Rosário, Viana).
- Projects that name only indigenous lands are now mapped by overlaying INPE's indigenous-land polygons (from the PRODES dashboard) on IBGE municipal polygons. Money is split by each municipality's share of the named lands' area. FUNAI's own download is blocked (HTTP 403).

**Fixed by hand:** `data/manual/coverage_overrides.csv`, 25 rows, each with a note and the project page as source.
- **Spelling differences on the project pages:** "Uricurituba", "Senador José Profírio", "Lageado Novo", "Governador Newton Belo", "Santa Isabel" (= Santa Izabel do Pará), "Machadinho do Oeste".
- **Names given without their suffix:** "Ouro Preto" (do Oeste), "Novo Horizonte" (do Oeste), and five "... do Tocantins".
- **One false match removed:** "Moju I e II" is a settlement, not the municipality of Moju.
- **Reclassified as state-level**, so absorbed by state × year fixed effects instead of given to municipalities:
  - two state fire-brigade or police projects that only name their base towns (Proteção Florestal Tocantins, Rondônia Mais Verde)
  - five projects whose indigenous lands or municipalities are not named (e.g. "eight indigenous lands in Acre", "41 municipalities in Pará"), plus Amazônia Socioambiental, whose regions can only partly be identified
- **Reclassified as indigenous-land project:** Gestão Territorial Indígena no Sul do Amazonas. "Boca do Acre" and "Ipixuna" there are the names of indigenous lands.

**Result after review** (share of Fund support, all 156 projects):

| Coverage | Projects | R$ mi | Share |
|---|---|---|---|
| Biome-wide / federal | 33 | 2,769 | 46 % |
| Whole state(s) | 59 | 1,803 | 30 % |
| Named municipalities | 54 | 1,312 | 22 % |
| Indigenous lands (overlay) | 6 | 50 | 1 % |
| Outside the Legal Amazon | 4 | 35 | 1 % |

That gives 330 municipalities linked to at least one municipality-specific project, through 571 project–municipality links.

**Open doubts (worth a look in the spot-check):**
- *Coopera-Amazônia* (R$ 103.5 mi, the biggest municipal project): only its first cycle names municipalities. Cycle 2 ("Acre, Amazonas and Pará, municipalities still to be defined") is ignored.
- *Frutificar* lists Nova Ipixuna under Amapá. It's a Pará municipality and is kept as such.
- *Cuidar para Preservar* covers São Gabriel da Cachoeira "and other municipalities of the Alto Rio Negro health district". Only São Gabriel is linked.
- *Mamirauá* links only Tefé, although the Mamirauá and Amanã reserves extend into neighbouring municipalities.
- *Gestão Territorial Indígena no Sul do Amazonas*: the "Jiahui" land is spelled "Diahui" in INPE's polygons and isn't matched. It lies in Humaitá, which is already linked through two other lands.

## 2. Priority-municipality blacklist

- **2008–2022:** from Seixas (2023), *Time series of priority municipalities for deforestation control* (Zenodo, [doi:10.5281/zenodo.8412144](https://doi.org/10.5281/zenodo.8412144), MIT licence). Every row cites its MMA portaria. 2008 has the 36 municipalities of Portaria MMA 28/2008, as the tests check.
- **2023–2026:** the lists of Portaria MMA 834/2023 and 1.202/2024 (81 municipalities) are not freely available in machine-readable form; the copies found online have the annex paywalled. The 2022 status is carried forward, which understates the list after 2023. Adding those two annexes by hand would close the gap.
