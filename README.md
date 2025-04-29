# Heterogeneous Quality and Tariff-Pass Through: The Solar Panel Case

## Introduction

This repository contains all the necessary files to ensure the replication of my master thesis "Heterogeneous Quality and Tariff-Pass Through: The Solar Panel Case". 

This project recovers the pass-through of anti-dumping and safeguards of solar panels depending on the observed good quality. We observe a full pass-through of tariff, with higher quality goods being less subject to tariff transmission than standard quality goods. We explain this phenomenon by the fact that solar panels are investment goods, very sensible to price variations, and then homogeneous despite some horizontal differentiation.

## Project Structure

------------

    ├── README.md          <- The top-level README for developers using this project.
    │
    ├── data
    │   ├── temp           <- Intermediate data that has been transformed.
    │   ├── final          <- The final, canonical data sets for modeling.
    │   └── raw            <- The original, immutable data dump.
    │
    ├── notebooks          <- exploratory notebooks.
    │   
    ├── references         <- Data dictionaries, manuals, and all other explanatory materials.
    │   
    ├── output             <- Generated outputs.
    │   │
    │   ├── paper
    │   │   ├── figures        <- contains the figures in latex and pdf/png/etc. formats.
    │   │   ├── files          <- contains the latex files to generate the paper and the paper in pdf format.
    │   │   └── tables         <- contains the tables in latex and pdf/png/etc. formats.
    │   │
    │   └── appendix
    │       ├── figures        <- contains the figures in latex and pdf/png/etc. formats.
    │       ├── files          <- contains the latex files to generate the paper and the paper in pdf format.
    │       └── tables         <- contains the tables in latex and pdf/png/etc. formats.
    │
    └── codes                   <- Source code for use in this project.
    
------------
## Data availability

Here we develop and comment on the different data used in this project.

| Data    | Availibity    | Producer   | Type |
|-------------|-------------|-------------|-------------|
| BACI | Public | CEPII | Trade|
| COMEXT | Public | Eurostat | Trade| 
| General Import Data | Public | USTIC | Trade|
| PRODCOM | Public | Eurostat | Production|
| Tracking the Sun | Public | Berkley Lab | Installed PVP capacity|

## Replication

This section develops the role of the different codes in processing data, figures, tables and regressions.

### Data

So far for our analysis we use the following HS6 id (2002)

| Product    | HS6 (2002)    |
|-------------|-------------|
| 854140| Electrical apparatus: photosensitive, including photovoltaic cells, whether or not assembled in modules or made up into panels, light emitting diodes | 
| 854150 | Electrical apparatus: photosensitive semiconductor devices n.e.s. in heading no. 8541, including photovoltaic cells, whether or not assembled in modules or made up into panels |
| 854190 | Electrical apparatus: parts for diodes, transistors and similar semiconductor devices and photosensitive semiconductor devices |


In question to integrate into the analysis:
| Product    | HS6 (2002)    |
|-------------|-------------|
|850610	|Cells and batteries: primary, manganese dioxide|
|850630	|Cells and batteries: primary, mercuric oxide|
|850640	|Cells and batteries: primary, silver oxide|
|850650	|Cells and batteries: primary, lithium|
|850660	|Cells and batteries: primary, air-zinc|
|850680	|Cells and batteries: primary, (other than manganese dioxide, mercuric oxide, silver oxide, lithium or air-zinc)|
|850690	|Cells and batteries: primary, parts thereof|
|850421	|Electrical transformers: liquid dielectric, having a power handling capacity not exceeding 650kVA|
|850422	|Electrical transformers: liquid dielectric, having a power handling capacity exceeding 650kVA but not exceeding 10,000kVA|
|850423	|Electrical transformers: liquid dielectric, having a power handling capacity exceeding 10,000kVA|
|850431	|Electrical transformers: n.e.s. in item no. 8504.2, having a power handling capacity not exceeding 1kVA|
| 853710 | Boards, panels, consoles, desks and other bases: for electric control or the distribution of electricity, (other than switching apparatus of heading no. 8517), for a voltage not exceeding 1000 volts |  
| 853720 | Boards, panels, consoles, desks and other bases: for electric control or the distribution of electricity, (other than switching apparatus of heading no. 8517), for a voltage exceeding 1000 volts | 
|854160|Crystals: mounted piezo-electric|
|854411	|Insulated electric conductors: winding wire, of copper|
|854419	|Insulated electric conductors: winding wire, (of other than copper)|
|854420	|Insulated electric conductors: co-axial cable and other co-axial electric conductors|
|854430	|Insulated electric conductors: ignition wiring sets and other wiring sets of a kind used in vehicles, aircraft or ships|
|854441	|Insulated electric conductors: for a voltage not exceeding 80 volts, fitted with connectors|
|854449	|Insulated electric conductors: for a voltage not exceeding 80 volts, not fitted with connectors|
|854451	|Insulated electric conductors: for a voltage exceeding 80 volts but not exceeding 1000 volts, fitted with connectors|
|854459	|Insulated electric conductors: for a voltage exceeding 80 volts but not exceeding 1000 volts, not fitted with connectors|
|854460	|Insulated electric conductors: for a voltage exceeding 1000 volts|
|854470	|Insulated electric conductors: optical fibre cables|


### Figures

### Tables


------------
