# DS-MaturationTiming
Repo for Delta smelt maturation timing project


## Description of files:

DS_MatTime_Analyses.R = code for the majority of analyses and figures for this paper (see separate folder for heredity analyses)

GenotypedFish.csv = list of all fish tagged in each spawn year, and accompanying metadata

MFG25_FirstPC.csv = for each spawn year, the first pair cross in multi-family group 25 (MFG 25 and later experienced the late season temperature regime), used to parse fish into which temperature regime they experienced

MasterPedigree.csv = list of all fish spawned in each spawn year, and accompanying metadata

RipeFemales.csv = list of all female fish identified as rippe in each spawn year, and accompanying metadata



## Detailed descriptions of columns in each data file:

#### GenotypedFish.csv
- ID = unique physical tag ID for this fish during its spawning season; IDs restart every spawn year
- Sex = fish sex (determined during spawning season by expression of eggs or milt)
- PC = identification number of the pair cross from the previous spawning season this fish was recovered from (determined using genetic parentage assignment)
- Year/DateTagged = date this fish was tagged as mature
- Dam/SireGenID = generation ID of this fish's parents
- Dam/SireSource = whether parent was wild-born or captive-born
- BirthDate = date this fish's parents were spawned in the previous spawning season
- TimeToRecovery = number of weeks from fertilization to the day this fish was tagged as sexually mature
- Domestication_Index = how many generations this fish's genome has spent in captivity (0 = wild-born fish; DI calculated as average of parents' DIs plus 1)
- Dam/SireID = tag ID for this fish's parents


#### MFG25_FirstPC.csv
- SpawnYear = year in program
- FirstPCinMFG25 = ID of the first pair cross in multi-family group 25 in the corresponding spawn year


#### MasterPedigree.csv
- GenerationID = unique identifier for each spawned fish, across all spawn years
- Generation = generation in hatchery
- SireGenID = GenerationID for sire of this fish (wild sires have Generation IDs in the thousands)
- DamGenID = GenerationID for dam of this fish (wild dams have Generation IDs in the thousands)
- Sex = fish sex (determined during spawning season by expression of eggs or milt)
- Wild = Y (wild-born fish), N (captive-born fish)
- Recovery = number of offspring that survived and were tagged as mature in the subsequent spawn year
- Cross_Type = W (wild-born fish), C (captive-born fish)
- Date/Month/YearSpawned = date this fish was spawned with its pair cross
- PC = identification number for this pair cross; PC numbering restarts every spawn year
- Notes = additional information
- Domestication_Index = how many generations this fish's genome has spent in captivity (0 = wild-born fish; DI calculated as average of parents' DIs plus 1)
- Tank_MF = multi-family group (offspring from multiple pair crosses raised in MFG tanks)
- OffsrpingDI = DI of this fish's offspring
- Recovery_WithMorts = number of offspring that survived and were tagged as mature in the subsequent spawn year, but later died during the season (and were therefore excluded from previous Recovery column)
- TagID = unique physical tag ID for this fish during its spawning season; TagIDs restart every spawn year
- TimeToRecovery = number of weeks from fertilization to the day this fish was tagged as sexually mature
- DateTagged = the date this fish was tagged as sexually mature


#### RipeFemales.csv
- DateChecked = date female was found to be ripe and ready to spawn
- ID = unique physical tag ID for this fish during its spawning season; TagIDs restart every spawn year
- Sex = fish sex (determined during spawning season by expression of eggs or milt)
- PC = identification number of the pair cross from the previous spawning season this fish was recovered from (determined using genetic parentage assignment)
- BY/BirthDate = year/date this fish was born (date parents were spawned)
- Notes = additional information
- Dam/SireGenID = generation ID of this fish's parents
- Dam/SireSource = whether parent was wild-born or captive-born
- TimeToRipen = number of weeks from fertilization to the day this fish was identified as ripe
- Domestication_Index = how many generations this fish's genome has spent in captivity (0 = wild-born fish; DI calculated as average of parents' DIs plus 1)
- Dam/SireID = tag ID for this fish's parents
