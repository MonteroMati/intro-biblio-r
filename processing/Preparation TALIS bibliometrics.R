# 1. Set up --------------------------------------------------------------------
if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse,
               bibliometrix,
               fuzzyjoin,
               igraph,
               scales,
               centiserve,
               openxlsx,
               beepr)

# 2. Read data -----------------------------------------------------------------
wos_talis <- convert2df(file = "input/data/original/wos_talis.txt", dbsource = "wos", format = "plaintext")
scopus_talis <- convert2df(file = "input/data/original/scopus_talis.csv", dbsource = "scopus", format = "csv")
scielo_talis <- convert2df(file = "input/data/original/scielo_talis.txt", dbsource = "wos", format = "plaintext")

# 3. Data cleaning -------------------------------------------------------------
delete_wos <- "DEVELOPING STUDENTS GLOBAL MINDSET|THE SYSTEM DESIGN AND PRELIMINARY TESTS OF THE THZ ATMOSPHERIC LIMB SOUNDER|EFFECT ANALYSIS OF THE DIGITAL SPECTROMETER FFT ALGORITHM ON THZ ATMOSPHERIC LIMB SOUNDER|PERFORMANCE ANALYSIS ON WIDEBAND FAST FOURIER TRANSFORM SPECTROMETER OF THZ ATMOSPHERIC LIMB SOUNDER|ACCYSARENTUR SAECULA|SIMULATION OF THE SPECTRUM RESPONSE|TALIS ORATIO|TALIS ERAT|THE ROMAN LAW OF TALION|MEASURING SYSTEMIC RISK|TALIS PATER|PYRALID AND CRAMBID|PRELIMINARY EVALUATION OF THE ERROR BUDGETS|PRELIMINARY SYSTEMATIC CHECKLIST|COMMON SNAPPING TURTLE PREYS|CRAMBINAE OF IRAN|PYRALID AND CRAMBID MOTHS|PERFORMANCE EVALUATION OF THZ ATMOSPHERIC|TRAFFIC LIGHT SYSTEM|LOGIC IN THE TREATISE OF REFORM|SEVEN NEW SPECIES OF LIMNOPHORA|SHADOW ECONOMY|TOWARDS FOODOMICS|MONEY AS A POLITICAL INSTITUTION|PHYSICAL FUNCTION AND LOW BACK PAIN IN LEEK FARMERS|EXPERIMENTAL OBSERVATION AND CRYSTALLOGRAPHIC DESCRIPTION|MAPPING THE LANDSCAPE OF INTERNET STUDIES|INTRATEXTUALITY OF SENECA|IMPLEMENTING A RESOURCE LIST MANAGEMENT SYSTEM IN AN ACADEMIC LIBRARY|GRAND PRINCE VLADIMIR|HELPING TEENAGERS RECONNECT TO LEARNING|VETERAN TEACHERS AND DIGITAL TECHNOLOGIES|PATTERNS OF DISSONANCE AND ALIGNMENT BETWEEN|SYSTEM MODERNIZATION OF GENERAL SECONDARY EDUCATION|VIEWS ON RESEARCH AS A MODEL OF CPD|THE PREPARATION OF FIJIAN SCHOOL LEADERS|MESH PAKISTAN|PEDAGOGICAL TECHNOLOGY"
wos_talis <- filter(wos_talis, !str_detect(TI, delete_wos))

delete_scp <- "CE-DOPED NMC 811 SYNTHESIS|PREVALENCE OF PARASITIC INFECTIONS|INFLUENCE OF AFFERENTATION FROM THE CONTRALATERAL|ISOTHERMAL AMPLIFICATION-ASSISTED DIAGNOSTICS|AI CHATBOT FOR EDUCATIONAL SERVICE IMPROVEMENT|PHYSICAL FUNCTION AND LOW BACK PAIN|THE SYSTEM DESIGN AND PRELIMINARY TESTS|BWORK STRESS OF FEMALE PRIMARY SCHOOL TEACHERS|POINT-OF-CARE DIAGNOSTICS|PERFORMANCE ANALYSIS ON WIDEBAND|NECROTIZING GINGIVITIS|COLLABORATIVE ANNOTATION TO SUPPORT|EFFECT ANALYSIS OF THE DIGITAL SPECTROMETER|TRAFFIC LIGHT SYSTEM FOR SYSTEMIC STRESS|DESIGN OF FAST FOURIER TRANSFORM SPECTROMETER|THE INFLUENCE OF CLIMATIC AND EDAPHIC CONDITIONS|ENCORE LA FEMME|PRELIMINARY EVALUATION OF THE ERROR BUDGETS IN THE TALIS MEASUREMENTS AND THEIR IMPACT|PERFORMANCE EVALUATION OF THZ ATMOSPHERIC|SIMULATION OF THE SPECTRUM RESPONSE FOR THE THZ ATMOSPHERE|TALIS CONCLUSIO, QUAMVIS CERTA SIT|VETERAN TEACHERS AND DIGITAL TECHNOLOGIES|MICROHABITAT USE AND SELECTION BY AN ANURAN ASSEMBLAGE|DEVELOPING YOUR RESILIENCE MANAGING STRESS|EXPLORING THE INBOUND AND OUTBOUND PERFORMATIVITY|MONEY AS A POLITICAL INSTITUTION IN THE COMMENTARIES|AGE DETERMINATION AND GROWTH CHARACTERISTICS|LANGUAGE, CULTURE AND INTERCULTURALITY THROUGH NARRATIVES|MATEC WEB OF CONFERENCES|IMPLEMENTATION OF TALIS AND DC HOUSE SYSTEM|TALIS PATER, TALIS FILIUS|VIEWS ON RESEARCH AS A MODEL OF CPD|PRELIMINARY SYSTEMATIC CHECKLIST OF THE PYRALOIDEA LATREILLE|CODA|HISTERIDAE OF SOCOTRA|A DESIGN STUDY FOR A WEARABLE DEVICE TO ASSIST PEOPLE|EFFECT OF SOIL-APPLIED LEAD ON MINERAL CONTENTS AND BIOMASS IN ACER CAPPADOCICUM|AVIFAUNA OF THE IMPERIAL PALACE|ACCOMMODATING MIXED-SEVERITY FIRE|THE ROMAN LAW OF TALION AND ITS CORRELATIVE ORIGIN|SERVICE-LEARNING AND PROJECT TALIS|THE INTER- AND INTRATEXTUALITY OF SENECA|LEXICALIZATION IN THE INDEFINITE LATINOS|CONFESSIONS WITH SPECIAL REFERENCE TO THE PROBLEM OF ESTABLISHING|HR-MAS AND NMR TOWARDS FOODOMICS|TEAMWORK IN ESTABLISHING A PROFESSIONAL LEARNING COMMUNITY|INCIDENCE AND RISK FACTORS OF FALLS|GRAND PRINCE VLADIMIR|CRAMBINAE OF IRAN|PYRALID AND CRAMBID MOTHS DETECTED IN ALMERIA|EXPERIMENTAL OBSERVATION AND CRYSTALLOGRAPHIC|MESH PAKISTAN|DESIGN AND VALIDATION OF A TOOL FOR THE FORMATIVE ASSESSMENT|IMPLEMENTING A RESOURCE LIST MANAGEMENT SYSTEM|PATTERNS OF DISSONANCE AND ALIGNMENT BETWEEN TEACHERS|QUALIS VILLA, TALIS VITA|GENETIC DIVERSITY AND DIFFERENTIATION|THE IDEAL OF KNIGHTHOOD IN ENGLISH|GRAVIMETRIC RESULT ON KUR|MAPPING THE LANDSCAPE OF INTERNET STUDIES|ASSEMBLING AND APPLYING AN EDUCATION GRAPH|TALIS ORATIO QUALIS VITA|IMPROVING THE PROFESSIONAL KNOWLEDGE BASE FOR EDUCATION|SPECIALIZED PROFESSIONAL FRONT NON-SPECIALISTS|ACCUSARENTUR SAECULA|PROJECT ENTERPRISE INTERIOR|PYRALID AND CRAMBID MOTHS DETECTED|TALIS COMPILATIO MAGIS|CHARACTERIZING THE SWIMMING PROPERTIES|WHAT IS THE SCOPE OF THE LEGAL TENDER|BIOPOTENTIALS OF SEAWEEDS COLLECTED|USEFUL TIPS FOR TEACHING INTERNATIONAL NURSING STUDENTS|SCHOOL RESTRUCTURING: INTERNATIONAL PERSPECTIVES"
scopus_talis <- filter(scopus_talis, !str_detect(TI, delete_scp))

# Countries
wos_talis <- metaTagExtraction(wos_talis, Field = "AU_CO", sep = ";")
au_co_miss <- filter(wos_talis, is.na(AU_CO))

scopus_talis$C1 <- str_replace_all(scopus_talis$C1, "RUSSIAN FEDERATION", "RUSSIA")
scopus_talis$C1 <- if_else(scopus_talis$DI == "10.1177/0031721714547855", "PHI DELTA KAPPAN, USA", scopus_talis$C1) 
scopus_talis <- metaTagExtraction(scopus_talis, Field = "AU_CO", sep = ";")
au_co_miss <- filter(scopus_talis, is.na(AU_CO))

scielo_talis <- metaTagExtraction(scielo_talis, Field = "AU_CO", sep = ";")
au_co_miss <- filter(scielo_talis, is.na(AU_CO))

# WoS and Scopus fuzzy matching
scopus_ti <- scopus_talis$TI %>% as.data.frame()
scopus_ti <- rename(scopus_ti, TI = ".")

titles <- stringdist_join(wos_talis, scopus_ti, 
                          by = 'TI',
                          mode='left',
                          method = "jw",
                          max_dist = 99, 
                          distance_col = 'dist') %>%
  group_by(TI.x) %>%
  slice_min(order_by = dist, n = 1) %>%
  select(TI.y, dist)

titles <- filter(titles, dist <= 0.19) %>% ungroup()
titles <- rename(titles, TI = "TI.y") %>% select(TI)
titles$IND <- rep("Web of Science and Scopus", 178)
scopus_talis <- filter(scopus_talis, !TI %in% titles$TI)

# Country first author
wos_talis <- metaTagExtraction(wos_talis, Field = "AU1_CO", sep = ";")
scopus_talis <- metaTagExtraction(scopus_talis, Field = "AU1_CO", sep = ";")
scielo_talis <- metaTagExtraction(scielo_talis, Field = "AU1_CO", sep = ";")

# Bind
talis <- bind_rows(wos_talis, scopus_talis)
talis <- left_join(talis, titles, by = "TI")
talis$PY <- if_else(talis$PY == 2023, 2022, talis$PY) # Early access articles
talis$PY <- if_else(is.na(talis$PY), 2022, talis$PY)
talis <- talis %>%
  mutate(IND = case_when(IND == "Web of Science and Scopus" ~ "Web of Science and Scopus",
                         DB == "ISI" ~ "Web of Science",
                         DB == "SCOPUS" ~ "Scopus",
                         TRUE ~ NA_character_))

talis <- talis %>%
  mutate(SO = case_when(SO == "VOPROSY OBRAZOVANIYA-EDUCATIONAL STUDIES MOSCOW" ~ "EDUCATIONAL STUDIES MOSCOW",
                        SO == "VOPROSY OBRAZOVANIYA / EDUCATIONAL STUDIES MOSCOW" ~ "EDUCATIONAL STUDIES MOSCOW",
                        TRUE ~ SO),
         SO = str_to_title(SO))

rm(au_co_miss, scopus_talis, scopus_ti, titles, wos_talis, delete_scp, delete_wos)

# Como el fuzzy join no detectó duplicado entre df TALIS con el df SciELO, pero 2 eran duplicados
# Entonces los he ingresado manualmente
talis$IND <- if_else(talis$DI == "10.14689/ejer.2015.61.1", "Scopus and SciELO", talis$IND)
talis$IND <- if_else(talis$DI == "10.17151/hpsal.2022.27.1.3", "Scopus and SciELO", talis$IND)
scielo_talis <- scielo_talis[3,]
scielo_talis$C1 <- "UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO, MÉXICO.;"
scielo_talis$AU_CO <- "MEXICO"

talis <- bind_rows(talis, scielo_talis)
rm(scielo_talis)

# LATAM papers
latam <- c("Brazil", "Mexico", "Chile", "Argentina", "Colombia", "Cuba", "Venezuela", "Peru", "Costa Rica", "Ecuador", "Uruguay", "Bolivia", "Paraguay", "Guatemala", "El Salvador", "Panama", "Nicaragua", "Dominican Republic", "Honduras", "Haiti")
latam <- toupper(latam)
latam <- paste0(latam, collapse = "|")
talis_latam <- filter(talis, str_detect(AU_CO, latam))

# LATAM first-author & language & index recoded
talis <- talis %>%
  mutate(
    LATAM_AU1_CO = case_when(
      str_detect(string = AU1_CO, pattern = latam) ~ 1,
      TRUE ~ 0),
    LA_REC = case_when(
      str_detect(string = LA, pattern = "ENGLISH") ~ 1,
      TRUE ~ 0),
    WOS = case_when(
      IND == "Scopus" ~ 0,
      IND == "Scopus and SciELO" ~ 0,
      IND == "Web of Science" ~ 1,
      IND == "Web of Science and Scopus" ~ 1
    ))
