# Coding Systems in UK Electronic Health Record Databases

---

## Quick Reference Summary

| Database | Setting | Diagnosis codes | Procedure codes | Drug codes |
|---|---|---|---|---|
| CPRD Aurum | Primary care | SNOMED CT | SNOMED CT | dm+d / BNF |
| CPRD Gold | Primary care | Read v2 (± CTV3) | Read v2 | Multilex / BNF |
| HES-APC | Secondary (inpatient) | ICD-10 | OPCS-4 | — |
| HES-OP | Secondary (outpatient) | ICD-10 | OPCS-4 | — |
| ECDS / HES-AE | Emergency care | SNOMED CT (ICD-10 legacy) | SNOMED CT | — |
| ONS Deaths | Mortality | ICD-10 (ICD-9 pre-2001) | — | — |
| GDPPR | Primary care (national) | SNOMED CT | SNOMED CT | — |
| MHSDS | Mental health | ICD-10 + SNOMED CT | SNOMED CT | — |
| SUS+ | Secondary care | ICD-10 | OPCS-4 | — |
| QResearch | Primary care | SNOMED CT (Read v2 legacy) | SNOMED CT | dm+d / BNF |
| THIN / Cegedim | Primary care | Read v2 / CTV3 | Read v2 | Multilex / BNF |


---

## CPRD Aurum

**Setting:** Primary care (EMIS Web practices, England)  
**Coverage:** ~13 million currently registered patients; data from ~2004 onward

CPRD Aurum is drawn from practices using the EMIS Web clinical system. It is a fully SNOMED CT-coded dataset for clinical events, making it one of the largest SNOMED-coded primary care datasets available for research.

### Coding systems

| Domain | Code system | Notes |
|---|---|---|
| Diagnoses, symptoms, findings, procedures | **SNOMED CT** | Primary code system; numeric concept IDs stored in the `MedCodeId` field |
| Medications (prescriptions) | **dm+d** | Dictionary of Medicines and Devices; VMP/AMP product codes in the `DrugIssue` table |
| Medications (broad groupings) | **BNF** | BNF chapter/section codes available alongside dm+d for pharmacological class phenotyping |
| Historical / mapped records | **Read v2 / CTV3** | Pre-SNOMED records may appear as mapped SNOMED codes; CPRD provides SNOMED–Read crosswalk files |

> **Phenotyping note:** CPRD Aurum codelists are built using SNOMED CT concept IDs. Use the CPRD Aurum Code Browser or OpenCodelists/OpenSAFELY to identify validated codelists. Many CALIBER and OpenSAFELY phenotypes are available in SNOMED format for Aurum.

---

## CPRD Gold

**Setting:** Primary care (Vision/Inps practices, UK-wide)  
**Coverage:** ~4 million currently registered patients; data from ~1987 for some practices

CPRD Gold is the original CPRD dataset, derived from practices using the Vision clinical system. It uses Read codes rather than SNOMED CT, making codelists non-interchangeable with CPRD Aurum without careful mapping.

### Coding systems

| Domain | Code system | Notes |
|---|---|---|
| Diagnoses, symptoms, findings, procedures | **Read v2** | Primary code system; hierarchical alphanumeric codes (e.g., `G30..` for acute MI) stored in the `medcode` field |
| Diagnoses (minority of practices) | **CTV3 (Read v3)** | Clinical Terms Version 3 used in a small subset of practices; 5-character codes beginning with uppercase letter |
| Medications | **Multilex** | Multilex drug dictionary product codes; BNF chapter classifications also available |
| Medications / diagnoses (very old data) | **Oxmis / EMIS codes** | Pre-Read legacy coding from the 1980s–90s; relevant only for practices with very long data histories |

> **Cross-database caution:** CPRD Gold (Read v2/Multilex) and CPRD Aurum (SNOMED CT/dm+d) codelists are not directly interchangeable. CPRD provides mapping tables, but phenotypes should be validated independently in each database rather than assumed equivalent.

---

## Hospital Episode Statistics (HES)

**Setting:** Secondary care (NHS-funded hospitals, England)  
**Coverage:** All NHS inpatient admissions, outpatient attendances, A&E attendances, and critical care episodes  
**Sub-datasets:** HES-APC (inpatient), HES-OP (outpatient), HES-AE (A&E), HES-CC (critical care)

HES is an administrative dataset coded by trained NHS clinical coders after patient discharge. Coding quality varies by trust, specialty, and time period.

### Coding systems

| Domain | Code system | Notes |
|---|---|---|
| Diagnoses (HES-APC, HES-OP) | **ICD-10** | Up to 20 diagnosis codes per episode (`diag_01`–`diag_20`); primary diagnosis in `diag_01`; WHO international edition (not ICD-10-CM) |
| Procedures (HES-APC, HES-OP) | **OPCS-4** | Office of Population Censuses and Surveys Classification of Interventions and Procedures v4; up to 24 codes per episode (`opertn_01`–`opertn_24`); 4-character codes (e.g., `K40.1`) |
| Diagnoses / procedures (HES-AE / ECDS) | **SNOMED CT** | Emergency Care Data Set (ECDS, from Oct 2017) uses SNOMED CT for chief complaint, diagnosis, and investigation; adoption variable across trusts |
| Diagnoses (pre-April 1995) | **ICD-9** | Older HES data used ICD-9; ICD-9-to-ICD-10 crosswalks required for time series spanning this transition |
| Procedures (pre-OPCS-4) | **OPCS-3** | Precursor to OPCS-4 used in earlier HES years |

> **Coding quality:** Primary diagnosis coding is generally more reliable than secondary diagnoses. Procedure coding completeness varies by trust and specialty. HES does not contain medication data.

---

## ONS Death Registration Data

**Setting:** Civil registration of deaths, England and Wales  
**Linkage:** Available linked to CPRD (via CPRD–ONS linkage) and HES

### Coding systems

| Domain | Code system | Notes |
|---|---|---|
| Cause of death (2001 onward) | **ICD-10** | Up to 15 causes coded across death certificate parts I and II; underlying cause determined by WHO rules |
| Cause of death (pre-2001) | **ICD-9** | Deaths registered before January 2001 coded in ICD-9; ONS published bridge-coding studies quantifying transition discontinuities (e.g., COPD, diabetes) |

---

## GDPPR

**Setting:** Primary care (national extract, England)  
**Full name:** General Practice Extraction Service Data for Pandemic Planning and Research  
**Coverage:** ~57 million patients registered at NHS GP practices; held in NHS Secure Data Environment (SDE)

GDPPR draws from multiple GP systems (EMIS, TPP SystmOne, Vision, Microtest), with all records harmonised to SNOMED CT.

### Coding systems

| Domain | Code system | Notes |
|---|---|---|
| All clinical events | **SNOMED CT** | Sole coding system; all GP systems map to SNOMED CT for this extract |

> **Important limitation:** GDPPR is not a complete GP record. It contains only records matching a defined SNOMED cluster list (the "GDPPR cluster"). Many routine consultation codes, referrals, and administrative codes are excluded. **Medication data is not included.**

---

## SUS+ / ECDS / CSDS / MHSDS

A family of NHS England secondary uses datasets covering different care settings, held in the NHS Secure Data Environment and NHS Digital repositories.

### Coding systems by dataset

| Dataset | Setting | Diagnosis codes | Procedure codes | Notes |
|---|---|---|---|---|
| **SUS+** | Inpatient / outpatient | ICD-10 | OPCS-4 | Source data warehouse from which HES is derived; more timely and granular |
| **ECDS** | Emergency care | SNOMED CT | SNOMED CT | Replaced HES-AE from October 2017; coding completeness improving |
| **MHSDS** | Mental health | ICD-10 + SNOMED CT | SNOMED CT | Also uses NHS-specific codes for MHA status, HoNOS care clusters |
| **CSDS** | Community services | SNOMED CT | SNOMED CT | Covers district nursing, health visiting, physiotherapy, etc. |

---

## QResearch / THIN

Alternative primary care research databases used alongside CPRD for epidemiological research.

### QResearch

**Setting:** Primary care (EMIS Web practices)  
QResearch data from current EMIS Web practices is coded in SNOMED CT. Earlier data from legacy EMIS systems uses Read v2. The QResearch team provides codelists in both formats.

| Domain | Code system |
|---|---|
| Diagnoses / procedures (current) | SNOMED CT |
| Diagnoses / procedures (legacy) | Read v2 |
| Medications | dm+d / BNF |

### THIN (Cegedim Health Data)

**Setting:** Primary care (Vision and Inps practices)  
THIN uses data predominantly coded in Read v2, with some CTV3. Medication data uses the Multilex drug dictionary.

| Domain | Code system |
|---|---|
| Diagnoses / procedures | Read v2 / CTV3 |
| Medications | Multilex / BNF |

---


*Reference guide for UK EHR database coding systems — compiled for epidemiology and health data science. Sources: CPRD documentation, NHS England data model specifications, ONS metadata, NHS Digital TRUD releases.*
