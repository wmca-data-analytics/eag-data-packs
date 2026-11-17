# WMCA EAG Data Packs

Data packs to support Expert Advisory Groups (EAGs) within the West Midlands Cancer Alliance (WMCA).

## ------------------------------------------------------------------------

## Overview

|  |  |
|------------------------------------|------------------------------------|
| **Development:** | ![badge](https://img.shields.io/badge/R-blue?logo=r&logoColor=white) ![badge](https://img.shields.io/badge/R%20Studio-blue?logo=rstudioide&logoColor=white) |
| **Sources:** | ![badge](https://img.shields.io/badge/CAIT-data-blue?logoColor=white) ![badge](https://img.shields.io/badge/CWT-data-blue?logoColor=white) ![badge](https://img.shields.io/badge/NDRS-data-blue?logoColor=white) ![badge](https://img.shields.io/badge/NHSE-data-blue?logoColor=white) |
| **Topics:** | ![badge](https://img.shields.io/badge/CWT-blue?logoColor=white) ![badge](https://img.shields.io/badge/Staging-blue?logoColor=white) ![badge](https://img.shields.io/badge/TreatmentVariation-blue?logoColor=white) ![badge](https://img.shields.io/badge/LWBC-blue?logoColor=white) ![badge](https://img.shields.io/badge/Survival-blue?logoColor=white) |
| **Output:** | ![badge](https://img.shields.io/badge/PowerPoint-file-blue?logoColor=white&logo=data:image/png;base64,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) ![badge](https://img.shields.io/badge/Restricted-audience-blue?logoColor=white&logo=data:image/png;base64,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) |

## ------------------------------------------------------------------------

## Metadata

### Source Data

- **Dataset Name:** Staging Data
  - **Owner:** NDRS
  - **Visibility:** Restricted
  - **URI:** <https://nhs.sharepoint.com/sites/msteams_275dcb/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=gK7S2I&ovuser=37c354b2%2D85b0%2D47f5%2Db222%2D07b48d774ee3%2Chaydn%2Ewilliams3%40nhs%2Enet&OR=Teams%2DHL&CT=1752582185776&clickparams=eyJBcHBOYW1lIjoiVGVhbXMtRGVza3RvcCIsIkFwcFZlcnNpb24iOiI0OS8yNTA2MTIxNjQyNCIsIkhhc0ZlZGVyYXRlZFVzZXIiOmZhbHNlfQ%3D%3D&CID=e7e8e931%2D1a87%2D4d8e%2Dbad7%2D28bb40d3195e&cidOR=SPO&FolderCTID=0x0120002C1016B090653942AC8DFD5854020938&id=%2Fsites%2Fmsteams%5F275dcb%2FShared%20Documents%2FRCRD%2FData%5FExtracts>
- **Dataset Name:** Staging Completeness
  - **Owner:** NDRS
  - **Visibility:** Public
  - **URI:** <https://nhsd-ndrs.shinyapps.io/staging_dashboard/>
- **Dataset Name:** Cancer Waiting Times Statistics
  - **Owner:** NHS England
  - **Visibility:** Public
  - **URI:** <https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-waiting-times/>
- **Dataset Name:** Cancer Waiting Times Statistics (detailed)
  - **Owner:** NHS England
  - **Visibility:** Restricted
  - **URI:** n/a - extract received monthly by email.
- **Dataset Name:** LWBC
  - **Owner:** NHS England
  - **Visibility:** Restricted
  - **URI:** <https://digital.nhs.uk/ndrs/data/data-outputs/data-quality-and-insights-hub?area=lwbc>
- **Dataset Name:** Treatment Variation
  - **Owner:** National Cancer Audit Collaborating Centre (NATCAN)
  - **Visibility:** Public
  - **URI:** <https://rcs-ceu.shinyapps.io/natcan-dashboards/>
- **Dataset Name:** Survival
  - **Owner:** NHS England
  - **Visibility:** Public
  - **URI:** <https://digital.nhs.uk/data-and-information/publications/statistical/cancer-survival-in-england>

### Access / Information Governance

- **Output type:** PowerPoint presentation
- **Output location:** WMCA Data Analytics FutureNHS workspace
- **Output URI:** <https://future.nhs.uk/MidlandsCancerAlliancesAnalytics/view?objectID=67197872>
- **Access restrictions:** As per FutureNHS
- **DSA/DARS/etc. in place?** Yes, for CWT extract

### Target Audience

- **Organisation(s):** WMCA, Provider and commissioner stakeholders
- **Committee(s):** EAGs
- **Job Role(s):** WMCA Pathway Leads, EAG Chairs

### Distribution

- **Method:** FutureNHS
- **Frequency:** Monthly

------------------------------------------------------------------------

## Contacts

### Contributor(s)

- **Name:** See Github
- **Organisation:** West Midlands Cancer Alliance

### Owner(s)

- **Organisation:** West Midlands Cancer Alliance
- **Contact Name:** Data Analytics team
- **Contact Email:** [rwh-tr.wmcadata\@nhs.net](mailto:rwh-tr.wmcadata@nhs.net){.email}

------------------------------------------------------------------------

## Usage

See the `docs` folder.

------------------------------------------------------------------------

## Licence

Copyright © Crown Copyright 2025 (National Health Service)

You may use and re-use this software and associated documentation files free of charge in any format or medium, under the terms of the Open Government Licence v3.0.

You may obtain a copy of the Open Government Licence at <https://www.nationalarchives.gov.uk/doc/open-government-licence/>

![Open Government Licence text logo](assets/logos/ogl.png)
