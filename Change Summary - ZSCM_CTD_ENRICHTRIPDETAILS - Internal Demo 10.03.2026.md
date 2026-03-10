# Change Summary – ZSCM_CTD_ENRICHTRIPDETAILS
## Internal Demo Observations + Enhancements – 10 March 2026

**Program:** ZSCM_CTD_ENRICHTRIPDETAILS  
**Function Module:** Z_SCM_CTD_ENRICHTRIPDETAILS  
**Tables Impacted:** ZSCE_CTD_HDR, ZSCE_CTD_ITM, ZSCM_ENRICH_DET_ST (DDIC)  
**Change Type:** Enhancement + Bug Fix  
**Date:** 10.03.2026  
**Functional Author:** Gaurav Prabhu  
**Technical Author:** Omkar More  
**Reference:** CD: TBD | TR: TBD  

---

## Pre-requisite: DDIC Changes

Before any code changes, the following fields must be added to the output structure **ZSCM_ENRICH_DET_ST**:

| Field | Type | Length | Purpose |
|---|---|---|---|
| `STATUS_DESC` | CHAR | 30 | Status Description text (displayed in ALV) |
| `ROWCOLOR` | CHAR | 4 | ALV row colour code (hidden from display; used internally) |

The existing `LIPS` SELECT in the FM must also be extended to additionally fetch the **`WERKS`** field (required for Point 11 region logic).

---

## Change Point 1 — Authorization-Based T-Code

**Type:** Configuration + Code  
**Area:** SE93 (Basis) + Report Program  

**Requirement:** Create a new transaction code for the CTD Trip Enrichment Program accessible only to authorised users. The T-Code must support two levels of authorization:
- **Test Run access only** (simulation — no DB changes)
- **Full execution access** (actual DB updates)

**Approach:**
1. Create T-Code in **SE93** pointing to report `ZSCM_CTD_ENRICHTRIPDETAILS`
2. Create custom authorization object **`ZCTD_ENRICH`** in SU21 with field `ACTVT`:
   - Activity `03` = Test Run / Display access
   - Activity `16` = Full Execution access
3. Assign roles in PFCG; assign users in SU01
4. Add two-stage `AUTHORITY-CHECK` in the program at `AT SELECTION-SCREEN`

**Authorization Matrix:**

| User Type | Can Access T-Code | Test Run | Full Run |
|---|---|---|---|
| No role | No | No | No |
| Test-only role (ACTVT=03) | Yes | Yes | No |
| Full access role (ACTVT=03+16) | Yes | Yes | Yes |

---

## Change Point 2 — New Optional Input Fields: Transporter Code & Vehicle Number

**Type:** Enhancement  
**Area:** FM Interface + Report Program Selection Screen  

**Requirement:** The selection screen must include two new **optional** input fields allowing users to filter trips by a specific transporter and/or vehicle number.

**Fields Added:**

| Selection Screen Field | Maps To | FM Parameter | Optional |
|---|---|---|---|
| Transporter Code | LIFNR | `I_LIFNR TYPE LIFNR` | Yes |
| Vehicle Number | TRUCK_NO | `I_TRUCK_NO TYPE YTRUCK_NO` | Yes |

**Approach:** Filters applied at **DB level** in the `WHERE` clause of the `ZSCE_CTD_HDR` SELECT for maximum efficiency. When not supplied, all transporters/vehicles within the date range are processed.

---

## Change Point 3 — ALV: Group by Trip ID

**Type:** Enhancement  
**Area:** Report Program — ALV Display Method  

**Requirement:** All leg rows belonging to the same trip must be visually grouped together in the ALV output under a collapsible group header row.

**Detail:** Grouping enabled on field `TRIP_NO` in the ALV field catalogue. Each group header displays the Trip Number, Vendor, and Truck Number. Groups can be expanded/collapsed by the user.

---

## Change Point 4 — ALV: Default Sort Order

**Type:** Enhancement  
**Area:** Report Program — ALV Display Method  

**Requirement:** By default, the ALV output must be sorted in the following sequence (all ascending):

1. Vendor (`LIFNR`)
2. Truck Number (`TRUCK_NO`)
3. Trip Number (`TRIP_NO`)
4. Count / Leg Counter (`COUNTER`)

---

## Change Point 5 — ALV: Column Header Renames

**Type:** Enhancement  
**Area:** Report Program — ALV Field Catalogue  

**Requirement:** Three column headers must be renamed for business clarity:

| Field | Current Label | New Label |
|---|---|---|
| `SOURCE_DATE` | Source Date | **Leg Start Date** |
| `DEST_DATE` | Destination Date | **Leg End Date** |
| `REMARKS` | CTD Rule Engine Remarks | **CTD Trip Enrichment Remarks** |

No structural or logic change — label change only in field catalogue (`SELTEXT_L` / `COLTEXT`).

---

## Change Point 6 — ALV: New "Status Description" Column

**Type:** Enhancement  
**Area:** DDIC + FM + Report Program  

**Requirement:** A new column **"Status Description"** must be added next to the existing Trip Status column, showing the descriptive text for each status code.

**Source:** Text values fetched from domain fixed values of data element `ZCTD_TRIP_ST` (via table `DD07T`).

**Examples:**

| Trip Status | Status Description |
|---|---|
| `03` | Error (or as maintained in domain) |
| `04` | Enriched (or as maintained in domain) |

---

## Change Point 7 — ALV: Hide "Indicator" Column

**Type:** Enhancement  
**Area:** Report Program — ALV Field Catalogue  

**Requirement:** The column currently showing as a checkbox/indicator (field `IS_NEW_LEG`) that appears after the CTD Trip Enrichment Remarks column must be **removed from the ALV display**.

**Approach:** Field is hidden via `lvc_s_fcat-tech = abap_true`. The field remains in the output structure as it is still used internally for the DB insert/update decision logic — it is just not visible in the output.

---

## Change Point 8 — ALV: Dynamic Page Header

**Type:** Enhancement  
**Area:** Report Program — ALV Display Method  

**Requirement:** The page header of the ALV output must dynamically reflect whether the program is running in Test (Simulation) mode or Full execution mode.

| Mode | Page Header |
|---|---|
| Test Run selected | **"Simulation Mode - CTD Trip Details Enrichment Program"** |
| Full Run (no Test Run) | **"CTD Trip Details Enrichment Program"** |

---

## Change Point 9 — Bug Fix: Status 03 (Error) Trips — Database Update Behaviour

**Type:** Bug Fix  
**Area:** FM — Merged Loop (Step 8+9+10) + HDR Update Loop (Step 10)  

**Problem:** Empty legs of trips with data errors (status `03`) are incorrectly being inserted into `ZSCE_CTD_ITM`. Additionally, the `ZSCE_CTD_HDR` update for error trips is unnecessarily overwriting fields that should remain unchanged.

**Root Cause:** The `is_new_leg = abap_true` flag is being set for empty legs of excluded trips due to a missing guard check on the trip action.

**Required Behaviour:**

| Table | For Status 03 (Error) Trips | For Status 04 (Success) Trips |
|---|---|---|
| `ZSCE_CTD_ITM` | **No inserts, no updates** | Normal update of loaded legs + insert of empty legs |
| `ZSCE_CTD_HDR` | **Only `CTD_RULEENG_REMARKS` + audit fields** updated; `TRIP_STATUS` left unchanged at `03` | Full update: `TRIP_STATUS = 04` + remarks + audit fields |

**Remarks Consolidation for Status 03 trips:**
- All distinct error remarks for the trip are collected and concatenated with `'; '` separator
- If combined length exceeds 255 characters (CHAR 255 field), truncated to 252 chars + `'...'`
- Consolidated remark written to `CTD_RULEENG_REMARKS` in HDR

---

## Change Point 10 — Bug Fix: Blank Source Date for Empty Leg Destination Date Derivation

**Type:** Bug Fix  
**Area:** FM — Step 7, ELSE Branch (three sub-case locations)  

**Scope:** Applies **only** to empty legs added after the last loaded leg (single-leg trip, multi-leg trip — last leg sub-cases). Sandwiched empty legs (between two loaded legs) are **not affected** — their destination date is taken directly from the next loaded leg's source entry date.

**Problem:** When `DEST_EXIT_DATE` of a loaded leg is blank, the empty leg added after it inherits a blank `SOURCE_DATE`. The existing `ZSCE_CTD_HDR` binary search uses `source_date` as the lower bound for finding the next trip's `created_date`, producing incorrect results when `source_date` is blank.

**Fix:** When `source_date` of the empty leg is blank, use the **current trip's `created_date`** (from `ZSCE_CTD_HDR`) as the fallback lower bound for the binary search.

---

## Change Point 11 — Region/State Fetching Logic: Full Replacement

**Type:** Enhancement  
**Area:** FM — Step 5 (Data Fetching) + Step 6 (Enrichment Loop — Region Assignment)  

**Problem:** Current region logic uses address numbers from `OIGSS` (ADRNA/ADRNZ) looked up in `ADRC`. This is being replaced with a more accurate delivery-based approach.

**New Logic:** Starting point is `DOC_NUMBER` (delivery number) from `OIGSI` (already fetched).

| | SO & STO Movement Type | PO Movement Type |
|---|---|---|
| **Source Region** | `LIPS` (Delivery → `WERKS`) → `T001W` (`WERKS` → `REGIO`) | `LIKP` (Delivery → `LIFNR`) → `LFA1` (`LIFNR` → `REGIO`) |
| **Destination Region** | `LIKP` (Delivery → `KUNNR`) → `KNA1` (`KUNNR` → `REGIO`) | `LIPS` (Delivery → `WERKS`) → `T001W` (`WERKS` → `REGIO`) |

**New Tables Added:** `LIKP`, `T001W`, `LFA1`, `KNA1`  
**Existing Change:** `LIPS` SELECT extended to also fetch `WERKS`  
**Removed:** `ADRC` SELECT and all OIGSS address-number based region logic  

---

## Change Point 12 — ALV: Row Colouring by Trip Status

**Type:** Enhancement  
**Area:** DDIC + FM (Output Loop) + Report Program (ALV Layout)  

**Requirement:** ALV rows must be colour-coded based on the trip's processing status for immediate visual identification.

| Trip Status | Meaning | ALV Row Colour |
|---|---|---|
| `04` — Enriched | Successfully processed; ready for further processing | **Light Green** |
| `03` — Error | Has data errors; requires attention | **Light Red** |

**Implementation:** Via standard SAP ALV `INFO_FNAME` mechanism using a `ROWCOLOR CHAR4` field in the output structure. Color codes: `C510` (Light Green), `C610` (Light Red).

---

## Change Point 13 — ALV: Interactive Trip Count Summary + Custom Filter Toolbar

**Type:** Enhancement  
**Area:** Report Program — ALV Display Method + Toolbar Event Handling  

**Requirement:** The ALV output must display a summary of trip counts and allow the user to interactively filter the displayed data.

**ALV Grid Title Bar (always visible):**
```
Total Trips Processed: XX  |  Successful: YY  |  Errors: ZZ
```

**Custom Toolbar Buttons:**

| Button | Icon | Default State | Action on Click |
|---|---|---|---|
| `All Trips (XX)` | Standard | **ACTIVE/PRESSED by default** | Clears filter; shows all rows |
| `Successful (YY)` | Green traffic light | Unpressed | Filters ALV to status 04 rows only |
| `Errors (ZZ)` | Red traffic light | Unpressed | Filters ALV to status 03 rows only |
| `Download to Excel` | Download icon | N/A | Exports current filtered view to Excel |

**Behaviour:**
- When a filter button is clicked, it becomes visually active/pressed; others revert to unpressed
- ALV data updates instantly via `CL_GUI_ALV_GRID->SET_FILTER_CRITERIA`
- Grid title updates to show active filter: e.g., `"Showing: Error Trips | Count: 4"`
- Download exports the currently filtered view with all renamed column headers

---

## Recommended ALV Format Enhancements (R1–R6)

| # | Enhancement | Detail |
|---|---|---|
| **R1** | **Freeze key columns** | LIFNR, TRUCK_NO, TRIP_NO, COUNTER remain fixed while scrolling right — context always visible |
| **R2** | **Auto column width** | All columns auto-sized to content (`col_opt = abap_true`) — no manual resizing needed |
| **R3** | **Subtotal on Distance** | Total KMs per trip displayed at group footer row |
| **R4** | **Tooltip on Remarks column** | Full remarks text shown on hover — avoids need to widen column |
| **R5** | **Layout Variants** | Users can save and reuse personal column arrangements |
| **R6** | **Leg Type cell colouring** | `L` (Loaded Leg) = Blue cell, `E` (Empty Leg) = Yellow cell |

---

## Master Change Register

| # | Change Point | Type | Area |
|---|---|---|---|
| 1 | Authorization-based T-Code (ZCTD_ENRICH auth object, two-stage check) | Config + Code | SE93 + Program |
| 2 | Transporter Code & Vehicle No. — optional selection screen + FM filter | Enhancement | FM + Program |
| 3 | ALV Group by Trip ID | Enhancement | Program |
| 4 | ALV Default Sort (Vendor → Truck → Trip → Count) | Enhancement | Program |
| 5 | Column header renames (Leg Start Date / Leg End Date / CTD Trip Enrichment Remarks) | Enhancement | Program |
| 6 | Add Status Description column (from ZCTD_TRIP_ST domain fixed values) | Enhancement | DDIC + FM + Program |
| 7 | Hide Indicator (IS_NEW_LEG) column | Enhancement | Program |
| 8 | Dynamic page header (Simulation Mode / Normal) | Enhancement | Program |
| 9 | Bug: No ITM updates for status 03; HDR remarks-only update with consolidation | Bug Fix | FM |
| 10 | Bug: Blank source date fallback for empty leg dest date (last leg only) | Bug Fix | FM |
| 11 | Replace ADRC region logic with LIPS/LIKP/T001W/LFA1/KNA1 | Enhancement | FM |
| 12 | ALV row colouring (Light Green = 04, Light Red = 03) | Enhancement | DDIC + FM + Program |
| 13 | Interactive trip count summary + custom filter toolbar buttons + Download | Enhancement | Program |
| R1 | Freeze key columns | Enhancement | Program |
| R2 | Auto column width | Enhancement | Program |
| R3 | Subtotal on Distance per trip | Enhancement | Program |
| R4 | Tooltip on Remarks column | Enhancement | Program |
| R5 | Layout Variants | Enhancement | Program |
| R6 | Leg Type cell colouring (L=Blue, E=Yellow) | Enhancement | Program |

**Total: 13 primary change points + 6 format enhancements**  
**DDIC:** 2 field additions to `ZSCM_ENRICH_DET_ST`  
**FM:** Points 2, 9, 10, 11 (+ LIPS SELECT extension for WERKS)  
**Program:** Points 1–8, 12, 13, R1–R6  
**Basis/Config:** Point 1 (T-Code + Auth Object)  

---

*End of Change Summary Document*  
*Date: 10.03.2026 | Functional Author: Gaurav Prabhu*
