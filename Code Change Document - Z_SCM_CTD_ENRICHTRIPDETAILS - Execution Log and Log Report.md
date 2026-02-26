# Code Change Document – Execution Log and Log Report (CTD Enrichment)

**Related FM:** Z_SCM_CTD_ENRICHTRIPDETAILS  
**Related Program:** ZSCM_CTD_ENRICHTRIPDETAILS  
**New Objects:** Log tables ZSCE_CTD_ENRICH_LOG_HDR, ZSCE_CTD_ENRICH_LOG_ITM; Report ZSCM_CTD_ENRICH_LOG (or equivalent)  
**Reference Tables:** ZSCE_CTD_HDR, ZSCE_CTD_ITM (field reference)  
**Change Type:** New development (Log persistence + Report)  
**Date:** 2026-02-25  
**Reference:** CD: 8086962 TR: RD2K9A5FGT | Technical: Omkar More | Functional: Gaurav Prabhu

---

## 1. Executive Summary

When the enrichment program runs in **normal mode** (Test Run **not** selected):

1. **Run ID:** Each run gets a unique **Run ID** (e.g. timestamp + user).
2. **Log write:** After processing, write one row to **ZSCE_CTD_ENRICH_LOG_HDR** (run summary) and one row per leg to **ZSCE_CTD_ENRICH_LOG_ITM** (trip/leg details with header remark and leg remark).
3. **Header remarks:** Executed = "Trips Executed"; Excluded (loaded leg) = "Trip Excluded - Source / Destination Date missing"; Excluded (empty leg) = "Trip Excluded - Empty Leg Route / Distance missing".
4. **Leg remarks:** Actual error text against the respective loaded leg or empty leg (e.g. "Empty Leg Route / Distance missing" for empty legs).
5. **Log Report:** New report with selection by Run date from/to (and optional User, Run ID). Summary ALV = list of runs; Detail = trips in that run (from LOG) with **Expand all legs** to show combined HDR + ITM data; **Download** option for current ALV.

---

## 2. Requirements Summary

| # | Requirement | Summary |
|---|-------------|--------|
| 1 | **When to log** | Only when program runs in **normal mode** (Test Run not selected). |
| 2 | **Run ID** | Unique per run (e.g. timestamp + user). |
| 3 | **What to store** | Run summary (HDR) + one row per leg (ITM) with trip key, status, header remark, leg remark. |
| 4 | **Header remark – Executed** | "Trips Executed". |
| 5 | **Header remark – Excluded (loaded leg)** | "Trip Excluded - Source / Destination Date missing"; actual error on respective loaded leg(s). |
| 6 | **Header remark – Excluded (empty leg)** | "Trip Excluded - Empty Leg Route / Distance missing"; actual error on respective empty leg(s). |
| 7 | **Report – Input** | Run date from, Run date to; optional Executed by (user), Run ID. |
| 8 | **Report – Summary** | One row per run (from LOG_HDR): Run ID, run date/time, user, from/to date, total trips, executed count, excluded (loaded leg), excluded (empty leg). |
| 9 | **Report – Detail** | First: all trips considered in that run (from LOG_ITM aggregated by trip) with run details from LOG_HDR. **Expand all legs:** combined HDR + ITM as ALV (one row per leg). **Download** for current view. |

---

## 3. New Database Tables (DDIC)

### 3.1 ZSCE_CTD_ENRICH_LOG_HDR (Run summary – one row per run)

| Field | Key | Data Element / Ref | Type | Length | Description |
|-------|-----|-------------------|------|--------|-------------|
| MANDT | Yes | MANDT | CLNT | 3 | Client |
| RUN_ID | Yes | CHAR(20) or domain | CHAR | 20 | Unique run identifier (e.g. timestamp + user) |
| RUN_DATE |  | SYDATUM | DATS | 8 | Date of run |
| RUN_TIME |  | SYSTTIMLO | TIMS | 6 | Time of run |
| EXECUTED_BY |  | SYUNAME | CHAR | 12 | User who executed (ref: ZSCE_CTD_HDR) |
| FROM_DATE |  | DATS | DATS | 8 | Enrichment From Date (selection) |
| TO_DATE |  | DATS | DATS | 8 | Enrichment To Date (selection) |
| TOTAL_TRIPS |  | INT4 | INT4 | 10 | Total trips considered in run |
| TRIPS_EXECUTED |  | INT4 | INT4 | 10 | Count of trips with status '04' |
| TRIPS_EXCL_LOADED |  | INT4 | INT4 | 10 | Count excluded (loaded leg validation) |
| TRIPS_EXCL_EMPTY |  | INT4 | INT4 | 10 | Count excluded (empty leg validation) |

*Note:* Adjust RUN_ID length and domain as per naming standards. Ensure unique index on RUN_ID (or MANDT + RUN_ID).

---

### 3.2 ZSCE_CTD_ENRICH_LOG_ITM (Leg-level log – one row per leg per run)

Fields to align with **ZSCE_CTD_HDR** and **ZSCE_CTD_ITM** where applicable:

| Field | Key | Data Element / Ref | Type | Length | Description |
|-------|-----|-------------------|------|--------|-------------|
| MANDT | Yes | MANDT | CLNT | 3 | Client |
| RUN_ID | Yes | Same as LOG_HDR | CHAR | 20 | Run identifier |
| LIFNR | Yes | LIFNR | CHAR | 10 | Vendor (ref: ZSCE_CTD_HDR) |
| TRUCK_NO | Yes | YTRUCK_NO | CHAR | 15 | Truck (ref: ZSCE_CTD_HDR) |
| TRIP_NO | Yes | ZTRIP_NO | CHAR | 14 | Trip (ref: ZSCE_CTD_HDR) |
| COUNTER | Yes | ZCOUNTER | NUMC | 2 | Leg counter (ref: ZSCE_CTD_ITM) |
| LEG_TYPE |  | ZLEG_TYPE | CHAR | 1 | L/E (ref: ZSCE_CTD_ITM) |
| TRIP_STATUS |  | ZCTD_TRIP_ST | CHAR | 2 | 03/04 (ref: ZSCE_CTD_HDR) |
| HEADER_REMARK |  | ZCTD_RULEENG_REMARKS | CHAR | 255 | Trip-level remark (ref: ZSCE_CTD_HDR.CTD_RULEENG_REMARKS) |
| LEG_REMARK |  | ZCTD_REMARKS | CHAR | 60 | Actual error for this leg (ref: ZSCE_CTD_ITM.REJ_REMARK) |
| AREA |  | YAREA | CHAR | 2 | Optional (ref: ZSCE_CTD_ITM) |

*Note:* HEADER_REMARK holds "Trips Executed" / "Trip Excluded - Source / Destination Date missing" / "Trip Excluded - Empty Leg Route / Distance missing". LEG_REMARK holds the actual error text for that leg. Add more columns from ZSCE_CTD_ITM if needed for report (e.g. SHNUMBER, ROUTE, SOURCE_DATE, DEST_DATE).

---

## 4. When and How to Write the Log (FM)

### 4.1 When

- **Only** when `i_test_run IS INITIAL` (normal mode).
- **After** main processing and **after** COMMIT (or in same LUW if log write is before final COMMIT). Recommended: write log in same LUW as enrichment updates, then COMMIT once.

### 4.2 Run ID

- Generate at **start** of normal-mode run (in calling program or at start of FM when i_test_run is initial). Format example: `RUN_ID = | sy-datum | sy-uzeit | sy-uname |` (e.g. 20260225143022USER01) or use a function to generate a unique ID. Pass Run ID into FM as an **optional importing** parameter so the report can call the FM with a pre-generated Run ID; if not passed, FM can generate it internally for log write.

### 4.3 What to write

**Step 1 – Build run summary**

- Total trips considered = count of all trips that were in scope (before any exclusion; or count of distinct trips from all sources: executed + excluded from Step 2 + Step 4 + Step 8).
- Trips executed = count of trips with action 'S' (status '04').
- Trips excluded (loaded leg) = count of trips excluded in Step 4 (missing source/dest date).
- Trips excluded (empty leg) = count of trips excluded in Step 8 (empty leg route/distance missing).
- From date / To date = i_from_date, i_to_date.
- Run date / Run time = sy-datum, sy-uzeit (or at time of run start). Executed by = sy-uname.

**Step 2 – Insert ZSCE_CTD_ENRICH_LOG_HDR**

- One row with RUN_ID, RUN_DATE, RUN_TIME, EXECUTED_BY, FROM_DATE, TO_DATE, TOTAL_TRIPS, TRIPS_EXECUTED, TRIPS_EXCL_LOADED, TRIPS_EXCL_EMPTY.

**Step 3 – Build leg-level rows**

- For **every** trip that was considered (executed + excluded):
  - **Header remark** per trip:
    - Executed: "Trips Executed".
    - Excluded (Step 4): "Trip Excluded - Source / Destination Date missing".
    - Excluded (Step 8): "Trip Excluded - Empty Leg Route / Distance missing".
  - For each **leg** of that trip: one row for ZSCE_CTD_ENRICH_LOG_ITM with RUN_ID, LIFNR, TRUCK_NO, TRIP_NO, COUNTER, LEG_TYPE, TRIP_STATUS, HEADER_REMARK (same for all legs of trip), LEG_REMARK (actual error for that leg: e.g. "Source/Destination Date missing" for loaded legs in Step 4 trips; "Empty Leg Route / Distance missing" for empty legs in Step 8 excluded trips; blank or "Trip Executed" for executed trips as needed).

**Step 4 – Insert ZSCE_CTD_ENRICH_LOG_ITM**

- INSERT from internal table into ZSCE_CTD_ENRICH_LOG_ITM.

**Data source in FM**

- Executed trips: use lt_enrichments (and lt_trip_headers) after Step 9; header remark = "Trips Executed"; leg remark from enrichment or blank.
- Step 4 excluded: use LT_EXCLUDED_LEGS / LT_EXCLUDED_HEADERS; header remark = "Trip Excluded - Source / Destination Date missing"; leg remark = "Excluded - missing source or destination date" (or text 014).
- Step 8 excluded: from lt_enrichments where action 'E'; header remark = "Trip Excluded - Empty Leg Route / Distance missing"; leg remark = " Empty Leg Route / Distance missing" (text 016) on empty legs.
- Step 2 excluded (outside date range): if stored, one row per trip with header/leg remark "Trip does not fall within the given date range".

---

## 5. FM Interface Change (Optional)

To pass Run ID from report into FM:

- Add **importing** parameter: `I_RUN_ID TYPE CHAR20 OPTIONAL`. If supplied, use it for log; if initial, generate inside FM (e.g. from sy-datum, sy-uzeit, sy-uname).

Calling program (ZSCM_CTD_ENRICHTRIPDETAILS) in normal mode:

- Generate RUN_ID at start (e.g. CONCATENATE sy-datum sy-uzeit sy-uname into lv_run_id). Call FM with I_RUN_ID = lv_run_id. After FM returns, log is already written by FM with that RUN_ID.

---

## 6. Log Report – Input Screen (Selection Screen)

| Parameter | Type | Mandatory | Description |
|-----------|------|-----------|--------------|
| Run date from | DATS | Yes | Start date of run (run timestamp) |
| Run date to | DATS | Yes | End date of run (run timestamp) |
| Executed by | SYUNAME | No | Filter by user (ref: ZSCE_CTD_HDR user fields) |
| Run ID | CHAR(20) | No | Direct search by Run ID |

---

## 7. Log Report – Output Screens and Actions

### 7.1 Summary screen (first output)

- **Source:** ZSCE_CTD_ENRICH_LOG_HDR.
- **Selection:** Run date between Run date from/to; optionally Executed by, Run ID.
- **ALV columns:** Run ID, Run date, Run time, Executed by, From date, To date, Total trips, Trips executed, Trips excluded (loaded leg), Trips excluded (empty leg).
- **User action:** Double-click a row (or select and push "Detail") → open **Detail** for that Run ID.

### 7.2 Detail screen – initial view (trips)

- **Content:** Run summary from ZSCE_CTD_ENRICH_LOG_HDR for selected Run ID + ALV with **one row per trip** (all trips considered in that run).
- **Trip list:** From ZSCE_CTD_ENRICH_LOG_ITM aggregated by RUN_ID, LIFNR, TRUCK_NO, TRIP_NO (e.g. first leg row per trip with HEADER_REMARK, TRIP_STATUS; or aggregate count of legs). Columns: Run ID, LIFNR, TRUCK_NO, TRIP_NO, TRIP_STATUS, HEADER_REMARK, (optional) leg count.
- **Button:** **Expand all legs**.

### 7.3 Detail screen – after "Expand all legs"

- **Content:** Same run info + ALV with **one row per leg** (combined HDR + ITM).
- **Data:** Join ZSCE_CTD_ENRICH_LOG_HDR (Run ID, Run date, Run time, Executed by, From date, To date, Total trips, Trips executed, Trips excluded loaded, Trips excluded empty) with ZSCE_CTD_ENRICH_LOG_ITM (Run ID, LIFNR, TRUCK_NO, TRIP_NO, COUNTER, LEG_TYPE, TRIP_STATUS, HEADER_REMARK, LEG_REMARK, …) on RUN_ID. Display all ITM columns plus key HDR columns (run date, time, user, from/to date, counts) on each row.
- **Download:** Toolbar or button **Download** → export current ALV (trip-level or leg-level) to Excel/CSV.

### 7.4 Action sequence summary

| Step | Action | Result |
|------|--------|--------|
| 1 | User enters Run date from/to, optionally Executed by / Run ID; Execute | Summary ALV (list of runs) |
| 2 | User double-clicks one run | Detail screen: run info + trips ALV |
| 3 | User clicks "Expand all legs" | Detail ALV shows combined HDR + ITM (one row per leg) |
| 4 | User clicks "Download" | Current ALV data exported (Excel/CSV) |

---

## 8. Header and Leg Remark Texts (Constants / Text symbols)

| Usage | Text |
|-------|------|
| Header – Executed | "Trips Executed" |
| Header – Excluded (loaded leg) | "Trip Excluded - Source / Destination Date missing" |
| Header – Excluded (empty leg) | "Trip Excluded - Empty Leg Route / Distance missing" |
| Leg – Empty leg error | "Empty Leg Route / Distance missing" |
| Leg – Loaded leg (source/dest missing) | "Excluded - missing source or destination date" (or use text 014) |
| Leg – Outside date range | "Trip does not fall within the given date range" (or text 011) |

Use same texts in log tables and in main program ALV for consistency.

---

## 9. Implementation Checklist

| # | Task | Object / Location |
|---|------|-------------------|
| 1 | Create table ZSCE_CTD_ENRICH_LOG_HDR | DDIC |
| 2 | Create table ZSCE_CTD_ENRICH_LOG_ITM | DDIC |
| 3 | Generate Run ID in normal mode; pass to FM or generate inside FM | Program / FM |
| 4 | After processing (normal mode), build run summary and insert LOG_HDR | FM Z_SCM_CTD_ENRICHTRIPDETAILS |
| 5 | Build leg-level internal table from lt_enrichments + excluded data; set HEADER_REMARK and LEG_REMARK per rules; insert LOG_ITM | FM |
| 6 | Create report program (e.g. ZSCM_CTD_ENRICH_LOG) with selection screen | Report |
| 7 | Implement Summary ALV from LOG_HDR with drill-down to Detail | Report |
| 8 | Implement Detail: first trips (from LOG_ITM aggregated), then "Expand all legs" (HDR + ITM combined ALV) | Report |
| 9 | Implement Download for current ALV (trip or leg level) | Report |

---

## 10. Test Checklist

- [ ] Normal run: one row in LOG_HDR; N rows in LOG_ITM (one per leg). Run ID unique (timestamp + user).
- [ ] Test run: no rows in LOG_HDR / LOG_ITM.
- [ ] Header remarks correct: Executed / Excluded (loaded) / Excluded (empty).
- [ ] Leg remarks correct on loaded and empty legs.
- [ ] Report: Run date from/to returns correct runs; Summary shows counts; Detail shows trips then Expand all legs shows legs; Download works for both views.
- [ ] Field reference: LOG_ITM uses same data elements as ZSCE_CTD_HDR / ZSCE_CTD_ITM where applicable.

---

*End of document*
