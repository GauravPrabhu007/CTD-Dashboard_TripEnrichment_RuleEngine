# Code Change Document
## ZCTD_RULEENG_EXEC – fetch_eligible_trips SELECT Optimisation

---

| Attribute         | Detail                                          |
|-------------------|-------------------------------------------------|
| **Document**      | Code Change Document (V1)                       |
| **CD Number**     | CD:YYYYYYY *(to be assigned)*                   |
| **Transport**     | TR:YYYYYYYYY *(to be assigned)*                 |
| **Author**        | Omkar More                                      |
| **Date**          | 02.04.2026                                      |
| **SAP System**    | ECC 6.0 / NetWeaver 7.31                        |
| **Approver**      | *(to be assigned)*                              |

---

## Table of Contents

1. [Business Context and Trigger](#1-business-context-and-trigger)
2. [Problem Statement – Current Inefficiency](#2-problem-statement--current-inefficiency)
3. [ABAP Coding Standards Applied](#3-abap-coding-standards-applied)
4. [Change Request – fetch_eligible_trips Optimisation](#4-change-request--fetch_eligible_trips-optimisation)
5. [Impact Analysis](#5-impact-analysis)
6. [Transport Sequence](#6-transport-sequence)
7. [End-to-End Impact on Calling Scenarios](#7-end-to-end-impact-on-calling-scenarios)

---

## 1. Business Context and Trigger

The CTD Rule Engine program `ZCTD_RULEENG_EXEC` evaluates CTD eligibility rules for trips at status `'07'` and advances them to status `'08'`. Previously this ran only as a **periodic batch job** processing all pending trips within a date range.

With the introduction of **CR4** in `Code Change Document - Z_SCM_CTD_TRIPCONFIRM - Auto Trip Confirmation v3.md`, the Rule Engine is now also called as an **immediate background job** after each individual trip reaches status `'07'` (either via manual RTC or automated RTCA). Each such job call passes a **specific trip number** via `s_tripno`.

This change was flagged during the CR4 design review as **Risk 2 – Rule Engine performance**:

> *The current `fetch_eligible_trips` method fetches ALL `TRIP_STATUS = '07'` trips into memory regardless of input, then discards unneeded rows via ABAP-level `DELETE` statements. For targeted single-trip calls, this is highly inefficient — the program reads potentially hundreds of trips only to retain one.*

This document describes the optimisation to eliminate that inefficiency by **pushing filters into the SQL WHERE clause**.

---

## 2. Problem Statement – Current Inefficiency

### 2.1 Current Code – fetch_eligible_trips

```abap
METHOD fetch_eligible_trips.
  DATA: lw_trip_hdr_sel  TYPE gty_trip_hdr_sel,
        lw_trip_hdr_full TYPE zsce_ctd_hdr.

  CLEAR gt_trip_hdr_sel.

  " Fetch confirmed trip headers from database
  SELECT lifnr
         truck_no
         trip_no
         area
         del_ind
         ctd_ruleeng_remarks
         trip_status
         created_by
         created_date
         created_time
    FROM zsce_ctd_hdr CLIENT SPECIFIED
    INTO TABLE gt_trip_hdr_sel
    WHERE mandt       = sy-mandt
      AND trip_status = gc_stat_confirmed.           " '07'
  IF sy-subrc = 0.

    " Remove soft-deleted trips
    DELETE gt_trip_hdr_sel WHERE del_ind = 'X'.

    " FIX: Original WHERE NOT a AND b parsed as (NOT a) AND b,
    " which only filtered created_date < p_fdate. Use two DELETEs instead.
    DELETE gt_trip_hdr_sel WHERE created_date < p_fdate.
    DELETE gt_trip_hdr_sel WHERE created_date > p_tdate.

    " Filter by trip number selection if specified
    IF s_tripno[] IS NOT INITIAL.
      DELETE gt_trip_hdr_sel WHERE trip_no NOT IN s_tripno[].
    ENDIF.

    gv_trips_fetched = lines( gt_trip_hdr_sel ).
    ...
  ELSE.
    MESSAGE 'No data found'(007) TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDMETHOD.
```

### 2.2 Why this is a Problem for Targeted (CR4) Calls

| Scenario | Trips with status '07' in DB | Trips after `s_tripno` DELETE | DB rows read unnecessarily |
|---|---|---|---|
| Batch job (date range, no trip filter) | e.g. 500 | 500 | 0 — date filter limits set, acceptable |
| CR4 targeted call (1 trip) | e.g. 500 | 1 | 499 rows read, transferred to ABAP memory, then discarded |

For a targeted call:
- All 500+ `TRIP_STATUS = '07'` trips are transferred from the database to the ABAP work process memory
- Then the `del_ind` DELETE runs over all 500 rows
- Then two `created_date` DELETE statements run over all rows
- Then the `s_tripno` DELETE reduces the table to 1 row
- **The result is exactly the same as if the SELECT had used `AND trip_no IN s_tripno[]` in the first place**

As CR4 introduces potentially 10 concurrent targeted background job calls (one per trip confirmed), this inefficiency is multiplied tenfold during peak usage.

---

## 3. ABAP Coding Standards Applied

*(Source: `ABAP Rules - 02-04-2026` folder — rules `01-compatibility`, `03-database`, `12-documentation`, `20-code-generation-checklist`)*

| Rule | Application in this Change |
|------|---------------------------|
| **NW 7.31 Compatibility** | No inline declarations. No `@` host variables in SQL. `IN s_tripno[]` syntax is standard NW 7.0+ compliant. |
| **Explicit Field SELECT** | Explicit field list preserved (unchanged from current) |
| **No IS NOT INITIAL in SQL** | `del_ind <> 'X'` used in SQL WHERE for the batch path; `IS INITIAL` retained for ABAP-level DELETE after targeted path |
| **Cursor Code Markers** | Changed block wrapped in `" BEGIN: Cursor Generated Code` / `" END: Cursor Generated Code` |
| **SY-SUBRC Checks** | Both SELECT branches check `sy-subrc`; behaviour consistent with existing code |

---

## 4. Change Request – fetch_eligible_trips Optimisation

### 4.1 Overview

| Item | Detail |
|---|---|
| Object | `ZCTD_RULEENG_EXEC` — include `ZCTD_RULEENG_EXECC01`, method `fetch_eligible_trips` |
| Location | Lines 423–475 (approximate) of the combined program listing |
| Change type | Replace single unconditional SELECT + 4 ABAP DELETE statements with conditional SELECT branching on `s_tripno[]` |
| Functional behaviour | **Unchanged** — same trips are processed; same output; same DB updates |

---

### 4.2 Design Logic

```
IF s_tripno[] IS NOT INITIAL
  → Targeted execution (called from CR4 background job, or operator-entered trip filter)
  → SELECT with AND trip_no IN s_tripno[] in WHERE clause
  → Date filter NOT applied (operator explicitly named the trip; date range is irrelevant)
  → del_ind filter applied via ABAP DELETE (small result set; negligible cost)

ELSE
  → Batch execution (periodic job with date range, no trip filter)
  → SELECT with AND created_date >= p_fdate AND created_date <= p_tdate in WHERE clause
  → del_ind filter pushed into SQL WHERE (AND del_ind <> 'X') to reduce DB transfer
ENDIF
```

---

### 4.3 Current Code (to be replaced)

```abap
  " Fetch confirmed trip headers from database
  SELECT lifnr
         truck_no
         trip_no
         area
         del_ind
         ctd_ruleeng_remarks
         trip_status
         created_by
         created_date
         created_time
    FROM zsce_ctd_hdr CLIENT SPECIFIED
    INTO TABLE gt_trip_hdr_sel
    WHERE mandt       = sy-mandt
      AND trip_status = gc_stat_confirmed.
  IF sy-subrc = 0.

    " Remove soft-deleted trips
    DELETE gt_trip_hdr_sel WHERE del_ind = 'X'.

    " FIX: Original WHERE NOT a AND b parsed as (NOT a) AND b,
    " which only filtered created_date < p_fdate. Use two DELETEs instead.
    DELETE gt_trip_hdr_sel WHERE created_date < p_fdate.
    DELETE gt_trip_hdr_sel WHERE created_date > p_tdate.

    " Filter by trip number selection if specified
    IF s_tripno[] IS NOT INITIAL.
      DELETE gt_trip_hdr_sel WHERE trip_no NOT IN s_tripno[].
    ENDIF.

    gv_trips_fetched = lines( gt_trip_hdr_sel ).
```

---

### 4.4 Replacement Code

**Location:** Replace the SELECT statement and the four `DELETE` statements described in Section 4.3 (from the `SELECT lifnr ...` line through the final `ENDIF.` of the `IF s_tripno[] IS NOT INITIAL` block) with the following:

```abap
" BEGIN: Cursor Generated Code - Change Document: CD:YYYYYYY  TR:YYYYYYYYY / Author:Omkar More on 02.04.2026
    IF s_tripno[] IS NOT INITIAL.
      " Targeted execution: trip number(s) explicitly provided.
      " Push trip_no filter into SQL WHERE to avoid reading all status-07 trips.
      " Date range (p_fdate / p_tdate) is intentionally skipped:
      " the caller already identified the exact trip(s) to process.
      SELECT lifnr
             truck_no
             trip_no
             area
             del_ind
             ctd_ruleeng_remarks
             trip_status
             created_by
             created_date
             created_time
        FROM zsce_ctd_hdr CLIENT SPECIFIED
        INTO TABLE gt_trip_hdr_sel
        WHERE mandt        = sy-mandt
          AND trip_status  = gc_stat_confirmed
          AND trip_no      IN s_tripno[].
      IF sy-subrc <> 0.
        MESSAGE 'No data found'(007) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      " Remove soft-deleted trips from targeted result set
      DELETE gt_trip_hdr_sel WHERE del_ind = 'X'.
    ELSE.
      " Batch execution: no trip filter; apply date range in SQL WHERE
      " to avoid loading all status-07 trips regardless of creation date.
      " del_ind filter also pushed into SQL WHERE to reduce DB transfer further.
      SELECT lifnr
             truck_no
             trip_no
             area
             del_ind
             ctd_ruleeng_remarks
             trip_status
             created_by
             created_date
             created_time
        FROM zsce_ctd_hdr CLIENT SPECIFIED
        INTO TABLE gt_trip_hdr_sel
        WHERE mandt        = sy-mandt
          AND trip_status  = gc_stat_confirmed
          AND del_ind      <> 'X'
          AND created_date >= p_fdate
          AND created_date <= p_tdate.
      IF sy-subrc <> 0.
        MESSAGE 'No data found'(007) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.

    gv_trips_fetched = lines( gt_trip_hdr_sel ).
" END: Cursor Generated Code
```

---

### 4.5 Change Header Update

**Location:** Program header comment block at the top of `ZCTD_RULEENG_EXECC01` (Change History section).

Add the following line:

```abap
*& 02.04.2026   Omkar More   fetch_eligible_trips: Conditional SELECT optimisation (CD:YYYYYYY TR:YYYYYYYYY)
```

---

### 4.6 Summary of Lines Changed

| Action | Lines (approx.) | Description |
|---|---|---|
| Replaced | 14 (SELECT block) + 4 (DELETE statements) + 3 (IF s_tripno block) = 21 lines | Single unconditional SELECT + 4 ABAP DELETEs |
| Added | ~35 lines | Conditional IF/ELSE SELECT block (2 SELECT statements, each with targeted WHERE clause) |
| Unchanged | All remaining method lines | `gv_trips_fetched = lines(...)`, `LOOP AT gt_trip_hdr_sel`, full header table copy, all other methods |

---

## 5. Impact Analysis

### 5.1 Functional Impact

| Scenario | Before | After | Functional Change? |
|---|---|---|---|
| Batch job — date range only | SELECT all status-07 → DELETE by date + del_ind | SELECT with date range + del_ind in WHERE | **None** — same trips processed |
| Batch job — date range + trip filter | SELECT all status-07 → DELETE by date, del_ind, trip_no | SELECT with trip_no IN WHERE (date skipped) | **None** — same trips processed |
| CR4 targeted call — 1 trip | SELECT all status-07 → DELETE by date, del_ind, trip_no | SELECT with single trip_no IN WHERE | **None** — same trip processed; date filter intentionally skipped |

> **Date filter skipped for targeted calls:** When `s_tripno[]` is provided, date filters (`p_fdate`/`p_tdate`) are not applied in SQL. This is correct — the caller (CR4) always passes `p_fdate = sy-datum - 30` as a conservative safeguard, but the true filter is the specific trip number. The trip was just confirmed so it will always fall within a 30-day window. Omitting the date from SQL does not change the result; it simply avoids a redundant condition.

### 5.2 Performance Impact

| Scenario | DB rows read before | DB rows read after | Improvement |
|---|---|---|---|
| Targeted call — 1 trip out of 500 pending | ~500 | 1–2 | ~499 rows eliminated at DB level |
| Batch — 50 trips in 30-day window | ~500 | ~50 (+ del_ind pushed to DB) | ~450 rows eliminated at DB level |
| Batch — 500 trips in window (large run) | ~500 | ~500 | No degradation |

### 5.3 Index Consideration

The optimised WHERE clauses should be supported by the existing `ZSCE_CTD_HDR` table key/index structure:

| WHERE clause | Expected index support |
|---|---|
| `trip_status = '07' AND trip_no IN s_tripno[]` | Primary key (`MANDT + LIFNR + TRUCK_NO + TRIP_NO`) covers `TRIP_NO`; `TRIP_STATUS` should have a secondary index if table is large |
| `trip_status = '07' AND del_ind <> 'X' AND created_date >= ... AND created_date <= ...` | Secondary index on `TRIP_STATUS + CREATED_DATE` would be optimal |

> **Recommendation:** Verify with the Basis/DBA team whether a secondary index on `ZSCE_CTD_HDR (TRIP_STATUS, CREATED_DATE)` exists. If not, consider creating one to maximise the benefit of the batch path optimisation. This is not a prerequisite for this CD but is a low-effort follow-up.

### 5.4 Other Objects Impacted

| Object | Impact |
|---|---|
| `ZCTD_RULEENG_EXECC01` (class include) | **Modified** — `fetch_eligible_trips` method body changed |
| `ZCTD_RULEENG_EXECTOP` (type/data include) | **Unchanged** |
| `ZCTD_RULEENG_EXECSEL` (selection screen include) | **Unchanged** |
| `ZCTD_RULEENG_EXEC` (main report) | **Unchanged** (includes unchanged) |
| All callers of `ZCTD_RULEENG_EXEC` | **No impact** — selection screen interface unchanged; program called via SUBMIT with same parameters |
| SM36/SM37 batch jobs | **No impact** — existing scheduled batch jobs continue to work identically |
| SM37 monitoring (CR4 background jobs) | **Improved** — targeted jobs complete faster; less memory consumption per job |

---

## 6. Transport Sequence

This change is a **single-object transport** containing only `ZCTD_RULEENG_EXECC01`.

| Step | Object | Type | Description |
|------|--------|------|-------------|
| 1 | `ZCTD_RULEENG_EXECC01` | Report Include | Modified `fetch_eligible_trips` method |

> **Dependency:** This CD is independent of `Code Change Document - Z_SCM_CTD_TRIPCONFIRM - Auto Trip Confirmation v3.md`. It can be transported before or after the FM transports. However, for full CR4 performance benefit, it is recommended to transport **this CD first** so that the Rule Engine SELECT is already optimised before the CR4 FMs begin scheduling targeted background jobs.

### Before Transport Checklist
- [ ] Program `ZCTD_RULEENG_EXEC` activates without syntax errors after include change
- [ ] Code Inspector (SCI) clean on `ZCTD_RULEENG_EXECC01`
- [ ] Extended Check (SLIN) clean
- [ ] No breakpoints or debug statements remain
- [ ] Tested in DEV in both modes:
  - [ ] Simulation mode with date range only (existing batch behaviour)
  - [ ] Simulation mode with specific trip number (targeted behaviour)
- [ ] Existing SM36 batch jobs for `ZCTD_RULEENG_EXEC` unaffected in QA
- [ ] Peer review completed
- [ ] Transport description includes CD number and TR number

---

## 7. End-to-End Impact on Calling Scenarios

### 7.1 Before this CD (inefficient targeted call)

```
CR4 background job (ZCTD_RULEENG_AUTO) starts
  └─ SUBMIT zctd_ruleeng_exec WITH s_tripno = 'TRIP001' ...
       fetch_eligible_trips:
         SELECT all TRIP_STATUS = '07' trips → e.g. 500 rows into ABAP memory
         DELETE del_ind = 'X'              → e.g. 495 rows remain
         DELETE created_date < p_fdate     → e.g. 60 rows remain
         DELETE created_date > p_tdate     → e.g. 55 rows remain
         DELETE trip_no NOT IN s_tripno[]  → 1 row remains
       process_all_trips:
         Processes 1 trip
         ...
```

### 7.2 After this CD (efficient targeted call)

```
CR4 background job (ZCTD_RULEENG_AUTO) starts
  └─ SUBMIT zctd_ruleeng_exec WITH s_tripno = 'TRIP001' ...
       fetch_eligible_trips:
         s_tripno[] IS NOT INITIAL → targeted branch
         SELECT ... WHERE trip_status = '07' AND trip_no IN s_tripno[] → 1 row
         DELETE del_ind = 'X' (small result set)
       process_all_trips:
         Processes 1 trip
         ...
```

### 7.3 Batch Job (unchanged behaviour)

```
SM36 scheduled batch job starts
  └─ SUBMIT zctd_ruleeng_exec WITH p_fdate = ... p_tdate = ... (no s_tripno)
       fetch_eligible_trips:
         s_tripno[] IS INITIAL → batch branch
         SELECT ... WHERE trip_status = '07' AND del_ind <> 'X'
                         AND created_date >= p_fdate AND created_date <= p_tdate
         → Returns only trips in date window (50–100 rows typically)
       process_all_trips:
         Processes all trips in range
         ...
```

---

*Document Version: V1 | Created: 02.04.2026 | Author: Omkar More*
*Change Document Number: CD:YYYYYYY (to be assigned by ABAP team)*
*Transport Request: TR:YYYYYYYYY (to be assigned)*
*Related CD: Code Change Document - Z_SCM_CTD_TRIPCONFIRM - Auto Trip Confirmation v3.md (CR4)*
