# Code Change Document – Z_SCM_CTD_EMPTYLEG_DELAY_CALC
## Version 2.0 — Optimised (No SELECT in Loop)

**Function Module:** `Z_SCM_CTD_EMPTYLEG_DELAY_CALC`
**Function Group:** TBD (same function group as existing CTD Rule FMs)
**Change Type:** New Function Module + DDIC Changes + Program Enhancement
**Date:** 2026-04-02
**Reference:** CD: TBD | TR: TBD
**Technical Author:** TBD
**Functional Author:** Gaurav Prabhu
**FS Reference:** `Tcode for CTD Rules/FM Logic.txt`

| Version | Date | Author | Change |
|---|---|---|---|
| 1.0 | 2026-04-01 | Gaurav Prabhu | Initial version |
| 2.0 | 2026-04-02 | Gaurav Prabhu | Removed SELECT in loop — replaced with pre-fetch + parallel cursor pattern; fully ABAP rules compliant |

---

## Executive Summary (for ABAP Team)

This change delivers:

1. **DDIC Changes** — 3 new data elements + 3 new fields added to `ZSCE_CTD_ITM` and mirror structure `ZSCM_CTD_TRIPITM`.
2. **New FM `Z_SCM_CTD_EMPTYLEG_DELAY_CALC`** — Calculation FM (PROCESS_TYPE = `CALC`) called by `ZCTD_RULEENG_EXEC`. Key design:
   - All DB reads happen **before** the main trip loop (pre-fetch pattern).
   - Trip → Business/Sub-Business mapping built in a single pre-processing pass.
   - `ZCTD_DAVG_DIS` and `ZLOG_EXEC_VAR` fetched using **FOR ALL ENTRIES** (bulk, outside loop).
   - Inside the main loop: only **READ TABLE BINARY SEARCH** and **parallel cursor** — zero DB calls.
   - For each Empty Leg (`LEG_TYPE = 'E'`), calculates Ideal Transit Days, Actual Transit Days, and Delay (net of buffer). Results written back via `ASSIGNING` field symbol.
3. **Enhancement to `ZCTD_RULEENG_EXEC`** — `update_database` method extended to persist 3 new delay fields.
4. **Config entry in `ZCTD_RULE_PROC`** — Register FM with `PROCESS_TYPE = 'CALC'`.

---

## 1. Pre-requisites: DDIC Changes (SE11)

### 1.1 Create New Data Elements

| Data Element | Type | Length | Short Description |
|---|---|:---:|---|
| `ZERUN_IDEAL_DAYS` | INT4 | 10 | Empty Run Ideal Transit Days |
| `ZERUN_ACTUAL_DAYS` | INT4 | 10 | Empty Run Actual Transit Days |
| `ZERUN_DELAY_DAYS` | INT4 | 10 | Empty Run Delay in Days (neg = early) |

> INT4 (4-byte signed integer) used for all three. `ZERUN_DELAY_DAYS` stores negative values (early arrival), zero (on-time), or positive (delayed).

---

### 1.2 Modify Transparent Table — `ZSCE_CTD_ITM`

Append 3 new fields after existing fields:

| Seq | Field | Data Element | Type | Key | Short Description |
|---|---|---|---|:---:|---|
| +1 | `ERUN_IDEAL_DAYS` | `ZERUN_IDEAL_DAYS` | INT4 | | Empty Run Ideal Transit Days |
| +2 | `ERUN_ACTUAL_DAYS` | `ZERUN_ACTUAL_DAYS` | INT4 | | Empty Run Actual Transit Days |
| +3 | `ERUN_DELAY_DAYS` | `ZERUN_DELAY_DAYS` | INT4 | | Empty Run Delay in Days |

> ⚠️ Live data table. Add via Append Structure or direct field addition. No key changes. Confirm with Basis/DB team before activating.

---

### 1.3 Modify Structure — `ZSCM_CTD_TRIPITM`

Add same 3 fields (same data elements, same order as §1.2). Activation of `ZSCM_CTD_TRIPITM` automatically propagates to `ZSCM_CTD_TRIPITM_TT`.

---

### 1.4 Activation Sequence

1. Data elements (`ZERUN_IDEAL_DAYS`, `ZERUN_ACTUAL_DAYS`, `ZERUN_DELAY_DAYS`)
2. Table `ZSCE_CTD_ITM`
3. Structure `ZSCM_CTD_TRIPITM`

---

## 2. New Function Module — `Z_SCM_CTD_EMPTYLEG_DELAY_CALC`

### 2.1 FM Interface

```
IMPORTING:
  VALUE(IV_SIMULATION_MODE)  TYPE  CHAR1               OPTIONAL

EXPORTING:
  VALUE(EV_RULE_STATUS)      TYPE  FLAG
  VALUE(EV_RULE_MESSAGE)     TYPE  STRING

TABLES:
  IT_TRIP_HEADER             TYPE  ZSCM_TRIPHDR_TT     OPTIONAL
  IT_TRIP_LEGS               TYPE  ZSCM_CTD_TRIPITM_TT OPTIONAL
```

**EV_RULE_STATUS Values:**

| Value | Meaning | Condition |
|---|---|---|
| `'S'` | Success — at least 1 empty leg calculated | `lv_legs_processed > 0` |
| `'N'` | Not applicable — no empty legs / no config | `lv_legs_processed = 0` |
| `'E'` | System error | Exception caught in TRY/CATCH |

---

### 2.2 FM Code — Optimised Version

```abap
FUNCTION z_scm_ctd_emptyleg_delay_calc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_SIMULATION_MODE) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(EV_RULE_STATUS) TYPE  FLAG
*"     VALUE(EV_RULE_MESSAGE) TYPE  STRING
*"  TABLES
*"      IT_TRIP_HEADER TYPE  ZSCM_TRIPHDR_TT OPTIONAL
*"      IT_TRIP_LEGS TYPE  ZSCM_CTD_TRIPITM_TT OPTIONAL
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module: Z_SCM_CTD_EMPTYLEG_DELAY_CALC
*&---------------------------------------------------------------------*
*& Purpose: CTD Calculation Rule - Empty Leg Transit Delay
*& Description: Pre-fetches all config data before the main trip loop.
*&              For each Trip, derives Business+Sub-Business from the
*&              1st Loaded Leg (LEG_TYPE='L') and looks up:
*&                ZCTD_DAVG_DIS  : Average km per day (validity-date checked)
*&                ZLOG_EXEC_VAR  : Buffer days (NAME=CTD_ELEG_DELAY_BUFFER)
*&              For each Empty Leg (LEG_TYPE='E'), calculates:
*&                Ideal Transit Days  = CEIL(Distance / Avg Per Day Dist)
*&                Actual Transit Days = VEND_DEST_DATE - VEND_SOURCE_DATE
*&                Delay (days)        = Actual - Ideal - Buffer Days
*&              Results written to ERUN_IDEAL_DAYS, ERUN_ACTUAL_DAYS,
*&              ERUN_DELAY_DAYS on the empty leg row in IT_TRIP_LEGS.
*&              Buffer defaults to 0 if no active config found.
*& Author: TBD
*& Creation Date: TBD
*& CD: TBD  TR: TBD
*& Technical Author  : TBD
*& Functional Author : Gaurav Prabhu
*& SAP Version: NetWeaver 7.31
*& Change History:
*& Date       User    Description
*& 2026-04-01 TBD     Initial creation
*& 2026-04-02 TBD     v2: Removed SELECT in loop; pre-fetch pattern applied
*&---------------------------------------------------------------------*

  " BEGIN: Cursor Generated Code

  " ===================================================================
  " TYPE DEFINITIONS (lty_ prefix per naming convention)
  " ===================================================================
  TYPES:
    " Trip → Business/Sub-Business mapping (1st loaded leg per trip)
    BEGIN OF lty_trip_biz,
      trip_no      TYPE ztrip_no,
      business     TYPE zbusiness_id,
      sub_business TYPE zsubusiness_id,
    END OF lty_trip_biz,

    " Unique Business+Sub-Business key — FOR ALL ENTRIES driving table
    BEGIN OF lty_biz_key,
      business     TYPE zbusiness_id,
      sub_business TYPE zsubusiness_id,
    END OF lty_biz_key,

    " Avg distance config — fields match SELECT from ZCTD_DAVG_DIS exactly
    " SELECT: business, sub_business, start_dt, end_date, ctd_dayavg_dist
    BEGIN OF lty_avg_config,
      business        TYPE zbusiness_id,
      sub_business    TYPE zsubusiness_id,
      start_dt        TYPE zscm_start_dt_de,
      end_date        TYPE zend_dt,
      ctd_dayavg_dist TYPE zscm_dayavg_dis_de,
    END OF lty_avg_config,

    " Buffer days config — fields match SELECT from ZLOG_EXEC_VAR exactly
    " SELECT: remarks, errormsg, zdays
    BEGIN OF lty_buffer_config,
      remarks  TYPE textr,    " CHAR 72 — Business ID stored in REMARKS
      errormsg TYPE natxt,    " CHAR 73 — Sub-Business ID stored in ERRORMSG
      zdays    TYPE i,        " INT4    — ABAP converts from NUMC 3 on SELECT
    END OF lty_buffer_config.

  " ===================================================================
  " CONSTANTS (lc_ prefix per naming convention)
  " ===================================================================
  CONSTANTS:
    lc_status_success   TYPE char1      VALUE 'S',
    lc_status_not_appld TYPE char1      VALUE 'N',
    lc_status_error     TYPE char1      VALUE 'E',

    lc_leg_type_loaded  TYPE char1      VALUE 'L',
    lc_leg_type_empty   TYPE char1      VALUE 'E',

    lc_active_x         TYPE char1      VALUE 'X',
    lc_buffer_name      TYPE rvari_vnam VALUE 'CTD_ELEG_DELAY_BUFFER'.

  CONSTANTS lc_msg_success   TYPE string
    VALUE 'Empty Leg Delay calculated successfully' ##NO_TEXT.
  CONSTANTS lc_msg_not_appld TYPE string
    VALUE 'No empty legs processed - config not found or no empty legs' ##NO_TEXT.
  CONSTANTS lc_sys_error     TYPE string
    VALUE 'System error:' ##NO_TEXT.

  " ===================================================================
  " DATA DECLARATIONS (all upfront per NW 7.31 — no inline DATA)
  " ===================================================================
  DATA:
    " --- Pre-fetch tables (populated before main loop) ---
    " Trip → Business/Sub-Business mapping
    lt_trip_biz        TYPE STANDARD TABLE OF lty_trip_biz,
    lw_trip_biz        TYPE lty_trip_biz,

    " Unique biz key — driving table for FOR ALL ENTRIES
    lt_biz_unique      TYPE STANDARD TABLE OF lty_biz_key,
    lw_biz_key         TYPE lty_biz_key,

    " Avg distance config (pre-fetched from ZCTD_DAVG_DIS)
    lt_avg_config      TYPE STANDARD TABLE OF lty_avg_config,
    lw_avg_config      TYPE lty_avg_config,

    " Buffer days config (pre-fetched from ZLOG_EXEC_VAR)
    lt_buffer_config   TYPE STANDARD TABLE OF lty_buffer_config,
    lw_buffer_config   TYPE lty_buffer_config,

    " --- Key variables for READ TABLE type matching ---
    " REMARKS (CHAR 72) / ERRORMSG (CHAR 73) key variables for buffer lookup
    lv_remarks_key     TYPE textr,
    lv_errmsg_key      TYPE natxt,

    " --- Per-trip working variables ---
    lv_business        TYPE zbusiness_id,
    lv_sub_business    TYPE zsubusiness_id,
    lv_created_date    TYPE sydatum,
    lv_dayavg_dist     TYPE zscm_dayavg_dis_de,
    lv_buffer_days     TYPE i,

    " --- Arithmetic variables for day calculation ---
    lv_distance_p      TYPE p DECIMALS 4,
    lv_dayavg_p        TYPE p DECIMALS 4,
    lv_division        TYPE p DECIMALS 6,
    lv_ideal_days      TYPE i,
    lv_actual_days     TYPE i,
    lv_delay_days      TYPE i,

    " --- Processing state ---
    lv_legs_processed  TYPE i,
    lv_prev_trip       TYPE ztrip_no,

    " --- Parallel cursor index ---
    lv_tabix           TYPE sy-tabix,

    " --- Exception handling ---
    lo_exception       TYPE REF TO cx_root,
    lv_error_text      TYPE string.

  FIELD-SYMBOLS:
    <lfs_trip_header> TYPE zscm_triphdr_st,
    <lfs_trip_legs>   TYPE zscm_ctd_tripitm.

  " ===================================================================
  " INITIALIZATION
  " ===================================================================
  CLEAR: ev_rule_status,
         ev_rule_message.

  lv_legs_processed = 0.

  TRY.

      " ==================================================================
      " PRE-PROCESSING — Runs once before the main trip loop
      " All database reads are performed here (zero DB calls in the loop)
      " ==================================================================

      " ------------------------------------------------------------------
      " Pre-Step 1: Sort IT_TRIP_LEGS by trip_no and leg_type.
      " 'E' (Empty, ASCII 69) sorts before 'L' (Loaded, ASCII 76).
      " Enables binary search on compound key: trip_no + leg_type.
      " ------------------------------------------------------------------
      SORT it_trip_legs BY trip_no leg_type.

      " ------------------------------------------------------------------
      " Pre-Step 2: Build Trip → Business/Sub-Business Mapping.
      " Single pass through IT_TRIP_LEGS. Captures 1st loaded leg per trip.
      " Business+Sub-Business is consistent across all legs of a trip —
      " derived once from the first loaded leg (LEG_TYPE = 'L').
      " ------------------------------------------------------------------
      CLEAR: lt_trip_biz, lv_prev_trip.

      LOOP AT it_trip_legs ASSIGNING <lfs_trip_legs>.
        " Skip non-loaded legs
        IF <lfs_trip_legs>-leg_type <> lc_leg_type_loaded.
          CONTINUE.
        ENDIF.
        " Skip if already captured the first loaded leg for this trip
        IF <lfs_trip_legs>-trip_no = lv_prev_trip.
          CONTINUE.
        ENDIF.
        " First loaded leg for this trip — capture business info
        lw_trip_biz-trip_no      = <lfs_trip_legs>-trip_no.
        lw_trip_biz-business     = <lfs_trip_legs>-business.
        lw_trip_biz-sub_business = <lfs_trip_legs>-sub_business.
        APPEND lw_trip_biz TO lt_trip_biz.
        lv_prev_trip = <lfs_trip_legs>-trip_no.
        CLEAR lw_trip_biz.
      ENDLOOP.
      SORT lt_trip_biz BY trip_no.

      " ------------------------------------------------------------------
      " Pre-Step 3: Build Unique Business+Sub-Business Key Table.
      " Deduplicated — used as driving table for FOR ALL ENTRIES.
      " ------------------------------------------------------------------
      CLEAR lt_biz_unique.

      LOOP AT lt_trip_biz INTO lw_trip_biz.
        lw_biz_key-business     = lw_trip_biz-business.
        lw_biz_key-sub_business = lw_trip_biz-sub_business.
        APPEND lw_biz_key TO lt_biz_unique.
        CLEAR lw_biz_key.
      ENDLOOP.
      SORT lt_biz_unique BY business sub_business.
      DELETE ADJACENT DUPLICATES FROM lt_biz_unique
        COMPARING business sub_business.

      " ------------------------------------------------------------------
      " Pre-Step 4: Bulk Fetch ZCTD_DAVG_DIS — Average Distance Config.
      " Fetches ALL validity periods for the relevant Business+Sub-Business
      " combinations. Date range filtering is done in ABAP (Step B in the
      " main loop) since different trips may have different creation dates.
      " ------------------------------------------------------------------
      CLEAR lt_avg_config.

      IF lt_biz_unique IS NOT INITIAL.
        " SELECT fields: business, sub_business, start_dt, end_date, ctd_dayavg_dist
        " Structure lty_avg_config matches SELECT field order exactly
        SELECT business sub_business start_dt end_date ctd_dayavg_dist
          FROM zctd_davg_dis CLIENT SPECIFIED
          INTO TABLE lt_avg_config
          FOR ALL ENTRIES IN lt_biz_unique
          WHERE mandt        = sy-mandt
            AND business     = lt_biz_unique-business
            AND sub_business = lt_biz_unique-sub_business.
        IF sy-subrc = 0.
          SORT lt_avg_config BY business sub_business start_dt.
        ENDIF.
      ENDIF.

      " ------------------------------------------------------------------
      " Pre-Step 5: Bulk Fetch ZLOG_EXEC_VAR — Buffer Days Config.
      " Fetches buffer days for CTD_ELEG_DELAY_BUFFER parameter,
      " filtered by Business (REMARKS) and Sub-Business (ERRORMSG).
      " Active records only (ACTIVE = 'X').
      " Default behaviour: buffer = 0 if no record found (no interruption).
      " ------------------------------------------------------------------
      CLEAR lt_buffer_config.

      IF lt_biz_unique IS NOT INITIAL.
        " SELECT fields: remarks, errormsg, zdays
        " Structure lty_buffer_config matches SELECT field order exactly
        SELECT remarks errormsg zdays
          FROM zlog_exec_var CLIENT SPECIFIED
          INTO TABLE lt_buffer_config
          FOR ALL ENTRIES IN lt_biz_unique
          WHERE mandt    = sy-mandt
            AND name     = lc_buffer_name
            AND active   = lc_active_x
            AND remarks  = lt_biz_unique-business
            AND errormsg = lt_biz_unique-sub_business.
        IF sy-subrc = 0.
          SORT lt_buffer_config BY remarks errormsg.
        ENDIF.
      ENDIF.

      " ==================================================================
      " MAIN LOOP — Process Each Trip
      " Zero DB calls inside this loop — all lookups use READ TABLE
      " BINARY SEARCH on pre-fetched in-memory tables only.
      " ==================================================================
      LOOP AT it_trip_header ASSIGNING <lfs_trip_header>.

        CLEAR: lv_business,
               lv_sub_business,
               lv_dayavg_dist,
               lv_buffer_days,
               lv_created_date.

        " Use trip creation date as validity reference for config lookup
        lv_created_date = <lfs_trip_header>-created_date.
        IF lv_created_date IS INITIAL.
          lv_created_date = sy-datum.
        ENDIF.

        " ----------------------------------------------------------------
        " Step A: Get Business + Sub-Business from pre-built mapping.
        " READ TABLE with BINARY SEARCH — O(log n), zero DB access.
        " ----------------------------------------------------------------
        READ TABLE lt_trip_biz INTO lw_trip_biz
          WITH KEY trip_no = <lfs_trip_header>-trip_no
          BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.   " No loaded leg for this trip — skip
        ENDIF.
        IF lw_trip_biz-business IS INITIAL OR
           lw_trip_biz-sub_business IS INITIAL.
          CONTINUE.   " Business info blank on loaded leg — skip
        ENDIF.

        lv_business     = lw_trip_biz-business.
        lv_sub_business = lw_trip_biz-sub_business.

        " ----------------------------------------------------------------
        " Step B: Find valid Average Distance from pre-fetched config.
        " Pattern: READ TABLE BINARY SEARCH to jump to the key range,
        " then parallel cursor (LOOP FROM lv_tabix with EXIT) to find
        " the record whose validity period covers the trip creation date.
        " This is NOT a nested LOOP with WHERE — it is the approved
        " parallel cursor technique (O(n) total, not O(n²)).
        " ----------------------------------------------------------------
        CLEAR: lw_avg_config, lv_dayavg_dist.

        READ TABLE lt_avg_config INTO lw_avg_config
          WITH KEY business     = lv_business
                   sub_business = lv_sub_business
          BINARY SEARCH.
        IF sy-subrc = 0.
          lv_tabix = sy-tabix.
          LOOP AT lt_avg_config INTO lw_avg_config FROM lv_tabix.
            " Exit when the key range for this biz/sub-biz is exhausted
            IF lw_avg_config-business     <> lv_business OR
               lw_avg_config-sub_business <> lv_sub_business.
              EXIT.
            ENDIF.
            " Date range check: trip creation date within validity period
            IF lw_avg_config-start_dt <= lv_created_date AND
               lw_avg_config-end_date >= lv_created_date.
              lv_dayavg_dist = lw_avg_config-ctd_dayavg_dist.
              EXIT.   " Valid record found — stop
            ENDIF.
          ENDLOOP.
        ENDIF.

        " Skip trip if no valid config found or avg distance is zero
        IF lv_dayavg_dist = 0.
          CONTINUE.
        ENDIF.

        " ----------------------------------------------------------------
        " Step C: Get Buffer Days from pre-fetched buffer config.
        " READ TABLE with BINARY SEARCH — O(log n), zero DB access.
        " Assign matching key variables to REMARKS/ERRORMSG field types
        " to ensure correct ABAP type comparison (CHAR 40 → CHAR 72/73).
        " ----------------------------------------------------------------
        CLEAR: lw_buffer_config, lv_buffer_days,
               lv_remarks_key,   lv_errmsg_key.

        lv_remarks_key = lv_business.      " CHAR 40 → CHAR 72 (right-padded)
        lv_errmsg_key  = lv_sub_business.  " CHAR 40 → CHAR 73 (right-padded)

        READ TABLE lt_buffer_config INTO lw_buffer_config
          WITH KEY remarks  = lv_remarks_key
                   errormsg = lv_errmsg_key
          BINARY SEARCH.
        IF sy-subrc = 0.
          lv_buffer_days = lw_buffer_config-zdays.
        ELSE.
          lv_buffer_days = 0.   " No active buffer config — default to zero
        ENDIF.

        " ----------------------------------------------------------------
        " Step D: Calculate Delay for Each Empty Leg (LEG_TYPE = 'E').
        " Parallel cursor on IT_TRIP_LEGS — O(n) total across all trips.
        " Results written directly via ASSIGNING field symbol (no MODIFY).
        " ----------------------------------------------------------------
        READ TABLE it_trip_legs
          WITH KEY trip_no  = <lfs_trip_header>-trip_no
                   leg_type = lc_leg_type_empty
          BINARY SEARCH
          TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          lv_tabix = sy-tabix.
          LOOP AT it_trip_legs ASSIGNING <lfs_trip_legs> FROM lv_tabix.
            IF <lfs_trip_legs>-trip_no  <> <lfs_trip_header>-trip_no OR
               <lfs_trip_legs>-leg_type <> lc_leg_type_empty.
              EXIT.   " Moved past this trip's empty legs — stop
            ENDIF.

            " Skip empty leg if vendor dates not yet entered by Transporter
            IF <lfs_trip_legs>-vend_source_date IS INITIAL OR
               <lfs_trip_legs>-vend_dest_date   IS INITIAL.
              CONTINUE.
            ENDIF.

            CLEAR: lv_distance_p,
                   lv_dayavg_p,
                   lv_division,
                   lv_ideal_days,
                   lv_actual_days,
                   lv_delay_days.

            " ----------------------------------------------------------
            " Ideal Transit Days = CEIL( Distance / Avg Per Day Distance )
            " Partial days round up to full transit day (ceiling function).
            " ----------------------------------------------------------
            lv_distance_p = <lfs_trip_legs>-distance.
            lv_dayavg_p   = lv_dayavg_dist.

            IF lv_distance_p > 0 AND lv_dayavg_p > 0.
              lv_division   = lv_distance_p / lv_dayavg_p.
              lv_ideal_days = CEIL( lv_division ).
            ELSE.
              lv_ideal_days = 0.
            ENDIF.

            " ----------------------------------------------------------
            " Actual Transit Days = VEND_DEST_DATE - VEND_SOURCE_DATE
            " Vendor-entered dates on the empty leg confirmation screen.
            " Date subtraction gives difference in full calendar days.
            " ----------------------------------------------------------
            lv_actual_days = <lfs_trip_legs>-vend_dest_date
                           - <lfs_trip_legs>-vend_source_date.

            " ----------------------------------------------------------
            " Delay = Actual - Ideal - Buffer Days
            " +ve = delayed beyond buffer tolerance
            "  0  = exactly on time (within buffer)
            " -ve = early or within buffer
            " ----------------------------------------------------------
            lv_delay_days = lv_actual_days - lv_ideal_days - lv_buffer_days.

            " ----------------------------------------------------------
            " Write results back to the empty leg row.
            " Direct assignment via ASSIGNING — no MODIFY FROM needed.
            " ----------------------------------------------------------
            <lfs_trip_legs>-erun_ideal_days  = lv_ideal_days.
            <lfs_trip_legs>-erun_actual_days = lv_actual_days.
            <lfs_trip_legs>-erun_delay_days  = lv_delay_days.

            lv_legs_processed = lv_legs_processed + 1.

          ENDLOOP.
        ENDIF.

      ENDLOOP.   " End main trip header loop

      " ==================================================================
      " POST-PROCESSING — Set Export Parameters
      " ==================================================================
      IF lv_legs_processed > 0.
        ev_rule_status  = lc_status_success.
        ev_rule_message = lc_msg_success.
      ELSE.
        ev_rule_status  = lc_status_not_appld.
        ev_rule_message = lc_msg_not_appld.
      ENDIF.

    CATCH cx_root INTO lo_exception.
      lv_error_text  = lo_exception->get_text( ).
      ev_rule_status = lc_status_error.
      CONCATENATE lc_sys_error lv_error_text
        INTO ev_rule_message SEPARATED BY space.

  ENDTRY.

  " END: Cursor Generated Code

ENDFUNCTION.
```

---

## 3. Enhancement to `ZCTD_RULEENG_EXEC` — `update_database` Method

**Locate this existing block in `ZCTD_RULEENG_EXECC01`:**

```abap
UPDATE zsce_ctd_itm  CLIENT SPECIFIED
  SET ctd_eligible  = lw_trip_itm-ctd_eligible
      modified_by   = lw_trip_itm-modified_by
      modified_date = lw_trip_itm-modified_date
      modified_time = lw_trip_itm-modified_time
  WHERE mandt    = sy-mandt
    AND lifnr    = lw_trip_itm-lifnr
    AND truck_no = lw_trip_itm-truck_no
    AND trip_no  = lw_trip_itm-trip_no
    AND counter  = lw_trip_itm-counter.
```

**Replace with:**

```abap
UPDATE zsce_ctd_itm  CLIENT SPECIFIED
  SET ctd_eligible       = lw_trip_itm-ctd_eligible
      erun_ideal_days    = lw_trip_itm-erun_ideal_days
      erun_actual_days   = lw_trip_itm-erun_actual_days
      erun_delay_days    = lw_trip_itm-erun_delay_days
      modified_by        = lw_trip_itm-modified_by
      modified_date      = lw_trip_itm-modified_date
      modified_time      = lw_trip_itm-modified_time
  WHERE mandt    = sy-mandt
    AND lifnr    = lw_trip_itm-lifnr
    AND truck_no = lw_trip_itm-truck_no
    AND trip_no  = lw_trip_itm-trip_no
    AND counter  = lw_trip_itm-counter.
```

> For loaded legs, `erun_*` fields are 0 (initial) — correct and expected.

---

## 4. Rule Engine Config — `ZCTD_RULE_PROC`

| Field | Value |
|---|---|
| `RULEMAP_ID` | `<applicable rulemap>` |
| `PROCESS_TYPE` | `CALC` |
| `SEQ_NO` | 999 (or last sequence) |
| `FM_NAME` | `Z_SCM_CTD_EMPTYLEG_DELAY_CALC` |
| `EXEC_CONDITION` | `CTD_ELIGIBLE` |
| `ACTIVE` | `X` |
| `DESCRIPTION` | `Empty Leg Transit Delay Calculation` |

---

## 5. Logic Summary

### Pre-Processing (runs once, before main loop)

| Step | Action | Result |
|---|---|---|
| Pre-1 | `SORT it_trip_legs BY trip_no leg_type` | Enables binary search on 2-key compound |
| Pre-2 | Loop `it_trip_legs` — capture 1st loaded leg per trip | `lt_trip_biz` (trip→biz mapping, sorted by trip_no) |
| Pre-3 | Deduplicate biz combinations | `lt_biz_unique` (FOR ALL ENTRIES driver) |
| Pre-4 | `SELECT FROM zctd_davg_dis FOR ALL ENTRIES` | `lt_avg_config` (sorted by business, sub_business, start_dt) |
| Pre-5 | `SELECT FROM zlog_exec_var FOR ALL ENTRIES` | `lt_buffer_config` (sorted by remarks, errormsg) |

### Main Loop (zero DB calls per iteration)

| Step | Action | Technique |
|---|---|---|
| A | Get Business/Sub-Business for trip | READ `lt_trip_biz` BINARY SEARCH |
| B | Find valid avg distance (date range) | READ `lt_avg_config` BINARY SEARCH + parallel cursor EXIT |
| C | Get buffer days | READ `lt_buffer_config` BINARY SEARCH |
| D | For each empty leg: calculate + write back | Parallel cursor on `it_trip_legs` + ASSIGNING |

### Per Empty Leg Calculation

| Output Field | Formula | Notes |
|---|---|---|
| `ERUN_IDEAL_DAYS` | `CEIL( DISTANCE / CTD_DAYAVG_DIST )` | Partial day rounds up |
| `ERUN_ACTUAL_DAYS` | `VEND_DEST_DATE – VEND_SOURCE_DATE` | Vendor dates on empty leg |
| `ERUN_DELAY_DAYS` | `ERUN_ACTUAL_DAYS – ERUN_IDEAL_DAYS – Buffer Days` | +ve=delay; 0/−ve=on-time/early |

### Skip Conditions

| Condition | Action |
|---|---|
| No loaded leg for trip | Skip trip (READ lt_trip_biz → sy-subrc ≠ 0) |
| BUSINESS or SUB_BUSINESS blank on loaded leg | Skip trip |
| No valid ZCTD_DAVG_DIS record for date | Skip trip (lv_dayavg_dist = 0) |
| CTD_DAYAVG_DIST = 0 | Skip trip (guard prevents divide-by-zero) |
| No ZLOG_EXEC_VAR record | Proceed with lv_buffer_days = 0 (no interruption) |
| VEND_SOURCE_DATE or VEND_DEST_DATE initial | Skip that empty leg only |
| DISTANCE = 0 on empty leg | ERUN_IDEAL_DAYS = 0 (guard in code) |

---

## 6. ABAP Compliance Checklist (New ABAP Rules — 02-04-2026)

| Rule | Status | How Applied |
|---|---|---|
| **No inline declarations** | ✅ | All variables declared upfront; no `DATA(x)` |
| **No `NEW` / `VALUE` / `CORRESPONDING`** | ✅ | Not used |
| **No `SELECT *`** | ✅ | All SELECTs name explicit fields; lty_ types match order |
| **No SELECT in loop** | ✅ | All SELECTs in pre-processing; zero DB calls in main loop |
| **No nested LOOP with WHERE** | ✅ | Step B uses READ + parallel cursor (LOOP FROM tabix + EXIT), not LOOP WHERE |
| **SORT before BINARY SEARCH** | ✅ | All tables sorted before first READ TABLE with BINARY SEARCH |
| **FOR ALL ENTRIES guard** | ✅ | `IF lt_biz_unique IS NOT INITIAL` before both FOR ALL ENTRIES |
| **Deduplicate before FOR ALL ENTRIES** | ✅ | `DELETE ADJACENT DUPLICATES` applied to `lt_biz_unique` |
| **Internal table structure matches SELECT** | ✅ | `lty_avg_config` and `lty_buffer_config` match SELECT field order exactly |
| **No `IS NOT INITIAL` in WHERE clause** | ✅ | Only in ABAP IF checks |
| **`SY-SUBRC` checked immediately** | ✅ | After every SELECT, READ TABLE |
| **ASSIGNING for table modification** | ✅ | Empty leg rows updated via `<lfs_trip_legs>` ASSIGNING — no MODIFY FROM |
| **Naming: `lv_`, `lt_`, `lw_`, `lty_`, `lc_`, `<lfs_>`** | ✅ | Applied throughout |
| **No `sy-*` in CONSTANTS** | ✅ | `sy-datum`, `sy-mandt` used only in executable statements |
| **No FORMs** | ✅ | FM is required pattern for Rule Engine — no FORM/PERFORM |
| **Division by zero guard** | ✅ | `IF lv_distance_p > 0 AND lv_dayavg_p > 0` before division |
| **TRY/CATCH** | ✅ | Entire logic wrapped; returns `EV_RULE_STATUS = 'E'` on exception |

---

## 7. Performance Impact (v1 vs v2)

| Metric | v1 (SELECT in loop) | v2 (Pre-fetch) |
|---|---|---|
| DB calls per 100 trips | 200 (2 × 100) | **2** (1 per table, bulk) |
| DB calls per 500 trips | 1,000 | **2** |
| Config lookup inside loop | SELECT UP TO 1 ROWS | READ TABLE BINARY SEARCH O(log n) |
| ABAP rule compliance | ❌ SELECT in loop | ✅ Fully compliant |

---

## 8. Implementation Checklist

| # | Task | Tool |
|---|---|---|
| 1 | Create data element `ZERUN_IDEAL_DAYS` (INT4) | SE11 |
| 2 | Create data element `ZERUN_ACTUAL_DAYS` (INT4) | SE11 |
| 3 | Create data element `ZERUN_DELAY_DAYS` (INT4) | SE11 |
| 4 | Add `ERUN_IDEAL_DAYS`, `ERUN_ACTUAL_DAYS`, `ERUN_DELAY_DAYS` to `ZSCE_CTD_ITM` | SE11 |
| 5 | Add same 3 fields to `ZSCM_CTD_TRIPITM` | SE11 |
| 6 | Activate all DDIC objects (data elements → table → structure) | SE11 |
| 7 | Create FM `Z_SCM_CTD_EMPTYLEG_DELAY_CALC` with interface per §2.1 | SE37 |
| 8 | Enter FM code per §2.2 (v2 — optimised) | SE37 |
| 9 | Verify data element for `ZLOG_EXEC_VAR-ZDAYS` in SE11; adjust `lty_buffer_config-zdays` type if needed | SE11 / SE37 |
| 10 | Syntax check FM — zero errors/warnings | SE37 |
| 11 | Run Code Inspector on FM — zero errors | SCI |
| 12 | Enhance `update_database` in `ZCTD_RULEENG_EXECC01` per §3 | SE38 |
| 13 | Syntax check + Code Inspector on `ZCTD_RULEENG_EXEC` | SE38 / SCI |
| 14 | Insert row in `ZCTD_RULE_PROC` per §4 | SM30 / custom Tcode |
| 15 | Unit test in DEV per §9 | DEV client |
| 16 | Transport to QA | STMS |

---

## 9. Test Checklist

| # | Test Case | Expected Result |
|---|---|---|
| 1 | Trip with valid config in ZCTD_DAVG_DIS and ZLOG_EXEC_VAR | `ERUN_IDEAL_DAYS`, `ERUN_ACTUAL_DAYS`, `ERUN_DELAY_DAYS` populated; `EV_RULE_STATUS = 'S'` |
| 2 | ZCTD_DAVG_DIS config START_DT > trip created_date | Trip skipped; `EV_RULE_STATUS = 'N'` |
| 3 | ZCTD_DAVG_DIS config END_DATE < trip created_date | Trip skipped; `EV_RULE_STATUS = 'N'` |
| 4 | No loaded leg for trip (`LEG_TYPE = 'L'` absent) | Trip skipped gracefully; no error |
| 5 | BUSINESS or SUB_BUSINESS blank on loaded leg | Trip skipped; `EV_RULE_STATUS = 'N'` |
| 6 | No empty legs (all loaded) | 0 legs processed; `EV_RULE_STATUS = 'N'` |
| 7 | VEND_SOURCE_DATE or VEND_DEST_DATE blank on empty leg | That leg skipped; other empty legs still processed |
| 8 | ZLOG_EXEC_VAR — no active buffer record for Business+Sub-Business | Buffer = 0; calculation proceeds normally |
| 9 | ZLOG_EXEC_VAR — buffer = 3 days; Actual=7, Ideal=5 → Delay = 7-5-3 = -1 | `ERUN_DELAY_DAYS = -1` (within buffer) |
| 10 | ZLOG_EXEC_VAR — buffer = 1 day; Actual=8, Ideal=5 → Delay = 8-5-1 = 2 | `ERUN_DELAY_DAYS = 2` (delayed beyond buffer) |
| 11 | Early completion: Actual=3, Ideal=5, Buffer=0 → Delay = 3-5-0 = -2 | `ERUN_DELAY_DAYS = -2` (negative stored correctly) |
| 12 | On-time: Actual = Ideal + Buffer | `ERUN_DELAY_DAYS = 0` |
| 13 | CTD_DAYAVG_DIST = 0 in ZCTD_DAVG_DIS | Trip skipped; no divide-by-zero; `EV_RULE_STATUS = 'N'` |
| 14 | CEIL rounding: Distance=350, Avg=300 → 350/300=1.167 → CEIL=2 | `ERUN_IDEAL_DAYS = 2` |
| 15 | Exact division: Distance=300, Avg=300 → 300/300=1.0 → CEIL=1 | `ERUN_IDEAL_DAYS = 1` |
| 16 | 100 trips in single Rule Engine run | Only 2 DB SELECT calls total; verify via ST05 |
| 17 | Multiple Business+Sub-Business combinations in single run | Each trip uses correct avg distance and buffer for its combination |
| 18 | `IV_SIMULATION_MODE = 'X'` | FM calculates and returns status; Rule Engine skips `update_database` |
| 19 | DB persistence — foreground run | `ERUN_IDEAL_DAYS`, `ERUN_ACTUAL_DAYS`, `ERUN_DELAY_DAYS` written to `ZSCE_CTD_ITM`; verify via SE16N |
| 20 | Loaded legs after run | `ERUN_*` fields = 0 on loaded legs — correct |

---

## 10. References

- `Tcode for CTD Rules/FM Logic.txt` — Functional specification
- `CTD Rule Engine/Code Change Document - Z_SCM_CTD_EMPTYLEG_DELAY_CALC.md` — v1 (superseded by this document)
- `CTD Rule Engine/Program - ZCTD_RULEENG_EXEC.txt` — Rule Engine (call_rule_fm, update_database)
- `CTD Rule Engine/FM - Z_SCM_CTD_ERUNDIS_RULE.txt` — Reference FM pattern
- `CTD Rule Engine/FM - Z_SCM_CTD_RELLEG_RULE.txt` — Parallel cursor + ASSIGNING pattern
- `ABAP Rules - 02-04-2026/00-main.mdc` — Core principles, NW 7.31
- `ABAP Rules - 02-04-2026/02-naming.mdc` — Naming conventions
- `ABAP Rules - 02-04-2026/03-database.mdc` — No SELECT *, no SELECT in loop, no nested loop with WHERE
- `ABAP Rules - 02-04-2026/13-sy-subrc.mdc` — SY-SUBRC checks
- `ABAP Rules - 02-04-2026/20-code-generation-checklist.mdc` — Performance checklist
- Table `ZCTD_DAVG_DIS` screenshot — Actual table name; data element `ZSCM_DAYAVG_DIS_DE`
- Table `ZLOG_EXEC_VAR` screenshots — ERRORMSG (NATXT, CHAR 73) and REMARKS (TEXTR, CHAR 72) confirmed

---

*End of Code Change Document v2.0*
