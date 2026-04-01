# Code Change Document – Z_SCM_CTD_EMPTYLEG_DELAY_CALC

**Function Module:** `Z_SCM_CTD_EMPTYLEG_DELAY_CALC`
**Function Group:** TBD (place in same function group as existing CTD Rule FMs)
**Change Type:** New Function Module + DDIC Changes + Program Enhancement
**Date:** 2026-04-01
**Reference:** CD: TBD | TR: TBD
**Technical Author:** TBD
**Functional Author:** Gaurav Prabhu
**FS Reference:** `Tcode for CTD Rules/FM Logic.txt`

---

## Executive Summary (for ABAP Team)

This change delivers:

1. **DDIC Changes** — 3 new data elements + 3 new fields added to transparent table `ZSCE_CTD_ITM` and its mirror structure `ZSCM_CTD_TRIPITM`.
2. **New FM `Z_SCM_CTD_EMPTYLEG_DELAY_CALC`** — Calculation FM called by the CTD Rule Engine (`ZCTD_RULEENG_EXEC`) with `PROCESS_TYPE = 'CALC'`. For each Trip, it:
   - Fetches the Average Empty Leg Distance per Day from config table `ZCTD_DAVG_DIS` (keyed on Business + Sub-Business, validated against Trip creation date).
   - Fetches the Buffer Days tolerance from `ZLOG_EXEC_VAR` (NAME = `CTD_ELEG_DELAY_BUFFER`, filtered by Business + Sub-Business via REMARKS + ERRORMSG fields). Defaults to 0 if no active record found.
   - For each Empty Leg (`LEG_TYPE = 'E'`), calculates **Ideal Transit Days** (config-based), **Actual Transit Days** (Vendor dates), and **Delay in Days** (net of buffer).
   - Writes the 3 calculated values back to the empty leg row in `IT_TRIP_LEGS` (passed as TABLES parameter).
3. **Enhancement to `ZCTD_RULEENG_EXEC`** — `update_database` method extended to persist the 3 new delay fields to `ZSCE_CTD_ITM`.
4. **Config entry in `ZCTD_RULE_PROC`** — New row registering the FM under the applicable RuleMap with `PROCESS_TYPE = 'CALC'`.

---

## 1. Pre-requisites: DDIC Changes (SE11)

### 1.1 Create New Data Elements

Create the following 3 data elements in SE11 before creating/activating the table and structure fields.

| Data Element | Type | Length | Decimals | Short Description |
|---|---|:---:|:---:|---|
| `ZERUN_IDEAL_DAYS` | INT4 | 10 | 0 | Empty Run Ideal Transit Days |
| `ZERUN_ACTUAL_DAYS` | INT4 | 10 | 0 | Empty Run Actual Transit Days |
| `ZERUN_DELAY_DAYS` | INT4 | 10 | 0 | Empty Run Delay in Days (neg = early) |

> **Note:** INT4 (4-byte signed integer) is used for all three. `ZERUN_DELAY_DAYS` will hold negative values for early completion, zero for on-time, and positive for delayed.

---

### 1.2 Modify Transparent Table — `ZSCE_CTD_ITM`

Append 3 new fields to the end of the table (after existing fields):

| Seq | Field | Data Element | Type | Length | Key | Short Description |
|---|---|---|---|:---:|:---:|---|
| +1 | `ERUN_IDEAL_DAYS` | `ZERUN_IDEAL_DAYS` | INT4 | 10 | | Empty Run Ideal Transit Days |
| +2 | `ERUN_ACTUAL_DAYS` | `ZERUN_ACTUAL_DAYS` | INT4 | 10 | | Empty Run Actual Transit Days |
| +3 | `ERUN_DELAY_DAYS` | `ZERUN_DELAY_DAYS` | INT4 | 10 | | Empty Run Delay in Days |

> ⚠️ Table contains live data. Add fields via **Append Structure** or direct field addition via SE11. No key changes — append-only. Confirm with Basis/DB team before activating.

---

### 1.3 Modify Structure — `ZSCM_CTD_TRIPITM`

Add the same 3 fields to the structure `ZSCM_CTD_TRIPITM` (the mirror of `ZSCE_CTD_ITM` used as the FM TABLES parameter type). Field names, data elements, and order must exactly match §1.2.

| Field | Data Element | Short Description |
|---|---|---|
| `ERUN_IDEAL_DAYS` | `ZERUN_IDEAL_DAYS` | Empty Run Ideal Transit Days |
| `ERUN_ACTUAL_DAYS` | `ZERUN_ACTUAL_DAYS` | Empty Run Actual Transit Days |
| `ERUN_DELAY_DAYS` | `ZERUN_DELAY_DAYS` | Empty Run Delay in Days |

> Activate `ZSCM_CTD_TRIPITM` after adding fields. This propagates to `ZSCM_CTD_TRIPITM_TT` (table type) automatically. The FM parameter `IT_TRIP_LEGS TYPE ZSCM_CTD_TRIPITM_TT` will then expose the 3 new fields.

---

### 1.4 Activate All DDIC Objects

Activation sequence:
1. Data elements (`ZERUN_IDEAL_DAYS`, `ZERUN_ACTUAL_DAYS`, `ZERUN_DELAY_DAYS`)
2. Table `ZSCE_CTD_ITM`
3. Structure `ZSCM_CTD_TRIPITM` (table type `ZSCM_CTD_TRIPITM_TT` activates automatically)

---

## 2. New Function Module — `Z_SCM_CTD_EMPTYLEG_DELAY_CALC`

### 2.1 FM Interface

Create in SE37 within the applicable Function Group. Interface definition:

```
IMPORTING:
  VALUE(IV_SIMULATION_MODE)  TYPE  CHAR1  OPTIONAL

EXPORTING:
  VALUE(EV_RULE_STATUS)      TYPE  FLAG
  VALUE(EV_RULE_MESSAGE)     TYPE  STRING

TABLES:
  IT_TRIP_HEADER             TYPE  ZSCM_TRIPHDR_TT    OPTIONAL
  IT_TRIP_LEGS               TYPE  ZSCM_CTD_TRIPITM_TT OPTIONAL
```

**Parameter Notes:**

| Parameter | Direction | Type | Description |
|---|---|---|---|
| `IV_SIMULATION_MODE` | IMPORTING | CHAR1 | `'X'` = simulation (no DB write); space = live run. Passed by Rule Engine. |
| `EV_RULE_STATUS` | EXPORTING | FLAG (CHAR1) | `'S'` = calculated; `'N'` = not applicable; `'E'` = error |
| `EV_RULE_MESSAGE` | EXPORTING | STRING | Human-readable result message |
| `IT_TRIP_HEADER` | TABLES | ZSCM_TRIPHDR_TT | Trip header(s) — passed by reference; read-only in this FM |
| `IT_TRIP_LEGS` | TABLES | ZSCM_CTD_TRIPITM_TT | Trip leg items — passed by reference; FM writes 3 fields on empty legs |

**EV_RULE_STATUS Values:**

| Value | Meaning | Condition |
|---|---|---|
| `'S'` | Success — at least 1 empty leg calculated | `lv_legs_processed > 0` |
| `'N'` | Not applicable — no empty legs, no config | `lv_legs_processed = 0` |
| `'E'` | System error | Exception caught in TRY/CATCH |

---

### 2.2 FM Code

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
*& Description: For each Trip, fetches Average Per Day Distance from
*&              ZCTD_DAVG_DIS (keyed on Business + Sub-Business,
*&              validated against Trip creation date). Fetches Buffer Days
*&              tolerance from ZLOG_EXEC_VAR (NAME=CTD_ELEG_DELAY_BUFFER,
*&              REMARKS=Business, ERRORMSG=Sub-Business, ACTIVE='X').
*&              For each Empty Leg (LEG_TYPE = 'E'), calculates:
*&                Ideal Transit Days  = CEIL(Distance / Avg Per Day Dist)
*&                Actual Transit Days = VEND_DEST_DATE - VEND_SOURCE_DATE
*&                Delay (days)        = Actual - Ideal - Buffer Days
*&              Buffer Days default to 0 if no active config found.
*&              Results written to ERUN_IDEAL_DAYS, ERUN_ACTUAL_DAYS,
*&              ERUN_DELAY_DAYS on the empty leg row in IT_TRIP_LEGS.
*& Author: TBD
*& Creation Date: TBD
*& CD: TBD  TR: TBD
*& Technical Author  : TBD
*& Functional Author : Gaurav Prabhu
*& SAP Version: NetWeaver 7.31
*& Change History:
*& Date       User    Description
*& 2026-04-01 TBD     Initial creation
*&---------------------------------------------------------------------*

  " BEGIN: Cursor Generated Code

  " ===================================================================
  " TYPE DEFINITIONS (lty_ prefix per naming convention)
  " ===================================================================
  TYPES:
    " Config structure - fields match SELECT from ZCTD_DAVG_DIS exactly
    BEGIN OF lty_config,
      ctd_dayavg_dist TYPE zscm_dayavg_dis_de,
    END OF lty_config.

  " ===================================================================
  " CONSTANTS (lc_ prefix per naming convention)
  " ===================================================================
  CONSTANTS:
    lc_status_success   TYPE char1      VALUE 'S',
    lc_status_not_appld TYPE char1      VALUE 'N',
    lc_status_error     TYPE char1      VALUE 'E',

    lc_leg_type_loaded  TYPE char1      VALUE 'L',
    lc_leg_type_empty   TYPE char1      VALUE 'E',

    " Active flag — used when reading ZLOG_EXEC_VAR
    lc_active_x         TYPE char1      VALUE 'X',

    " Parameter name for buffer days in ZLOG_EXEC_VAR
    lc_buffer_name      TYPE rvari_vnam VALUE 'CTD_ELEG_DELAY_BUFFER'.

  CONSTANTS lc_msg_success   TYPE string
    VALUE 'Empty Leg Delay calculated successfully' ##NO_TEXT.
  CONSTANTS lc_msg_not_appld TYPE string
    VALUE 'No empty legs processed - config not found or no empty legs' ##NO_TEXT.
  CONSTANTS lc_msg_no_biz    TYPE string
    VALUE 'Business/Sub-Business not found on loaded leg' ##NO_TEXT.
  CONSTANTS lc_remark_calc   TYPE string
    VALUE 'Empty Leg Delay Calculation Applied' ##NO_TEXT.
  CONSTANTS lc_sys_error     TYPE string
    VALUE 'System error:' ##NO_TEXT.

  " ===================================================================
  " DATA DECLARATIONS (all upfront per NW 7.31 rule - no inline DATA)
  " ===================================================================
  DATA:
    " Config work area (matches lty_config / SELECT field order)
    lw_config           TYPE lty_config,

    " Business/Sub-Business (derived from 1st loaded leg of trip)
    lv_business         TYPE zbusiness_id,
    lv_sub_business     TYPE zsubusiness_id,

    " Trip creation date (validity reference for config lookup)
    lv_created_date     TYPE sydatum,

    " Average distance per day from config table
    lv_dayavg_dist      TYPE zscm_dayavg_dis_de,

    " Packed numeric work variables for arithmetic
    lv_distance_p       TYPE p DECIMALS 4,
    lv_dayavg_p         TYPE p DECIMALS 4,
    lv_division         TYPE p DECIMALS 6,

    " Calculated day values (TYPE i = INT4; delay can be negative)
    lv_ideal_days       TYPE i,
    lv_actual_days      TYPE i,
    lv_delay_days       TYPE i,

    " Buffer days tolerance from ZLOG_EXEC_VAR (default 0 if not found)
    lv_buffer_days      TYPE i,

    " Processing state flags and counters
    lv_config_found     TYPE abap_bool,
    lv_legs_processed   TYPE i,
    lv_first_loaded     TYPE abap_bool,

    " Parallel cursor index
    lv_tabix            TYPE sy-tabix,

    " Exception handling
    lo_exception        TYPE REF TO cx_root,
    lv_error_text       TYPE string.

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
      " Step 1: Sort IT_TRIP_LEGS by trip_no and leg_type
      " Enables binary search on both trip_no + leg_type keys.
      " 'E' (Empty) sorts before 'L' (Loaded) — both ranges accessible.
      " ==================================================================
      SORT it_trip_legs BY trip_no leg_type.

      " ==================================================================
      " Step 2: Process Each Trip
      " ==================================================================
      LOOP AT it_trip_header ASSIGNING <lfs_trip_header>.

        CLEAR: lv_business,
               lv_sub_business,
               lv_config_found,
               lv_dayavg_dist,
               lw_config,
               lv_created_date,
               lv_buffer_days.

        " Use trip creation date as validity reference for config lookup
        lv_created_date = <lfs_trip_header>-created_date.
        IF lv_created_date IS INITIAL.
          lv_created_date = sy-datum.
        ENDIF.

        " ----------------------------------------------------------------
        " Step 2.1: Get Business + Sub-Business from 1st Loaded Leg
        " Business and Sub-Business are consistent across all legs in a
        " trip — derive once from the first loaded leg (LEG_TYPE = 'L').
        " ----------------------------------------------------------------
        CLEAR lv_first_loaded.

        READ TABLE it_trip_legs
          WITH KEY trip_no  = <lfs_trip_header>-trip_no
                   leg_type = lc_leg_type_loaded
          BINARY SEARCH
          TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          lv_tabix = sy-tabix.
          LOOP AT it_trip_legs ASSIGNING <lfs_trip_legs> FROM lv_tabix.
            IF <lfs_trip_legs>-trip_no  <> <lfs_trip_header>-trip_no OR
               <lfs_trip_legs>-leg_type <> lc_leg_type_loaded.
              EXIT.
            ENDIF.
            " Take Business and Sub-Business from first loaded leg only
            lv_business     = <lfs_trip_legs>-business.
            lv_sub_business = <lfs_trip_legs>-sub_business.
            lv_first_loaded = abap_true.
            EXIT.
          ENDLOOP.
        ENDIF.

        " Skip trip if no loaded leg found or business info is blank
        IF lv_first_loaded = abap_false OR
           lv_business     IS INITIAL   OR
           lv_sub_business IS INITIAL.
          CONTINUE.
        ENDIF.

        " ----------------------------------------------------------------
        " Step 2.2: Fetch Average Per Day Distance from ZCTD_DAVG_DIS
        " Single SELECT outside inner loop — one fetch per trip.
        " Date range check in WHERE ensures valid config for the trip.
        " ----------------------------------------------------------------
        CLEAR lv_dayavg_dist.

        SELECT ctd_dayavg_dist
          FROM zctd_davg_dis CLIENT SPECIFIED
          INTO lv_dayavg_dist
          UP TO 1 ROWS
          WHERE mandt        = sy-mandt
            AND business     = lv_business
            AND sub_business = lv_sub_business
            AND start_dt    <= lv_created_date
            AND end_date    >= lv_created_date.
        ENDSELECT.
        IF sy-subrc = 0.
          lv_config_found = abap_true.
        ELSE.
          lv_config_found = abap_false.
        ENDIF.

        " Skip trip if no valid config or avg distance is zero
        IF lv_config_found = abap_false OR lv_dayavg_dist = 0.
          CONTINUE.
        ENDIF.

        " ----------------------------------------------------------------
        " Step 2.3: Fetch Buffer Days from ZLOG_EXEC_VAR
        " Buffer days provide a tolerance window for transit delay.
        " Keyed by NAME = 'CTD_ELEG_DELAY_BUFFER', REMARKS = Business,
        " ERRORMSG = Sub-Business, ACTIVE = 'X'.
        " If no active record found, lv_buffer_days remains 0 —
        " calculation proceeds without buffer (Delay = Actual - Ideal).
        " ----------------------------------------------------------------
        CLEAR lv_buffer_days.

        SELECT zdays
          FROM zlog_exec_var CLIENT SPECIFIED
          INTO lv_buffer_days
          UP TO 1 ROWS
          WHERE mandt    = sy-mandt
            AND name     = lc_buffer_name
            AND active   = lc_active_x
            AND remarks  = lv_business
            AND errormsg = lv_sub_business.
        ENDSELECT.
        IF sy-subrc <> 0.
          lv_buffer_days = 0.   " Default: no buffer tolerance applied
        ENDIF.

        " ----------------------------------------------------------------
        " Step 2.4: Calculate Delay for Each Empty Leg
        " Parallel cursor on IT_TRIP_LEGS filtered to current trip's
        " empty legs (LEG_TYPE = 'E'). Write results back via ASSIGNING.
        " Delay formula: Actual - Ideal - Buffer Days.
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
              EXIT.
            ENDIF.

            " Skip empty leg if vendor dates not yet entered by Transporter
            IF <lfs_trip_legs>-vend_source_date IS INITIAL OR
               <lfs_trip_legs>-vend_dest_date   IS INITIAL.
              CONTINUE.
            ENDIF.

            " Clear calculation variables before each leg
            CLEAR: lv_distance_p,
                   lv_dayavg_p,
                   lv_division,
                   lv_ideal_days,
                   lv_actual_days,
                   lv_delay_days.

            " ----------------------------------------------------------
            " Ideal Transit Days = CEIL( Distance / Avg Per Day Distance )
            " Partial days round up to full transit day (ceiling).
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
            " Date subtraction in ABAP gives difference in full days.
            " ----------------------------------------------------------
            lv_actual_days = <lfs_trip_legs>-vend_dest_date
                           - <lfs_trip_legs>-vend_source_date.

            " ----------------------------------------------------------
            " Delay = Actual - Ideal - Buffer Days
            " Buffer Days provide a tolerance window for transit.
            " Positive = delayed beyond buffer; Zero/negative = within
            " tolerance or early. Buffer defaults to 0 if not configured.
            " ----------------------------------------------------------
            lv_delay_days = lv_actual_days - lv_ideal_days - lv_buffer_days.

            " ----------------------------------------------------------
            " Write calculated values back to leg row (direct via ASSIGNING)
            " ----------------------------------------------------------
            <lfs_trip_legs>-erun_ideal_days  = lv_ideal_days.
            <lfs_trip_legs>-erun_actual_days = lv_actual_days.
            <lfs_trip_legs>-erun_delay_days  = lv_delay_days.

            lv_legs_processed = lv_legs_processed + 1.

          ENDLOOP.
        ENDIF.

      ENDLOOP.

      " ==================================================================
      " Step 3: Set Export Parameters
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

### 3.1 Change in `update_database`

The `update_database` method in include `ZCTD_RULEENG_EXECC01` (class `lcl_report`) must be enhanced to persist the 3 new delay fields.

**Locate this existing code block:**

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

> **Note:** The UPDATE runs for all trip items (loaded and empty). For loaded legs, `erun_ideal_days`, `erun_actual_days`, and `erun_delay_days` will be 0 (initial), which is the correct and expected state for loaded legs.

---

## 4. Rule Engine Config — `ZCTD_RULE_PROC`

After transporting the FM, register it in the Rule Engine configuration table `ZCTD_RULE_PROC` for the applicable RuleMap(s).

**Entry to be inserted (via SM30 or custom maintenance):**

| Field | Value | Description |
|---|---|---|
| `RULEMAP_ID` | `<applicable rulemap>` | RuleMap ID for the Business/Sub-Business combination |
| `PROCESS_TYPE` | `CALC` | Identifies this as a Calculation FM (not an eligibility rule) |
| `SEQ_NO` | `999` (or last seq) | Executed after all eligibility rules |
| `FM_NAME` | `Z_SCM_CTD_EMPTYLEG_DELAY_CALC` | FM to be called dynamically |
| `EXEC_CONDITION` | `CTD_ELIGIBLE` | Execute only if at least one leg is CTD eligible |
| `ACTIVE` | `X` | Active flag |
| `DESCRIPTION` | `Empty Leg Transit Delay Calculation` | For display purposes |

> The Rule Engine `execute_trip_rules` method already enforces: only 1 CALC rule per RuleMap is allowed. If a RuleMap already has a CALC entry, consult the functional team before adding.

---

## 5. Logic Summary

### Step 1 — Derive Business / Sub-Business (once per Trip)

```
Source        ←  1st Loaded Leg (LEG_TYPE = 'L') of the Trip
Fields        ←  BUSINESS, SUB_BUSINESS from ZSCE_CTD_ITM / IT_TRIP_LEGS
```

### Step 2 — Average Distance Config Lookup (once per Trip)

```
Table         ←  ZCTD_DAVG_DIS
Key Filter    ←  BUSINESS + SUB_BUSINESS
Validity      ←  START_DT ≤ CREATED_DATE ≤ END_DATE  (CREATED_DATE from trip header)
Result        ←  CTD_DAYAVG_DIST (average km per day, NUMC 3)
```

### Step 3 — Buffer Days Lookup (once per Trip)

```
Table         ←  ZLOG_EXEC_VAR
Filter        ←  NAME     = 'CTD_ELEG_DELAY_BUFFER'
                 REMARKS  = lv_business      (Business ID)
                 ERRORMSG = lv_sub_business  (Sub-Business ID)
                 ACTIVE   = 'X'
Result        ←  ZDAYS (buffer days, NUMC 3)
Default       ←  0 if no active record found (no tolerance, Delay = Actual – Ideal)
```

### Step 4 — Per Empty Leg Calculation (LEG_TYPE = 'E')

| Output Field | Formula | Notes |
|---|---|---|
| `ERUN_IDEAL_DAYS` | `CEIL( DISTANCE / CTD_DAYAVG_DIST )` | Partial day rounds up (ceiling) |
| `ERUN_ACTUAL_DAYS` | `VEND_DEST_DATE – VEND_SOURCE_DATE` | Vendor-entered dates on empty leg |
| `ERUN_DELAY_DAYS` | `ERUN_ACTUAL_DAYS – ERUN_IDEAL_DAYS – Buffer Days` | +ve = delayed beyond buffer; 0/−ve = within tolerance or early |

### Skip Conditions

| Condition | Action |
|---|---|
| No loaded leg found for trip | Skip entire trip |
| `BUSINESS` or `SUB_BUSINESS` blank on loaded leg | Skip entire trip |
| No matching record in `ZCTD_DAVG_DIS` | Skip entire trip |
| `CTD_DAYAVG_DIST = 0` in config | Skip entire trip |
| No active record in `ZLOG_EXEC_VAR` for Business + Sub-Business | Proceed with `lv_buffer_days = 0` (no skip) |
| `VEND_SOURCE_DATE` or `VEND_DEST_DATE` initial on empty leg | Skip that leg only |
| `DISTANCE = 0` on empty leg | `ERUN_IDEAL_DAYS = 0` (guard in code) |

---

## 6. ABAP Compliance Checklist

| Rule | How Applied |
|---|---|
| **No inline declarations** (`DATA(x)`) | All variables declared upfront in DATA section (NW 7.31) |
| **No `NEW` / `VALUE` / `CORRESPONDING`** | Not used anywhere |
| **No `SELECT *`** | Only `ctd_dayavg_dist` selected from `ZCTD_DAVG_DIS` |
| **No SELECT in loop** | Config SELECT is outside the LOOP AT it_trip_header; done once per trip via UP TO 1 ROWS |
| **No nested LOOPs** | Inner LOOP uses parallel cursor technique (READ + LOOP FROM lv_tabix); not a nested LOOP |
| **`IS NOT INITIAL` not in WHERE** | `IS INITIAL` used only in ABAP IF checks, not in SQL WHERE |
| **`SY-SUBRC` checked immediately** | After every SELECT, READ TABLE, checked on next line |
| **Naming: `lv_`, `lt_`, `lw_`, `lty_`, `lc_`** | Applied throughout; FIELD-SYMBOLS use `<lfs_>` |
| **Constants for fixed values** | `lc_leg_type_loaded`, `lc_leg_type_empty`, `lc_status_*`, `lc_msg_*` declared as CONSTANTS |
| **No `sy-*` in CONSTANTS** | `sy-datum`, `sy-mandt` used directly in executable statements only |
| **OOP — no FORMs** | FM is the required pattern for Rule Engine; no FORM/PERFORM inside FM |
| **ASSIGNING for table modification** | Empty leg rows updated using `LOOP AT ... ASSIGNING <lfs_trip_legs>` — no MODIFY FROM |
| **FOR ALL ENTRIES guard** | Not used in this FM (uses UP TO 1 ROWS and parallel cursor) |
| **SORT before BINARY SEARCH** | `SORT it_trip_legs BY trip_no leg_type` at top; all READ TABLE calls use BINARY SEARCH |
| **TRY/CATCH** | Entire logic wrapped in TRY/CATCH cx_root; returns `EV_RULE_STATUS = 'E'` on exception |
| **Division by zero guard** | `IF lv_distance_p > 0 AND lv_dayavg_p > 0` check before division |
| **Vendor date blank guard** | `IF vend_source_date IS INITIAL OR vend_dest_date IS INITIAL` — skip leg |

---

## 7. Implementation Checklist

| # | Task | Object / Tool |
|---|---|---|
| 1 | Create data element `ZERUN_IDEAL_DAYS` (INT4) | SE11 |
| 2 | Create data element `ZERUN_ACTUAL_DAYS` (INT4) | SE11 |
| 3 | Create data element `ZERUN_DELAY_DAYS` (INT4) | SE11 |
| 4 | Add `ERUN_IDEAL_DAYS`, `ERUN_ACTUAL_DAYS`, `ERUN_DELAY_DAYS` to `ZSCE_CTD_ITM` | SE11 |
| 5 | Add same 3 fields to structure `ZSCM_CTD_TRIPITM` | SE11 |
| 6 | Activate all DDIC objects (data elements → table → structure) | SE11 |
| 7 | Create Function Group for FM (if not using existing) | SE37 / SE80 |
| 8 | Create FM `Z_SCM_CTD_EMPTYLEG_DELAY_CALC` with interface per §2.1 | SE37 |
| 9 | Enter FM code per §2.2 | SE37 |
| 10 | Syntax check FM — zero errors/warnings | SE37 |
| 11 | Enhance `update_database` in `ZCTD_RULEENG_EXECC01` per §3.1 | SE38 |
| 12 | Syntax check `ZCTD_RULEENG_EXEC` — zero errors/warnings | SE38 |
| 13 | Run Code Inspector on both FM and report — zero errors | SCI |
| 14 | Insert row in `ZCTD_RULE_PROC` for applicable RuleMap(s) per §4 | SM30 / custom Tcode |
| 15 | Unit test in DEV (see §8) | DEV client |
| 16 | Transport to QA | STMS |

---

## 8. Test Checklist

| # | Test Case | Expected Result |
|---|---|---|
| 1 | Trip with Empty Legs, valid ZCTD_DAVG_DIS config | `ERUN_IDEAL_DAYS`, `ERUN_ACTUAL_DAYS`, `ERUN_DELAY_DAYS` populated on all empty legs; `EV_RULE_STATUS = 'S'` |
| 2 | Trip with Empty Legs — config record START_DT > trip created_date | FM skips trip; `EV_RULE_STATUS = 'N'` |
| 3 | Trip with Empty Legs — config record END_DATE < trip created_date | FM skips trip; `EV_RULE_STATUS = 'N'` |
| 4 | Trip with no Empty Legs (all loaded) | FM processes 0 empty legs; `EV_RULE_STATUS = 'N'` |
| 5 | Trip with Empty Legs but `VEND_SOURCE_DATE` / `VEND_DEST_DATE` blank | Leg skipped; other legs with dates still calculated |
| 6 | Empty Leg where Actual < Ideal (early completion) | `ERUN_DELAY_DAYS` is negative; stored correctly as INT4 |
| 7 | Empty Leg where Actual = Ideal (on-time) | `ERUN_DELAY_DAYS = 0` |
| 8 | Empty Leg where Actual > Ideal (delayed) | `ERUN_DELAY_DAYS` is positive |
| 9 | `ZCTD_DAVG_DIS` has `CTD_DAYAVG_DIST = 0` | Trip skipped; no divide-by-zero; `EV_RULE_STATUS = 'N'` |
| 10 | No loaded leg in trip (no `LEG_TYPE = 'L'`) | Trip skipped gracefully; no error |
| 11 | Loaded leg has blank `BUSINESS` or `SUB_BUSINESS` | Trip skipped; `EV_RULE_STATUS = 'N'` |
| 12 | `IV_SIMULATION_MODE = 'X'` | FM calculates and returns status; Rule Engine skips `update_database` (simulation handled by engine, not FM) |
| 13 | Verify DB persistence: re-execute `ZCTD_RULEENG_EXEC` in foreground mode | `ERUN_IDEAL_DAYS`, `ERUN_ACTUAL_DAYS`, `ERUN_DELAY_DAYS` written to `ZSCE_CTD_ITM`; verify via SE16N |
| 14 | Multiple trips in single Rule Engine run | Each trip independently processed; no cross-trip contamination |
| 15 | CEIL rounding: Distance = 350 km, Avg = 300 km/day → 350/300 = 1.167 → CEIL = 2 | `ERUN_IDEAL_DAYS = 2` |
| 16 | Exact division: Distance = 300 km, Avg = 300 km/day → 300/300 = 1.0 → CEIL = 1 | `ERUN_IDEAL_DAYS = 1` |

---

## 9. References

- `Tcode for CTD Rules/FM Logic.txt` — Functional specification for delay calculation
- `Tcode for CTD Rules/Code Change Document - ZCTD_RULES.md` — ZCTD_DAVG_DIS table definition
- `CTD Rule Engine/Program - ZCTD_RULEENG_EXEC.txt` — Rule Engine orchestration (call_rule_fm, update_database)
- `CTD Rule Engine/FM - Z_SCM_CTD_ERUNDIS_RULE.txt` — Reference pattern for FM structure
- `CTD Rule Engine/FM - Z_SCM_CTD_RELLEG_RULE.txt` — Reference pattern (parallel cursor, ASSIGNING)
- `ABAP RULES/00-main.mdc` — Core principles, NW 7.31 compatibility
- `ABAP RULES/02-naming.mdc` — Naming conventions (`lv_`, `lw_`, `lc_`, `lty_`, `<lfs_>`)
- `ABAP RULES/03-database.mdc` — No SELECT *, no SELECT in loop, structure matches SELECT
- `ABAP RULES/13-sy-subrc.mdc` — SY-SUBRC check requirements
- Table `ZCTD_DAVG_DIS` screenshot — Confirmed actual table name and data element `ZSCM_DAYAVG_DIS_DE`

---

*End of Code Change Document*
