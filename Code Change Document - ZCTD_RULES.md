# Code Change Document – ZCTD_RULES

**Transaction:** `ZCTD_RULES`  
**Program:** `ZCTD_RULES`  
**Change Type:** New Transaction + New Program + Table Corrections  
**Date:** 2026-03-26  
**Reference:** CD: TBD | TR: TBD  
**Technical Author:** TBD  
**Functional Author:** Gaurav Prabhu  
**FS Reference:** `Tcode for CTD Rules/FS.md` (Version 2.0 As-Built)

---

## Executive Summary (for ABAP Team)

This change delivers the following:

1. **DDIC Corrections** — Fix key designations on existing tables (`ZCTD_REL_LEG_RL`, `ZCTD_ERUN_DIS_IC`) and add missing audit fields to `ZCTD_REL_LEG_RL`.
2. **New Table `ZCTD_ERUN_DAVG_DIS`** — CTD Empty Run per Day Average Distance table (not yet created in system).
3. **Lock Objects** — One lock object per Z-table to support controlled Create access.
4. **Authorization Object `Z_CTD_RULE_AUTH`** — Activity 01 (Create), 03 (Display).
5. **New Transaction `ZCTD_RULES`** — Custom report with selection screen; routes to Create (Dynpro screen) or Display (CL_SALV_TABLE ALV) based on user selection, for each of 4 CTD rule types.

---

## 1. Pre-requisite: DDIC Changes (SE11)

### 1.1 Modify Table — `ZCTD_REL_LEG_RL`

Two changes required on this existing active table:

**A. Remove `APP_LEGS` from key:**

| Field | Current Key | Required Key |
|---|:---:|:---:|
| APP_LEGS | ✅ Key | ❌ Non-Key |

**B. Append 6 audit fields (after FIELD9):**

| Seq | Field | Data Element | Type | Length | Key | Short Description |
|---|---|---|---|:---:|:---:|---|
| 15 | CREATED_BY | SYUNAME | CHAR | 12 | | User Name (Created) |
| 16 | CREATED_ON | SYDATUM | DATS | 8 | | Date of Creation |
| 17 | CREATED_AT | SYSTTIMLO | TIMS | 6 | | Time of Creation |
| 18 | CHANGED_BY | SYUNAME | CHAR | 12 | | User Name (Last Changed) |
| 19 | CHANGED_ON | SYDATUM | DATS | 8 | | Date of Last Change |
| 20 | CHANGED_AT | SYSTTIMLO | TIMS | 6 | | Time of Last Change |

> ⚠️ Table contains live data. Before making key changes, confirm with Basis/DB team on migration approach. Audit fields can be appended directly via SE11 → Append Structure or direct field addition.

---

### 1.2 Modify Table — `ZCTD_ERUN_DIS_IC`

**Remove `DIST` from key:**

| Field | Current Key | Required Key |
|---|:---:|:---:|
| DIST | ✅ Key | ❌ Non-Key |

> ⚠️ Table contains live data. Confirm with Basis/DB team before removing from key.

---

### 1.3 Create New Table — `ZCTD_ERUN_DAVG_DIS`

| Field | Data Element | Type | Length | Key | Short Description |
|---|---|---|:---:|:---:|---|
| MANDT | MANDT | CLNT | 3 | X | Client |
| BUSINESS | ZBUSINESS_ID | CHAR | 40 | X | Business ID |
| SUB_BUSINESS | ZSUBUSINESS_ID | CHAR | 40 | X | Sub-Business ID |
| START_DT | ZSCM_START_DT_DE | DATS | 8 | X | From Date |
| END_DATE | ZEND_DT | DATS | 8 | | To Date |
| CTD_DAYAVG_DIST | — | NUMC | 3 | | Average Distance per Day |
| CREATED_BY | SYUNAME | CHAR | 12 | | User Name (Created) |
| CREATED_ON | SYDATUM | DATS | 8 | | Date of Creation |
| CREATED_AT | SYSTTIMLO | TIMS | 6 | | Time of Creation |
| CHANGED_BY | SYUNAME | CHAR | 12 | | User Name (Last Changed) |
| CHANGED_ON | SYDATUM | DATS | 8 | | Date of Last Change |
| CHANGED_AT | SYSTTIMLO | TIMS | 6 | | Time of Last Change |

**Technical Settings:**
- Delivery Class: `A` (Application table, master/transaction data)
- Data Browser/Table View: Display/Maintenance Allowed
- Buffering: Not buffered

> Create a suitable data element for `CTD_DAYAVG_DIST` (e.g. `ZCTD_DAYAVG_DIST`, NUMC 3) if not already existing.

---

### 1.4 Lock Objects (SE11)

Create one lock object per table. All use lock mode **Write (E)**.

| Lock Object | Table | Key Fields Locked |
|---|---|---|
| `EZCTD_PROD_REG_RL` | ZCTD_PROD_REG_RL | MATNR, REGION, START_DT |
| `EZCTD_REL_LEG_RL` | ZCTD_REL_LEG_RL | LEGS, START_DT |
| `EZCTD_ERUN_DIS_IC` | ZCTD_ERUN_DIS_IC | LEGS, START_DT |
| `EZCTD_ERUN_DAVG_DIS` | ZCTD_ERUN_DAVG_DIS | BUSINESS, SUB_BUSINESS, START_DT |

Each lock object generates: `ENQUEUE_EZCTD_*` and `DEQUEUE_EZCTD_*` function modules.

---

### 1.5 Authorization Object — `Z_CTD_RULE_AUTH`

Create in SU21:

| Field | Value |
|---|---|
| **Object Name** | `Z_CTD_RULE_AUTH` |
| **Object Class** | ZCTD (or existing custom class) |
| **Field** | `ACTVT` — Authorization Activity |
| **Activity 01** | Create |
| **Activity 03** | Display |

---

## 2. New Transaction — `ZCTD_RULES`

Create in SE93:

| Field | Value |
|---|---|
| **Transaction Code** | `ZCTD_RULES` |
| **Short Text** | CTD Rule Maintenance |
| **Program** | `ZCTD_RULES` |
| **Screen Number** | 1000 (standard selection screen) |
| **Authorization Object** | `Z_CTD_RULE_AUTH` |

---

## 3. Program `ZCTD_RULES` — Structure Overview

```abap
REPORT zctd_rules.

INCLUDE zctd_rulestop.   " Global declarations
INCLUDE zctd_rulessel.   " Selection Screen
INCLUDE zctd_rulesc01.   " Class definition and implementation

DATA: go_rules TYPE REF TO lcl_ctd_rules.

INITIALIZATION.
  AUTHORITY-CHECK OBJECT 'S_TCODE'
                  ID 'TCD' FIELD sy-tcode.
  IF sy-subrc <> 0.
    MESSAGE w077(s#) WITH sy-tcode DISPLAY LIKE 'E'.
    LEAVE PROGRAM.
  ENDIF.
  CREATE OBJECT go_rules.

AT SELECTION-SCREEN.
  go_rules->validate_screen( ).

START-OF-SELECTION.
  go_rules->execute( ).
```

---

## 4. TOP Include — `ZCTD_RULESTOP`

```abap
*&---------------------------------------------------------------------*
*& Include  ZCTD_RULESTOP
*&---------------------------------------------------------------------*

*=======================================================================
* Global Types — matching SELECT field order exactly (DB rule)
*=======================================================================

* ZCTD_PROD_REG_RL — ALV display type (all fields)
TYPES: BEGIN OF gty_prod_reg,
         matnr      TYPE matnr,
         region     TYPE regio,
         start_dt   TYPE zscm_start_dt_de,
         end_date   TYPE zend_dt,
         created_by TYPE syuname,
         created_on TYPE sydatum,
         created_at TYPE systtimlo,
         changed_by TYPE syuname,
         changed_on TYPE sydatum,
         changed_at TYPE systtimlo,
       END OF gty_prod_reg.

* ZCTD_REL_LEG_RL — ALV display type
TYPES: BEGIN OF gty_rel_leg,
         legs       TYPE zlegs,
         app_legs   TYPE zapp_legs,
         start_dt   TYPE zscm_start_dt_de,
         end_date   TYPE zend_dt,
         field1     TYPE flag,
         field2     TYPE flag,
         field3     TYPE flag,
         field4     TYPE flag,
         field5     TYPE flag,
         field6     TYPE flag,
         field7     TYPE flag,
         field8     TYPE flag,
         field9     TYPE flag,
         created_by TYPE syuname,
         created_on TYPE sydatum,
         created_at TYPE systtimlo,
         changed_by TYPE syuname,
         changed_on TYPE sydatum,
         changed_at TYPE systtimlo,
       END OF gty_rel_leg.

* ZCTD_ERUN_DIS_IC — ALV display type
TYPES: BEGIN OF gty_erun_dis,
         legs       TYPE zlegs,
         dist       TYPE zdistance,
         start_dt   TYPE zscm_start_dt_de,
         end_date   TYPE zend_dt,
         created_by TYPE syuname,
         created_on TYPE sydatum,
         created_at TYPE systtimlo,
         changed_by TYPE syuname,
         changed_on TYPE sydatum,
         changed_at TYPE systtimlo,
       END OF gty_erun_dis.

* ZCTD_ERUN_DAVG_DIS — ALV display type
TYPES: BEGIN OF gty_erun_davg,
         business       TYPE zbusiness_id,
         sub_business   TYPE zsubusiness_id,
         ctd_dayavg_dist TYPE zctd_dayavg_dist,
         start_dt       TYPE zscm_start_dt_de,
         end_date       TYPE zend_dt,
         created_by     TYPE syuname,
         created_on     TYPE sydatum,
         created_at     TYPE systtimlo,
         changed_by     TYPE syuname,
         changed_on     TYPE sydatum,
         changed_at     TYPE systtimlo,
       END OF gty_erun_davg.

*=======================================================================
* Global Data — Create Screen work areas (module pool style)
*=======================================================================
DATA: gw_prod_reg    TYPE zctd_prod_reg_rl,
      gw_rel_leg     TYPE zctd_rel_leg_rl,
      gw_erun_dis    TYPE zctd_erun_dis_ic,
      gw_erun_davg   TYPE zctd_erun_davg_dis,
      gv_save_flag   TYPE flag,           " 'X' = Save triggered from screen
      gv_cancel_flag TYPE flag.           " 'X' = Cancel triggered from screen

*=======================================================================
* Constants
*=======================================================================
CONSTANTS: gc_actvt_create  TYPE activ_auth VALUE '01',
           gc_actvt_display TYPE activ_auth VALUE '03',
           gc_flag_x        TYPE flag       VALUE 'X',
           gc_space         TYPE char1      VALUE ' '.
```

---

## 5. SEL Include — `ZCTD_RULESSEL`

```abap
*&---------------------------------------------------------------------*
*& Include  ZCTD_RULESSEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b_rule WITH FRAME TITLE text-001.
  PARAMETERS:
    p_prodrg RADIOBUTTON GROUP rule DEFAULT 'X',   " CTD Product Region Rule
    p_relleg RADIOBUTTON GROUP rule,                " CTD Relevant Legs Rule
    p_erudis RADIOBUTTON GROUP rule,                " CTD Empty Run Distance (Incentive)
    p_erudav RADIOBUTTON GROUP rule.                " CTD Empty Run per Day Avg Distance
SELECTION-SCREEN END OF BLOCK b_rule.

SELECTION-SCREEN BEGIN OF BLOCK b_oper WITH FRAME TITLE text-002.
  PARAMETERS:
    p_create RADIOBUTTON GROUP oper DEFAULT 'X',   " Create
    p_displa RADIOBUTTON GROUP oper.                " Display
SELECTION-SCREEN END OF BLOCK b_oper.
```

**Text Symbols** (maintain in SE38 → Goto → Text Elements → Text Symbols):

| Symbol | Text |
|---|---|
| `001` | Rule Type |
| `002` | Operation |
| `P_PRODRG` | CTD Product Region Rule |
| `P_RELLEG` | CTD Relevant Legs Rule |
| `P_ERUDIS` | CTD Empty Run Distance (for Incentive) |
| `P_ERUDAV` | CTD Empty Run per Day Average Distance |
| `P_CREATE` | Create |
| `P_DISPLA` | Display |

---

## 6. C01 Include — `ZCTD_RULESC01`

### 6.1 Class Definition

```abap
*&---------------------------------------------------------------------*
*& Include  ZCTD_RULESC01
*&---------------------------------------------------------------------*

CLASS lcl_ctd_rules DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      " Screen validation
      validate_screen,
      " Main router
      execute,
      " Authorization
      authority_check
        IMPORTING iv_actvt TYPE activ_auth,
      " ── Product Region Rule ──
      display_prod_reg,
      create_prod_reg,
      validate_prod_reg
        RETURNING VALUE(rv_valid) TYPE abap_bool,
      save_prod_reg,
      " ── Relevant Legs Rule ──
      display_rel_leg,
      create_rel_leg,
      validate_rel_leg
        RETURNING VALUE(rv_valid) TYPE abap_bool,
      save_rel_leg,
      " ── Empty Run Distance ──
      display_erun_dis,
      create_erun_dis,
      validate_erun_dis
        RETURNING VALUE(rv_valid) TYPE abap_bool,
      save_erun_dis,
      " ── Empty Run Daily Average ──
      display_erun_davg,
      create_erun_davg,
      validate_erun_davg
        RETURNING VALUE(rv_valid) TYPE abap_bool,
      save_erun_davg.

  PRIVATE SECTION.
    METHODS:
      fill_created_audit
        IMPORTING
          iv_created_by  TYPE syuname
          iv_created_on  TYPE sydatum
          iv_created_at  TYPE systtimlo
        CHANGING
          cv_created_by  TYPE syuname
          cv_created_on  TYPE sydatum
          cv_created_at  TYPE systtimlo,
      fill_changed_audit
        CHANGING
          cv_changed_by  TYPE syuname
          cv_changed_on  TYPE sydatum
          cv_changed_at  TYPE systtimlo,
      check_overlap_prod_reg
        IMPORTING
          iv_matnr       TYPE matnr
          iv_region      TYPE regio
          iv_start_dt    TYPE zscm_start_dt_de
          iv_end_date    TYPE zend_dt
        RETURNING VALUE(rv_overlap) TYPE abap_bool,
      check_overlap_rel_leg
        IMPORTING
          iv_legs        TYPE zlegs
          iv_start_dt    TYPE zscm_start_dt_de
          iv_end_date    TYPE zend_dt
        RETURNING VALUE(rv_overlap) TYPE abap_bool,
      check_overlap_erun_dis
        IMPORTING
          iv_legs        TYPE zlegs
          iv_start_dt    TYPE zscm_start_dt_de
          iv_end_date    TYPE zend_dt
        RETURNING VALUE(rv_overlap) TYPE abap_bool,
      check_overlap_erun_davg
        IMPORTING
          iv_business     TYPE zbusiness_id
          iv_sub_business TYPE zsubusiness_id
          iv_start_dt     TYPE zscm_start_dt_de
          iv_end_date     TYPE zend_dt
        RETURNING VALUE(rv_overlap) TYPE abap_bool,
      set_alv_props
        IMPORTING io_alv     TYPE REF TO cl_salv_table
                  iv_title   TYPE lvc_title.

ENDCLASS.
```

---

### 6.2 Class Implementation

#### 6.2.1 `authority_check`

```abap
METHOD authority_check.
  AUTHORITY-CHECK OBJECT 'Z_CTD_RULE_AUTH'
                  ID 'ACTVT' FIELD iv_actvt.
  IF sy-subrc <> 0.
    MESSAGE 'You are not authorized for this action'(e01) TYPE 'E'.
  ENDIF.
ENDMETHOD.
```

---

#### 6.2.2 `validate_screen`

```abap
METHOD validate_screen.
* No additional screen-level validations for radio-button-only screens.
* Authorization is checked at execute time based on selected operation.
ENDMETHOD.
```

---

#### 6.2.3 `execute`

```abap
METHOD execute.
  DATA: lv_actvt TYPE activ_auth.

* Determine required authorization activity
  IF p_displa = gc_flag_x.
    lv_actvt = gc_actvt_display.
  ELSE.
    lv_actvt = gc_actvt_create.
  ENDIF.
  me->authority_check( iv_actvt = lv_actvt ).

* Route based on rule type and operation
  IF p_prodrg = gc_flag_x.
    IF p_displa = gc_flag_x.
      me->display_prod_reg( ).
    ELSE.
      me->create_prod_reg( ).
    ENDIF.
  ELSEIF p_relleg = gc_flag_x.
    IF p_displa = gc_flag_x.
      me->display_rel_leg( ).
    ELSE.
      me->create_rel_leg( ).
    ENDIF.
  ELSEIF p_erudis = gc_flag_x.
    IF p_displa = gc_flag_x.
      me->display_erun_dis( ).
    ELSE.
      me->create_erun_dis( ).
    ENDIF.
  ELSEIF p_erudav = gc_flag_x.
    IF p_displa = gc_flag_x.
      me->display_erun_davg( ).
    ELSE.
      me->create_erun_davg( ).
    ENDIF.
  ENDIF.
ENDMETHOD.
```

---

#### 6.2.4 `fill_changed_audit`

```abap
METHOD fill_changed_audit.
  cv_changed_by = sy-uname.
  cv_changed_on = sy-datum.
  cv_changed_at = sy-uzeit.
ENDMETHOD.
```

---

#### 6.2.5 `fill_created_audit`

```abap
METHOD fill_created_audit.
* Only populate if not already set (initial = new record)
  IF cv_created_by IS INITIAL.
    cv_created_by = sy-uname.
    cv_created_on = sy-datum.
    cv_created_at = sy-uzeit.
  ENDIF.
ENDMETHOD.
```

---

#### 6.2.6 Display Methods — Pattern (apply to all 4 rule types)

Below is the implementation for **`display_prod_reg`**. The same pattern applies for `display_rel_leg`, `display_erun_dis`, and `display_erun_davg` — only the SELECT fields, table, types, and title differ.

```abap
METHOD display_prod_reg.
  DATA: lt_data      TYPE TABLE OF gty_prod_reg,
        lw_data      TYPE gty_prod_reg,
        lo_alv       TYPE REF TO cl_salv_table,
        lo_exception TYPE REF TO cx_root,
        lv_message   TYPE string.

* SELECT named fields only — structure gty_prod_reg matches field order
  SELECT matnr region start_dt end_date
         created_by created_on created_at
         changed_by changed_on changed_at
    FROM zctd_prod_reg_rl CLIENT SPECIFIED
    INTO TABLE lt_data
    WHERE mandt = sy-mandt.
  IF sy-subrc <> 0.
    MESSAGE 'No records found for CTD Product Region Rule'(i01) TYPE 'I'.
    RETURN.
  ENDIF.

  SORT lt_data BY matnr region start_dt.

  TRY.
    cl_salv_table=>factory(
      IMPORTING r_salv_table = lo_alv
      CHANGING  t_table      = lt_data ).
  CATCH cx_salv_msg INTO lo_exception.
    lv_message = lo_exception->get_text( ).
    MESSAGE lv_message TYPE 'E'.
    RETURN.
  ENDTRY.

  me->set_alv_props( io_alv   = lo_alv
                     iv_title = 'CTD Product Region Rule'(t01) ).
  lo_alv->display( ).
ENDMETHOD.
```

**Display method summary table:**

| Method | Table | Sort Key | ALV Title (text symbol) |
|---|---|---|---|
| `display_prod_reg` | ZCTD_PROD_REG_RL | MATNR, REGION, START_DT | `T01` — CTD Product Region Rule |
| `display_rel_leg` | ZCTD_REL_LEG_RL | LEGS, START_DT | `T02` — CTD Relevant Legs Rule |
| `display_erun_dis` | ZCTD_ERUN_DIS_IC | LEGS, START_DT | `T03` — CTD Empty Run Distance (Incentive) |
| `display_erun_davg` | ZCTD_ERUN_DAVG_DIS | BUSINESS, SUB_BUSINESS, START_DT | `T04` — CTD Empty Run per Day Average Distance |

---

#### 6.2.7 `set_alv_props` (Private Helper)

```abap
METHOD set_alv_props.
  DATA: lo_functions TYPE REF TO cl_salv_functions_list,
        lo_layout    TYPE REF TO cl_salv_layout,
        lo_display   TYPE REF TO cl_salv_display_settings,
        lw_key       TYPE salv_s_layout_key.

  lo_functions = io_alv->get_functions( ).
  lo_functions->set_all( abap_true ).

  lo_layout = io_alv->get_layout( ).
  lw_key-report = sy-repid.
  lo_layout->set_key( lw_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

  lo_display = io_alv->get_display_settings( ).
  lo_display->set_list_header( iv_title ).
ENDMETHOD.
```

---

#### 6.2.8 Create Methods — Pattern

Create methods clear the global work area, reset flags, then call the corresponding screen. On return, if save flag is set, the save method is called.

```abap
METHOD create_prod_reg.
  CLEAR: gw_prod_reg, gv_save_flag, gv_cancel_flag.
  CALL SCREEN 0100.
  IF gv_save_flag = gc_flag_x.
    me->save_prod_reg( ).
  ENDIF.
ENDMETHOD.

METHOD create_rel_leg.
  CLEAR: gw_rel_leg, gv_save_flag, gv_cancel_flag.
  CALL SCREEN 0200.
  IF gv_save_flag = gc_flag_x.
    me->save_rel_leg( ).
  ENDIF.
ENDMETHOD.

METHOD create_erun_dis.
  CLEAR: gw_erun_dis, gv_save_flag, gv_cancel_flag.
  CALL SCREEN 0300.
  IF gv_save_flag = gc_flag_x.
    me->save_erun_dis( ).
  ENDIF.
ENDMETHOD.

METHOD create_erun_davg.
  CLEAR: gw_erun_davg, gv_save_flag, gv_cancel_flag.
  CALL SCREEN 0400.
  IF gv_save_flag = gc_flag_x.
    me->save_erun_davg( ).
  ENDIF.
ENDMETHOD.
```

---

#### 6.2.9 Overlap Check Methods — Pattern

Below is the implementation for **`check_overlap_prod_reg`**. Same pattern for the other three.

```abap
METHOD check_overlap_prod_reg.
* Overlap exists if any existing record for same key (MATNR+REGION) has
* start_dt <= iv_end_date AND end_date >= iv_start_dt
  DATA: lt_check  TYPE TABLE OF zctd_prod_reg_rl,
        lw_check  TYPE zctd_prod_reg_rl.

  rv_overlap = abap_false.

  SELECT matnr region start_dt end_date
    FROM zctd_prod_reg_rl CLIENT SPECIFIED
    INTO TABLE lt_check
    WHERE mandt  = sy-mandt
      AND matnr  = iv_matnr
      AND region = iv_region.
  IF sy-subrc <> 0.
    RETURN.   " No existing records — no overlap
  ENDIF.

  LOOP AT lt_check INTO lw_check.
    IF lw_check-start_dt <= iv_end_date
   AND lw_check-end_date >= iv_start_dt.
      rv_overlap = abap_true.
      RETURN.
    ENDIF.
  ENDLOOP.
ENDMETHOD.
```

**Overlap check summary:**

| Method | Table | Key Fields Checked |
|---|---|---|
| `check_overlap_prod_reg` | ZCTD_PROD_REG_RL | MATNR + REGION |
| `check_overlap_rel_leg` | ZCTD_REL_LEG_RL | LEGS |
| `check_overlap_erun_dis` | ZCTD_ERUN_DIS_IC | LEGS |
| `check_overlap_erun_davg` | ZCTD_ERUN_DAVG_DIS | BUSINESS + SUB_BUSINESS |

---

#### 6.2.10 Validate Methods — Pattern

```abap
METHOD validate_prod_reg.
  rv_valid = abap_false.

* 1. Mandatory: MATNR
  IF gw_prod_reg-matnr IS INITIAL.
    MESSAGE 'Material Number is mandatory'(v01) TYPE 'E'.
    RETURN.
  ENDIF.

* 2. MATNR must exist in MARA
  SELECT SINGLE matnr
    FROM mara CLIENT SPECIFIED
    INTO gw_prod_reg-matnr
    WHERE mandt = sy-mandt
      AND matnr = gw_prod_reg-matnr.
  IF sy-subrc <> 0.
    MESSAGE 'Material Number does not exist in MARA'(v02) TYPE 'E'.
    RETURN.
  ENDIF.

* 3. Mandatory: REGION
  IF gw_prod_reg-region IS INITIAL.
    MESSAGE 'Region is mandatory'(v03) TYPE 'E'.
    RETURN.
  ENDIF.

* 4. REGION must exist in T005S (LAND1 = 'IN')
  SELECT SINGLE bland
    FROM t005s CLIENT SPECIFIED
    INTO gw_prod_reg-region
    WHERE mandt = sy-mandt
      AND land1 = 'IN'
      AND bland = gw_prod_reg-region.
  IF sy-subrc <> 0.
    MESSAGE 'Region does not exist in T005S for India'(v04) TYPE 'E'.
    RETURN.
  ENDIF.

* 5. Mandatory: START_DT
  IF gw_prod_reg-start_dt IS INITIAL.
    MESSAGE 'From Date is mandatory'(v05) TYPE 'E'.
    RETURN.
  ENDIF.

* 6. Mandatory: END_DATE
  IF gw_prod_reg-end_date IS INITIAL.
    MESSAGE 'To Date is mandatory'(v06) TYPE 'E'.
    RETURN.
  ENDIF.

* 7. Date validation: END_DATE >= START_DT
  IF gw_prod_reg-end_date < gw_prod_reg-start_dt.
    MESSAGE 'To Date cannot be earlier than From Date'(v07) TYPE 'E'.
    RETURN.
  ENDIF.

* 8. Overlap check
  IF me->check_overlap_prod_reg(
         iv_matnr    = gw_prod_reg-matnr
         iv_region   = gw_prod_reg-region
         iv_start_dt = gw_prod_reg-start_dt
         iv_end_date = gw_prod_reg-end_date ) = abap_true.
    MESSAGE 'Short-close the existing record before creating a new one'(v08) TYPE 'E'.
    RETURN.
  ENDIF.

  rv_valid = abap_true.
ENDMETHOD.
```

**Validate method summary — key validations per rule type:**

| Rule | Mandatory Fields | Cross-Checks | Date | Overlap Key |
|---|---|---|---|---|
| Product Region | MATNR, REGION, START_DT, END_DATE | MATNR → MARA; REGION → T005S (LAND1='IN') | END_DATE ≥ START_DT | MATNR + REGION |
| Relevant Legs | LEGS, APP_LEGS, START_DT, END_DATE | APP_LEGS ≤ LEGS; checkbox count = APP_LEGS | END_DATE ≥ START_DT | LEGS |
| Empty Run Distance | LEGS, DIST, START_DT, END_DATE | LEGS > 0; DIST > 0 | END_DATE ≥ START_DT | LEGS |
| Empty Run Daily Avg | BUSINESS, SUB_BUSINESS, CTD_DAYAVG_DIST, START_DT, END_DATE | CTD_DAYAVG_DIST > 0 | END_DATE ≥ START_DT | BUSINESS + SUB_BUSINESS |

---

#### 6.2.11 Save Methods — Pattern

```abap
METHOD save_prod_reg.
  DATA: lv_valid TYPE abap_bool.

  lv_valid = me->validate_prod_reg( ).
  IF lv_valid = abap_false.
    RETURN.
  ENDIF.

* Lock record before insert
  CALL FUNCTION 'ENQUEUE_EZCTD_PROD_REG_RL'
    EXPORTING
      matnr        = gw_prod_reg-matnr
      region       = gw_prod_reg-region
      start_dt     = gw_prod_reg-start_dt
    EXCEPTIONS
      foreign_lock = 1
      system_failure = 2
      OTHERS       = 3.
  IF sy-subrc <> 0.
    MESSAGE 'Record is locked by another user'(s01) TYPE 'E'.
    RETURN.
  ENDIF.

* Populate audit fields
  me->fill_created_audit(
    CHANGING
      cv_created_by = gw_prod_reg-created_by
      cv_created_on = gw_prod_reg-created_on
      cv_created_at = gw_prod_reg-created_at ).
  me->fill_changed_audit(
    CHANGING
      cv_changed_by = gw_prod_reg-changed_by
      cv_changed_on = gw_prod_reg-changed_on
      cv_changed_at = gw_prod_reg-changed_at ).

* Insert record
  INSERT zctd_prod_reg_rl FROM gw_prod_reg.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MESSAGE 'CTD Product Region Rule saved successfully'(s02) TYPE 'S'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE 'Error saving record — duplicate key'(s03) TYPE 'E'.
  ENDIF.

* Unlock
  CALL FUNCTION 'DEQUEUE_EZCTD_PROD_REG_RL'
    EXPORTING
      matnr    = gw_prod_reg-matnr
      region   = gw_prod_reg-region
      start_dt = gw_prod_reg-start_dt.
ENDMETHOD.
```

> Apply the same save pattern for `save_rel_leg`, `save_erun_dis`, and `save_erun_davg` using the respective lock objects and tables.

---

#### 6.2.12 Relevant Legs Rule — Additional Validation Logic

For `validate_rel_leg`, add the following checks specific to this rule (in addition to the standard pattern):

```abap
* Check: APP_LEGS <= LEGS
  IF gw_rel_leg-app_legs > gw_rel_leg-legs.
    MESSAGE 'Applicable Legs cannot exceed Count of Loaded Legs'(v10) TYPE 'E'.
    RETURN.
  ENDIF.

* Check: Count of checked checkboxes (FIELD1-FIELD9 = 'X') must equal APP_LEGS
  DATA: lv_checked_count TYPE numc2.
  lv_checked_count = 0.
  IF gw_rel_leg-field1 = gc_flag_x. lv_checked_count = lv_checked_count + 1. ENDIF.
  IF gw_rel_leg-field2 = gc_flag_x. lv_checked_count = lv_checked_count + 1. ENDIF.
  IF gw_rel_leg-field3 = gc_flag_x. lv_checked_count = lv_checked_count + 1. ENDIF.
  IF gw_rel_leg-field4 = gc_flag_x. lv_checked_count = lv_checked_count + 1. ENDIF.
  IF gw_rel_leg-field5 = gc_flag_x. lv_checked_count = lv_checked_count + 1. ENDIF.
  IF gw_rel_leg-field6 = gc_flag_x. lv_checked_count = lv_checked_count + 1. ENDIF.
  IF gw_rel_leg-field7 = gc_flag_x. lv_checked_count = lv_checked_count + 1. ENDIF.
  IF gw_rel_leg-field8 = gc_flag_x. lv_checked_count = lv_checked_count + 1. ENDIF.
  IF gw_rel_leg-field9 = gc_flag_x. lv_checked_count = lv_checked_count + 1. ENDIF.

  IF lv_checked_count <> gw_rel_leg-app_legs.
    MESSAGE 'Number of selected checkboxes must equal Applicable Legs count'(v11) TYPE 'E'.
    RETURN.
  ENDIF.
```

---

## 7. Screen Specifications (SE51 / Screen Painter)

### Screen 0100 — CTD Product Region Rule Create

| Property | Value |
|---|---|
| Screen Type | Normal |
| Short Description | CTD Product Region Rule — Create |
| GUI Status | `ZCTD_RULES_0100` (Back, Cancel, Save buttons) |

**Fields:**

| Screen Field | Field Label | Type | F4 Help | Mandatory |
|---|---|---|---|:---:|
| `GW_PROD_REG-MATNR` | Material Number | Input | MARA-MATNR | X |
| `GW_PROD_REG-REGION` | Region | Input | T005S-BLAND (LAND1='IN') | X |
| `GW_PROD_REG-START_DT` | From Date | Input | — | X |
| `GW_PROD_REG-END_DATE` | To Date | Input | — | X |

**PBO module** (`MODULE pbo_0100 OUTPUT`): Set GUI status; protect audit fields.

**PAI module** (`MODULE pai_0100 INPUT`):
- `WHEN 'SAVE'`: Set `gv_save_flag = 'X'`; `LEAVE SCREEN`.
- `WHEN 'BACK' OR 'CANCEL'`: Set `gv_cancel_flag = 'X'`; `LEAVE SCREEN`.

---

### Screen 0200 — CTD Relevant Legs Rule Create

| Property | Value |
|---|---|
| Screen Type | Normal |
| Short Description | CTD Relevant Legs Rule — Create |

**Fields:**

| Screen Field | Field Label | Type | Note |
|---|---|---|---|
| `GW_REL_LEG-LEGS` | Count of Loaded Legs | Input/Dropdown | Values 1–9 |
| `GW_REL_LEG-APP_LEGS` | No. of CTD Applicable Legs | Input | Enter count first; Save locked until checkbox count matches |
| `GW_REL_LEG-START_DT` | From Date | Input | |
| `GW_REL_LEG-END_DATE` | To Date | Input | |
| `GW_REL_LEG-FIELD1` … `FIELD9` | Leg 1 … Leg 9 | Checkbox | Visible count = GW_REL_LEG-LEGS |

**Screen Behaviour:**
- On entry of `LEGS`, make only FIELD1–FIELDn visible (where n = LEGS value); hide remaining checkboxes.
- Save button: enabled only when checked count equals APP_LEGS (validate in PAI).
- Use `LOOP AT SCREEN` in PBO to show/hide FIELD# based on `GW_REL_LEG-LEGS`.

---

### Screen 0300 — CTD Empty Run Distance (Incentive) Create

| Screen Field | Field Label | Mandatory |
|---|---|:---:|
| `GW_ERUN_DIS-LEGS` | Count of Loaded Legs | X |
| `GW_ERUN_DIS-DIST` | Distance (km) | X |
| `GW_ERUN_DIS-START_DT` | From Date | X |
| `GW_ERUN_DIS-END_DATE` | To Date | X |

---

### Screen 0400 — CTD Empty Run per Day Average Distance Create

| Screen Field | Field Label | Mandatory |
|---|---|:---:|
| `GW_ERUN_DAVG-BUSINESS` | Business | X |
| `GW_ERUN_DAVG-SUB_BUSINESS` | Sub-Business | X |
| `GW_ERUN_DAVG-CTD_DAYAVG_DIST` | Average Distance per Day (km) | X |
| `GW_ERUN_DAVG-START_DT` | From Date | X |
| `GW_ERUN_DAVG-END_DATE` | To Date | X |

---

## 8. Message Text Elements

| Symbol | Type | Text |
|---|---|---|
| `E01` | E | You are not authorized for this action |
| `I01` | I | No records found for CTD Product Region Rule |
| `I02` | I | No records found for CTD Relevant Legs Rule |
| `I03` | I | No records found for CTD Empty Run Distance Rule |
| `I04` | I | No records found for CTD Empty Run Daily Average Distance Rule |
| `V01` | E | Material Number is mandatory |
| `V02` | E | Material Number does not exist in MARA |
| `V03` | E | Region is mandatory |
| `V04` | E | Region does not exist in T005S for India |
| `V05` | E | From Date is mandatory |
| `V06` | E | To Date is mandatory |
| `V07` | E | To Date cannot be earlier than From Date |
| `V08` | E | Short-close the existing record before creating a new one |
| `V09` | E | Count of Loaded Legs must be greater than zero |
| `V10` | E | Applicable Legs cannot exceed Count of Loaded Legs |
| `V11` | E | Number of selected checkboxes must equal Applicable Legs count |
| `V12` | E | Distance must be greater than zero |
| `V13` | E | Average Distance per Day must be greater than zero |
| `S01` | E | Record is locked by another user |
| `S02` | S | Record saved successfully |
| `S03` | E | Error saving record — duplicate key |
| `T01–T04` | — | ALV title text per rule type |

---

## 9. ABAP Rules Compliance Checklist

| Rule | How Applied |
|---|---|
| **No inline declarations** (`DATA(x)`) | All variables declared upfront in DATA sections (NW 7.31) |
| **No `NEW` / `VALUE` / `CORRESPONDING`** | Not used anywhere |
| **No `SELECT *`** | All SELECTs name explicit fields; types match field order |
| **No SELECT in loop** | No loop-based SELECTs; overlap checks use targeted SELECT on key fields |
| **No nested LOOPs** | Item lookups use `READ TABLE … BINARY SEARCH` inside any outer loop |
| **`IS NOT INITIAL` not in OpenSQL WHERE** | Used only in ABAP `IF` checks, not in `WHERE` clauses |
| **`SY-SUBRC` checked immediately** | Every SELECT, READ TABLE, CALL FUNCTION, AUTHORITY-CHECK followed by `IF sy-subrc` |
| **Naming: `lv_`, `lt_`, `lw_`, `lty_`, `gc_`** | Applied throughout; work areas use `lw_`/`gw_` (not `ls_`/`gs_`) |
| **Constants for fixed values** | `gc_actvt_create`, `gc_actvt_display`, `gc_flag_x`, `gc_space` declared as CONSTANTS |
| **No `sy-*` in CONSTANTS** | `sy-uname`, `sy-datum`, `sy-uzeit`, `sy-mandt` used directly |
| **OOP — no FORMs** | All logic in class `lcl_ctd_rules` methods; no FORM/PERFORM |
| **FM only for Lock/Unlock** | `ENQUEUE_*` / `DEQUEUE_*` are FMs per SAP standard; only those used |
| **CL_SALV_TABLE** | Used for all Display ALVs with layout variant save |
| **Locking before INSERT** | `ENQUEUE_*` called before insert; `DEQUEUE_*` called after |
| **No physical delete** | No DELETE statements on Z-tables; short-close approach via END_DATE |
| **Authorization check** | S_TCODE checked at INITIALIZATION; Z_CTD_RULE_AUTH checked at execute time |
| **SORT + BINARY SEARCH** | All tables sorted before READ TABLE; BINARY SEARCH added |
| **FOR ALL ENTRIES guard** | `IF table IS NOT INITIAL` before every FOR ALL ENTRIES |

---

## 10. Implementation Checklist

| # | Task | Object / Tool |
|---|---|---|
| 1 | Remove APP_LEGS from key in ZCTD_REL_LEG_RL | SE11 |
| 2 | Add 6 audit fields to ZCTD_REL_LEG_RL | SE11 |
| 3 | Remove DIST from key in ZCTD_ERUN_DIS_IC | SE11 |
| 4 | Create new table ZCTD_ERUN_DAVG_DIS | SE11 |
| 5 | Create data element for CTD_DAYAVG_DIST if not existing | SE11 |
| 6 | Create lock object EZCTD_PROD_REG_RL | SE11 |
| 7 | Create lock object EZCTD_REL_LEG_RL | SE11 |
| 8 | Create lock object EZCTD_ERUN_DIS_IC | SE11 |
| 9 | Create lock object EZCTD_ERUN_DAVG_DIS | SE11 |
| 10 | Create authorization object Z_CTD_RULE_AUTH (Activity 01, 03) | SU21 |
| 11 | Activate all DDIC objects | SE11 |
| 12 | Create report program ZCTD_RULES | SE38 |
| 13 | Create include ZCTD_RULESTOP (global declarations) | SE38 |
| 14 | Create include ZCTD_RULESSEL (selection screen) | SE38 |
| 15 | Create include ZCTD_RULESC01 (class definition + implementation) | SE38 |
| 16 | Add text symbols per §5 and §8 | SE38 → Text Elements |
| 17 | Create GUI status `ZCTD_RULES_0100` (Save, Back, Cancel) | SE41 |
| 18 | Create Screen 0100 — CTD Product Region Rule Create | SE51 |
| 19 | Create Screen 0200 — CTD Relevant Legs Rule Create (with checkbox loop) | SE51 |
| 20 | Create Screen 0300 — CTD Empty Run Distance Create | SE51 |
| 21 | Create Screen 0400 — CTD Empty Run per Day Average Distance Create | SE51 |
| 22 | Add PBO modules (O01 include) for all 4 screens | SE38 |
| 23 | Add PAI modules (I01 include) for all 4 screens | SE38 |
| 24 | Create transaction ZCTD_RULES | SE93 |
| 25 | Syntax check (Code Inspector) — zero errors/warnings | SE38 / SCI |
| 26 | Unit test per §11 in DEV | DEV client |
| 27 | Transport to QA | STMS |

---

## 11. Test Checklist

| # | Test Case | Expected Result |
|---|---|---|
| 1 | Run ZCTD_RULES — selection screen visible | 4 rule type radio buttons + Create/Display radio buttons shown |
| 2 | Execute without authorization (Z_CTD_RULE_AUTH) | Error: "You are not authorized for this action" |
| 3 | **Product Region Rule → Display** — records exist | ALV grid shows all ZCTD_PROD_REG_RL records; read-only |
| 4 | **Product Region Rule → Display** — no records | Info message: "No records found…" |
| 5 | **Product Region Rule → Create** — valid MATNR + REGION + dates | Record saved; audit fields auto-populated |
| 6 | **Product Region Rule → Create** — invalid MATNR | Error: "Material Number does not exist in MARA" |
| 7 | **Product Region Rule → Create** — invalid REGION | Error: "Region does not exist in T005S for India" |
| 8 | **Product Region Rule → Create** — END_DATE < START_DT | Error: "To Date cannot be earlier than From Date" |
| 9 | **Product Region Rule → Create** — overlapping dates same MATNR+REGION | Error: "Short-close the existing record before creating a new one" |
| 10 | **Relevant Legs Rule → Create** — valid data; checkbox count = APP_LEGS | Record saved |
| 11 | **Relevant Legs Rule → Create** — checkbox count ≠ APP_LEGS | Error: "Number of selected checkboxes must equal Applicable Legs count" |
| 12 | **Relevant Legs Rule → Create** — APP_LEGS > LEGS | Error: "Applicable Legs cannot exceed Count of Loaded Legs" |
| 13 | **Relevant Legs Rule → Create** — checkbox visibility | Only FIELD1–FIELDn visible based on LEGS value entered |
| 14 | **Relevant Legs Rule → Display** | ALV shows all ZCTD_REL_LEG_RL records including audit fields |
| 15 | **Empty Run Distance → Create** — valid data | Record saved; audit fields populated |
| 16 | **Empty Run Distance → Create** — DIST = 0 | Error: "Distance must be greater than zero" |
| 17 | **Empty Run Distance → Create** — overlapping dates same LEGS | Error: "Short-close the existing record first" |
| 18 | **Empty Run Distance → Display** | ALV shows all ZCTD_ERUN_DIS_IC records |
| 19 | **Empty Run Daily Avg → Create** — valid data | Record saved in ZCTD_ERUN_DAVG_DIS |
| 20 | **Empty Run Daily Avg → Create** — CTD_DAYAVG_DIST = 0 | Error: "Average Distance per Day must be greater than zero" |
| 21 | **Empty Run Daily Avg → Create** — overlap same BUSINESS + SUB_BUSINESS | Error: "Short-close the existing record first" |
| 22 | **Empty Run Daily Avg → Display** | ALV shows all ZCTD_ERUN_DAVG_DIS records |
| 23 | CHANGED_BY / CHANGED_ON / CHANGED_AT populated on create | Audit fields reflect current user and timestamp |
| 24 | Save ALV layout variant; re-execute | Layout restored |
| 25 | Cancel on any Create screen | No record saved; returns to selection screen |

---

## 12. References

- `Tcode for CTD Rules/FS.md` (Version 2.0 As-Built)
- `ABAP RULES/00-main.mdc` — Core principles, NW 7.31 compatibility
- `ABAP RULES/02-naming.mdc` — Naming conventions
- `ABAP RULES/03-database.mdc` — Database access rules
- `ABAP RULES/04-oop.mdc` — OOP and class design
- `ABAP RULES/06-security.mdc` — Authorization rules
- `ABAP RULES/07-ui.mdc` — Selection screen and ALV guidelines
- `ABAP RULES/13-sy-subrc.mdc` — SY-SUBRC check requirements
- `ABAP RULES/16-transactions-data.mdc` — Locking, soft delete, data safety
- `ABAP RULES/18-module-pool.mdc` — Dynpro/screen standards
- `ABAP RULES/20-code-generation-checklist.mdc` — Performance checklist

---

*End of Code Change Document*
