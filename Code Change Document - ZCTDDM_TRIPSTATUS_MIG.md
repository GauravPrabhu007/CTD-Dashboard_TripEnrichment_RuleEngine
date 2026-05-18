# Code Change Document — CTD Trip Status Cutover Migration Program

**Module:** SCM — CTD (Converted to Dedicated)  
**Change Type:** New Development — Executable Report  
**Date:** 18-May-2026  
**Prepared By:** Gaurav Prabhu  
**Reference FS:** FS - CTD Trip Status Cutover Migration Program.md  
**SAP Target:** ECC 6.0 / NetWeaver 7.31  

---

| Attribute | Detail |
|-----------|--------|
| **CD Number** | CD: *(to be assigned)* |
| **Transport** | TR: *(to be assigned)* |
| **Technical Author** | *(ABAP developer)* |
| **Functional Author** | Gaurav Prabhu |

---

## Summary of New Objects

| # | Object Type | Object Name | Description |
|---|-------------|-------------|-------------|
| 1 | Program | `ZCTDDM_TRIPSTATUS_MIG` | Main report — CTD cutover trip status migration |
| 2 | Include | `ZCTDDM_TRIPSTATUS_MIGTOP` | Global types, data, constants |
| 3 | Include | `ZCTDDM_TRIPSTATUS_MIGSEL` | Selection screen |
| 4 | Include | `ZCTDDM_TRIPSTATUS_MIGC01` | Local class `LCL_REPORT` — all business logic |
| 5 | Transaction *(optional)* | `ZCTDDM_TRIP_MIG` | T-code for program execution |
| 6 | Message Class *(SE91)* | `ZCTDDM_MIG` | Program messages |
| 7 | Authorization Object *(optional)* | `ZCTD_CUTOVER` | Cutover program execution |

**No new dictionary tables.** Configuration via existing `ZLOG_EXEC_VAR` (SE16 — no transport for data).

---

## ABAP Coding Standards Applied

Per folder `ABAP Rules - 02-04-2026`:

| Rule | Application |
|------|-------------|
| **NW 7.31** | No inline `DATA(...)`, no `VALUE()` / `NEW`, no string templates, no table expressions |
| **OOP report** | All logic in local class `LCL_REPORT`; no PERFORM / F01 include |
| **Includes** | TOP → SEL → C01 (mandatory structure) |
| **Naming** | `lv_`, `lt_`, `lw_`, `lc_`, `lty_` prefixes |
| **Database** | Explicit field list; no `SELECT *`; no `SELECT` in loop; `CLIENT SPECIFIED` |
| **Loops** | `LOOP AT ... ASSIGNING` where rows updated |
| **ALV** | `CL_SALV_TABLE` object-oriented |
| **Comments** | `"` style; BEGIN/END Cursor Generated Code markers on new blocks |
| **sy-subrc** | Checked immediately after READ / SELECT / UPDATE / ENQUEUE |

---

## 1. Business Purpose

One-time cutover program to update `ZSCE_CTD_HDR-TRIP_STATUS` for legacy trips from **old** status codes to **new** codes per `ZLOG_EXEC_VAR`, only when `CREATED_DATE` is **before** the configured cut-off date.

**Primary case:** Old `07` (Complete Trip) → New `11` (CTD Confirmed by RIL Operations).

**Must not call:** `Z_SCM_CTD_TRIPCONFIRM`, `Z_SCM_CTD_AUTO_TRIPCONFIRM`, `ZCTD_RULEENG_EXEC`.

---

## 2. Configuration — ZLOG_EXEC_VAR (SE16 — Post-Import)

### 2.1 Status mapping — `ZCTDDM_TRIPSTATUS_OLDTONEW`

| NAME | NUMB | REMARKS (old) | ERRORMSG (new) | ACTIVE |
|------|------|---------------|----------------|--------|
| ZCTDDM_TRIPSTATUS_OLDTONEW | 0001 | 04 | 05 | X |
| ZCTDDM_TRIPSTATUS_OLDTONEW | 0002 | 05 | 07 | X |
| ZCTDDM_TRIPSTATUS_OLDTONEW | 0003 | 06 | 06 | X |
| ZCTDDM_TRIPSTATUS_OLDTONEW | 0004 | 07 | 11 | X |

### 2.2 Cut-off date — `ZCTDDM_TRIPSTATUS_CUTOFF`

| NAME | NUMB | REMARKS | ACTIVE |
|------|------|---------|--------|
| ZCTDDM_TRIPSTATUS_CUTOFF | 0001 | 20260518 | X |

*`REMARKS` = `YYYYMMDD` — first day of new CTD process. Trips with `CREATED_DATE >=` this date are skipped.*

---

## 3. Message Class — SE91 `ZCTDDM_MIG`

Create message class and attach to program attributes.

| No | Type | Text |
|----|------|------|
| 001 | E | Status mapping not maintained in ZLOG_EXEC_VAR (ZCTDDM_TRIPSTATUS_OLDTONEW) |
| 002 | E | Cut-off date not maintained or invalid in ZLOG_EXEC_VAR (ZCTDDM_TRIPSTATUS_CUTOFF) |
| 003 | E | Duplicate old status &1 in mapping configuration |
| 004 | E | Input file name is required when single-trip parameters are not filled |
| 005 | E | Could not read input file &1 |
| 006 | E | No valid trip records in input |
| 007 | E | Trip &1 / &2 / &3 not found in ZSCE_CTD_HDR |
| 008 | E | Trip &1 / &2 / &3 is deleted (DEL_IND set) |
| 009 | W | Trip &1 / &2 / &3 created on or after cut-off &4 — use CTD Dashboard |
| 010 | W | Trip &1 status &2 is not eligible for migration |
| 011 | S | Trip &1 status updated from &2 to &3 |
| 012 | I | Test mode: Trip &1 would be updated from &2 to &3 |
| 013 | E | Trip &1 / &2 / &3 could not be locked |
| 014 | W | Trip &1 already at target status &2 — no change |
| 015 | E | Failed to update trip &1 / &2 / &3 |
| 016 | S | Migration completed: &1 success, &2 warning, &3 error (&4 test) |
| 017 | I | Active cut-off date: &1 (&2 mapping rows loaded) |
| 018 | E | Invalid line in input file (line &1) |
| 019 | E | Authorization check failed for cutover program |

---

## 4. Program Structure

```
ZCTDDM_TRIPSTATUS_MIG          (main)
├── ZCTDDM_TRIPSTATUS_MIGTOP   (types, data, constants)
├── ZCTDDM_TRIPSTATUS_MIGSEL   (selection screen)
└── ZCTDDM_TRIPSTATUS_MIGC01   (class LCL_REPORT)
```

---

## 5. Selection Screen

| Parameter | Type | Description |
|-----------|------|-------------|
| `P_FILE` | `RLGRAP-FILENAME` | Input file path (tab-delimited: LIFNR, TRUCK_NO, TRIP_NO) |
| `P_LIFNR` | `LIFNR` | Optional single trip — vendor |
| `P_TRUCK` | `YTRUCK_NO` | Optional single trip — truck |
| `P_TRIP` | `ZTRIP_NO` | Optional single trip — trip number |
| `P_TEST` | `ABAP_BOOL` as checkbox | Test mode — no DB update / no commit |
| `P_OVRD` | Checkbox | Override cut-off from `P_CUTOFF` (UAT only) |
| `P_CUTOFF` | `SYDATUM` | Override cut-off date when `P_OVRD` = X |

**Input rule:** Provide **`P_FILE`** **or** all three `P_LIFNR` + `P_TRUCK` + `P_TRIP`.

**File format:** Tab-separated; optional header row containing `LIFNR`. Example:

```text
LIFNR	TRUCK_NO	TRIP_NO
00001029043	TRUCK001	20251019151538
```

---

## 6. Include — `ZCTDDM_TRIPSTATUS_MIGTOP`

```abap
*&---------------------------------------------------------------------*
*& Include          ZCTDDM_TRIPSTATUS_MIGTOP
*&---------------------------------------------------------------------*
*& Purpose: Global declarations for CTD trip status cutover migration
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

TYPES: BEGIN OF lty_trip_input,
         lifnr    TYPE lifnr,
         truck_no TYPE ytruck_no,
         trip_no  TYPE ztrip_no,
       END OF lty_trip_input.

TYPES: lty_trip_input_tt TYPE STANDARD TABLE OF lty_trip_input.

TYPES: BEGIN OF lty_status_map,
         old_status TYPE zctd_trip_st,
         new_status TYPE zctd_trip_st,
       END OF lty_status_map.

TYPES: lty_status_map_tt TYPE STANDARD TABLE OF lty_status_map.

TYPES: BEGIN OF lty_mig_result,
         lifnr         TYPE lifnr,
         truck_no      TYPE ytruck_no,
         trip_no       TYPE ztrip_no,
         created_date  TYPE sydatum,
         status_before TYPE zctd_trip_st,
         status_after  TYPE zctd_trip_st,
         result        TYPE char1,
         message       TYPE char255,
       END OF lty_mig_result.

TYPES: lty_mig_result_tt TYPE STANDARD TABLE OF lty_mig_result.

TYPES: BEGIN OF lty_file_raw,
         line TYPE char1024,
       END OF lty_file_raw.

TYPES: lty_file_raw_tt TYPE STANDARD TABLE OF lty_file_raw.

TYPES: BEGIN OF lty_zlog_cfg,
         name     TYPE rvari_vnam,
         numb     TYPE tvarv_numb,
         remarks  TYPE textr,
         errormsg TYPE natxt,
       END OF lty_zlog_cfg.

TYPES: lty_zlog_cfg_tt TYPE STANDARD TABLE OF lty_zlog_cfg.

DATA: go_report TYPE REF TO lcl_report.

DATA: gt_trip_input TYPE lty_trip_input_tt,
      gt_status_map TYPE lty_status_map_tt,
      gt_result     TYPE lty_mig_result_tt,
      gv_cutoff     TYPE sydatum,
      gv_map_count  TYPE i,
      gv_test_mode  TYPE abap_bool,
      gv_commit_ok  TYPE abap_bool.

CONSTANTS: lc_name_map    TYPE rvari_vnam VALUE 'ZCTDDM_TRIPSTATUS_OLDTONEW',
           lc_name_cutoff TYPE rvari_vnam VALUE 'ZCTDDM_TRIPSTATUS_CUTOFF',
           lc_num_cutoff  TYPE tvarv_numb VALUE '0001',
           lc_res_success TYPE char1      VALUE 'S',
           lc_res_warning TYPE char1      VALUE 'W',
           lc_res_error   TYPE char1      VALUE 'E',
           lc_res_test    TYPE char1      VALUE 'T',
           lc_active      TYPE char1      VALUE 'X',
           lc_del_ind     TYPE char1      VALUE 'X'.

" END: Cursor Generated Code
```

---

## 7. Include — `ZCTDDM_TRIPSTATUS_MIGSEL`

```abap
*&---------------------------------------------------------------------*
*& Include          ZCTDDM_TRIPSTATUS_MIGSEL
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_file TYPE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_lifnr TYPE lifnr,
            p_truck TYPE ytruck_no,
            p_trip  TYPE ztrip_no.
SELECTION-SCREEN COMMENT /1(79) text-003.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-004.
PARAMETERS: p_test  AS CHECKBOX,
            p_ovrd  AS CHECKBOX,
            p_cutoff TYPE sydatum.
SELECTION-SCREEN END OF BLOCK b3.

" END: Cursor Generated Code
```

**Text symbols (SE38 → Text Elements):**

| ID | Text |
|----|------|
| 001 | Trip input file (Tab: LIFNR, TRUCK_NO, TRIP_NO) |
| 002 | Or single trip (full key) |
| 003 | Use file OR all three fields below — trip number alone is not sufficient |
| 004 | Processing options |

---

## 8. Main Program — `ZCTDDM_TRIPSTATUS_MIG`

```abap
*&---------------------------------------------------------------------*
*& Report  ZCTDDM_TRIPSTATUS_MIG
*&---------------------------------------------------------------------*
*& Purpose         : CTD Cutover - Migrate legacy trip status on ZSCE_CTD_HDR
*& Author          : Gaurav Prabhu
*& Creation Date   : 18.05.2026
*& Reference FS    : FS - CTD Trip Status Cutover Migration Program.md
*& CD              : CD: TBD  TR: TBD
*&---------------------------------------------------------------------*
REPORT zctddm_tripstatus_mig MESSAGE-ID zctddm_mig.

INCLUDE zctddm_tripstatus_migtop.
INCLUDE zctddm_tripstatus_migsel.
INCLUDE zctddm_tripstatus_migc01.

INITIALIZATION.
  CREATE OBJECT go_report.
  go_report->initialization( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  go_report->f4_filename( ).

AT SELECTION-SCREEN.
  go_report->validate_screen( ).

START-OF-SELECTION.
  go_report->execute( ).

END-OF-SELECTION.
  go_report->display_alv( ).
```

---

## 9. Include — `ZCTDDM_TRIPSTATUS_MIGC01` (Class `LCL_REPORT`)

```abap
*&---------------------------------------------------------------------*
*& Include          ZCTDDM_TRIPSTATUS_MIGC01
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    METHODS: initialization,
             f4_filename,
             validate_screen,
             execute,
             display_alv.
  PRIVATE SECTION.
    METHODS: authorization_check
               RETURNING value(rv_ok) TYPE abap_bool,
             load_configuration
               RETURNING value(rv_ok) TYPE abap_bool,
             read_input_trips
               RETURNING value(rv_ok) TYPE abap_bool,
             process_trips,
             process_one_trip
               IMPORTING is_input TYPE lty_trip_input,
             append_result
               IMPORTING is_input       TYPE lty_trip_input
                         iv_created     TYPE sydatum
                         iv_stat_before TYPE zctd_trip_st
                         iv_stat_after  TYPE zctd_trip_st
                         iv_result      TYPE char1
                         iv_message     TYPE char255,
             extract_status_2char
               IMPORTING iv_source      TYPE clike
               RETURNING value(rv_stat) TYPE zctd_trip_st,
             validate_cutoff_char
               IMPORTING iv_date_char   TYPE clike
               RETURNING value(rv_date) TYPE sydatum
               RAISING   cx_sy_conversion_error,
             show_summary_message.
    DATA: mv_cutoff    TYPE sydatum,
          mv_test_mode TYPE abap_bool,
          mv_success   TYPE i,
          mv_warning   TYPE i,
          mv_error     TYPE i,
          mv_test_cnt  TYPE i.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.

  METHOD initialization.
    DATA: lv_msg TYPE string.
    CLEAR: gv_cutoff, gv_map_count, gt_status_map[].

    IF load_configuration( ) = abap_true.
      gv_cutoff    = mv_cutoff.
      gv_map_count = lines( gt_status_map ).
      CONCATENATE 'Active cut-off:'
                  gv_cutoff
                  '('
                  gv_map_count
                  'mapping rows)'
             INTO lv_msg SEPARATED BY space.
      MESSAGE lv_msg TYPE 'I'.
    ENDIF.
  ENDMETHOD.

  METHOD f4_filename.
    DATA: lt_filetab TYPE filetable,
          lw_file    TYPE fileline,
          lv_rc      TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      CHANGING
        file_table              = lt_filetab
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        OTHERS                  = 2 ).
    IF sy-subrc <> 0 OR lv_rc < 1.
      RETURN.
    ENDIF.
    READ TABLE lt_filetab INTO lw_file INDEX 1.
    p_file = lw_file-filename.
  ENDMETHOD.

  METHOD validate_screen.
    IF authorization_check( ) = abap_false.
      MESSAGE e019.
    ENDIF.

    IF p_file IS INITIAL.
      IF p_lifnr IS INITIAL OR p_truck IS INITIAL OR p_trip IS INITIAL.
        MESSAGE e004.
      ENDIF.
    ENDIF.

    IF p_ovrd = abap_true AND p_cutoff IS INITIAL.
      MESSAGE e002.
    ENDIF.
  ENDMETHOD.

  METHOD authorization_check.
    " Optional: maintain object ZCTD_CUTOVER activity 16
    " AUTHORITY-CHECK OBJECT 'ZCTD_CUTOVER' ID 'ACTVT' FIELD '16'.
    " IF sy-subrc <> 0. rv_ok = abap_false. RETURN. ENDIF.
    rv_ok = abap_true.
  ENDMETHOD.

  METHOD load_configuration.
    DATA: lt_cfg_map TYPE lty_zlog_cfg_tt,
          lw_cfg     TYPE lty_zlog_cfg,
          lw_map     TYPE lty_status_map,
          lv_old     TYPE zctd_trip_st,
          lv_new     TYPE zctd_trip_st,
          lv_cut_ch  TYPE string.

    CLEAR: gt_status_map[], mv_cutoff.

    " --- Status mapping ---
    SELECT name numb remarks errormsg
      FROM zlog_exec_var CLIENT SPECIFIED
      INTO TABLE lt_cfg_map
      WHERE mandt  = sy-mandt
        AND name   = lc_name_map
        AND active = lc_active.
    IF sy-subrc <> 0 OR lt_cfg_map IS INITIAL.
      MESSAGE e001.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    LOOP AT lt_cfg_map INTO lw_cfg.
      lv_old = extract_status_2char( lw_cfg-remarks ).
      lv_new = extract_status_2char( lw_cfg-errormsg ).
      IF lv_old IS INITIAL OR lv_new IS INITIAL.
        CONTINUE.
      ENDIF.
      READ TABLE gt_status_map TRANSPORTING NO FIELDS
        WITH KEY old_status = lv_old.
      IF sy-subrc = 0.
        MESSAGE e003 WITH lv_old.
        rv_ok = abap_false.
        RETURN.
      ENDIF.
      CLEAR lw_map.
      lw_map-old_status = lv_old.
      lw_map-new_status = lv_new.
      APPEND lw_map TO gt_status_map.
    ENDLOOP.

    IF gt_status_map IS INITIAL.
      MESSAGE e001.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    SORT gt_status_map BY old_status.

    " --- Cut-off date ---
    IF p_ovrd = abap_true AND p_cutoff IS NOT INITIAL.
      mv_cutoff = p_cutoff.
    ELSE.
      CLEAR lw_cfg.
      SELECT SINGLE remarks
        FROM zlog_exec_var CLIENT SPECIFIED
        INTO lv_cut_ch
        WHERE mandt  = sy-mandt
          AND name   = lc_name_cutoff
          AND numb   = lc_num_cutoff
          AND active = lc_active.
      IF sy-subrc <> 0 OR lv_cut_ch IS INITIAL.
        MESSAGE e002.
        rv_ok = abap_false.
        RETURN.
      ENDIF.
      TRY.
          mv_cutoff = validate_cutoff_char( lv_cut_ch ).
        CATCH cx_sy_conversion_error.
          MESSAGE e002.
          rv_ok = abap_false.
          RETURN.
      ENDTRY.
    ENDIF.

    rv_ok = abap_true.
  ENDMETHOD.

  METHOD validate_cutoff_char.
    DATA: lv_ch TYPE c LENGTH 8.
    lv_ch = iv_date_char(8).
    IF lv_ch CO '0123456789' AND strlen( lv_ch ) = 8.
      rv_date = lv_ch.
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_conversion_error.
    ENDIF.
  ENDMETHOD.

  METHOD extract_status_2char.
    DATA: lv_tmp TYPE string.
    lv_tmp = iv_source.
    CONDENSE lv_tmp NO-GAPS.
    rv_stat = lv_tmp(2).
  ENDMETHOD.

  METHOD read_input_trips.
    DATA: lt_raw    TYPE lty_file_raw_tt,
          lw_raw    TYPE lty_file_raw,
          lw_input  TYPE lty_trip_input,
          lv_line   TYPE i,
          lt_split  TYPE TABLE OF string,
          lv_lifnr  TYPE string,
          lv_truck  TYPE string,
          lv_trip   TYPE string.

    CLEAR gt_trip_input[].

    IF p_file IS NOT INITIAL.
      CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          filename            = p_file
          filetype            = 'ASC'
          has_field_separator = 'X'
        TABLES
          data_tab            = lt_raw
        EXCEPTIONS
          file_open_error     = 1
          file_read_error     = 2
          OTHERS              = 3.
      IF sy-subrc <> 0.
        MESSAGE e005 WITH p_file.
        rv_ok = abap_false.
        RETURN.
      ENDIF.

      lv_line = 0.
      LOOP AT lt_raw INTO lw_raw.
        lv_line = lv_line + 1.
        IF lw_raw-line IS INITIAL.
          CONTINUE.
        ENDIF.
        TRANSLATE lw_raw-line TO UPPER CASE.
        IF lw_raw-line CS 'LIFNR'.
          CONTINUE.
        ENDIF.

        CLEAR: lt_split[], lv_lifnr, lv_truck, lv_trip.
        SPLIT lw_raw-line AT cl_abap_char_utilities=>horizontal_tab
          INTO TABLE lt_split.
        IF lines( lt_split ) < 3.
          SPLIT lw_raw-line AT ';' INTO TABLE lt_split.
        ENDIF.
        IF lines( lt_split ) < 3.
          MESSAGE e018 WITH lv_line.
          rv_ok = abap_false.
          RETURN.
        ENDIF.

        READ TABLE lt_split INDEX 1 INTO lv_lifnr.
        READ TABLE lt_split INDEX 2 INTO lv_truck.
        READ TABLE lt_split INDEX 3 INTO lv_trip.
        CONDENSE lv_lifnr NO-GAPS.
        CONDENSE lv_truck NO-GAPS.
        CONDENSE lv_trip NO-GAPS.

        CLEAR lw_input.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING input  = lv_lifnr
          IMPORTING output = lw_input-lifnr.
        lw_input-truck_no = lv_truck.
        lw_input-trip_no  = lv_trip.
        APPEND lw_input TO gt_trip_input.
      ENDLOOP.
    ELSE.
      CLEAR lw_input.
      lw_input-lifnr    = p_lifnr.
      lw_input-truck_no = p_truck.
      lw_input-trip_no  = p_trip.
      APPEND lw_input TO gt_trip_input.
    ENDIF.

    IF gt_trip_input IS INITIAL.
      MESSAGE e006.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    SORT gt_trip_input BY lifnr truck_no trip_no.
    DELETE ADJACENT DUPLICATES FROM gt_trip_input COMPARING lifnr truck_no trip_no.

    rv_ok = abap_true.
  ENDMETHOD.

  METHOD execute.
    CLEAR: gt_result[], mv_success, mv_warning, mv_error, mv_test_cnt, gv_commit_ok.

    gv_test_mode = p_test.
    mv_test_mode = p_test.

    IF load_configuration( ) = abap_false.
      RETURN.
    ENDIF.
    gv_cutoff = mv_cutoff.

    IF read_input_trips( ) = abap_false.
      RETURN.
    ENDIF.

    process_trips( ).

    IF mv_test_mode = abap_false AND gv_commit_ok = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

    show_summary_message( ).
  ENDMETHOD.

  METHOD process_trips.
    DATA: lw_input TYPE lty_trip_input.

    LOOP AT gt_trip_input INTO lw_input.
      process_one_trip( lw_input ).
    ENDLOOP.
  ENDMETHOD.

  METHOD process_one_trip.
    DATA: lw_hdr      TYPE zsce_ctd_hdr,
          lw_map      TYPE lty_status_map,
          lv_msg      TYPE char255,
          lv_locked   TYPE abap_bool,
          lv_updated  TYPE abap_bool.

    CLEAR: lv_locked, lv_updated.

    SELECT SINGLE lifnr truck_no trip_no trip_status
                  created_date del_ind
      FROM zsce_ctd_hdr CLIENT SPECIFIED
      INTO CORRESPONDING FIELDS OF lw_hdr
      WHERE mandt    = sy-mandt
        AND lifnr    = is_input-lifnr
        AND truck_no = is_input-truck_no
        AND trip_no  = is_input-trip_no.
    IF sy-subrc <> 0.
      MESSAGE e007 WITH is_input-trip_no is_input-truck_no is_input-lifnr INTO lv_msg.
      append_result( is_input = is_input
                     iv_created = space
                     iv_stat_before = space
                     iv_stat_after = space
                     iv_result = lc_res_error
                     iv_message = lv_msg ).
      mv_error = mv_error + 1.
      RETURN.
    ENDIF.

    IF lw_hdr-del_ind = lc_del_ind.
      MESSAGE e008 WITH is_input-trip_no is_input-truck_no is_input-lifnr INTO lv_msg.
      append_result( is_input = is_input
                     iv_created = lw_hdr-created_date
                     iv_stat_before = lw_hdr-trip_status
                     iv_stat_after = space
                     iv_result = lc_res_error
                     iv_message = lv_msg ).
      mv_error = mv_error + 1.
      RETURN.
    ENDIF.

    IF lw_hdr-created_date >= mv_cutoff.
      MESSAGE w009 WITH is_input-trip_no is_input-truck_no is_input-lifnr mv_cutoff INTO lv_msg.
      append_result( is_input = is_input
                     iv_created = lw_hdr-created_date
                     iv_stat_before = lw_hdr-trip_status
                     iv_stat_after = space
                     iv_result = lc_res_warning
                     iv_message = lv_msg ).
      mv_warning = mv_warning + 1.
      RETURN.
    ENDIF.

    READ TABLE gt_status_map INTO lw_map
      WITH KEY old_status = lw_hdr-trip_status BINARY SEARCH.
    IF sy-subrc <> 0.
      MESSAGE w010 WITH is_input-trip_no lw_hdr-trip_status INTO lv_msg.
      append_result( is_input = is_input
                     iv_created = lw_hdr-created_date
                     iv_stat_before = lw_hdr-trip_status
                     iv_stat_after = space
                     iv_result = lc_res_warning
                     iv_message = lv_msg ).
      mv_warning = mv_warning + 1.
      RETURN.
    ENDIF.

    IF lw_hdr-trip_status = lw_map-new_status.
      MESSAGE w014 WITH is_input-trip_no lw_map-new_status INTO lv_msg.
      append_result( is_input = is_input
                     iv_created = lw_hdr-created_date
                     iv_stat_before = lw_hdr-trip_status
                     iv_stat_after = lw_map-new_status
                     iv_result = lc_res_warning
                     iv_message = lv_msg ).
      mv_warning = mv_warning + 1.
      RETURN.
    ENDIF.

    IF mv_test_mode = abap_true.
      MESSAGE i012 WITH is_input-trip_no lw_hdr-trip_status lw_map-new_status INTO lv_msg.
      append_result( is_input = is_input
                     iv_created = lw_hdr-created_date
                     iv_stat_before = lw_hdr-trip_status
                     iv_stat_after = lw_map-new_status
                     iv_result = lc_res_test
                     iv_message = lv_msg ).
      mv_test_cnt = mv_test_cnt + 1.
      RETURN.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_EZSCE_CTD_HDR'
      EXPORTING
        mode_zsce_ctd_hdr = 'E'
        mandt             = sy-mandt
        lifnr             = is_input-lifnr
        truck_no          = is_input-truck_no
        trip_no           = is_input-trip_no
      EXCEPTIONS
        foreign_lock      = 1
        system_failure    = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      MESSAGE e013 WITH is_input-trip_no is_input-truck_no is_input-lifnr INTO lv_msg.
      append_result( is_input = is_input
                     iv_created = lw_hdr-created_date
                     iv_stat_before = lw_hdr-trip_status
                     iv_stat_after = space
                     iv_result = lc_res_error
                     iv_message = lv_msg ).
      mv_error = mv_error + 1.
      RETURN.
    ENDIF.
    lv_locked = abap_true.

    UPDATE zsce_ctd_hdr CLIENT SPECIFIED
      SET trip_status   = lw_map-new_status
          modified_by   = sy-uname
          modified_date = sy-datum
          modified_time = sy-uzeit
      WHERE mandt    = sy-mandt
        AND lifnr    = is_input-lifnr
        AND truck_no = is_input-truck_no
        AND trip_no  = is_input-trip_no.

    IF sy-subrc = 0.
      lv_updated = abap_true.
      gv_commit_ok = abap_true.
      MESSAGE s011 WITH is_input-trip_no lw_hdr-trip_status lw_map-new_status INTO lv_msg.
      append_result( is_input = is_input
                     iv_created = lw_hdr-created_date
                     iv_stat_before = lw_hdr-trip_status
                     iv_stat_after = lw_map-new_status
                     iv_result = lc_res_success
                     iv_message = lv_msg ).
      mv_success = mv_success + 1.
    ELSE.
      MESSAGE e015 WITH is_input-trip_no is_input-truck_no is_input-lifnr INTO lv_msg.
      append_result( is_input = is_input
                     iv_created = lw_hdr-created_date
                     iv_stat_before = lw_hdr-trip_status
                     iv_stat_after = space
                     iv_result = lc_res_error
                     iv_message = lv_msg ).
      mv_error = mv_error + 1.
    ENDIF.

    IF lv_locked = abap_true.
      CALL FUNCTION 'DEQUEUE_EZSCE_CTD_HDR'
        EXPORTING
          mode_zsce_ctd_hdr = 'E'
          lifnr             = is_input-lifnr
          truck_no          = is_input-truck_no
          trip_no           = is_input-trip_no
        EXCEPTIONS
          OTHERS = 0.
    ENDIF.
  ENDMETHOD.

  METHOD append_result.
    DATA: lw_res TYPE lty_mig_result.
    CLEAR lw_res.
    lw_res-lifnr         = is_input-lifnr.
    lw_res-truck_no      = is_input-truck_no.
    lw_res-trip_no       = is_input-trip_no.
    lw_res-created_date  = iv_created.
    lw_res-status_before = iv_stat_before.
    lw_res-status_after  = iv_stat_after.
    lw_res-result        = iv_result.
    lw_res-message       = iv_message.
    APPEND lw_res TO gt_result.
  ENDMETHOD.

  METHOD show_summary_message.
    MESSAGE s016 WITH mv_success mv_warning mv_error mv_test_cnt.
  ENDMETHOD.

  METHOD display_alv.
    DATA: lo_salv      TYPE REF TO cl_salv_table,
          lo_functions TYPE REF TO cl_salv_functions,
          lo_display   TYPE REF TO cl_salv_display_settings,
          lo_columns   TYPE REF TO cl_salv_columns_table,
          lo_column    TYPE REF TO cl_salv_column_table,
          lo_exc       TYPE REF TO cx_root,
          lv_col       TYPE lvc_fname.

    IF gt_result IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = lo_salv
          CHANGING  t_table      = gt_result ).

        lo_functions = lo_salv->get_functions( ).
        lo_functions->set_all( abap_true ).

        lo_display = lo_salv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'CTD Trip Status Cutover Migration Results' ).

        lo_columns = lo_salv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        lv_col = 'RESULT'.
        lo_column ?= lo_columns->get_column( lv_col ).
        lo_column->set_short_text( 'Res' ).
        lo_column->set_medium_text( 'Result' ).
        lo_column->set_long_text( 'Result' ).

        lo_salv->display( ).
      CATCH cx_root INTO lo_exc.
        MESSAGE lo_exc TYPE 'I'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

" END: Cursor Generated Code
```

---

## 10. Processing Logic Summary

| Step | Action |
|------|--------|
| 1 | `authorization_check` *(optional object)* |
| 2 | `load_configuration` — mapping + cut-off |
| 3 | `read_input_trips` — file or single key |
| 4 | Loop `process_one_trip` — validate, enqueue, update, dequeue |
| 5 | `COMMIT WORK AND WAIT` if any update and not test mode |
| 6 | `display_alv` — result log |

---

## 11. Explicit Exclusions (Do Not Implement)

| Excluded | Reason |
|----------|--------|
| `SUBMIT zctd_ruleeng_exec` | Wrong lifecycle for legacy complete trips |
| `CALL FUNCTION z_scm_ctd_tripconfirm` | Full business validation not applicable |
| `UPDATE zsce_ctd_itm` | Out of scope per FS |
| `RTCA_FLAG` / `RCCA_FLAG` updates | False audit trail |

---

## 12. Transport and Go-Live Checklist

| Step | Activity |
|------|----------|
| 1 | Create message class `ZCTDDM_MIG` (SE91) |
| 2 | Create program + includes in SE38 |
| 3 | Create T-code `ZCTDDM_TRIP_MIG` *(optional)* |
| 4 | Create auth object `ZCTD_CUTOVER` and role *(optional)* |
| 5 | Transport to QA |
| 6 | Maintain `ZLOG_EXEC_VAR` in QA (mapping + cut-off) |
| 7 | Execute test mode with sample file |
| 8 | Functional sign-off on ALV |
| 9 | Production: maintain config → test → update run |
| 10 | Deactivate `ZLOG_EXEC_VAR` rows after cutover |

---

## 13. Unit / Integration Test Checklist

| TC | Scenario | Expected |
|----|----------|----------|
| 1 | Test mode, valid old `07` trip | ALV `T`, DB unchanged |
| 2 | Update mode, valid old `07` | Status `11`, MODIFIED_* set |
| 3 | Trip on cut-off date | Warning, no update |
| 4 | Status `02` not in mapping | Warning, no update |
| 5 | Invalid file line | Error, program stops read |
| 6 | Missing ZLOG config | Error message 001/002 |
| 7 | Duplicate mapping row for same old status | Error 003 |
| 8 | Single-trip parameters (no file) | One row processed |
| 9 | `P_OVRD` + `P_CUTOFF` | Uses override date |
| 10 | Lock failure | Error 013, no update |

---

## 14. Follow-Up (Separate Transport — Not This CD)

| Item | Action |
|------|--------|
| `Z_SCE_TRIP_STLMNT` | If migrated `11` must behave like legacy `07` on next settlement, add `11` to status OR block — functional decision |
| CR7 in-flight trips | `09`→`10`, `10`→`11` patch per Auto Trip Confirmation CD if applicable |

---

## 15. ABAP Team Implementation Notes

1. **`GUI_UPLOAD`:** Requires frontend; for background job use `OPEN DATASET` / `TEXT_MODE` alternative.
2. **`cl_abap_char_utilities=>horizontal_tab`:** Available in 7.31; if unavailable on target, use fixed hex tab constant.
3. **Alpha conversion:** `CONVERSION_EXIT_ALPHA_INPUT` on `LIFNR` for file upload; screen parameters may already be converted.
4. **Activate** all includes before syntax check of main program.
5. Attach **message class** `ZCTDDM_MIG` to report attributes.

---

**End of Code Change Document**
