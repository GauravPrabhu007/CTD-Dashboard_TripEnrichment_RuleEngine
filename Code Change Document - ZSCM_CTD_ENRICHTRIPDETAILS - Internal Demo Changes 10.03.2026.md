# Code Change Document – ZSCM_CTD_ENRICHTRIPDETAILS
## Internal Demo Observations – Technical Implementation Guide

**Function Module:** Z_SCM_CTD_ENRICHTRIPDETAILS  
**Program:** ZSCM_CTD_ENRICHTRIPDETAILS  
**Change Type:** Enhancement + Bug Fix  
**Date:** 10.03.2026  
**Reference:** CD: TBD | TR: TBD  
**Technical Author:** Omkar More  
**Functional Author:** Gaurav Prabhu  

---

## 1. Pre-requisite: DDIC Changes (SE11)

### 1.1 Structure ZSCM_ENRICH_DET_ST — Add Two Fields

| Field Name | Data Type | Length | Position | Purpose |
|---|---|---|---|---|
| `STATUS_DESC` | CHAR | 30 | After `TRIP_STATUS` | Status description text for ALV display |
| `ROWCOLOR` | CHAR | 4 | End of structure | ALV row colour code (hidden from display) |

> These fields must be activated in SE11 before any code changes are transported.

### 1.2 LIPS SELECT — Extend to Include WERKS

In FM Step 5, the existing SELECT from `LIPS` currently fetches `vbeln posnr matnr`.  
**Change:** Add `werks` to this SELECT:

```abap
" BEFORE:
SELECT vbeln posnr matnr
  FROM lips
  INTO TABLE lt_lips
  ...

" AFTER:
SELECT vbeln posnr matnr werks
  FROM lips
  INTO TABLE lt_lips
  ...
```

Also update the corresponding type `ty_lips` to include:
```abap
TYPES: BEGIN OF ty_lips,
         vbeln TYPE vbeln_vl,
         posnr TYPE posnr_vl,
         matnr TYPE matnr,
         werks TYPE werks_d,    " <-- ADD THIS
       END OF ty_lips.
```

---

## 2. FM Changes: Z_SCM_CTD_ENRICHTRIPDETAILS

---

### 2.1 Change Point 2 — New Optional Importing Parameters (Transporter Code & Vehicle Number)

**Location:** FM Interface + Step 2 SELECT from ZSCE_CTD_HDR  

#### 2.1.1 FM Interface — Add Parameters

```abap
*"  IMPORTING
*"     VALUE(I_FROM_DATE)  TYPE DATS
*"     VALUE(I_TO_DATE)    TYPE DATS
*"     VALUE(I_TEST_RUN)   TYPE FLAG   OPTIONAL
*"     VALUE(IT_TRIP_NUMBERS) TYPE ZSCM_TRIPNO_TT OPTIONAL
*"     VALUE(I_LIFNR)      TYPE LIFNR       OPTIONAL    " <-- ADD
*"     VALUE(I_TRUCK_NO)   TYPE YTRUCK_NO   OPTIONAL    " <-- ADD
```

#### 2.1.2 Step 2 SELECT — Apply Filters at DB Level

```abap
" BEFORE:
  SELECT lifnr truck_no trip_no trip_status area created_date
    FROM zsce_ctd_hdr CLIENT SPECIFIED
    INTO TABLE lt_trip_headers
    WHERE mandt        = sy-mandt
    AND   trip_status  = lc_status_completed.

" AFTER:
  SELECT lifnr truck_no trip_no trip_status area created_date
    FROM zsce_ctd_hdr CLIENT SPECIFIED
    INTO TABLE lt_trip_headers
    WHERE mandt        = sy-mandt
    AND   trip_status  = lc_status_completed
    AND ( lifnr    = i_lifnr    OR i_lifnr    IS INITIAL )
    AND ( truck_no = i_truck_no OR i_truck_no IS INITIAL ).
```

---

### 2.2 Change Point 9 — Bug Fix: Status 03 Trips DB Update Behaviour

**Location:** Merged Loop (Step 8+9+10) ~lines 1326–1373 + HDR Update Loop (Step 10) ~lines 1434–1467

#### 2.2.1 Merged Loop — Guard ITM Record Building for Action = 'S' Only

```abap
" BEFORE (lines ~1357–1371):
    IF i_test_run IS INITIAL.
      CLEAR lw_ctd_itm_update.
      MOVE-CORRESPONDING <lfs_enrichment> TO lw_ctd_itm_update.
      ...
      IF <lfs_enrichment>-is_new_leg = abap_true.
        APPEND lw_ctd_itm_update TO lt_ctd_itm_insert.
      ELSE.
        APPEND lw_ctd_itm_update TO lt_ctd_itm_update.
      ENDIF.
    ENDIF.

" AFTER — wrap entire ITM block with action check:
    IF i_test_run IS INITIAL.
      READ TABLE lt_trip_action INTO ls_trip_action
           WITH KEY trip_no = <lfs_enrichment>-trip_no BINARY SEARCH.
      IF sy-subrc = 0 AND ls_trip_action-action = 'S'.  " <-- GUARD: success trips only
        CLEAR lw_ctd_itm_update.
        MOVE-CORRESPONDING <lfs_enrichment> TO lw_ctd_itm_update.
        lw_ctd_itm_update-mandt        = sy-mandt.
        lw_ctd_itm_update-created_by   = sy-uname.
        lw_ctd_itm_update-created_time = sy-uzeit.
        lw_ctd_itm_update-created_date = sy-datum.
        IF <lfs_enrichment>-is_new_leg = abap_true.
          APPEND lw_ctd_itm_update TO lt_ctd_itm_insert.
        ELSE.
          APPEND lw_ctd_itm_update TO lt_ctd_itm_update.
        ENDIF.
      ENDIF.  " <-- END GUARD
    ENDIF.
```

#### 2.2.2 New: Remarks Consolidation per Trip (Add before HDR update loop)

Add new data declarations (in the existing data declarations block):
```abap
  DATA: lv_remarks_string  TYPE string,   " Unlimited length for safe concat
        lv_hdr_remarks     TYPE zctd_ruleeng_remarks.  " CHAR 255 target
```

Add consolidation logic just before the HDR update loop:
```abap
" --- Consolidate remarks per trip for HDR update ---
" Build an internal table of trip_no + consolidated_remarks
  TYPES: BEGIN OF ty_trip_remarks,
           trip_no  TYPE ztrip_no,
           remarks  TYPE zctd_ruleeng_remarks,
         END OF ty_trip_remarks.
  DATA: lt_trip_remarks TYPE TABLE OF ty_trip_remarks,
        lw_trip_remarks TYPE ty_trip_remarks.

  SORT lt_enrichments BY trip_no.
  LOOP AT lt_enrichments INTO lw_enrichment.
    AT NEW trip_no.
      CLEAR: lw_trip_remarks, lv_remarks_string.
      lw_trip_remarks-trip_no = lw_enrichment-trip_no.
    ENDAT.
    IF lw_enrichment-remarks IS NOT INITIAL.
      IF lv_remarks_string IS INITIAL.
        lv_remarks_string = lw_enrichment-remarks.
      ELSE.
        " Avoid duplicate remarks
        IF lv_remarks_string NS lw_enrichment-remarks.
          CONCATENATE lv_remarks_string '; ' lw_enrichment-remarks
            INTO lv_remarks_string.
        ENDIF.
      ENDIF.
    ENDIF.
    AT END OF trip_no.
      " Safe truncation to CHAR 255
      IF strlen( lv_remarks_string ) > 255.
        lv_hdr_remarks = lv_remarks_string+0(252).
        CONCATENATE lv_hdr_remarks '...' INTO lv_hdr_remarks.
      ELSE.
        lv_hdr_remarks = lv_remarks_string.
      ENDIF.
      lw_trip_remarks-remarks = lv_hdr_remarks.
      APPEND lw_trip_remarks TO lt_trip_remarks.
    ENDAT.
  ENDLOOP.
  SORT lt_trip_remarks BY trip_no.
```

#### 2.2.3 HDR Update Loop — Split into Two Conditional UPDATE Statements

```abap
" BEFORE (single UPDATE for all trips):
    LOOP AT lt_trip_headers INTO lw_trip_header.
      ...
      UPDATE zsce_ctd_hdr
        SET trip_status         = lw_ctd_hdr_update-trip_status
            ctd_ruleeng_remarks = lw_ctd_hdr_update-ctd_ruleeng_remarks
            modified_by         = sy-uname
            modified_date       = sy-datum
            modified_time       = sy-uzeit
        WHERE ...
    ENDLOOP.

" AFTER — two conditional branches:
    LOOP AT lt_trip_headers INTO lw_trip_header.
      " Get consolidated remarks for this trip
      CLEAR lw_trip_remarks.
      READ TABLE lt_trip_remarks INTO lw_trip_remarks
        WITH KEY trip_no = lw_trip_header-trip_no BINARY SEARCH.

      " Determine trip action
      CLEAR ls_trip_action.
      READ TABLE lt_trip_action INTO ls_trip_action
        WITH KEY trip_no = lw_trip_header-trip_no BINARY SEARCH.

      IF sy-subrc = 0 AND ls_trip_action-action = 'S'.
        " STATUS 04 TRIPS: Full update — status, remarks, audit
        UPDATE zsce_ctd_hdr
          SET trip_status         = lc_status_enriched
              ctd_ruleeng_remarks = lw_trip_remarks-remarks
              modified_by         = sy-uname
              modified_date       = sy-datum
              modified_time       = sy-uzeit
          WHERE lifnr    = lw_trip_header-lifnr
            AND truck_no = lw_trip_header-truck_no
            AND trip_no  = lw_trip_header-trip_no.
      ELSE.
        " STATUS 03 TRIPS: Remarks + audit ONLY — trip_status left unchanged at 03
        UPDATE zsce_ctd_hdr
          SET ctd_ruleeng_remarks = lw_trip_remarks-remarks
              modified_by         = sy-uname
              modified_date       = sy-datum
              modified_time       = sy-uzeit
          WHERE lifnr    = lw_trip_header-lifnr
            AND truck_no = lw_trip_header-truck_no
            AND trip_no  = lw_trip_header-trip_no.
      ENDIF.

      IF sy-subrc <> 0.
        CONCATENATE 'Error updating ZSCE_CTD_HDR for trip'(020)
                    lw_trip_header-trip_no
          INTO lv_message SEPARATED BY space.
        lw_return-type    = lc_msgty_error.
        lw_return-id      = 'ZCTD'.
        lw_return-number  = '012'.
        lw_return-message = lv_message.
        APPEND lw_return TO et_return.
        CLEAR lw_return.
      ENDIF.
    ENDLOOP.
```

---

### 2.3 Change Point 10 — Bug Fix: Blank Source Date Fallback for Empty Leg

**Location:** FM Step 7 — ELSE branch — all three sub-cases where empty leg dest_date is derived from ZSCE_CTD_HDR  
**Scope:** Only the three sub-cases in the ELSE block (last-leg scenarios). Sandwiched empty legs are NOT affected.

Add a data variable for the current trip's created_date lookup:
```abap
  DATA: lv_fallback_date TYPE sydatum.
```

**Pattern to apply in ALL THREE sub-cases** (single-leg, multi-leg same-trip, last-leg-of-multi-leg):  
Before the binary search block, add the fallback check:

```abap
" --- Point 10 Fix: Use created_date as fallback when source_date is blank ---
  CLEAR lv_fallback_date.
  IF lw_enrichment_new-source_date IS INITIAL.
    " Fallback: use current trip's created_date from ZSCE_CTD_HDR
    READ TABLE lt_zsce_ctd_hdr INTO lw_zsce_ctd_hdr
      WITH KEY lifnr    = lw_enrichment_new-lifnr
               truck_no = lw_enrichment_new-truck_no
               trip_no  = lw_enrichment_new-trip_no.
    IF sy-subrc = 0.
      lv_fallback_date = lw_zsce_ctd_hdr-created_date.
    ENDIF.
  ENDIF.

" Then in the binary search, replace the key field:
  CLEAR: lw_zsce_ctd_hdr, lw_enrichment_new-dest_date.
  READ TABLE lt_zsce_ctd_hdr INTO lw_zsce_ctd_hdr
    WITH KEY lifnr        = lw_enrichment_new-lifnr
             truck_no     = lw_enrichment_new-truck_no
             created_date = COND #( WHEN lw_enrichment_new-source_date IS NOT INITIAL
                                    THEN lw_enrichment_new-source_date
                                    ELSE lv_fallback_date )
             trip_no      = space
    BINARY SEARCH.
" ... remainder of binary search logic unchanged ...
```

> Apply this same pattern in all three occurrences of the dest_date binary search within the ELSE branch (~lines 1051–1081, ~1132–1162, ~1205–1235).

---

### 2.4 Change Point 11 — Region Fetching: Replace ADRC Logic with Delivery-Based Logic

**Location:** FM Step 5 (data fetching ~lines 548–623) + Step 6 enrichment loop (~lines 769–806)

#### 2.4.1 New Type Declarations (add to TYPES block)

```abap
  TYPES: BEGIN OF ty_likp,
           vbeln TYPE vbeln_vl,
           kunnr TYPE kunnr,
           lifnr TYPE lifnr,
         END OF ty_likp.

  TYPES: BEGIN OF ty_t001w,
           werks TYPE werks_d,
           regio TYPE regio,
         END OF ty_t001w.

  TYPES: BEGIN OF ty_lfa1,
           lifnr TYPE lifnr,
           regio TYPE regio,
         END OF ty_lfa1.

  TYPES: BEGIN OF ty_kna1,
           kunnr TYPE kunnr,
           regio TYPE regio,
         END OF ty_kna1.
```

#### 2.4.2 New Data Declarations (add to DATA block)

```abap
  DATA: lt_likp   TYPE TABLE OF ty_likp,
        lw_likp   TYPE ty_likp,
        lt_t001w  TYPE TABLE OF ty_t001w,
        lw_t001w  TYPE ty_t001w,
        lt_lfa1   TYPE TABLE OF ty_lfa1,
        lw_lfa1   TYPE ty_lfa1,
        lt_kna1   TYPE TABLE OF ty_kna1,
        lw_kna1   TYPE ty_kna1,
        lt_werks  TYPE TABLE OF werks_d,
        lt_kunnr  TYPE TABLE OF kunnr,
        lt_lifnr_vendor TYPE TABLE OF lifnr.
```

#### 2.4.3 Step 5 — Replace ADRC SELECT with New SELECTs

**Remove:** The existing `SELECT addrnumber date_from nation region FROM adrc` block and the `lt_addrnumbers` population logic.

**Add** the following new SELECTs after the existing LIPS SELECT (within the `IF lt_shnumbers IS NOT INITIAL` block):

```abap
" --- Step 5: Fetch LIKP (delivery header) for region determination ---
  IF lt_doc_numbers IS NOT INITIAL.
    SELECT vbeln kunnr lifnr
      FROM likp
      INTO TABLE lt_likp
      FOR ALL ENTRIES IN lt_doc_numbers
      WHERE vbeln = lt_doc_numbers-table_line.
    IF sy-subrc = 0.
      SORT lt_likp BY vbeln.
    ENDIF.
  ENDIF.

" --- Collect WERKS for T001W lookup (from LIPS) ---
  LOOP AT lt_lips INTO lw_lips.
    IF lw_lips-werks IS NOT INITIAL.
      APPEND lw_lips-werks TO lt_werks.
    ENDIF.
  ENDLOOP.
  SORT lt_werks.
  DELETE ADJACENT DUPLICATES FROM lt_werks.

  IF lt_werks IS NOT INITIAL.
    SELECT werks regio
      FROM t001w
      INTO TABLE lt_t001w
      FOR ALL ENTRIES IN lt_werks
      WHERE werks = lt_werks-table_line.
    IF sy-subrc = 0.
      SORT lt_t001w BY werks.
    ENDIF.
  ENDIF.

" --- Collect LIFNR (vendor) for LFA1 lookup (from LIKP for PO) ---
  LOOP AT lt_likp INTO lw_likp.
    IF lw_likp-lifnr IS NOT INITIAL.
      APPEND lw_likp-lifnr TO lt_lifnr_vendor.
    ENDIF.
  ENDLOOP.
  SORT lt_lifnr_vendor.
  DELETE ADJACENT DUPLICATES FROM lt_lifnr_vendor.

  IF lt_lifnr_vendor IS NOT INITIAL.
    SELECT lifnr regio
      FROM lfa1
      INTO TABLE lt_lfa1
      FOR ALL ENTRIES IN lt_lifnr_vendor
      WHERE lifnr = lt_lifnr_vendor-table_line.
    IF sy-subrc = 0.
      SORT lt_lfa1 BY lifnr.
    ENDIF.
  ENDIF.

" --- Collect KUNNR (ship-to) for KNA1 lookup (from LIKP for SO/STO) ---
  LOOP AT lt_likp INTO lw_likp.
    IF lw_likp-kunnr IS NOT INITIAL.
      APPEND lw_likp-kunnr TO lt_kunnr.
    ENDIF.
  ENDLOOP.
  SORT lt_kunnr.
  DELETE ADJACENT DUPLICATES FROM lt_kunnr.

  IF lt_kunnr IS NOT INITIAL.
    SELECT kunnr regio
      FROM kna1
      INTO TABLE lt_kna1
      FOR ALL ENTRIES IN lt_kunnr
      WHERE kunnr = lt_kunnr-table_line.
    IF sy-subrc = 0.
      SORT lt_kna1 BY kunnr.
    ENDIF.
  ENDIF.
```

#### 2.4.4 Step 6 — Replace ADRC-Based Region Assignment

**Remove:** The entire `IF lt_addrnumbers IS NOT INITIAL ... ENDIF` block (~lines 755–807) including the ADRC SELECT and the region assignment loop.

**Replace with** the following region assignment logic (inside the enrichment build loop `LOOP AT lt_trip_items INTO lw_trip_item`), after the OIGSI/LIPS material fetch:

```abap
" --- New Region Assignment Logic (Point 11) ---
    IF lw_trip_item-shnumber IS NOT INITIAL.
      " Get delivery number from OIGSI
      READ TABLE lt_oigsi INTO lw_oigsi
        WITH KEY shnumber = lw_trip_item-shnumber
        BINARY SEARCH.
      IF sy-subrc = 0.
        " Get delivery item data (LIPS) for WERKS
        READ TABLE lt_lips INTO lw_lips
          WITH KEY vbeln = lw_oigsi-doc_number
          BINARY SEARCH.
        " Get delivery header data (LIKP) for KUNNR and LIFNR
        READ TABLE lt_likp INTO lw_likp
          WITH KEY vbeln = lw_oigsi-doc_number
          BINARY SEARCH.

        CASE lw_trip_item-mvt_type.
          WHEN lc_mvt_so OR lc_mvt_sto.
            " Source Region: LIPS(WERKS) -> T001W -> REGIO
            IF lw_lips-werks IS NOT INITIAL.
              READ TABLE lt_t001w INTO lw_t001w
                WITH KEY werks = lw_lips-werks
                BINARY SEARCH.
              IF sy-subrc = 0.
                lw_enrichment-source_region = lw_t001w-regio.
              ENDIF.
            ENDIF.
            " Destination Region: LIKP(KUNNR) -> KNA1 -> REGIO
            IF lw_likp-kunnr IS NOT INITIAL.
              READ TABLE lt_kna1 INTO lw_kna1
                WITH KEY kunnr = lw_likp-kunnr
                BINARY SEARCH.
              IF sy-subrc = 0.
                lw_enrichment-dest_region = lw_kna1-regio.
              ENDIF.
            ENDIF.

          WHEN lc_mvt_po.
            " Source Region: LIKP(LIFNR) -> LFA1 -> REGIO
            IF lw_likp-lifnr IS NOT INITIAL.
              READ TABLE lt_lfa1 INTO lw_lfa1
                WITH KEY lifnr = lw_likp-lifnr
                BINARY SEARCH.
              IF sy-subrc = 0.
                lw_enrichment-source_region = lw_lfa1-regio.
              ENDIF.
            ENDIF.
            " Destination Region: LIPS(WERKS) -> T001W -> REGIO
            IF lw_lips-werks IS NOT INITIAL.
              READ TABLE lt_t001w INTO lw_t001w
                WITH KEY werks = lw_lips-werks
                BINARY SEARCH.
              IF sy-subrc = 0.
                lw_enrichment-dest_region = lw_t001w-regio.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDIF.

      " Remarks if source or destination region is missing
      IF lw_enrichment-dest_region IS INITIAL AND lw_enrichment-source_region IS NOT INITIAL.
        lw_enrichment-remarks = |{ 'Destination region'(041) } { 'is missing'(043) }|.
      ELSEIF lw_enrichment-source_region IS INITIAL AND lw_enrichment-dest_region IS NOT INITIAL.
        lw_enrichment-remarks = |{ 'Source region'(040) } { 'is missing'(043) }|.
      ELSEIF lw_enrichment-dest_region IS INITIAL AND lw_enrichment-source_region IS INITIAL.
        lw_enrichment-remarks = |{ 'Source region'(040) } { 'Destination region'(041) } { 'is missing'(043) }|.
      ENDIF.
    ENDIF.
```

#### 2.4.5 Remove: ADRC-Related Declarations

Remove from DATA declarations:
```abap
" REMOVE these:
  lt_adrc        TYPE TABLE OF ty_adrc,
  lw_adrc        TYPE ty_adrc,
  lt_addrnumbers TYPE TABLE OF ad_addrnum,
```

Remove type `ty_adrc` from TYPES block.

---

### 2.5 Change Point 12 — Set ROWCOLOR in FM Output Loop

**Location:** Merged Loop (Step 8+9+10) — after `MOVE-CORRESPONDING <lfs_enrichment> TO lw_enrichment_op`

```abap
" --- Point 12: Set ALV row colour based on trip status ---
    CASE lw_enrichment_op-trip_status.
      WHEN lc_status_enriched.    " '04' - Success - Light Green
        lw_enrichment_op-rowcolor = 'C510'.
      WHEN lc_status_completed.   " '03' - Error   - Light Red
        lw_enrichment_op-rowcolor = 'C610'.
    ENDCASE.
```

Also populate `STATUS_DESC` in the same loop:
```abap
" --- Point 6: Populate Status Description ---
    CLEAR lw_enrichment_op-status_desc.
    READ TABLE lt_dd07t INTO lw_dd07t                    " lt_dd07t fetched once before loop
      WITH KEY domvalue_l = lw_enrichment_op-trip_status
      BINARY SEARCH.
    IF sy-subrc = 0.
      lw_enrichment_op-status_desc = lw_dd07t-ddtext.
    ENDIF.
```

> `lt_dd07t` must be fetched **once before the loop** using:
```abap
  SELECT domvalue_l ddtext
    FROM dd07t
    INTO TABLE lt_dd07t
    WHERE domname    = 'ZCTD_TRIP_ST'
      AND ddlanguage = sy-langu.
  IF sy-subrc = 0.
    SORT lt_dd07t BY domvalue_l.
  ENDIF.
```

---

## 3. Program Changes: ZSCM_CTD_ENRICHTRIPDETAILS

> The program uses class-based OO approach with includes (TOP, SEL, C01). Changes below reference the relevant include/method.

---

### 3.1 Change Point 1 — Authorization Check

**Location:** `AT SELECTION-SCREEN` event (include SEL or main program)

```abap
AT SELECTION-SCREEN.
  " Stage 1: Basic program access check
  AUTHORITY-CHECK OBJECT 'ZCTD_ENRICH'
                  ID 'ACTVT' FIELD '03'.
  IF sy-subrc <> 0.
    MESSAGE e001(zctd)
      WITH 'Not authorized to access CTD Trip Enrichment Program'.
  ENDIF.

  " Stage 2: Full Run requires execute authorization
  IF p_test IS INITIAL.   " p_test = Test Run checkbox parameter
    AUTHORITY-CHECK OBJECT 'ZCTD_ENRICH'
                    ID 'ACTVT' FIELD '16'.
    IF sy-subrc <> 0.
      MESSAGE e001(zctd)
        WITH 'Not authorized for Full Run - please use Test Run mode'.
    ENDIF.
  ENDIF.

  " Existing date validation
  go_report->validate_date_range( ).
```

---

### 3.2 Change Point 2 — Selection Screen Parameters

**Location:** Include SEL or TOP (global declarations)

```abap
" Add to selection screen (after existing parameters):
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-020.
  PARAMETERS: p_lifnr  TYPE lifnr    OPTIONAL,   " Transporter Code
              p_truck  TYPE ytruck_no OPTIONAL.   " Vehicle Number
SELECTION-SCREEN END OF BLOCK b2.
```

Pass to FM in the `execute( )` method:
```abap
  CALL FUNCTION 'Z_SCM_CTD_ENRICHTRIPDETAILS'
    EXPORTING
      i_from_date    = p_from
      i_to_date      = p_to
      i_test_run     = p_test
      i_lifnr        = p_lifnr    " <-- ADD
      i_truck_no     = p_truck    " <-- ADD
      it_trip_numbers = gt_trips
    IMPORTING
      et_enrichdet   = gt_enrichdet
      et_return      = gt_return.
```

---

### 3.3 Change Points 3, 4, 5, 6, 7, 8, R1–R6 — ALV Display Method

**Location:** `display_results( )` method in class (include C01)

#### 3.3.1 Dynamic Page Header (Point 8)

```abap
  " Point 8: Dynamic grid title
  IF p_test IS NOT INITIAL.
    gs_layout-grid_title = 'Simulation Mode - CTD Trip Details Enrichment Program'(xxx).
  ELSE.
    gs_layout-grid_title = 'CTD Trip Details Enrichment Program'(xxx).
  ENDIF.
```

#### 3.3.2 ALV Layout Settings (Points 3, R1, R6, 12)

```abap
  " Point 12: Row colour field
  gs_layout-info_fname   = 'ROWCOLOR'.

  " Point 3: Enable grouping (set sort with group trigger)
  gs_layout-no_rowmark   = abap_false.

  " R1: Freeze first 4 columns (LIFNR, TRUCK_NO, TRIP_NO, COUNTER)
  gs_layout-freeze_col   = 4.
```

#### 3.3.3 Field Catalogue — All Column Changes

```abap
  " Build field catalogue (representative entries showing all changes):

  " --- LIFNR (Frozen - handled by freeze_col) ---
  CLEAR lw_fcat.
  lw_fcat-fieldname = 'LIFNR'.    lw_fcat-col_opt = abap_true.
  APPEND lw_fcat TO lt_fcat.

  " --- TRUCK_NO ---
  CLEAR lw_fcat.
  lw_fcat-fieldname = 'TRUCK_NO'.  lw_fcat-col_opt = abap_true.
  APPEND lw_fcat TO lt_fcat.

  " --- TRIP_NO (Grouping field) ---
  CLEAR lw_fcat.
  lw_fcat-fieldname = 'TRIP_NO'.   lw_fcat-col_opt = abap_true.
  lw_fcat-do_sum    = abap_false.
  APPEND lw_fcat TO lt_fcat.

  " --- COUNTER ---
  CLEAR lw_fcat.
  lw_fcat-fieldname = 'COUNTER'.   lw_fcat-col_opt = abap_true.
  APPEND lw_fcat TO lt_fcat.

  " --- SOURCE_DATE: Point 5 rename ---
  CLEAR lw_fcat.
  lw_fcat-fieldname = 'SOURCE_DATE'.
  lw_fcat-seltext_l = 'Leg Start Date'(xxx).
  lw_fcat-coltext   = 'Leg Start Date'(xxx).
  lw_fcat-col_opt   = abap_true.   " R2: auto width
  APPEND lw_fcat TO lt_fcat.

  " --- DEST_DATE: Point 5 rename ---
  CLEAR lw_fcat.
  lw_fcat-fieldname = 'DEST_DATE'.
  lw_fcat-seltext_l = 'Leg End Date'(xxx).
  lw_fcat-coltext   = 'Leg End Date'(xxx).
  lw_fcat-col_opt   = abap_true.
  APPEND lw_fcat TO lt_fcat.

  " --- LEG_TYPE: R6 cell colouring ---
  CLEAR lw_fcat.
  lw_fcat-fieldname  = 'LEG_TYPE'.
  lw_fcat-col_opt    = abap_true.
  " Cell colour set separately via LVC_T_SCOL per row — see 3.3.4

  " --- DISTANCE: R3 subtotal ---
  CLEAR lw_fcat.
  lw_fcat-fieldname = 'DISTANCE'.
  lw_fcat-do_sum    = abap_true.   " R3: subtotal per group
  lw_fcat-col_opt   = abap_true.
  APPEND lw_fcat TO lt_fcat.

  " --- TRIP_STATUS ---
  CLEAR lw_fcat.
  lw_fcat-fieldname = 'TRIP_STATUS'.
  lw_fcat-col_opt   = abap_true.
  APPEND lw_fcat TO lt_fcat.

  " --- STATUS_DESC: Point 6 new column ---
  CLEAR lw_fcat.
  lw_fcat-fieldname = 'STATUS_DESC'.
  lw_fcat-seltext_l = 'Status Description'(xxx).
  lw_fcat-coltext   = 'Status Description'(xxx).
  lw_fcat-col_opt   = abap_true.
  APPEND lw_fcat TO lt_fcat.

  " --- REMARKS: Point 5 rename + R4 tooltip ---
  CLEAR lw_fcat.
  lw_fcat-fieldname = 'REMARKS'.
  lw_fcat-seltext_l = 'CTD Trip Enrichment Remarks'(xxx).
  lw_fcat-coltext   = 'CTD Trip Enrichment Remarks'(xxx).
  lw_fcat-col_opt   = abap_true.
  lw_fcat-tooltip   = abap_true.   " R4: tooltip shows full text on hover
  APPEND lw_fcat TO lt_fcat.

  " --- IS_NEW_LEG: Point 7 hide ---
  CLEAR lw_fcat.
  lw_fcat-fieldname = 'IS_NEW_LEG'.
  lw_fcat-tech      = abap_true.   " Hidden from display; still in structure
  APPEND lw_fcat TO lt_fcat.

  " --- ROWCOLOR: Always hidden (used by INFO_FNAME) ---
  CLEAR lw_fcat.
  lw_fcat-fieldname = 'ROWCOLOR'.
  lw_fcat-tech      = abap_true.
  APPEND lw_fcat TO lt_fcat.
```

#### 3.3.4 Default Sort (Point 4)

```abap
  " Point 4: Default sort
  DATA: lt_sort TYPE lvc_t_sort,
        lw_sort TYPE lvc_s_sort.

  CLEAR lw_sort.
  lw_sort-spos = 1. lw_sort-fieldname = 'LIFNR'.   lw_sort-up = abap_true.
  lw_sort-group = 'UL'. APPEND lw_sort TO lt_sort.  " Group header per LIFNR

  CLEAR lw_sort.
  lw_sort-spos = 2. lw_sort-fieldname = 'TRUCK_NO'. lw_sort-up = abap_true.
  APPEND lw_sort TO lt_sort.

  CLEAR lw_sort.
  lw_sort-spos = 3. lw_sort-fieldname = 'TRIP_NO'.  lw_sort-up = abap_true.
  lw_sort-group = 'UL'. APPEND lw_sort TO lt_sort.  " Group header per TRIP_NO (Point 3)

  CLEAR lw_sort.
  lw_sort-spos = 4. lw_sort-fieldname = 'COUNTER'.  lw_sort-up = abap_true.
  APPEND lw_sort TO lt_sort.
```

#### 3.3.5 Layout Variants (R5)

```abap
  " R5: Allow user layout variants
  DATA: lw_variant TYPE disvariant.
  lw_variant-report   = sy-repid.
  lw_variant-username = sy-uname.
  " Pass lw_variant to ALV display call
```

---

### 3.4 Change Point 13 — Trip Count Summary + Custom Toolbar Filter Buttons

**Location:** `display_results( )` method in class (include C01)

#### 3.4.1 Compute Counts Before ALV Display

```abap
  " Point 13: Compute trip counts from ET_ENRICHDET (unique trip_no level)
  DATA: lv_cnt_all  TYPE i,
        lv_cnt_ok   TYPE i,
        lv_cnt_err  TYPE i,
        lt_trips_unique TYPE TABLE OF ztrip_no.

  SORT gt_enrichdet BY trip_no trip_status.
  DELETE ADJACENT DUPLICATES FROM gt_enrichdet COMPARING trip_no trip_status.

  LOOP AT gt_enrichdet INTO lw_enrichdet.
    COLLECT lw_enrichdet-trip_no INTO lt_trips_unique.
    IF lw_enrichdet-trip_status = '04'.
      ADD 1 TO lv_cnt_ok.
    ELSEIF lw_enrichdet-trip_status = '03'.
      ADD 1 TO lv_cnt_err.
    ENDIF.
  ENDLOOP.
  lv_cnt_all = lv_cnt_ok + lv_cnt_err.

  " Set grid title with counts
  DATA lv_title TYPE lvc_title.
  lv_title = |Total Trips Processed: { lv_cnt_all } | Successful: { lv_cnt_ok } | Errors: { lv_cnt_err }|.
  gs_layout-grid_title = lv_title.
```

#### 3.4.2 Add Custom Toolbar Buttons

Register for toolbar event and add buttons:

```abap
  " Register toolbar event
  SET HANDLER go_report->on_toolbar FOR go_alv_grid.
  SET HANDLER go_report->on_user_command FOR go_alv_grid.

  " In event ON_TOOLBAR:
  METHOD on_toolbar.
    DATA: lw_button TYPE stb_button.

    " Separator
    CLEAR lw_button. lw_button-butn_type = 3. " Separator
    APPEND lw_button TO e_object->mt_toolbar.

    " Download button
    CLEAR lw_button.
    lw_button-function  = 'DOWNLOAD'.
    lw_button-icon      = icon_export.
    lw_button-butn_type = 0.
    lw_button-text      = 'Download to Excel'.
    lw_button-quickinfo = 'Download current view to Excel'.
    APPEND lw_button TO e_object->mt_toolbar.

    " Separator
    CLEAR lw_button. lw_button-butn_type = 3.
    APPEND lw_button TO e_object->mt_toolbar.

    " All Trips button (default active — set pressed state on display)
    CLEAR lw_button.
    lw_button-function  = 'FILTER_ALL'.
    lw_button-icon      = icon_overview.
    lw_button-butn_type = 0.
    lw_button-text      = |All Trips ({ lv_cnt_all })|.
    lw_button-quickinfo = 'Show all trips'.
    lw_button-checked   = abap_true.   " Default: pressed/active
    APPEND lw_button TO e_object->mt_toolbar.

    " Successful Trips button
    CLEAR lw_button.
    lw_button-function  = 'FILTER_OK'.
    lw_button-icon      = icon_green_light.   " @5B@
    lw_button-butn_type = 0.
    lw_button-text      = |Successful ({ lv_cnt_ok })|.
    lw_button-quickinfo = 'Show successfully enriched trips only'.
    APPEND lw_button TO e_object->mt_toolbar.

    " Error Trips button
    CLEAR lw_button.
    lw_button-function  = 'FILTER_ERR'.
    lw_button-icon      = icon_red_light.     " @5C@
    lw_button-butn_type = 0.
    lw_button-text      = |Errors ({ lv_cnt_err })|.
    lw_button-quickinfo = 'Show error trips only'.
    APPEND lw_button TO e_object->mt_toolbar.
  ENDMETHOD.
```

#### 3.4.3 Handle Button Clicks — Filter + State Management

```abap
  " In event ON_USER_COMMAND:
  METHOD on_user_command.
    DATA: lt_filter  TYPE lvc_t_filt,
          lw_filter  TYPE lvc_s_filt.

    CASE e_ucomm.
      WHEN 'FILTER_ALL'.
        CLEAR lt_filter.  " Clear all filters — show everything
        go_alv_grid->set_filter_criteria( CHANGING it_filter = lt_filter ).
        " Update grid title
        gs_layout-grid_title = |All Trips ({ lv_cnt_all })|.

      WHEN 'FILTER_OK'.
        CLEAR lt_filter.
        lw_filter-fieldname = 'TRIP_STATUS'.
        lw_filter-sign      = 'I'.
        lw_filter-option    = 'EQ'.
        lw_filter-low       = '04'.
        APPEND lw_filter TO lt_filter.
        go_alv_grid->set_filter_criteria( CHANGING it_filter = lt_filter ).
        gs_layout-grid_title = |Showing: Successful Trips | Count: { lv_cnt_ok }|.

      WHEN 'FILTER_ERR'.
        CLEAR lt_filter.
        lw_filter-fieldname = 'TRIP_STATUS'.
        lw_filter-sign      = 'I'.
        lw_filter-option    = 'EQ'.
        lw_filter-low       = '03'.
        APPEND lw_filter TO lt_filter.
        go_alv_grid->set_filter_criteria( CHANGING it_filter = lt_filter ).
        gs_layout-grid_title = |Showing: Error Trips | Count: { lv_cnt_err }|.

      WHEN 'DOWNLOAD'.
        " Trigger standard ALV local file download
        go_alv_grid->execute_function( EXPORTING i_function = 'EXPORT' ).

    ENDCASE.

    " Refresh display
    go_alv_grid->refresh_table_display( ).
  ENDMETHOD.
```

---

## 4. Implementation Checklist

| # | Task | Object | Location |
|---|---|---|---|
| **DDIC** | | | |
| D1 | Add `STATUS_DESC CHAR30` to `ZSCM_ENRICH_DET_ST` | SE11 | Structure |
| D2 | Add `ROWCOLOR CHAR4` to `ZSCM_ENRICH_DET_ST` | SE11 | Structure |
| D3 | Activate structure after changes | SE11 | — |
| **Basis/Config** | | | |
| C1 | Create T-Code in SE93 → `ZSCM_CTD_ENRICHTRIPDETAILS` | SE93 | — |
| C2 | Create auth object `ZCTD_ENRICH` with `ACTVT` field in SU21 | SU21 | — |
| C3 | Create roles in PFCG (Test-only: ACTVT=03; Full: ACTVT=03+16) | PFCG | — |
| C4 | Assign roles to users in SU01 | SU01 | — |
| **FM Changes** | | | |
| F1 | Extend `ty_lips` type to include `werks` | FM | Types block |
| F2 | Extend `LIPS` SELECT to fetch `werks` | FM | Step 5 |
| F3 | Add new type declarations (ty_likp, ty_t001w, ty_lfa1, ty_kna1) | FM | Types block |
| F4 | Add new data declarations (lt_likp, lt_t001w, lt_lfa1, lt_kna1, etc.) | FM | Data block |
| F5 | Add `I_LIFNR` and `I_TRUCK_NO` to FM interface | FM | Interface |
| F6 | Add filter conditions to `ZSCE_CTD_HDR` SELECT (Point 2) | FM | Step 2 |
| F7 | Add ITM guard `IF action = 'S'` in merged loop (Point 9) | FM | Step 8+9+10 |
| F8 | Add remarks consolidation loop + truncation logic (Point 9) | FM | Before Step 10 |
| F9 | Split HDR UPDATE into two conditional branches (Point 9) | FM | Step 10 |
| F10 | Add fallback date logic for empty leg dest_date — 3 locations (Point 10) | FM | Step 7 ELSE |
| F11 | Add new SELECTs for LIKP, T001W, LFA1, KNA1 (Point 11) | FM | Step 5 |
| F12 | Replace ADRC region assignment with new delivery-based logic (Point 11) | FM | Step 6 |
| F13 | Remove ADRC SELECT + ty_adrc + lt_adrc + lt_addrnumbers (Point 11) | FM | Types + Data + Step 6 |
| F14 | Fetch DD07T for status descriptions before output loop (Point 6) | FM | Before merged loop |
| F15 | Set `ROWCOLOR` and `STATUS_DESC` in output loop (Points 12 + 6) | FM | Merged loop |
| **Program Changes** | | | |
| P1 | Add two-stage AUTHORITY-CHECK at AT SELECTION-SCREEN (Point 1) | Program | SEL include |
| P2 | Add `p_lifnr` and `p_truck` selection screen parameters (Point 2) | Program | SEL include |
| P3 | Pass `i_lifnr` and `i_truck_no` to FM call (Point 2) | Program | C01 execute method |
| P4 | Dynamic grid title based on Test Run flag (Point 8) | Program | C01 display method |
| P5 | Set `gs_layout-info_fname = 'ROWCOLOR'` (Point 12) | Program | C01 display method |
| P6 | Set `gs_layout-freeze_col = 4` (R1) | Program | C01 display method |
| P7 | Rename SOURCE_DATE, DEST_DATE, REMARKS in field catalogue (Point 5) | Program | C01 display method |
| P8 | Add STATUS_DESC column to field catalogue (Point 6) | Program | C01 display method |
| P9 | Hide IS_NEW_LEG via `tech = abap_true` (Point 7) | Program | C01 display method |
| P10 | Set `col_opt = abap_true` for all columns (R2) | Program | C01 display method |
| P11 | Set `do_sum = abap_true` for DISTANCE column (R3) | Program | C01 display method |
| P12 | Set `tooltip = abap_true` for REMARKS column (R4) | Program | C01 display method |
| P13 | Add sort table with group triggers for Point 3 + Point 4 | Program | C01 display method |
| P14 | Add layout variant support (R5) | Program | C01 display method |
| P15 | Compute trip counts (lv_cnt_all, lv_cnt_ok, lv_cnt_err) (Point 13) | Program | C01 display method |
| P16 | Register ON_TOOLBAR and ON_USER_COMMAND events (Point 13) | Program | C01 display method |
| P17 | Implement ON_TOOLBAR method with custom buttons (Point 13) | Program | C01 class |
| P18 | Implement ON_USER_COMMAND method with filter logic (Point 13) | Program | C01 class |

---

## 5. Test Checklist

| # | Test Scenario | Expected Result |
|---|---|---|
| T1 | User without role tries to access T-Code | Authorization error — cannot open T-Code |
| T2 | Test-only role user tries Full Run (uncheck Test Run) | Authorization error at selection screen |
| T3 | Full access role user — Test Run mode | ALV shows all trips; no DB updates; page header shows "Simulation Mode -..." |
| T4 | Full access role user — Full Run mode | DB updated; page header shows normal title |
| T5 | Enter Transporter Code only — run program | Only trips for that transporter shown |
| T6 | Enter Vehicle Number only — run program | Only trips for that vehicle shown |
| T7 | Status 03 trip with errors | ITM: no insert/no update; HDR: only remarks updated; status stays 03 |
| T8 | Status 04 trip (successful) | ITM: loaded legs updated, empty legs inserted; HDR: full update with status 04 |
| T9 | Trip where last loaded leg has blank DEST_EXIT_DATE | Empty leg after last loaded leg — dest_date derived using trip created_date as fallback |
| T10 | SO trip — verify Source Region = plant region (T001W), Dest Region = customer region (KNA1) | Regions populated correctly |
| T11 | PO trip — verify Source Region = vendor region (LFA1), Dest Region = plant region (T001W) | Regions populated correctly |
| T12 | ALV displays — Row colours | Status 04 rows = Light Green; Status 03 rows = Light Red |
| T13 | ALV displays — Leg Type cells | L = Blue cell; E = Yellow cell |
| T14 | ALV toolbar — All Trips (default active) | All rows shown; All Trips button pressed |
| T15 | ALV toolbar — Click Successful | Only Green (04) rows shown; Successful button pressed |
| T16 | ALV toolbar — Click Errors | Only Red (03) rows shown; Errors button pressed |
| T17 | ALV toolbar — Click All Trips after filter | All rows restored; All Trips button pressed |
| T18 | Download to Excel | Current filtered view exported with renamed column headers |
| T19 | Multiple errors on one trip | All error messages consolidated in remarks; max 255 chars with '...' if exceeded |
| T20 | Column headers | "Leg Start Date", "Leg End Date", "CTD Trip Enrichment Remarks" visible |
| T21 | Status Description column | Correct domain text shown for each trip status |
| T22 | Indicator column | IS_NEW_LEG not visible in ALV output |

---

## 6. Objects Impacted Summary

| Object | Type | Change |
|---|---|---|
| `ZSCM_ENRICH_DET_ST` | DDIC Structure | Add `STATUS_DESC`, `ROWCOLOR` fields |
| `Z_SCM_CTD_ENRICHTRIPDETAILS` | Function Module | Points 2, 6, 9, 10, 11, 12, 15 |
| `ZSCM_CTD_ENRICHTRIPDETAILS` | Report Program | Points 1, 2, 3, 4, 5, 6, 7, 8, 12, 13, R1–R6 |
| `ZSCM_CTD_ENRICHTRIPDETAILSTOP` | Include (TOP) | Global class declarations for toolbar events |
| `ZSCM_CTD_ENRICHTRIPDETAILSSEL` | Include (SEL) | Selection screen + auth check |
| `ZSCM_CTD_ENRICHTRIPDETAILSC01` | Include (C01) | ALV display + toolbar methods |
| `ZCTD_ENRICH` | Auth Object (new) | Created in SU21 |
| T-Code (new) | SE93 | Created pointing to report |

---

*End of Code Change Document*  
*Date: 10.03.2026 | Technical Author: Omkar More | Functional Author: Gaurav Prabhu*
