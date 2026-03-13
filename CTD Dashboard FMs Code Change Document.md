# Code Change Document – CTD Dashboard Function Modules

**Module:** SCM — CTD (Converted to Dedicated)
**Change Type:** Enhancement — Existing Function Module Modification
**Date:** 13-Mar-2026
**Prepared By:** Gaurav Prabhu
**SAP Target:** ECC 6.0 / NetWeaver 7.31

---

## ABAP Coding Standards Applied

The following standards from the ABAP Rules are applied throughout:

- **NW 7.31 Compatibility:** No inline declarations (`DATA(...)`), no `VALUE()`, no `NEW`, no string templates
- **Naming:** `lv_` variables, `lt_` tables, `lw_` work areas, `lc_` constants, `lty_` local types
- **Database:** No `SELECT *`, fields specified explicitly; no SELECT in loops; `FOR ALL ENTRIES` guarded by `IS NOT INITIAL` check; internal table structure matches SELECT field order exactly
- **Loops:** `READ TABLE ... BINARY SEARCH` used after sorting; no nested loops
- **Comments:** Double-quote style; meaningful comments only; no obvious narration
- **AI Code Markers:** All new code blocks marked with `" BEGIN: Cursor Generated Code` / `" END: Cursor Generated Code`
- **FM Header:** Standard header updated with Change History entry

---

## Change Request 1 — FM: `Z_SCM_CTD_GETTRIPHDR`

### Purpose of Change

1. **Point 1:** Add `AREA` field to the SELECT on table `ZSCE_CTD_HDR` and fetch the corresponding Area Description from domain `YAREA` using `DD_DOMA_GET`
2. **Point 2:** Fetch Transporter Name (`NAME1`) from table `LFA1` against the Transporter Code (`LIFNR`) using `FOR ALL ENTRIES` pattern; populate in output

---

### Pre-requisite: SE11 Structure Change

The following two fields must be added to DDIC structure **`ZSCM_TRIPHDR_ST`** in SE11 and activated **before** the FM changes are transported:

| Field Name  | Type   | Length | Description          |
|-------------|--------|--------|----------------------|
| `VEND_DESC` | `CHAR` | 30     | Transporter Name     |
| `AREA_DESC` | `CHAR` | 30     | Area Description     |

> **Note:** `NAME1` in LFA1 is 35 chars (`NAME1_GP`) and domain text (`DDTEXT`) is 60 chars. Values exceeding 30 characters will be truncated. Confirmed acceptable by functional team.

---

### Change History Entry (FM Header)

Add the following to the Change History comment block in the FM:

```abap
* CHANGE HISTORY
*----------------------------------------------------------------------
* Date        User ID    Description
* 13.03.2026  [USERID]   Point 1: Added AREA field in SELECT on ZSCE_CTD_HDR;
*                                 Area Description fetched via DD_DOMA_GET (domain YAREA)
*                        Point 2: Transporter Name fetched from LFA1 via FOR ALL ENTRIES
```

---

### Code Change 1 of 8 — Add `AREA` to Local Type `lty_trip_hdr`

The local type must match the SELECT field order exactly.

**Before:**
```abap
  TYPES: BEGIN OF lty_trip_hdr,
          lifnr           TYPE lifnr,
          truck_no        TYPE ytruck_no,
          trip_no         TYPE ztrip_no,
          trip_status     TYPE zctd_trip_st,
          created_by      TYPE syuname,
          created_date    TYPE sydatum,
          created_time    TYPE systtimlo,
         END OF lty_trip_hdr.
```

**After:**
```abap
  TYPES: BEGIN OF lty_trip_hdr,
          lifnr           TYPE lifnr,
          truck_no        TYPE ytruck_no,
          trip_no         TYPE ztrip_no,
          trip_status     TYPE zctd_trip_st,
" BEGIN: Cursor Generated Code
          area            TYPE yarea,
" END: Cursor Generated Code
          created_by      TYPE syuname,
          created_date    TYPE sydatum,
          created_time    TYPE systtimlo,
         END OF lty_trip_hdr.
```

---

### Code Change 2 of 8 — Add New Local Type `lty_lfa1`

Add immediately after the closing of `lty_trip_hdr` type definition.

**Add (new):**
```abap
" BEGIN: Cursor Generated Code
  TYPES: BEGIN OF lty_lfa1,
          lifnr TYPE lifnr,
          name1 TYPE name1_gp,
         END OF lty_lfa1.
" END: Cursor Generated Code
```

---

### Code Change 3 of 8 — Add New DATA Declarations

Add as a new DATA block immediately after the existing DATA block.

**Existing DATA block (unchanged — shown for reference):**
```abap
  DATA:lt_trip_hdr TYPE TABLE OF lty_trip_hdr,
       lw_trip_hdr TYPE lty_trip_hdr,
       lw_trip_hdr_op TYPE zscm_triphdr_st,
       lt_dd07v_tab   TYPE TABLE OF dd07v,
       lw_dd07v_tab   TYPE dd07v,
       lw_return    TYPE bapiret2,
       ltr_trip_status TYPE RANGE OF zctd_trip_st,
       lwr_trip_status LIKE LINE OF ltr_trip_status.
```

**Add new DATA block below:**
```abap
" BEGIN: Cursor Generated Code
  DATA: lt_lfa1       TYPE TABLE OF lty_lfa1,
        lw_lfa1       TYPE lty_lfa1,
        lt_dd07v      TYPE TABLE OF dd07v,
        lw_dd07v      TYPE dd07v,
        lt_trip_hdr_t TYPE TABLE OF lty_trip_hdr.
" END: Cursor Generated Code
```

---

### Code Change 4 of 8 — Add New CONSTANT for Area Domain

Add immediately after the existing CONSTANTS block.

**Existing CONSTANTS block (unchanged — shown for reference):**
```abap
  CONSTANTS: lc_user_type_vendor TYPE char1 VALUE 'V',
             lc_user_type_ril    TYPE char1 VALUE 'R',
             lc_trip_status      TYPE dd07l-domname VALUE 'ZCTD_TRIP_ST',
             lc_t                TYPE ddrefstruc-bool VALUE 'T'.
```

**Add new CONSTANTS block below:**
```abap
" BEGIN: Cursor Generated Code
  CONSTANTS: lc_area_domain TYPE dd07l-domname VALUE 'YAREA'.
" END: Cursor Generated Code
```

---

### Code Change 5 of 8 — Add `AREA` to SELECT on `ZSCE_CTD_HDR`

**Before:**
```abap
      SELECT lifnr
             truck_no
             trip_no
             trip_status
             created_by
             created_date
             created_time
      FROM zsce_ctd_hdr CLIENT SPECIFIED
      INTO TABLE lt_trip_hdr
      WHERE mandt       = sy-mandt
      AND   trip_status IN ltr_trip_status[]. " Index used ZSCE_CTD_HDR~Z01
```

**After:**
```abap
      SELECT lifnr
             truck_no
             trip_no
             trip_status
" BEGIN: Cursor Generated Code
             area
" END: Cursor Generated Code
             created_by
             created_date
             created_time
      FROM zsce_ctd_hdr CLIENT SPECIFIED
      INTO TABLE lt_trip_hdr
      WHERE mandt       = sy-mandt
      AND   trip_status IN ltr_trip_status[]. " Index used ZSCE_CTD_HDR~Z01
```

---

### Code Change 6 of 8 — Fetch LFA1 and Area Domain Values

Insert this entire block **after** the `RETURN` statement in the "no data found" check and **before** `CALL FUNCTION 'DD_DOMVALUES_GET'`.

**Insert after (existing code — shown for reference):**
```abap
  " Return if no data found
  IF lt_trip_hdr IS INITIAL.
    lw_return-type    = 'E'.
    lw_return-message = text-022.
    APPEND lw_return TO et_return.
    CLEAR  lw_return.
    RETURN.
  ENDIF.
```

**New block to insert:**
```abap
" BEGIN: Cursor Generated Code
  " Deduplicate by Transporter Code before fetching from LFA1
  lt_trip_hdr_t = lt_trip_hdr.
  SORT lt_trip_hdr_t BY lifnr.
  DELETE lt_trip_hdr_t WHERE lifnr IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM lt_trip_hdr_t COMPARING lifnr.

  " Fetch Transporter Names from LFA1
  IF lt_trip_hdr_t IS NOT INITIAL.
    SELECT lifnr name1
      FROM lfa1 CLIENT SPECIFIED
      INTO TABLE lt_lfa1
      FOR ALL ENTRIES IN lt_trip_hdr_t
      WHERE mandt = sy-mandt
      AND   lifnr = lt_trip_hdr_t-lifnr.
    IF sy-subrc = 0.
      SORT lt_lfa1 BY lifnr.
    ENDIF.
  ENDIF.

  " Fetch Area Descriptions from domain YAREA
  CALL FUNCTION 'DD_DOMA_GET'
    EXPORTING
      domain_name   = lc_area_domain
      langu         = sy-langu
    TABLES
      dd07v_tab_a   = lt_dd07v
    EXCEPTIONS
      illegal_value = 1
      op_failure    = 2
      OTHERS        = 3.
  IF sy-subrc = 0.
    SORT lt_dd07v BY domvalue_l.
  ENDIF.
" END: Cursor Generated Code
```

---

### Code Change 7 of 8 — Populate `VEND_DESC` and `AREA_DESC` in Output Loop

**Before (inside existing LOOP):**
```abap
    LOOP AT lt_trip_hdr INTO lw_trip_hdr.
      MOVE-CORRESPONDING lw_trip_hdr TO lw_trip_hdr_op.
      READ TABLE lt_dd07v_tab INTO lw_dd07v_tab WITH KEY domvalue_l = lw_trip_hdr-trip_status BINARY SEARCH.
      IF sy-subrc = 0.
        lw_trip_hdr_op-status_description = lw_dd07v_tab-ddtext.
      ENDIF.
      APPEND lw_trip_hdr_op TO et_trip_hdr.
      CLEAR: lw_trip_hdr_op,lw_trip_hdr.
    ENDLOOP.
```

**After:**
```abap
    LOOP AT lt_trip_hdr INTO lw_trip_hdr.
      MOVE-CORRESPONDING lw_trip_hdr TO lw_trip_hdr_op.
      READ TABLE lt_dd07v_tab INTO lw_dd07v_tab WITH KEY domvalue_l = lw_trip_hdr-trip_status BINARY SEARCH.
      IF sy-subrc = 0.
        lw_trip_hdr_op-status_description = lw_dd07v_tab-ddtext.
      ENDIF.
" BEGIN: Cursor Generated Code
      " Populate Transporter Name from LFA1
      READ TABLE lt_lfa1 INTO lw_lfa1
        WITH KEY lifnr = lw_trip_hdr-lifnr BINARY SEARCH.
      IF sy-subrc = 0.
        lw_trip_hdr_op-vend_desc = lw_lfa1-name1.
      ENDIF.
      " Populate Area Description from domain YAREA
      READ TABLE lt_dd07v INTO lw_dd07v
        WITH KEY domvalue_l = lw_trip_hdr-area BINARY SEARCH.
      IF sy-subrc = 0.
        lw_trip_hdr_op-area_desc = lw_dd07v-ddtext.
      ENDIF.
" END: Cursor Generated Code
      APPEND lw_trip_hdr_op TO et_trip_hdr.
      CLEAR: lw_trip_hdr_op,lw_trip_hdr.
    ENDLOOP.
```

---

### Code Change 8 of 8 — Add Cleanup for New Internal Tables

**Before:**
```abap
*   Cleanup
  CLEAR: lt_trip_hdr.
```

**After:**
```abap
*   Cleanup
  CLEAR: lt_trip_hdr.
" BEGIN: Cursor Generated Code
  CLEAR: lt_lfa1, lt_dd07v, lt_trip_hdr_t.
" END: Cursor Generated Code
```

---

### Summary of All Objects Impacted — Change Request 1

| Object Type       | Object Name           | Change                                                  |
|-------------------|-----------------------|---------------------------------------------------------|
| DDIC Structure    | `ZSCM_TRIPHDR_ST`     | Add `VEND_DESC` (CHAR30) and `AREA_DESC` (CHAR30)       |
| Function Module   | `Z_SCM_CTD_GETTRIPHDR`| 8 code changes as detailed above                        |
| Referenced Table  | `LFA1`                | Read only — no structural change                        |
| Referenced Table  | `ZSCE_CTD_HDR`        | Read only — `AREA` field added to SELECT                |
| Referenced FM     | `DD_DOMA_GET`         | Called for domain `YAREA` — no change to FM itself      |

---

---

## Change Request 2 — Table: `ZSCE_CTD_ITM`

### Purpose of Change

Add 4 new fields to the database table `ZSCE_CTD_ITM` in SE11 to capture vendor-confirmed dates and additional remarks at both Trip Confirmation (TTC/RTC) and CTD Confirmation (TCC/RCC) stages.

---

### SE11 — Fields to Add to Table `ZSCE_CTD_ITM`

| Field Name         | Data Element   | Type | Length | Short Description          |
|--------------------|----------------|------|--------|----------------------------|
| `VEND_SOURCE_DATE` | `ZSOURCE_DATE` | DATS | 8      | Vendor Confirmed Source Date |
| `VEND_DEST_DATE`   | `ZDEST_DATE`   | DATS | 8      | Vendor Confirmed Dest Date   |
| `VENDOR_REMARKS2`  | `ZMSG`         | CHAR | 255    | Vendor Remarks 2             |
| `RIL_REMARKS2`     | `ZMSG`         | CHAR | 255    | RIL Remarks 2                |

> **Activation:** Activate the table after adding fields before any FM changes are transported.
> **No FM changes** are part of this Change Request. FM changes follow in CR3 and CR4.

---

### Summary of All Objects Impacted — Change Request 2

| Object Type | Object Name     | Change                          |
|-------------|-----------------|---------------------------------|
| DB Table    | `ZSCE_CTD_ITM`  | 4 new fields added as above     |

---

## Change Request 3 — FM: `Z_SCM_CTD_TRIPCONFIRM`

### Purpose of Change

Update the `UPDATE ZSCE_CTD_ITM` statements across all call contexts to:
- **TTC:** Replace `source_date`/`dest_date` with new vendor date fields; apply date-context stamp to `vendor_remarks`
- **RTC:** Apply date-context stamp to existing `ril_remarks`
- **TCC:** Populate new `vendor_remarks2` with date-context stamp; remove update of `vendor_remarks`
- **RCC:** Populate new `ril_remarks2` with date-context stamp; remove update of `ril_remarks`
- **Validation Removal:** Remove leg-type checks for TTC/RTC and TCC/RCC, and remove date sequence validation — both Loaded and Empty legs are now processed; date validation is handled by the UI
- **RCC — OIGSV Update:** After updating `ZSCE_CTD_ITM`, fetch SHNUMBERs where `ril_ctd = 'X'` and update `OIGSV` table setting `shift = '1'`

**Remarks Concatenation Format:** `<date> : <context> - <remarks>`
Example: `13.03.2026 : TTC - Truck arrived on time`

---

### Pre-requisite

CR2 (table `ZSCE_CTD_ITM` with 4 new fields) must be activated in SE11 before this FM is transported.

---

### Change History Entry (FM Header)

```abap
* CHANGE HISTORY
*----------------------------------------------------------------------
* Date        User ID    Description
* 13.03.2026  [USERID]   CR3: TTC - replaced source_date/dest_date with vend_source_date/
*                             vend_dest_date; date-context stamp added to vendor_remarks
*                             RTC - date-context stamp added to ril_remarks
*                             TCC - vendor_remarks replaced by vendor_remarks2 with stamp
*                             RCC - ril_remarks replaced by ril_remarks2 with stamp;
*                                   OIGSV shift updated for legs where ril_ctd = 'X'
*                             Removed leg-type validations for TTC/RTC and TCC/RCC
*                             Removed date sequence validation (handled by UI)
```

---

### Code Change 1 of 8 — Add New TYPE and DATA Declarations

Add new local type, internal table/work area for SHNUMBER, two remarks variables, and a date formatting variable.

**Before (existing TYPES and DATA block):**
```abap
  DATA:
    lw_upd            TYPE zscm_ctd_legupd_st,
    lw_return         TYPE bapiret2,
    lv_error          TYPE abap_bool VALUE abap_false,
    lv_new_status     TYPE zctd_trip_st,
    lv_item_upd_flag  TYPE flag,
    lv_hdr_upd_flag   TYPE flag,
    lv_hdr_remark     TYPE zsce_ctd_hdr-ctd_ruleeng_remarks,
    lv_vendor_remarks TYPE zsce_ctd_itm-vendor_remarks,
    lv_ril_remarks    TYPE zsce_ctd_itm-ril_remarks,
    lv_rej_remark     TYPE zsce_ctd_itm-rej_remark.
```

**After:**
```abap
" BEGIN: Cursor Generated Code
  TYPES: BEGIN OF lty_shnumber,
          shnumber TYPE oig_shnum,
         END OF lty_shnumber.
" END: Cursor Generated Code

  DATA:
    lw_upd            TYPE zscm_ctd_legupd_st,
    lw_return         TYPE bapiret2,
    lv_error          TYPE abap_bool VALUE abap_false,
    lv_new_status     TYPE zctd_trip_st,
    lv_item_upd_flag  TYPE flag,
    lv_hdr_upd_flag   TYPE flag,
    lv_hdr_remark     TYPE zsce_ctd_hdr-ctd_ruleeng_remarks,
    lv_vendor_remarks TYPE zsce_ctd_itm-vendor_remarks,
    lv_ril_remarks    TYPE zsce_ctd_itm-ril_remarks,
    lv_rej_remark     TYPE zsce_ctd_itm-rej_remark.
" BEGIN: Cursor Generated Code
  DATA: lv_vendor_remarks2 TYPE zmsg,
        lv_ril_remarks2    TYPE zmsg,
        lv_date_c          TYPE char10,
        lt_shnumber        TYPE TABLE OF lty_shnumber,
        lw_shnumber        TYPE lty_shnumber.
" END: Cursor Generated Code
```

---

### Code Change 2 of 8 — Add New Variables to CLEAR Statement

**Before:**
```abap
    CLEAR: lv_new_status, lv_hdr_remark,
           lv_vendor_remarks, lv_ril_remarks, lv_rej_remark.
```

**After:**
```abap
    CLEAR: lv_new_status, lv_hdr_remark,
           lv_vendor_remarks, lv_ril_remarks, lv_rej_remark.
" BEGIN: Cursor Generated Code
    CLEAR: lv_vendor_remarks2, lv_ril_remarks2, lv_date_c.
" END: Cursor Generated Code
```

---

### Code Change 3 of 8 — TTC: Replace Date Fields and Stamp `vendor_remarks`

**Before:**
```abap
      WHEN 'TTC'.
        IF im_confirm_flag = 'C'.
          UPDATE zsce_ctd_itm CLIENT SPECIFIED
            SET  source_date    = lw_upd-source_date
                 dest_date      = lw_upd-dest_date
                 vendor_remarks = lv_vendor_remarks
                modified_by   = sy-uname
                modified_date = sy-datum
                modified_time = sy-uzeit
           WHERE mandt    = sy-mandt
             AND lifnr    = lw_upd-lifnr
             AND truck_no = lw_upd-truck_no
             AND trip_no  = lw_upd-trip_no
             AND counter  = lw_upd-counter.
        ENDIF.
```

**After:**
```abap
      WHEN 'TTC'.
        IF im_confirm_flag = 'C'.
" BEGIN: Cursor Generated Code
          WRITE sy-datum TO lv_date_c.
          CONCATENATE lv_date_c ':' 'TTC' '-' lv_vendor_remarks
            INTO lv_vendor_remarks SEPARATED BY space.
" END: Cursor Generated Code
          UPDATE zsce_ctd_itm CLIENT SPECIFIED
            SET  vend_source_date = lw_upd-source_date
                 vend_dest_date   = lw_upd-dest_date
                 vendor_remarks   = lv_vendor_remarks
                 modified_by      = sy-uname
                 modified_date    = sy-datum
                 modified_time    = sy-uzeit
           WHERE mandt    = sy-mandt
             AND lifnr    = lw_upd-lifnr
             AND truck_no = lw_upd-truck_no
             AND trip_no  = lw_upd-trip_no
             AND counter  = lw_upd-counter.
        ENDIF.
```

> **Note:** `source_date` and `dest_date` removed from SET; replaced by `vend_source_date` and `vend_dest_date`. `vendor_remarks` retains update but now carries the date-context stamp.

---

### Code Change 4 of 8 — RTC: Stamp `ril_remarks` with Date-Context

**Before:**
```abap
        IF im_confirm_flag = 'C'.
          UPDATE zsce_ctd_itm CLIENT SPECIFIED
            SET  ril_remarks = lv_ril_remarks
                modified_by   = sy-uname
                modified_date = sy-datum
                modified_time = sy-uzeit
           WHERE mandt    = sy-mandt
             AND lifnr    = lw_upd-lifnr
             AND truck_no = lw_upd-truck_no
             AND trip_no  = lw_upd-trip_no
             AND counter  = lw_upd-counter.
```

**After:**
```abap
        IF im_confirm_flag = 'C'.
" BEGIN: Cursor Generated Code
          WRITE sy-datum TO lv_date_c.
          CONCATENATE lv_date_c ':' 'RTC' '-' lv_ril_remarks
            INTO lv_ril_remarks SEPARATED BY space.
" END: Cursor Generated Code
          UPDATE zsce_ctd_itm CLIENT SPECIFIED
            SET  ril_remarks  = lv_ril_remarks
                 modified_by  = sy-uname
                 modified_date = sy-datum
                 modified_time = sy-uzeit
           WHERE mandt    = sy-mandt
             AND lifnr    = lw_upd-lifnr
             AND truck_no = lw_upd-truck_no
             AND trip_no  = lw_upd-trip_no
             AND counter  = lw_upd-counter.
```

---

### Code Change 5 of 8 — TCC: Replace `vendor_remarks` with `vendor_remarks2`

**Before:**
```abap
      WHEN 'TCC'.
        IF im_confirm_flag = 'C'.
          UPDATE zsce_ctd_itm CLIENT SPECIFIED
            SET  vendor_ctd     = lw_upd-vendor_ctd
                 vendor_remarks = lv_vendor_remarks
                modified_by   = sy-uname
                modified_date = sy-datum
                modified_time = sy-uzeit
           WHERE mandt    = sy-mandt
             AND lifnr    = lw_upd-lifnr
             AND truck_no = lw_upd-truck_no
             AND trip_no  = lw_upd-trip_no
             AND counter  = lw_upd-counter.
        ENDIF.
```

**After:**
```abap
      WHEN 'TCC'.
        IF im_confirm_flag = 'C'.
" BEGIN: Cursor Generated Code
          WRITE sy-datum TO lv_date_c.
          CONCATENATE lv_date_c ':' 'TCC' '-' lv_vendor_remarks
            INTO lv_vendor_remarks2 SEPARATED BY space.
" END: Cursor Generated Code
          UPDATE zsce_ctd_itm CLIENT SPECIFIED
            SET  vendor_ctd      = lw_upd-vendor_ctd
                 vendor_remarks2 = lv_vendor_remarks2
                 modified_by     = sy-uname
                 modified_date   = sy-datum
                 modified_time   = sy-uzeit
           WHERE mandt    = sy-mandt
             AND lifnr    = lw_upd-lifnr
             AND truck_no = lw_upd-truck_no
             AND trip_no  = lw_upd-trip_no
             AND counter  = lw_upd-counter.
        ENDIF.
```

> **Note:** `vendor_remarks` removed from TCC SET; replaced by `vendor_remarks2`. `vendor_ctd` unchanged.

---

### Code Change 6 of 8 — RCC: Replace `ril_remarks` with `ril_remarks2`

**Before:**
```abap
      WHEN 'RCC'.
        IF im_confirm_flag = 'C'.
          UPDATE zsce_ctd_itm CLIENT SPECIFIED
            SET ril_remarks = lv_ril_remarks
                ril_ctd     = lw_upd-ril_ctd
                modified_by   = sy-uname
                modified_date = sy-datum
                modified_time = sy-uzeit
           WHERE mandt    = sy-mandt
             AND lifnr    = lw_upd-lifnr
             AND truck_no = lw_upd-truck_no
             AND trip_no  = lw_upd-trip_no
             AND counter  = lw_upd-counter.
        ENDIF.
```

**After:**
```abap
      WHEN 'RCC'.
        IF im_confirm_flag = 'C'.
" BEGIN: Cursor Generated Code
          WRITE sy-datum TO lv_date_c.
          CONCATENATE lv_date_c ':' 'RCC' '-' lv_ril_remarks
            INTO lv_ril_remarks2 SEPARATED BY space.
" END: Cursor Generated Code
          UPDATE zsce_ctd_itm CLIENT SPECIFIED
            SET  ril_ctd      = lw_upd-ril_ctd
                 ril_remarks2 = lv_ril_remarks2
                 modified_by  = sy-uname
                 modified_date = sy-datum
                 modified_time = sy-uzeit
           WHERE mandt    = sy-mandt
             AND lifnr    = lw_upd-lifnr
             AND truck_no = lw_upd-truck_no
             AND trip_no  = lw_upd-trip_no
             AND counter  = lw_upd-counter.
        ENDIF.
```

> **Note:** `ril_remarks` removed from RCC SET; replaced by `ril_remarks2`. `ril_ctd` unchanged.

---

### Code Change 7 of 8 — Remove Leg-Type and Date Sequence Validations

Both leg types (Loaded `'L'` and Empty `'E'`) must now be processed in all contexts. Date sequence validation is handled by the UI.

**Before (3 consecutive validation blocks inside LOOP):**
```abap
    IF im_call_context EQ 'TTC' OR im_call_context EQ 'RTC'.
      "EMPTY leg validation
      IF lw_upd-leg_type <> 'E'.
        lw_return-type    = 'E'.
        lw_return-message =
          |{ 'Loaded leg cannot be modified. Counter'(034) } { lw_upd-counter }|.
        APPEND lw_return TO et_return.
        lv_error = abap_true.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF im_call_context EQ 'TCC' OR im_call_context EQ 'RCC'.
      "EMPTY leg validation
      IF lw_upd-leg_type <> 'L'.
        lw_return-type    = 'E'.
        lw_return-message =
          |{ 'Empty leg cannot be modified. Counter'(035) } { lw_upd-counter }|.
        APPEND lw_return TO et_return.
        lv_error = abap_true.
        CONTINUE.
      ENDIF.
    ENDIF.

    "Date sequence validation
    IF lw_upd-source_date > lw_upd-dest_date.
      lw_return-type    = 'E'.
      lw_return-message =
        |Invalid date sequence for Empty Leg. Counter { lw_upd-counter }|.
      APPEND lw_return TO et_return.
      lv_error = abap_true.
      CONTINUE.
    ENDIF.
```

**After:**
```abap
" BEGIN: Cursor Generated Code
*   Leg-type validations removed — both Loaded and Empty legs processed in all contexts
*   Date sequence validation removed — handled by UI
" END: Cursor Generated Code
```

> **Note:** All three blocks are fully removed. No replacement logic required.

---

### Code Change 8 of 8 — RCC: Fetch SHNUMBERs and Update OIGSV

After the `ENDLOOP` and before `COMMIT WORK`, add logic to update `OIGSV` for all legs in the trip where `ril_ctd = 'X'`. Error handling follows the hard-error pattern consistent with the rest of the FM.

**Insert after `ENDLOOP` and before `STEP 3 – Commit & Success`:**

```abap
" BEGIN: Cursor Generated Code
*---------------------------------------------------------------------*
* STEP 2B – RCC: Update OIGSV shift flag for CTD-confirmed legs
*---------------------------------------------------------------------*
  IF im_call_context = 'RCC'.
    IF it_leg_update IS NOT INITIAL.
      SELECT shnumber
        FROM zsce_ctd_itm CLIENT SPECIFIED
        INTO TABLE lt_shnumber
        FOR ALL ENTRIES IN it_leg_update
        WHERE mandt    = sy-mandt
        AND   lifnr    = it_leg_update-lifnr
        AND   truck_no = it_leg_update-truck_no
        AND   trip_no  = it_leg_update-trip_no
        AND   counter  = it_leg_update-counter
        AND   ril_ctd  = 'X'.
      IF sy-subrc = 0.
        DELETE lt_shnumber WHERE shnumber IS INITIAL.
        LOOP AT lt_shnumber INTO lw_shnumber.
          UPDATE oigsv CLIENT SPECIFIED
            SET   shift    = '1'
            WHERE client   = sy-mandt
            AND   shnumber = lw_shnumber-shnumber.
          IF sy-subrc <> 0.
            lw_return-type    = 'E'.
            lw_return-message = 'OIGSV shift update failed for shipment'(036).
            APPEND lw_return TO et_return.
            lv_error = abap_true.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    IF lv_error = abap_true.
      RETURN.
    ENDIF.
  ENDIF.
" END: Cursor Generated Code
```

> **Note:** `STEP 3 – Commit & Success` follows immediately after this block. If any OIGSV UPDATE fails, `lv_error = abap_true` causes RETURN before COMMIT, leaving the full transaction uncommitted for retry.
> Message text `'OIGSV shift update failed for shipment'(036)` must be added as Text Element 036 in the FM.

---

### Summary of All Objects Impacted — Change Request 3

| Object Type      | Object Name              | Change                                                                          |
|------------------|--------------------------|---------------------------------------------------------------------------------|
| Function Module  | `Z_SCM_CTD_TRIPCONFIRM`  | 8 code changes as detailed above                                                |
| DB Table         | `ZSCE_CTD_ITM`           | Written — new fields `vend_source_date`, `vend_dest_date`, `vendor_remarks2`, `ril_remarks2`; read back for SHNUMBER fetch (RCC) |
| DB Table         | `OIGSV`                  | Written — `shift = '1'` for RCC-confirmed legs where `ril_ctd = 'X'`           |
| DDIC Structure   | `ZSCM_CTD_LEGUPD_ST`     | No change — existing input fields reused for new DB fields                      |
| FM Text Elements | `Z_SCM_CTD_TRIPCONFIRM`  | Add Text Element 036: `OIGSV shift update failed for shipment`                  |

---

## Change Request 4 — FM: `Z_SCM_CTD_GETTRIPITM`

### Purpose of Change

Extend the SELECT on `ZSCE_CTD_ITM` to fetch the 4 new fields added in CR2 and return them in the output table `ET_TRIP_ITM`.

---

### Pre-requisite: SE11 Structure Change

The following 4 fields must be added to DDIC structure **`ZSCM_CTD_TRIPITM_ST`** in SE11 and activated **before** the FM change is transported:

| Field Name         | Data Element   | Type | Length | Short Description            |
|--------------------|----------------|------|--------|------------------------------|
| `VEND_SOURCE_DATE` | `ZSOURCE_DATE` | DATS | 8      | Vendor Confirmed Source Date |
| `VEND_DEST_DATE`   | `ZDEST_DATE`   | DATS | 8      | Vendor Confirmed Dest Date   |
| `VENDOR_REMARKS2`  | `ZMSG`         | CHAR | 255    | Vendor Remarks 2             |
| `RIL_REMARKS2`     | `ZMSG`         | CHAR | 255    | RIL Remarks 2                |

> The FM uses `lt_trip_itm TYPE zscm_ctd_tripitm_tt` directly as the SELECT target and output table. The structure must be extended before the SELECT change is activated.

---

### Change History Entry (FM Header)

```abap
* CHANGE HISTORY
*----------------------------------------------------------------------
* Date        User ID    Description
* 13.03.2026  [USERID]   CR4: Added vend_source_date, vend_dest_date,
*                             vendor_remarks2, ril_remarks2 to SELECT
*                             on ZSCE_CTD_ITM
```

---

### Code Change 1 of 1 — Add 4 New Fields to SELECT

**Before:**
```abap
    SELECT  lifnr
            truck_no
            trip_no
            counter
            area
            del_ind
            shnumber
            shtype
            ytts_rep_no
            mvt_type
            source_id
            source_date
            dest_id
            dest_date
            ctd_eligible
            rej_remark
            rej_code
            source_tzone
            source_region
            dest_tzone
            dest_region
            dest_area
            leg_type
            source_ent_date
            dest_exit_date
            route
            distance
            vendor_remarks
            ril_remarks
            business
            sub_business
            vendor_ctd
            ril_ctd
      FROM zsce_ctd_itm CLIENT SPECIFIED
      INTO TABLE lt_trip_itm
      WHERE mandt      = sy-mandt
      AND   lifnr      = im_user_id
      AND   truck_no   = im_truck_no
      AND   trip_no    = im_trip_no.
```

**After:**
```abap
    SELECT  lifnr
            truck_no
            trip_no
            counter
            area
            del_ind
            shnumber
            shtype
            ytts_rep_no
            mvt_type
            source_id
            source_date
            dest_id
            dest_date
            ctd_eligible
            rej_remark
            rej_code
            source_tzone
            source_region
            dest_tzone
            dest_region
            dest_area
            leg_type
            source_ent_date
            dest_exit_date
            route
            distance
            vendor_remarks
            ril_remarks
            business
            sub_business
            vendor_ctd
            ril_ctd
" BEGIN: Cursor Generated Code
            vend_source_date
            vend_dest_date
            vendor_remarks2
            ril_remarks2
" END: Cursor Generated Code
      FROM zsce_ctd_itm CLIENT SPECIFIED
      INTO TABLE lt_trip_itm
      WHERE mandt      = sy-mandt
      AND   lifnr      = im_user_id
      AND   truck_no   = im_truck_no
      AND   trip_no    = im_trip_no.
```

---

### Summary of All Objects Impacted — Change Request 4

| Object Type      | Object Name               | Change                                              |
|------------------|---------------------------|-----------------------------------------------------|
| DDIC Structure   | `ZSCM_CTD_TRIPITM_ST`     | Add 4 new fields matching the new table fields      |
| Function Module  | `Z_SCM_CTD_GETTRIPITM`    | 1 code change — 4 fields added to SELECT            |
| DB Table         | `ZSCE_CTD_ITM`            | Read only — no structural change in this CR         |

---

*Further change requests will be added to this document.*
