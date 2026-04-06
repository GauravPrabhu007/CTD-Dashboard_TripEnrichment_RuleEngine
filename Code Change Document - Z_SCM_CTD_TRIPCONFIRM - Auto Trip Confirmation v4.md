# Code Change Document
## Z_SCM_CTD_TRIPCONFIRM – RIL Auto Trip & CTD Confirmation

---

| Attribute         | Detail                                          |
|-------------------|-------------------------------------------------|
| **Document**      | Code Change Document (V4 — Final)               |
| **CD Number**     | CD:XXXXXXX *(to be assigned)*                   |
| **Transport**     | TR:XXXXXXXXX *(to be assigned)*                 |
| **Author**        | Omkar More                                      |
| **Date**          | 02.04.2026                                      |
| **SAP System**    | ECC 6.0 / NetWeaver 7.31                        |
| **Approver**      | *(to be assigned)*                              |

---

## Table of Contents

1. [Business Requirements](#1-business-requirements)
2. [Design Overview – Two-LUW Architecture](#2-design-overview--two-luw-architecture)
3. [ABAP Coding Standards Applied](#3-abap-coding-standards-applied)
4. [Change Request 1 – Modifications to Z_SCM_CTD_TRIPCONFIRM](#4-change-request-1--modifications-to-z_scm_ctd_tripconfirm)
5. [Change Request 2 – New FM Z_SCM_CTD_AUTO_TRIPCONFIRM](#5-change-request-2--new-fm-z_scm_ctd_auto_tripconfirm)
6. [Change Request 3 – Dictionary Change ZSCE_CTD_HDR](#6-change-request-3--dictionary-change-zsce_ctd_hdr)
7. [Change Request 4 – CTD Rule Engine Scheduling](#7-change-request-4--ctd-rule-engine-scheduling)
8. [Change Request 5 – OIGSV SHIFT Update for Non-CTD Legs](#8-change-request-5--oigsv-shift-update-for-non-ctd-legs)
9. [Transport Sequence](#9-transport-sequence)
10. [End-to-End Process Flow](#10-end-to-end-process-flow)
11. [ABAP Team – Implementation Summary](#11-abap-team--implementation-summary)

---

## 1. Business Requirements

### 1.1 Scenario A – RIL Trip Confirmation Automation (RTCA)

After the **Transporter Trip Confirmation (TTC)** step (`IM_CALL_CONTEXT = 'TTC'`), the vendor provides actual travel dates (`VEND_SOURCE_DATE`, `VEND_DEST_DATE`) for each leg. The next manual step is **RIL Trip Confirmation (RTC)** where a RIL Ops user validates and approves the dates.

**Automation Trigger:** If `VEND_SOURCE_DATE = SOURCE_DATE` **AND** `VEND_DEST_DATE = DEST_DATE` for **all Empty Legs** (`LEG_TYPE = 'E'`) of the trip, then RIL Trip Confirmation can be auto-approved immediately after TTC without manual intervention.

**Expected Outcome:**
- Trip status advances from `'05'` → `'07'` automatically.
- `RIL_REMARKS` on each empty leg is stamped with the auto-approval remark.
- `RTCA_FLAG` on the trip header (`ZSCE_CTD_HDR`) is set to `'X'` as a permanent trip-level audit marker.

---

### 1.2 Scenario B – RIL CTD Confirmation Automation (RCCA)

After the **Transporter CTD Confirmation (TCC)** step (`IM_CALL_CONTEXT = 'TCC'`), the vendor provides the CTD flag (`VENDOR_CTD`) for each loaded leg.

**Automation Trigger:** If `VENDOR_CTD = CTD_ELIGIBLE` for **all Loaded Legs** (`LEG_TYPE = 'L'`) of the trip, then RIL CTD Confirmation can be auto-approved immediately after TCC without manual intervention.

**Expected Outcome:**
- Trip status advances from `'08'` → `'10'` automatically.
- `RIL_CTD` is set to the value of `CTD_ELIGIBLE` for each loaded leg.
- `RIL_REMARKS2` on each loaded leg is stamped with the auto-approval remark.
- `RCCA_FLAG` on the trip header (`ZSCE_CTD_HDR`) is set to `'X'` as a permanent trip-level audit marker.
- `OIGSV` table is updated for all loaded legs that have a shipment:
  - `SHIFT = '1'` where `RIL_CTD = 'X'` (CTD confirmed for that shipment)
  - `SHIFT = '2'` where `RIL_CTD = space` (CTD not applicable for that shipment)

---

### 1.3 Scenario C – CTD Rule Engine Invocation (CR4)

After trip status `'07'` is set — either by a **manual RTC** or by **automated RTCA** — the CTD Rule Engine program `ZCTD_RULEENG_EXEC` must be triggered **immediately** as an asynchronous background job so the trip becomes visible on the Transporter CTD Confirmation screen without waiting for the periodic batch run.

**Expected Outcome:**
- An immediate background job (`ZCTD_RULEENG_AUTO`) is scheduled for the confirmed trip.
- Job scheduling failure is non-blocking — the RTC/RTCA result is not reversed.

---

### 1.4 Audit Trail Requirement

Both intermediate statuses (`'05'` for TTC and `'08'` for TCC) **must be persisted** in the database for audit trail before auto-confirmation fires. This is achieved via the Two-LUW Architecture described in Section 2.

---

## 2. Design Overview – Two-LUW Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│  EXISTING FM: Z_SCM_CTD_TRIPCONFIRM                             │
│                                                                  │
│  Step 1-3: Existing TTC/RTC/TCC/RCC logic (unchanged)           │
│  Step 3 COMMIT WORK → LUW-1 closes                              │
│    ↳ Status '05' (TTC) / '07' (RTC) / '08' (TCC) in DB        │
│                                                                  │
│  Step 4 [NEW/UPDATED]:                                           │
│    IF IM_CALL_CONTEXT = 'TTC'  → RTCA validation + call        │
│    ELSEIF IM_CALL_CONTEXT = 'TCC'  → RCCA validation + call    │
│    ELSEIF IM_CALL_CONTEXT = 'RTC'  → Rule Engine job schedule  │
└─────────────────────────────────────────────────────────────────┘
              │ CALL FUNCTION             │ Background Job (RTC)
              ▼                           ▼
┌─────────────────────────────────────────────────────────────────┐
│  NEW FM: Z_SCM_CTD_AUTO_TRIPCONFIRM                             │
│                                                                  │
│  Step 1: UPDATE ZSCE_CTD_ITM (RIL_REMARKS / RIL_CTD)           │
│  Step 1B (RCCA): UPDATE OIGSV                                   │
│    SHIFT='1' where RIL_CTD='X' (CTD confirmed)                  │
│    SHIFT='2' where RIL_CTD=space (CTD not applicable)  ← CR5   │
│  Step 2: UPDATE ZSCE_CTD_HDR                                    │
│    (status '07'/'10' + RTCA_FLAG or RCCA_FLAG = 'X')  ← CR3   │
│  Step 3: COMMIT WORK → LUW-2 closes                             │
│    WHEN 'RTCA': Schedule Rule Engine job  ← CR4                 │
└─────────────────────────────────────────────────────────────────┘
```

**Key Design Decisions:**

| Decision | Rationale |
|----------|-----------|
| Two separate FMs | Clean LUW separation; audit status persisted before auto-confirmation |
| Single new FM for both RTCA and RCCA | Reduces object count; `IM_CALL_CONTEXT` differentiates logic |
| `CTD_RULEENG_REMARKS` not updated | Consistent with existing FM behaviour |
| OIGSV updated for both SHIFT values | SHIFT='1' (CTD confirmed) and SHIFT='2' (CTD not applicable) stamp both outcomes atomically in the same RCCA COMMIT |
| Flags at header level (not leg level) | RTCA_FLAG / RCCA_FLAG are trip-level events — auto-confirmation either passes for all legs or not at all; header is the correct granularity |
| Rule Engine scheduling entity = status setter | `Z_SCM_CTD_TRIPCONFIRM` for manual RTC; `Z_SCM_CTD_AUTO_TRIPCONFIRM` for RTCA |
| Auto-user via `ZLOG_EXEC_VAR` | Configurable without transport; fallback to `sy-uname` |
| UI limited to 10 trips | Portal-side safeguard against job storm |

---

## 3. ABAP Coding Standards Applied

*(Source: `ABAP Rules - 02-04-2026` folder)*

| Rule | Application |
|------|-------------|
| **NW 7.31 Compatibility** | No string templates; no inline declarations; no `NEW`/`VALUE`/`CORRESPONDING`; no `@` host variables |
| **Naming Convention** | `lv_` scalar, `lt_` table, `lw_` work area, `lty_` local type |
| **Explicit SELECT Fields** | No `SELECT *` anywhere |
| **FOR ALL ENTRIES Guard** | `IS NOT INITIAL` check before every FOR ALL ENTRIES |
| **FOR ALL ENTRIES Dedup** | SORT + DELETE ADJACENT DUPLICATES on driver table copy |
| **SY-SUBRC Checks** | Checked after every SELECT, UPDATE, READ TABLE, CALL FUNCTION |
| **EXCEPTIONS Clause** | `EXCEPTIONS OTHERS = 1` on all CALL FUNCTION |
| **VALUE Initialisation** | Boolean flags initialised in DATA declaration |
| **Cursor Code Markers** | All new blocks wrapped in `" BEGIN/END: Cursor Generated Code` |
| **No SELECT in Loops** | OIGSV keys collected via FOR ALL ENTRIES outside LOOP |
| **Text Elements** | All messages externalised as `'text'(NNN)` |
| **ROLLBACK on Error** | Explicit `ROLLBACK WORK` + `RETURN` on any update failure |
| **Batch Processing** | `JOB_OPEN` / `SUBMIT VIA JOB` / `JOB_CLOSE` with `STRTIMMED = 'X'` |

---

## 4. Change Request 1 – Modifications to Z_SCM_CTD_TRIPCONFIRM

### 4.1 Overview

| Change | Location | Description |
|--------|----------|-------------|
| Code Change 1 of 3 | After existing TYPES block | Two new local types for validation |
| Code Change 2 of 3 | After existing DATA block | New work variables for auto-confirmation |
| Code Change 3 of 3 | After existing COMMIT WORK | Step 4: RTCA/RCCA validation + auto-confirmation call |

> CR4 adds Code Change 4 of 4 (Rule Engine scheduling for manual RTC) — see Section 7.4.

---

### 4.2 Code Change 1 of 3 – New TYPE Declarations

**Location:** After last existing `END OF lty_...` type definition.

```abap
" BEGIN: Cursor Generated Code - Change Document: CD:XXXXXXX  TR:XXXXXXXXX / Author:Omkar More on 02.04.2026
  TYPES:
    BEGIN OF lty_itm_date_check,
      lifnr            TYPE lifnr,
      truck_no         TYPE ytruck_no,
      trip_no          TYPE ztrip_no,
      counter          TYPE zcounter,
      source_date      TYPE zsource_date,
      dest_date        TYPE zdest_date,
      vend_source_date TYPE zsource_date,
      vend_dest_date   TYPE zdest_date,
    END OF lty_itm_date_check,
    BEGIN OF lty_itm_ctd_check,
      lifnr        TYPE lifnr,
      truck_no     TYPE ytruck_no,
      trip_no      TYPE ztrip_no,
      counter      TYPE zcounter,
      leg_type     TYPE zleg_type,
      ctd_eligible TYPE zctd_eligible,
      vendor_ctd   TYPE flag,
    END OF lty_itm_ctd_check.
" END: Cursor Generated Code
```

---

### 4.3 Code Change 2 of 3 – New DATA Declarations

**Location:** After last existing `DATA:` variable declaration.

```abap
" BEGIN: Cursor Generated Code - Change Document: CD:XXXXXXX  TR:XXXXXXXXX / Author:Omkar More on 02.04.2026
  DATA:
    lt_itm_date_check TYPE TABLE OF lty_itm_date_check,
    lw_itm_date_check TYPE lty_itm_date_check,
    lv_auto_rtc_ok    TYPE abap_bool VALUE abap_false,
    lt_itm_ctd_check  TYPE TABLE OF lty_itm_ctd_check,
    lw_itm_ctd_check  TYPE lty_itm_ctd_check,
    lv_auto_rcc_ok    TYPE abap_bool VALUE abap_false,
    lt_auto_leg_upd   TYPE zscm_ctd_legupd_tt,
    lw_auto_leg_upd   TYPE zscm_ctd_legupd_st,
    lt_rtca_return    TYPE bapiret2_t,
    lw_rtca_return    TYPE bapiret2,
    lw_first_leg_rtca TYPE zscm_ctd_legupd_st.
" END: Cursor Generated Code
```

---

### 4.4 Code Change 3 of 3 – Step 4: Post-Commit Auto-Confirmation

**Location:** After `COMMIT WORK.` success-message `APPEND` in Step 3, before the closing `ENDIF.`.

```abap
" BEGIN: Cursor Generated Code - Change Document: CD:XXXXXXX  TR:XXXXXXXXX / Author:Omkar More on 02.04.2026
*&--------------------------------------------------------------------*
*& Step 4 – Post-Commit Auto-Confirmation (RTCA / RCCA)
*&--------------------------------------------------------------------*
    IF im_call_context = 'TTC'.

      CLEAR: lt_itm_date_check, lw_itm_date_check, lv_auto_rtc_ok.
      lv_auto_rtc_ok = abap_true.

      READ TABLE it_leg_update INTO lw_first_leg_rtca INDEX 1.
      IF sy-subrc = 0.
        SELECT lifnr truck_no trip_no counter
               source_date dest_date vend_source_date vend_dest_date
          FROM zsce_ctd_itm CLIENT SPECIFIED
          INTO TABLE lt_itm_date_check
         WHERE mandt    = sy-mandt
           AND lifnr    = lw_first_leg_rtca-lifnr
           AND truck_no = lw_first_leg_rtca-truck_no
           AND trip_no  = lw_first_leg_rtca-trip_no
           AND leg_type = 'E'.
        IF sy-subrc NE 0.
          lv_auto_rtc_ok = abap_false.
        ELSE.
          LOOP AT lt_itm_date_check INTO lw_itm_date_check.
            IF lw_itm_date_check-vend_source_date NE lw_itm_date_check-source_date
            OR lw_itm_date_check-vend_dest_date   NE lw_itm_date_check-dest_date.
              lv_auto_rtc_ok = abap_false.
              EXIT.
            ENDIF.
            CLEAR lw_itm_date_check.
          ENDLOOP.
        ENDIF.

        IF lv_auto_rtc_ok = abap_true.
          CLEAR lt_auto_leg_upd.
          LOOP AT lt_itm_date_check INTO lw_itm_date_check.
            lw_auto_leg_upd-lifnr    = lw_itm_date_check-lifnr.
            lw_auto_leg_upd-truck_no = lw_itm_date_check-truck_no.
            lw_auto_leg_upd-trip_no  = lw_itm_date_check-trip_no.
            lw_auto_leg_upd-counter  = lw_itm_date_check-counter.
            lw_auto_leg_upd-leg_type = 'E'.
            CLEAR: lw_auto_leg_upd-ril_remarks, lw_auto_leg_upd-ril_remarks2.
            APPEND lw_auto_leg_upd TO lt_auto_leg_upd.
            CLEAR: lw_auto_leg_upd, lw_itm_date_check.
          ENDLOOP.

          CLEAR lt_rtca_return.
          CALL FUNCTION 'Z_SCM_CTD_AUTO_TRIPCONFIRM'
            EXPORTING
              im_trip_no      = im_trip_no
              im_call_context = 'RTCA'
            TABLES
              it_leg_update   = lt_auto_leg_upd
              et_return       = lt_rtca_return
            EXCEPTIONS
              OTHERS          = 1.
          IF sy-subrc <> 0.
            CLEAR lw_return.
            lw_return-type    = 'E'.
            lw_return-message = 'Auto RTC FM call failed unexpectedly.'(060).
            APPEND lw_return TO et_return.
            CLEAR  lw_return.
          ELSE.
            LOOP AT lt_rtca_return INTO lw_rtca_return.
              APPEND lw_rtca_return TO et_return.
              CLEAR  lw_rtca_return.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSEIF im_call_context = 'TCC'.

      CLEAR: lt_itm_ctd_check, lw_itm_ctd_check, lv_auto_rcc_ok.
      lv_auto_rcc_ok = abap_true.

      READ TABLE it_leg_update INTO lw_first_leg_rtca INDEX 1.
      IF sy-subrc = 0.
        SELECT lifnr truck_no trip_no counter leg_type ctd_eligible vendor_ctd
          FROM zsce_ctd_itm CLIENT SPECIFIED
          INTO TABLE lt_itm_ctd_check
         WHERE mandt    = sy-mandt
           AND lifnr    = lw_first_leg_rtca-lifnr
           AND truck_no = lw_first_leg_rtca-truck_no
           AND trip_no  = lw_first_leg_rtca-trip_no
           AND leg_type = 'L'.
        IF sy-subrc NE 0.
          lv_auto_rcc_ok = abap_false.
        ELSE.
          LOOP AT lt_itm_ctd_check INTO lw_itm_ctd_check.
            IF lw_itm_ctd_check-vendor_ctd NE lw_itm_ctd_check-ctd_eligible.
              lv_auto_rcc_ok = abap_false.
              EXIT.
            ENDIF.
            CLEAR lw_itm_ctd_check.
          ENDLOOP.
        ENDIF.

        IF lv_auto_rcc_ok = abap_true.
          CLEAR lt_auto_leg_upd.
          LOOP AT lt_itm_ctd_check INTO lw_itm_ctd_check.
            lw_auto_leg_upd-lifnr    = lw_itm_ctd_check-lifnr.
            lw_auto_leg_upd-truck_no = lw_itm_ctd_check-truck_no.
            lw_auto_leg_upd-trip_no  = lw_itm_ctd_check-trip_no.
            lw_auto_leg_upd-counter  = lw_itm_ctd_check-counter.
            lw_auto_leg_upd-leg_type = lw_itm_ctd_check-leg_type.
            lw_auto_leg_upd-ril_ctd  = lw_itm_ctd_check-ctd_eligible.
            CLEAR: lw_auto_leg_upd-ril_remarks, lw_auto_leg_upd-ril_remarks2.
            APPEND lw_auto_leg_upd TO lt_auto_leg_upd.
            CLEAR: lw_auto_leg_upd, lw_itm_ctd_check.
          ENDLOOP.

          CLEAR lt_rtca_return.
          CALL FUNCTION 'Z_SCM_CTD_AUTO_TRIPCONFIRM'
            EXPORTING
              im_trip_no      = im_trip_no
              im_call_context = 'RCCA'
            TABLES
              it_leg_update   = lt_auto_leg_upd
              et_return       = lt_rtca_return
            EXCEPTIONS
              OTHERS          = 1.
          IF sy-subrc <> 0.
            CLEAR lw_return.
            lw_return-type    = 'E'.
            lw_return-message = 'Auto RCC FM call failed unexpectedly.'(061).
            APPEND lw_return TO et_return.
            CLEAR  lw_return.
          ELSE.
            LOOP AT lt_rtca_return INTO lw_rtca_return.
              APPEND lw_rtca_return TO et_return.
              CLEAR  lw_rtca_return.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
    " Note: Consolidated Rule Engine scheduling block (CR4) follows
    " immediately after this ENDIF — see Section 7.3 of this document.
" END: Cursor Generated Code
```

---

### 4.5 New Text Elements for Z_SCM_CTD_TRIPCONFIRM

| Text Element | Text | Change |
|---|---|---|
| `(060)` | `Auto RTC FM call failed unexpectedly.` | CR1 |
| `(061)` | `Auto RCC FM call failed unexpectedly.` | CR1 |
| `(062)` | `Rule Engine job scheduling failed.` | CR4 |
| `(063)` | `Rule Engine job could not be created.` | CR4 |

---

### 4.6 Change Summary – Z_SCM_CTD_TRIPCONFIRM

| Item | Detail |
|------|--------|
| Existing logic | **Unchanged** |
| New TYPES (CR1) | `lty_itm_date_check` (8 fields), `lty_itm_ctd_check` (7 fields) |
| New DATA (CR1) | 11 variables |
| New DATA (CR4) | 8 variables: `lrt_tripno`, `lrw_tripno`, `lv_jobname_r`, `lv_jobcount_r`, `lv_job_usr_r`, `lv_job_raw_r`, `lv_curr_status`, `lv_trip_cdate` |
| New Step 4 TTC/TCC (CR1) | ~85 lines; fires for `'TTC'` or `'TCC'` after COMMIT WORK |
| New consolidated Rule Engine block (CR4) | ~50 lines; fires for `'RTC'` or `'TTC'`; reads ZSCE_CTD_HDR to confirm status `'07'`; schedules Rule Engine job using `CREATED_DATE` as date input |
| No structural change | FM interface unchanged |

---

## 5. Change Request 2 – New FM Z_SCM_CTD_AUTO_TRIPCONFIRM

### 5.1 FM Interface

| Direction | Parameter | Type | Description |
|-----------|-----------|------|-------------|
| IMPORTING | `IM_TRIP_NO` | `ZTRIP_NO` | Trip Number (mandatory) |
| IMPORTING | `IM_CALL_CONTEXT` | `CHAR4` | `'RTCA'` or `'RCCA'` (mandatory) |
| TABLES | `IT_LEG_UPDATE` | `ZSCM_CTD_LEGUPD_TT` | Leg update records (mandatory) |
| TABLES | `ET_RETURN` | `BAPIRET2_T` | Return messages to caller |

---

### 5.2 Text Elements for Z_SCM_CTD_AUTO_TRIPCONFIRM

| Text Element | Text | Change |
|---|---|---|
| `(001)` | `Trip Number is mandatory.` | CR2 |
| `(002)` | `No leg update data provided for Auto Confirmation.` | CR2 |
| `(003)` | `Invalid call context for Auto Confirmation.` | CR2 |
| `(004)` | `Auto RTC: Empty Leg update failed for counter` | CR2 |
| `(005)` | `Auto RTC: Failed to update Trip Header.` | CR2 |
| `(006)` | `Trip Auto-Confirmed by RIL Ops successfully.` | CR2 |
| `(007)` | `Auto Confirmation: No updates applied – Rolled back.` | CR2 |
| `(008)` | `Auto RCC: Loaded Leg update failed for counter` | CR2 |
| `(009)` | `Auto RCC: OIGSV update failed for shipment` | CR2 |
| `(010)` | `Auto RCC: Failed to update Trip Header.` | CR2 |
| `(011)` | `Trip Auto CTD-Confirmed by RIL Ops successfully.` | CR2 |

---

### 5.3 Complete FM Code (Final — includes CR2 + CR4 + CR5)

```abap
FUNCTION z_scm_ctd_auto_tripconfirm.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_TRIP_NO)      TYPE  ZTRIP_NO
*"     VALUE(IM_CALL_CONTEXT) TYPE  CHAR4
*"  TABLES
*"     IT_LEG_UPDATE          TYPE  ZSCM_CTD_LEGUPD_TT
*"     ET_RETURN              TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
*& Purpose : Auto-confirmation of RIL Trip (RTCA) or CTD (RCCA).
*&           Called only from Z_SCM_CTD_TRIPCONFIRM (Step 4) after its
*&           COMMIT WORK. Manages its own LUW (second commit in chain).
*&           RCCA: Updates OIGSV SHIFT='1' (RIL_CTD='X') and
*&                 SHIFT='2' (RIL_CTD=space) for all loaded leg shipments.
*&           CR3: Sets RTCA_FLAG or RCCA_FLAG on ZSCE_CTD_HDR (trip-level).
*&           Rule Engine scheduling is handled by the calling FM
*&           (Z_SCM_CTD_TRIPCONFIRM) after this FM returns.
*& Author  : Omkar More
*& Created : 02.04.2026
*& Change History:
*& Date         User         Description
*& 02.04.2026   Omkar More   Initial creation (CD:XXXXXXX TR:XXXXXXXXX)
*"----------------------------------------------------------------------

" BEGIN: Cursor Generated Code - Change Document: CD:XXXXXXX  TR:XXXXXXXXX / Author:Omkar More on 02.04.2026

  TYPES:
    " Structure for OIGSV key collection (RCCA only)
    " SELECT fields: lifnr truck_no trip_no counter shnumber ril_ctd
    BEGIN OF lty_oigsv_check,
      lifnr    TYPE lifnr,
      truck_no TYPE ytruck_no,
      trip_no  TYPE ztrip_no,
      counter  TYPE zcounter,
      shnumber TYPE oig_shnum,
      ril_ctd  TYPE flag,
    END OF lty_oigsv_check.

  DATA:
    lw_upd            TYPE zscm_ctd_legupd_st,
    lw_return         TYPE bapiret2,
    lv_error          TYPE abap_bool VALUE abap_false,
    lv_item_upd_flag  TYPE flag,
    lv_hdr_upd_flag   TYPE flag,
    lv_oigsv_upd_flag TYPE flag,
    lv_ril_remarks    TYPE zmsg,
    lv_ril_remarks2   TYPE zmsg,
    lv_date_c         TYPE char10,
    lv_new_status     TYPE zctd_trip_st,
    lw_first_leg      TYPE zscm_ctd_legupd_st,
    lt_oigsv_check    TYPE TABLE OF lty_oigsv_check,
    lw_oigsv_check    TYPE lty_oigsv_check,
    lt_leg_upd_sort   TYPE zscm_ctd_legupd_tt,
    lv_auto_user_raw  TYPE textr,
    lv_auto_user      TYPE xubname,
    " CR3: Header-level auto-confirmation audit flags
    lv_rtca_flag      TYPE flag,
    lv_rcca_flag      TYPE flag,
    " CR5: SHIFT value derived per shipment from RIL_CTD
    lv_oigsv_shift    TYPE char1.

  REFRESH et_return.

*&--------------------------------------------------------------------*
*& Step 0 – Mandatory Input Checks
*&--------------------------------------------------------------------*
  IF im_trip_no IS INITIAL.
    CLEAR lw_return.
    lw_return-type    = 'E'.
    lw_return-message = 'Trip Number is mandatory.'(001).
    APPEND lw_return TO et_return.
    RETURN.
  ENDIF.

  IF im_call_context NE 'RTCA' AND im_call_context NE 'RCCA'.
    CLEAR lw_return.
    lw_return-type    = 'E'.
    lw_return-message = 'Invalid call context for Auto Confirmation.'(003).
    APPEND lw_return TO et_return.
    RETURN.
  ENDIF.

  IF it_leg_update IS INITIAL.
    CLEAR lw_return.
    lw_return-type    = 'E'.
    lw_return-message = 'No leg update data provided for Auto Confirmation.'(002).
    APPEND lw_return TO et_return.
    RETURN.
  ENDIF.

*&--------------------------------------------------------------------*
*& Read auto-confirmation user ID from ZLOG_EXEC_VAR
*&   Parameter: ZCTD_RILOPS_AUTO_USER / NUMB: 0001 / Field: REMARKS
*&   Falls back to sy-uname if not maintained.
*&--------------------------------------------------------------------*
  CLEAR: lv_auto_user_raw, lv_auto_user.
  SELECT SINGLE remarks
    FROM zlog_exec_var CLIENT SPECIFIED
    INTO lv_auto_user_raw
   WHERE mandt = sy-mandt
     AND name  = 'ZCTD_RILOPS_AUTO_USER'
     AND numb  = '0001'.
  IF sy-subrc = 0 AND lv_auto_user_raw IS NOT INITIAL.
    lv_auto_user = lv_auto_user_raw.
  ELSE.
    lv_auto_user = sy-uname.
  ENDIF.

*&--------------------------------------------------------------------*
*& Determine target status and remark strings
*&--------------------------------------------------------------------*
  CASE im_call_context.
    WHEN 'RTCA'.
      lv_new_status = '07'.
      WRITE sy-datum TO lv_date_c.
      CONCATENATE lv_date_c ':' 'RTCA' '-' 'RIL Ops - Trip Auto Approved'
        INTO lv_ril_remarks SEPARATED BY space.

    WHEN 'RCCA'.
      lv_new_status = '10'.
      WRITE sy-datum TO lv_date_c.
      CONCATENATE lv_date_c ':' 'RCCA' '-' 'RIL Ops - CTD Auto Approved'
        INTO lv_ril_remarks2 SEPARATED BY space.
  ENDCASE.

*&--------------------------------------------------------------------*
*& Step 1 – Update ZSCE_CTD_ITM (Item Records)
*&--------------------------------------------------------------------*
  CASE im_call_context.

    WHEN 'RTCA'.
      LOOP AT it_leg_update INTO lw_upd.
        UPDATE zsce_ctd_itm CLIENT SPECIFIED
          SET   ril_remarks   = lv_ril_remarks
                modified_by   = lv_auto_user
                modified_date = sy-datum
                modified_time = sy-uzeit
          WHERE mandt    = sy-mandt
            AND lifnr    = lw_upd-lifnr
            AND truck_no = lw_upd-truck_no
            AND trip_no  = lw_upd-trip_no
            AND counter  = lw_upd-counter.
        IF sy-subrc NE 0.
          CLEAR lw_return.
          lw_return-type = 'E'.
          CONCATENATE 'Auto RTC: Empty Leg update failed for counter'(004) lw_upd-counter
            INTO lw_return-message SEPARATED BY space.
          APPEND lw_return TO et_return.
          CLEAR  lw_return.
          lv_error = abap_true.
        ELSE.
          lv_item_upd_flag = abap_true.
        ENDIF.
        IF lv_error = abap_true.
          ROLLBACK WORK.
          RETURN.
        ENDIF.
        CLEAR lw_upd.
      ENDLOOP.

    WHEN 'RCCA'.
      LOOP AT it_leg_update INTO lw_upd.
        UPDATE zsce_ctd_itm CLIENT SPECIFIED
          SET   ril_ctd       = lw_upd-ril_ctd
                ril_remarks2  = lv_ril_remarks2
                modified_by   = lv_auto_user
                modified_date = sy-datum
                modified_time = sy-uzeit
          WHERE mandt    = sy-mandt
            AND lifnr    = lw_upd-lifnr
            AND truck_no = lw_upd-truck_no
            AND trip_no  = lw_upd-trip_no
            AND counter  = lw_upd-counter.
        IF sy-subrc NE 0.
          CLEAR lw_return.
          lw_return-type = 'E'.
          CONCATENATE 'Auto RCC: Loaded Leg update failed for counter'(008) lw_upd-counter
            INTO lw_return-message SEPARATED BY space.
          APPEND lw_return TO et_return.
          CLEAR  lw_return.
          lv_error = abap_true.
        ELSE.
          lv_item_upd_flag = abap_true.
        ENDIF.
        IF lv_error = abap_true.
          ROLLBACK WORK.
          RETURN.
        ENDIF.
        CLEAR lw_upd.
      ENDLOOP.

  ENDCASE.

*&--------------------------------------------------------------------*
*& Step 1B – OIGSV Update (RCCA only)
*&   Fetches SHNUMBER + RIL_CTD from ZSCE_CTD_ITM (post-step-1 state)
*&   for all loaded legs passed by caller.
*&   CR5: Two SHIFT values stamped based on RIL_CTD outcome:
*&     SHIFT = '1'  where RIL_CTD = 'X'     (CTD confirmed for shipment)
*&     SHIFT = '2'  where RIL_CTD = space    (CTD not applicable)
*&   Rows with no shipment number are skipped.
*&   Non-blocking: OIGSV failure logged but does not rollback item/header.
*&--------------------------------------------------------------------*
  IF im_call_context = 'RCCA'.

    REFRESH lt_leg_upd_sort.
    lt_leg_upd_sort = it_leg_update.
    SORT lt_leg_upd_sort BY lifnr truck_no trip_no counter.
    DELETE ADJACENT DUPLICATES FROM lt_leg_upd_sort
      COMPARING lifnr truck_no trip_no counter.

    IF lt_leg_upd_sort IS NOT INITIAL.
      SELECT lifnr truck_no trip_no counter shnumber ril_ctd
        FROM zsce_ctd_itm CLIENT SPECIFIED
        INTO TABLE lt_oigsv_check
        FOR ALL ENTRIES IN lt_leg_upd_sort
        WHERE mandt    = sy-mandt
          AND lifnr    = lt_leg_upd_sort-lifnr
          AND truck_no = lt_leg_upd_sort-truck_no
          AND trip_no  = lt_leg_upd_sort-trip_no
          AND counter  = lt_leg_upd_sort-counter.
      IF sy-subrc = 0.
        " Skip legs that have no shipment number — nothing to update in OIGSV
        DELETE lt_oigsv_check WHERE shnumber IS INITIAL.

        LOOP AT lt_oigsv_check INTO lw_oigsv_check.
          " CR5: Derive SHIFT value from RIL_CTD
          "   SHIFT = '1'  →  CTD confirmed  (RIL_CTD = 'X')
          "   SHIFT = '2'  →  CTD not applicable  (RIL_CTD = space)
          IF lw_oigsv_check-ril_ctd = 'X'.
            lv_oigsv_shift = '1'.
          ELSE.
            lv_oigsv_shift = '2'.
          ENDIF.
          UPDATE oigsv CLIENT SPECIFIED
            SET   shift    = lv_oigsv_shift
                  cha_date = sy-datum
                  cha_name = lv_auto_user
                  cha_time = sy-uzeit
            WHERE client   = sy-mandt
              AND shnumber = lw_oigsv_check-shnumber.
          IF sy-subrc EQ 0 OR sy-dbcnt GE 1.
            lv_oigsv_upd_flag = abap_true.
          ELSE.
            CLEAR lw_return.
            lw_return-type = 'E'.
            CONCATENATE 'Auto RCC: OIGSV update failed for shipment'(009) lw_oigsv_check-shnumber
              INTO lw_return-message SEPARATED BY space.
            APPEND lw_return TO et_return.
            CLEAR  lw_return.
          ENDIF.
          CLEAR: lw_oigsv_check, lv_oigsv_shift.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDIF.

*&--------------------------------------------------------------------*
*& Step 2 – Update ZSCE_CTD_HDR (Trip Header Status + Audit Flag)
*&   CR3: RTCA_FLAG / RCCA_FLAG set at header level (trip-level event).
*&   Only one flag is set per call; the other remains space.
*&--------------------------------------------------------------------*
  " CR3: Derive header audit flag before UPDATE
  CLEAR: lv_rtca_flag, lv_rcca_flag.
  IF im_call_context = 'RTCA'.
    lv_rtca_flag = 'X'.
  ELSEIF im_call_context = 'RCCA'.
    lv_rcca_flag = 'X'.
  ENDIF.

  READ TABLE it_leg_update INTO lw_first_leg INDEX 1.
  IF sy-subrc = 0.
    UPDATE zsce_ctd_hdr CLIENT SPECIFIED
      SET   trip_status   = lv_new_status
            rtca_flag     = lv_rtca_flag
            rcca_flag     = lv_rcca_flag
            modified_by   = lv_auto_user
            modified_date = sy-datum
            modified_time = sy-uzeit
      WHERE mandt    = sy-mandt
        AND lifnr    = lw_first_leg-lifnr
        AND truck_no = lw_first_leg-truck_no
        AND trip_no  = lw_first_leg-trip_no.
    IF sy-subrc NE 0.
      CLEAR lw_return.
      CASE im_call_context.
        WHEN 'RTCA'.
          lw_return-message = 'Auto RTC: Failed to update Trip Header.'(005).
        WHEN 'RCCA'.
          lw_return-message = 'Auto RCC: Failed to update Trip Header.'(010).
      ENDCASE.
      lw_return-type = 'E'.
      APPEND lw_return TO et_return.
      CLEAR  lw_return.
      ROLLBACK WORK.
      RETURN.
    ELSE.
      lv_hdr_upd_flag = abap_true.
    ENDIF.
  ENDIF.

*&--------------------------------------------------------------------*
*& Step 3 – Commit or Rollback
*&--------------------------------------------------------------------*
  IF lv_item_upd_flag IS NOT INITIAL
  OR lv_hdr_upd_flag  IS NOT INITIAL
  OR lv_oigsv_upd_flag IS NOT INITIAL.
    COMMIT WORK.
    CLEAR lw_return.
    CASE im_call_context.
      WHEN 'RTCA'.
        lw_return-message = 'Trip Auto-Confirmed by RIL Ops successfully.'(006).
      WHEN 'RCCA'.
        lw_return-message = 'Trip Auto CTD-Confirmed by RIL Ops successfully.'(011).
    ENDCASE.
    lw_return-type = 'S'.
    APPEND lw_return TO et_return.
    CLEAR  lw_return.
    " Rule Engine scheduling is handled by Z_SCM_CTD_TRIPCONFIRM
    " (caller) after this FM returns — see CR4 in that FM.

  ELSE.
    ROLLBACK WORK.
    CLEAR lw_return.
    lw_return-type    = 'W'.
    lw_return-message = 'Auto Confirmation: No updates applied – Rolled back.'(007).
    APPEND lw_return TO et_return.
    CLEAR  lw_return.
  ENDIF.

" END: Cursor Generated Code

ENDFUNCTION.
```

---

### 5.4 Change Summary – Z_SCM_CTD_AUTO_TRIPCONFIRM

| Item | Detail |
|------|--------|
| Object type | New Function Module |
| Function group | Same as Z_SCM_CTD_TRIPCONFIRM |
| Interface | 2 importing, 2 tables |
| TYPES | `lty_oigsv_check` (6 fields) |
| DATA (CR2) | 17 variables incl. `lv_auto_user_raw`, `lv_auto_user` |
| DATA (CR3) | `lv_rtca_flag TYPE flag`, `lv_rcca_flag TYPE flag` |
| DATA (CR5) | `lv_oigsv_shift TYPE char1` |
| DB reads | `ZLOG_EXEC_VAR` (SELECT SINGLE for auto-user ID) |
| DB updates | `ZSCE_CTD_ITM` (item — `RIL_REMARKS` / `RIL_CTD` only; no flags), `ZSCE_CTD_HDR` (status + `RTCA_FLAG` or `RCCA_FLAG`), `OIGSV` (RCCA: `SHIFT='1'` or `SHIFT='2'`) |
| OIGSV logic (CR5) | All loaded legs with a shipment: `SHIFT='1'` if `RIL_CTD='X'`; `SHIFT='2'` if `RIL_CTD=space` |
| Audit flags (CR3) | `RTCA_FLAG` / `RCCA_FLAG` stamped on `ZSCE_CTD_HDR` in Step 2 UPDATE (trip-level) |
| Background job | **Not scheduled here** — Rule Engine scheduling handled entirely by `Z_SCM_CTD_TRIPCONFIRM` (CR4) after this FM returns |
| LUW | Own COMMIT WORK / ROLLBACK WORK (2nd LUW in chain) |

---

## 6. Change Request 3 – Dictionary Change ZSCE_CTD_HDR

### 6.1 Overview

The two audit flags are added to the **trip header table `ZSCE_CTD_HDR`** because auto-confirmation (RTCA/RCCA) is a **trip-level event** — the condition is evaluated across all legs of the trip and, if satisfied, the entire trip is auto-confirmed. A single flag on the header is therefore the correct granularity and avoids redundant stamping on every leg.

| Object | Change | Transaction |
|--------|--------|-------------|
| `ZSCE_CTD_HDR` | Add 2 new fields | SE11 → Table Maintenance |

---

### 6.2 New Fields

| Field Name | Data Element | Type | Length | Description |
|---|---|---|---|---|
| `RTCA_FLAG` | `FLAG` | CHAR | 1 | `'X'` if this trip was auto-confirmed via RTCA. Set once — never reset. |
| `RCCA_FLAG` | `FLAG` | CHAR | 1 | `'X'` if this trip was auto-CTD-confirmed via RCCA. Set once — never reset. |

> Place both fields after the existing `TRIP_STATUS` field (or at the end of the table — confirm with the current field layout in SE11). Both fields use the standard `FLAG` data element (CHAR 1); no new data elements need to be created.

---

### 6.3 Impact Analysis

| Object | Impact | Action Required |
|--------|--------|-----------------|
| `ZSCE_CTD_HDR` | Add 2 CHAR(1) fields | SE11 change + activate + table conversion |
| `Z_SCM_CTD_AUTO_TRIPCONFIRM` (new FM) | SET new flags in Step 2 `UPDATE zsce_ctd_hdr` | Covered in Section 5.3 Step 2 code |
| `Z_SCM_CTD_TRIPCONFIRM` (existing FM) | No change | New fields default to space; existing SELECT lists explicit — no impact |
| `ZSCE_CTD_ITM` | **No change** | Flags are NOT added to the item table |
| Other programs reading `ZSCE_CTD_HDR` | No change | Explicit field lists everywhere — new fields ignored |
| UI / Portal | No change at this time | Header flags are backend audit markers |

---

### 6.4 Where the Flags are Set

Both flags are set exclusively in `Z_SCM_CTD_AUTO_TRIPCONFIRM` (CR2) — in **Step 2** `UPDATE zsce_ctd_hdr`, within the same statement that sets `TRIP_STATUS`:

| Context | Field Set | Value | Table |
|---------|-----------|-------|-------|
| `RTCA` | `RTCA_FLAG` | `'X'` | `ZSCE_CTD_HDR` |
| `RCCA` | `RCCA_FLAG` | `'X'` | `ZSCE_CTD_HDR` |

Only one flag is set per call; the other is explicitly cleared to space (`lv_rtca_flag` / `lv_rcca_flag`). The flag is part of the same `UPDATE` as `TRIP_STATUS`, so it is atomic with the status change — if the header UPDATE fails and `ROLLBACK WORK` is called, neither the status nor the flag is persisted.

---

### 6.5 SE11 Steps (for ABAP Developer)

1. Open `ZSCE_CTD_HDR` in **SE11** (Table Maintenance)
2. Append field `RTCA_FLAG`:
   - Data element: `FLAG`
   - Short description: `Auto Trip Conf Flag (RTCA)`
3. Append field `RCCA_FLAG` after `RTCA_FLAG`:
   - Data element: `FLAG`
   - Short description: `Auto CTD Conf Flag (RCCA)`
4. Save → Activate → Run table conversion (no data loss; new CHAR fields default to space)
5. Include the SE11 change in Transport 1; it must be imported before the FM transports

---

## 7. Change Request 4 – CTD Rule Engine Scheduling

### 7.1 Overview

| Change | Object | Description |
|--------|--------|-------------|
| Code Change 1 of 2 | `Z_SCM_CTD_TRIPCONFIRM` | Add 8 new DATA variables for Rule Engine scheduling |
| Code Change 2 of 2 | `Z_SCM_CTD_TRIPCONFIRM` | Add consolidated Rule Engine block **after** Step 4 IF/ELSEIF/ENDIF chain |

> **Design Principle:** Rule Engine scheduling is handled **entirely in `Z_SCM_CTD_TRIPCONFIRM`** for both Manual RTC and Auto RTCA paths. The New FM (`Z_SCM_CTD_AUTO_TRIPCONFIRM`) has no scheduling responsibility. A fresh `SELECT SINGLE` from `ZSCE_CTD_HDR` confirms that status `'07'` is genuinely committed before the job is created — this acts as a common gate for both paths.

| Scenario | Who commits '07'? | Status read from DB | Job scheduled? |
|---|---|---|---|
| Manual RTC | Main FM Step 3 COMMIT WORK | `'07'` ✓ | Yes |
| Auto RTCA (dates match) | New FM Step 3 COMMIT WORK | `'07'` ✓ | Yes |
| Auto RTCA fails | Nobody | `'05'` | No — gate prevents scheduling |
| TCC / RCC / other | N/A | Not `'07'` | No — context filter prevents scheduling |

---

### 7.2 Code Change 1 of 2 – New DATA Declarations

**Location:** Insert immediately after the CR1 DATA block (Section 4.3).

```abap
" BEGIN: Cursor Generated Code - Change Document: CD:XXXXXXX  TR:XXXXXXXXX / Author:Omkar More on 02.04.2026
  " CR4: Consolidated Rule Engine scheduling (RTC and RTCA paths)
  DATA:
    lrt_tripno     TYPE RANGE OF ztrip_no,
    lrw_tripno     LIKE LINE OF lrt_tripno,
    lv_jobname_r   TYPE btcjob,
    lv_jobcount_r  TYPE btcjobcnt,
    lv_job_usr_r   TYPE xubname,
    lv_job_raw_r   TYPE textr,
    " Fresh DB read — confirm status '07' and obtain CREATED_DATE for date input
    lv_curr_status TYPE zctd_trip_st,
    lv_trip_cdate  TYPE zsce_ctd_hdr-created_date.
" END: Cursor Generated Code
```

---

### 7.3 Code Change 2 of 2 – Consolidated Rule Engine Block

**Location:** Insert as a **new Cursor Generated Code block** immediately **after** the closing `ENDIF.` of the Step 4 `IF im_call_context = 'TTC' ... ELSEIF im_call_context = 'TCC' ... ENDIF.` block (i.e., as a separate block appended after the Step 4 code, still inside the commit-success `IF sy-subrc EQ 0` branch).

```abap
" BEGIN: Cursor Generated Code - Change Document: CD:XXXXXXX  TR:XXXXXXXXX / Author:Omkar More on 02.04.2026
*&--------------------------------------------------------------------*
*& Step 4 Extension – Consolidated Rule Engine Scheduling (CR4)
*&   Fires for IM_CALL_CONTEXT = 'RTC' (manual) and 'TTC' (RTCA path).
*&   A fresh SELECT from ZSCE_CTD_HDR confirms TRIP_STATUS = '07'
*&   before scheduling — this is the single gate for both scenarios:
*&     RTC:  status '07' committed by Step 3 COMMIT WORK (this FM).
*&     RTCA: status '07' committed by Z_SCM_CTD_AUTO_TRIPCONFIRM
*&           which has already returned before this point.
*&   CREATED_DATE is used as p_fdate and p_tdate so the Rule Engine
*&   targets exactly this trip without loading unrelated data.
*&--------------------------------------------------------------------*
    IF im_call_context = 'RTC' OR im_call_context = 'TTC'.

      CLEAR: lv_curr_status, lv_trip_cdate.
      READ TABLE it_leg_update INTO lw_first_leg_rtca INDEX 1.
      IF sy-subrc = 0.
        SELECT SINGLE trip_status created_date
          FROM zsce_ctd_hdr CLIENT SPECIFIED
          INTO (lv_curr_status, lv_trip_cdate)
          WHERE mandt    = sy-mandt
            AND lifnr    = lw_first_leg_rtca-lifnr
            AND truck_no = lw_first_leg_rtca-truck_no
            AND trip_no  = im_trip_no.
        IF sy-subrc = 0 AND lv_curr_status = '07'.
          " Status confirmed '07' — schedule Rule Engine immediately
          CLEAR: lv_job_raw_r, lv_job_usr_r.
          SELECT SINGLE remarks
            FROM zlog_exec_var CLIENT SPECIFIED
            INTO lv_job_raw_r
            WHERE mandt = sy-mandt
              AND name  = 'ZCTD_RILOPS_AUTO_USER'
              AND numb  = '0001'.
          IF sy-subrc = 0 AND lv_job_raw_r IS NOT INITIAL.
            lv_job_usr_r = lv_job_raw_r.
          ELSE.
            lv_job_usr_r = sy-uname.
          ENDIF.

          CLEAR: lrt_tripno, lrw_tripno.
          lrw_tripno-sign   = 'I'.
          lrw_tripno-option = 'EQ'.
          lrw_tripno-low    = im_trip_no.
          APPEND lrw_tripno TO lrt_tripno.

          CLEAR: lv_jobname_r, lv_jobcount_r.
          lv_jobname_r = 'ZCTD_RULEENG_AUTO'.
          CALL FUNCTION 'JOB_OPEN'
            EXPORTING
              jobname   = lv_jobname_r
              sdlstrtdt = sy-datum
              sdlstrttm = sy-uzeit
            IMPORTING
              jobcount  = lv_jobcount_r
            EXCEPTIONS
              OTHERS    = 1.
          IF sy-subrc = 0.
            SUBMIT zctd_ruleeng_exec
              WITH p_fdate  = lv_trip_cdate
              WITH p_tdate  = lv_trip_cdate
              WITH s_tripno IN lrt_tripno
              WITH p_simul  = space
              USER          lv_job_usr_r
              VIA JOB       lv_jobname_r
              NUMBER        lv_jobcount_r
              AND RETURN.
            CALL FUNCTION 'JOB_CLOSE'
              EXPORTING
                jobcount  = lv_jobcount_r
                jobname   = lv_jobname_r
                strtimmed = 'X'
              EXCEPTIONS
                OTHERS    = 1.
            IF sy-subrc <> 0.
              CLEAR lw_return.
              lw_return-type    = 'W'.
              lw_return-message = 'Rule Engine job scheduling failed.'(062).
              APPEND lw_return TO et_return.
              CLEAR  lw_return.
            ENDIF.
          ELSE.
            CLEAR lw_return.
            lw_return-type    = 'W'.
            lw_return-message = 'Rule Engine job could not be created.'(063).
            APPEND lw_return TO et_return.
            CLEAR  lw_return.
          ENDIF.
        ENDIF.  " lv_curr_status = '07'
      ENDIF.  " it_leg_update not empty
    ENDIF.  " im_call_context = 'RTC' OR 'TTC'
" END: Cursor Generated Code
```

---

### 7.4 Performance Considerations

| Risk | Mitigation |
|---|---|
| Multiple jobs for batch confirmation | UI limit of 10 trips per session |
| Rule Engine reading all status-07 trips | Mitigated by `ZCTD_RULEENG_EXEC` SELECT optimisation (separate CD) |
| Dialog timeout | ~50–100 ms overhead per trip for DB read + 3 FM calls; non-blocking on failure |
| Redundant DB read | The `SELECT SINGLE` on `ZSCE_CTD_HDR` is a single-key read (primary key lookup) — negligible overhead |

---

## 8. Change Request 5 – OIGSV SHIFT Update for Non-CTD Legs

### 8.1 Overview

| Item | Detail |
|---|---|
| Object changed | `Z_SCM_CTD_AUTO_TRIPCONFIRM` — Step 1B (OIGSV update, RCCA path only) |
| Change type | Extend OIGSV update to stamp `SHIFT = '2'` for loaded legs where `RIL_CTD = space` |
| Impact on other steps | None — Steps 0, 1, 2, 3 and the RTCA path are entirely unaffected |

---

### 8.2 Business Rule

| `RIL_CTD` on loaded leg | `OIGSV.SHIFT` to set |
|---|---|
| `'X'` — CTD confirmed | `'1'` |
| `space` — CTD not applicable | `'2'` |
| `shnumber` is blank | Skip — no OIGSV row to update |

Both outcomes are part of the **same RCCA COMMIT WORK** (LUW-2), so the entire OIGSV stamp is atomic with the item and header updates.

---

### 8.3 Specific Code Changes in Z_SCM_CTD_AUTO_TRIPCONFIRM

#### Change A – New DATA declaration

**Add** the following variable to the DATA block (already reflected in Section 5.3):

```abap
lv_oigsv_shift    TYPE char1,    " CR5: SHIFT value derived per shipment
```

#### Change B – Step 1B DELETE statement

**Before (V3):**
```abap
DELETE lt_oigsv_check WHERE shnumber IS INITIAL OR ril_ctd NE 'X'.
```

**After (V4 / CR5):**
```abap
" Skip only legs with no shipment; retain both ril_ctd='X' and ril_ctd=space
DELETE lt_oigsv_check WHERE shnumber IS INITIAL.
```

#### Change C – Step 1B LOOP body: derive SHIFT before UPDATE

**Before (V3):**
```abap
LOOP AT lt_oigsv_check INTO lw_oigsv_check.
  UPDATE oigsv CLIENT SPECIFIED
    SET   shift    = '1'
          cha_date = sy-datum
          cha_name = lv_auto_user
          cha_time = sy-uzeit
    WHERE client   = sy-mandt
      AND shnumber = lw_oigsv_check-shnumber.
  ...
  CLEAR lw_oigsv_check.
ENDLOOP.
```

**After (V4 / CR5):**
```abap
LOOP AT lt_oigsv_check INTO lw_oigsv_check.
  " CR5: Set SHIFT based on RIL_CTD value for this shipment
  IF lw_oigsv_check-ril_ctd = 'X'.
    lv_oigsv_shift = '1'.
  ELSE.
    lv_oigsv_shift = '2'.
  ENDIF.
  UPDATE oigsv CLIENT SPECIFIED
    SET   shift    = lv_oigsv_shift
          cha_date = sy-datum
          cha_name = lv_auto_user
          cha_time = sy-uzeit
    WHERE client   = sy-mandt
      AND shnumber = lw_oigsv_check-shnumber.
  ...
  CLEAR: lw_oigsv_check, lv_oigsv_shift.
ENDLOOP.
```

---

### 8.4 No New Text Elements Required

The existing error message `(009)` — *`Auto RCC: OIGSV update failed for shipment`* — covers both SHIFT='1' and SHIFT='2' update failures generically. No additional text elements are needed for CR5.

---

## 9. Transport Sequence

| Step | Object | Type | Description | Change |
|------|--------|------|-------------|--------|
| 1 | `ZSCE_CTD_HDR` | SE11 Table | Add `RTCA_FLAG` + `RCCA_FLAG` to header table; activate + table conversion | CR3 |
| 2 | `Z_SCM_CTD_AUTO_TRIPCONFIRM` | Function Module | Create new FM (all CR2, CR4, CR5 changes included) | CR2+CR4+CR5 |
| 3 | `Z_SCM_CTD_TRIPCONFIRM` | Function Module | Apply CR1 changes + CR4 changes (DATA + Step 4) | CR1+CR4 |

> **Step 1** must be a separate, earlier transport released and imported first.
> **Steps 2 and 3** may share a transport if both FMs are active before release.
> **`ZCTD_RULEENG_EXEC`** is called but NOT modified in this CD — see separate optimisation CD.

---

## 10. End-to-End Process Flow

### 10.1 Manual RTC → Rule Engine

```
RIL Ops confirms trip on Screen 2 (RTC)
  Z_SCM_CTD_TRIPCONFIRM (IM_CALL_CONTEXT = 'RTC')
    Step 3: COMMIT WORK  [LUW-1 — status '07' persisted]
    Step 4 (CR4 consolidated block — fires for 'RTC' OR 'TTC'):
      SELECT SINGLE trip_status, created_date FROM ZSCE_CTD_HDR → status = '07' ✓
      Read ZLOG_EXEC_VAR → lv_job_usr_r
      JOB_OPEN → SUBMIT zctd_ruleeng_exec
                   p_fdate = created_date, p_tdate = created_date, s_tripno = trip
               → JOB_CLOSE (STRTIMMED = 'X')
        Background job ZCTD_RULEENG_AUTO starts immediately
        Trip status → '08'; trip visible on Transporter CTD screen
```

### 10.2 RTCA → Rule Engine

```
Transporter confirms trip on Screen 2 (TTC)
  Z_SCM_CTD_TRIPCONFIRM (IM_CALL_CONTEXT = 'TTC')
    Step 3: COMMIT WORK  [LUW-1 — status '05' persisted]
    Step 4 (TTC): All empty legs: VEND dates = system dates?
      YES → CALL Z_SCM_CTD_AUTO_TRIPCONFIRM (RTCA)
        Step 1: UPDATE ZSCE_CTD_ITM — RIL_REMARKS
        Step 2: UPDATE ZSCE_CTD_HDR — status '07', RTCA_FLAG = 'X'
        Step 3: COMMIT WORK  [LUW-2 — status '07' persisted; FM returns]
    Step 4 (CR4 consolidated block — fires for 'RTC' OR 'TTC'):
      SELECT SINGLE trip_status, created_date FROM ZSCE_CTD_HDR → status = '07' ✓
      Read ZLOG_EXEC_VAR → lv_job_usr_r
      JOB_OPEN → SUBMIT zctd_ruleeng_exec
                   p_fdate = created_date, p_tdate = created_date, s_tripno = trip
               → JOB_CLOSE (STRTIMMED = 'X')
        Background job ZCTD_RULEENG_AUTO starts immediately
        Trip status → '08'; trip visible on Transporter CTD screen
    (If RTCA failed → DB still shows status '05' → gate check fails → job NOT scheduled ✓)
```

### 10.3 RCCA (with SHIFT update)

```
Transporter confirms CTD on Screen 3 (TCC)
  Z_SCM_CTD_TRIPCONFIRM (IM_CALL_CONTEXT = 'TCC')
    Step 3: COMMIT WORK  [LUW-1 — status '08' persisted]
    Step 4 (TCC): All loaded legs: VENDOR_CTD = CTD_ELIGIBLE?
      YES → CALL Z_SCM_CTD_AUTO_TRIPCONFIRM (RCCA)
        Step 1: UPDATE ZSCE_CTD_ITM — RIL_CTD, RIL_REMARKS2
        Step 1B: SELECT shnumber + ril_ctd from ZSCE_CTD_ITM
                 For each shipment:
                   RIL_CTD = 'X'     → UPDATE OIGSV SET shift = '1'
                   RIL_CTD = space   → UPDATE OIGSV SET shift = '2'
                   shnumber blank    → skip
        Step 2: UPDATE ZSCE_CTD_HDR — status '10', RCCA_FLAG = 'X' (trip-level audit)
        Step 3: COMMIT WORK  [LUW-2 — all updates atomic]
```

### 10.4 Trip Status Transitions

| Context | LUW-1 result | LUW-2 result | Rule Engine (async) |
|---------|-------------|-------------|---------------------|
| Manual RTC | `'07'` | — | `'08'` |
| TTC + RTCA (dates match) | `'05'` | `'07'` | `'08'` |
| TTC (no RTCA — mismatch) | `'05'` | — | — |
| TCC + RCCA (CTD matches) | `'08'` | `'10'` | — |
| TCC (no RCCA — mismatch) | `'08'` | — | — |

---

## 11. ABAP Team – Implementation Summary

> Quick reference for developers implementing all five change requests. Work through the checklist in order.

---

### 11.1 Objects to Change / Create

| # | Object | SE | Action |
|---|--------|-----|--------|
| 1 | `ZSCE_CTD_HDR` | SE11 | Add 2 fields (`RTCA_FLAG`, `RCCA_FLAG`); activate; table conversion |
| 2 | `Z_SCM_CTD_AUTO_TRIPCONFIRM` | SE37 | Create new FM (complete code in Section 5.3) |
| 3 | `Z_SCM_CTD_TRIPCONFIRM` | SE37 | Insert 4 code blocks (Sections 4.2, 4.3, 4.4, 7.2–7.3) |

---

### 11.2 SE11 – ZSCE_CTD_HDR (Transport 1)

> Fields go on the **header** table, not the item table.

| Step | Action |
|------|--------|
| 1 | SE11 → `ZSCE_CTD_HDR` → Change |
| 2 | Append field `RTCA_FLAG` — Data Element: `FLAG` — Text: `Auto Trip Conf Flag (RTCA)` |
| 3 | Append field `RCCA_FLAG` after `RTCA_FLAG` — Data Element: `FLAG` — Text: `Auto CTD Conf Flag (RCCA)` |
| 4 | Save → Activate → Table Conversion (no data loss; CHAR fields default to space) |
| 5 | Add to Transport 1; release Transport 1 first |

---

### 11.3 SE37 – Z_SCM_CTD_AUTO_TRIPCONFIRM (Transport 2 — Create New)

**Step 1:** Create FM in SE37 in the same function group as `Z_SCM_CTD_TRIPCONFIRM`.

**Step 2:** Copy the complete FM code from **Section 5.3** verbatim.

**Step 3:** Maintain text elements (SE37 → Goto → Text Elements):

| No. | Text |
|-----|------|
| 001 | `Trip Number is mandatory.` |
| 002 | `No leg update data provided for Auto Confirmation.` |
| 003 | `Invalid call context for Auto Confirmation.` |
| 004 | `Auto RTC: Empty Leg update failed for counter` |
| 005 | `Auto RTC: Failed to update Trip Header.` |
| 006 | `Trip Auto-Confirmed by RIL Ops successfully.` |
| 007 | `Auto Confirmation: No updates applied – Rolled back.` |
| 008 | `Auto RCC: Loaded Leg update failed for counter` |
| 009 | `Auto RCC: OIGSV update failed for shipment` |
| 010 | `Auto RCC: Failed to update Trip Header.` |
| 011 | `Trip Auto CTD-Confirmed by RIL Ops successfully.` |

**Step 4:** Activate FM → Add to Transport 2.

---

### 11.4 SE37 – Z_SCM_CTD_TRIPCONFIRM (Transport 2 — Modify Existing)

Four code insertions required. All inside `Cursor Generated Code` markers with CD:XXXXXXX TR:XXXXXXXXX.

| Insertion | Where | What (Section reference) |
|---|---|---|
| **1 of 4** — New TYPES | After last existing `END OF lty_...` | Section 4.2 — `lty_itm_date_check`, `lty_itm_ctd_check` |
| **2 of 4** — New DATA (CR1) | After last existing DATA variable | Section 4.3 — 11 variables for RTCA/RCCA |
| **3 of 4** — Step 4 TTC/TCC (CR1) | After COMMIT WORK + success message APPEND in Step 3 | Section 4.4 — RTCA/RCCA validation + CALL FUNCTION |
| **4 of 4** — New DATA + consolidated Rule Engine block (CR4) | DATA: after CR1 DATA block; Rule Engine block: as new Cursor block after Step 4 ENDIF | Section 7.2 — 8 new variables; Section 7.3 — consolidated scheduling block for both RTC and RTCA |

**Text Elements** (verify highest existing number before assigning — `060–063` suggested):

| No. | Text |
|-----|------|
| 060 | `Auto RTC FM call failed unexpectedly.` |
| 061 | `Auto RCC FM call failed unexpectedly.` |
| 062 | `Rule Engine job scheduling failed.` |
| 063 | `Rule Engine job could not be created.` |

**Activate FM → Add to Transport 2.**

---

### 11.5 SE16 – ZLOG_EXEC_VAR (Post-Transport, No Transport Required)

| Field | Value |
|-------|-------|
| `NAME` | `ZCTD_RILOPS_AUTO_USER` |
| `NUMB` | `0001` |
| `REMARKS` | `RILCTD_AUTO` *(update to Basis-agreed user ID once created in SU01)* |

> All other fields can remain blank. This entry drives both record stamping and Rule Engine job user.

---

### 11.6 Full Before-Transport Checklist

- [ ] Transport 1 (SE11) released and imported before Transport 2 in all target systems
- [ ] Both FMs activate without syntax errors
- [ ] Code Inspector (SCI) clean on both FMs
- [ ] Extended Check (SLIN) clean
- [ ] No breakpoints remain
- [ ] Text elements maintained in SE37 for both FMs
- [ ] Peer review signed off
- [ ] `ZCTD_RULEENG_EXEC` SELECT optimisation CD (separate) recommended to be transported before or alongside this CD for full CR4 performance benefit
- [ ] Basis team creates background user (`RILCTD_AUTO`) in SU01
- [ ] `ZLOG_EXEC_VAR` entry maintained post-import (SE16)
- [ ] Portal UI team confirms 10-trip selection limit is enforced
- [ ] SM37 monitoring set up for job name `ZCTD_RULEENG_AUTO`
- [ ] UAT sign-off on: RTCA, RCCA, Manual RTC, Rule Engine job scheduling, OIGSV SHIFT values

---

### 11.7 Change Request Summary Table

| CR | Object | Type | Business Purpose | Key Outcome |
|----|--------|------|-----------------|-------------|
| CR1 | `Z_SCM_CTD_TRIPCONFIRM` | Modify existing FM | Post-TTC: auto-validate dates → trigger RTCA; Post-TCC: auto-validate CTD → trigger RCCA | Adds Step 4 with date/CTD validation and auto-confirmation call |
| CR2 | `Z_SCM_CTD_AUTO_TRIPCONFIRM` | Create new FM | Execute RTCA/RCCA auto-confirmation in its own LUW | Status `'07'` (RTCA) or `'10'` (RCCA) committed atomically with remarks + flags |
| CR3 | `ZSCE_CTD_HDR` | SE11 dictionary | Audit trail — record which trips were auto-confirmed | New fields `RTCA_FLAG` / `RCCA_FLAG` on header; set once in Step 2 UPDATE alongside status change |
| CR4 | `Z_SCM_CTD_TRIPCONFIRM` only | Code extension | Trigger Rule Engine immediately after status `'07'` (manual RTC or auto RTCA) — single orchestration point | Trip visible on Transporter CTD screen without waiting for batch run; New FM has no scheduling responsibility |
| CR5 | `Z_SCM_CTD_AUTO_TRIPCONFIRM` | Modify Step 1B | Stamp OIGSV SHIFT for all loaded legs, not just CTD-confirmed ones | `SHIFT='1'` (CTD confirmed) + `SHIFT='2'` (CTD not applicable) in same COMMIT |

---

*Document Version: V4 (Final — Rule Engine scheduling consolidated in Z_SCM_CTD_TRIPCONFIRM) | Created: 02.04.2026 | Author: Omkar More*
*Change Document Number: CD:XXXXXXX (to be assigned by ABAP team)*
*Transports: TR-1 (SE11 — ZSCE_CTD_ITM) | TR-2 (FMs — Z_SCM_CTD_AUTO_TRIPCONFIRM + Z_SCM_CTD_TRIPCONFIRM)*
*V4 includes: CR1 (RTCA/RCCA auto-confirmation in existing FM), CR2 (New FM), CR3 (ZSCE_CTD_HDR header-level flags RTCA_FLAG/RCCA_FLAG), CR4 (Rule Engine scheduling), CR5 (OIGSV SHIFT='2' for non-CTD legs)*
*Companion CD: Code Change Document - ZCTD_RULEENG_EXEC - SELECT Optimization v1.md*
