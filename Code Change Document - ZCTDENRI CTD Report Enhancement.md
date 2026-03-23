# Code Change Document – ZCTDENRI CTD Report Enhancement
## CTD Report (Read-Only) and Transaction Dual-Radio Enhancement

**Program:** `ZSCM_CTD_ENRICHTRIPDETAILS`  
**Transaction:** `ZCTDENRI`  
**New FM:** `Z_SCM_CTD_GET_TRIP_REPORT`  
**Tables:** `ZSCE_CTD_HDR`, `ZSCE_CTD_ITM`, `LFA1`, `T005U`  
**Change Type:** New FM + Program Enhancement  
**Date:** 2026-03-23  
**Reference:** CD: TBD | TR: TBD  
**Technical Author:** TBD  
**Functional Author:** Gaurav Prabhu  
**FS Reference:** `CTD Process Report/Functional Specification - ZCTDENRI CTD Report and Transaction Enhancement.md`

---

## Executive Summary (for ABAP team)

This change delivers two deliverables:

1. **New FM `Z_SCM_CTD_GET_TRIP_REPORT`** — read-only data fetch for the CTD Report. It is reusable: called from the SAP program (`IV_CALLER_TYPE = 'R'`) and from the Transporter portal (`IV_CALLER_TYPE = 'V'`, with mandatory `IV_LIFNR`). No DB insert/update in this FM.

2. **Program `ZSCM_CTD_ENRICHTRIPDETAILS` enhancement** — adds a **radio-button** on the selection screen so the user chooses between **Trip Enrichment Program** (existing, unchanged) and **CTD Report** (new). When CTD Report is selected, the program calls the new FM and displays the result in a read-only ALV.

**No change to the existing enrichment logic** (`Z_SCM_CTD_ENRICHTRIPDETAILS`), enrichment includes, or enrichment flow.

---

## 1. Pre-requisite: DDIC Changes (SE11)

### 1.1 New Structure — `ZSCM_CTD_REP_ALV_ST`

Create a new flat structure in SE11 to hold one ALV output row for the CTD Report.  
Field sequence must match the ALV column order. All fields typed via data elements.

| Seq | Field Name | Data Element | Description |
|-----|------------|--------------|-------------|
| 1 | `LIFNR` | `LIFNR` | Transporter Code |
| 2 | `VEND_NAME` | `NAME1_GP` | Transporter Name (from LFA1) |
| 3 | `TRUCK_NO` | `YTRUCK_NO` | Vehicle Number |
| 4 | `TRIP_NO` | `ZTRIP_NO` | Trip Number |
| 5 | `COUNTER` | `ZCOUNTER` | Counter |
| 6 | `TRIP_STATUS` | `ZCTD_TRIP_ST` | Trip Status (code) |
| 7 | `STATUS_DESC` | `DDTEXT` (CHAR 60) | Trip Status Description (from `ZCTD_TRIP_ST`) |
| 8 | `SHNUMBER` | `OIG_SHNUM` | Shipment Number |
| 9 | `SHTYPE` | `OIG_TDSTYP` | Shipment Type |
| 10 | `MVT_TYPE` | `ZCTD_MVT_TYPE` | Movement Type |
| 11 | `LEG_TYPE` | `ZLEG_TYPE` | Leg Type |
| 12 | `BUSINESS` | `ZBUSINESS_ID` | Business |
| 13 | `SUB_BUSINESS` | `ZSUBUSINESS_ID` | Sub-Business |
| 14 | `MATERIAL` | `MATNR` | Material |
| 15 | `AREA` | `YAREA` | Source Area (code) |
| 16 | `AREA_DESC` | `DDTEXT` (CHAR 60) | Source Area Description (from domain YAREA) |
| 17 | `SOURCE_REGION` | `ZSOURCE_REGION` | Source Region (code) |
| 18 | `SOURCE_REG_DESC` | `BEZEI25` (CHAR 25) | Source Region Description (from T005U) |
| 19 | `SOURCE_TZONE` | `ZSOURCE_TZONE` | Source Zone |
| 20 | `DEST_AREA` | `ZDEST_AREA` | Destination Area (code) |
| 21 | `DEST_AREA_DESC` | `DDTEXT` (CHAR 60) | Destination Area Description (from domain YAREA) |
| 22 | `DEST_REGION` | `ZDEST_REGION` | Destination Region (code) |
| 23 | `DEST_REG_DESC` | `BEZEI25` (CHAR 25) | Destination Region Description (from T005U) |
| 24 | `DEST_TZONE` | `ZDEST_TZONE` | Destination Zone |
| 25 | `ROUTE` | `ZCTD_ROUTE` | Route |
| 26 | `DISTANCE` | `DISTZ` | Distance |
| 27 | `SOURCE_ENT_DATE` | `ZSOURCE_ENT_DATE` | Source Entry Date |
| 28 | `SOURCE_DATE` | `ZSOURCE_DATE` | Leg Start Date |
| 29 | `VEND_SOURCE_DATE` | `ZSOURCE_DATE` | Vendor Leg Start Date |
| 30 | `DEST_DATE` | `ZDEST_DATE` | Leg End Date |
| 31 | `VEND_DEST_DATE` | `ZDEST_DATE` | Vendor Leg End Date |
| 32 | `DEST_EXIT_DATE` | `ZDEST_EXIT_DATE` | Destination Exit Date |
| 33 | `CTD_ELIGIBLE` | `ZCTD_ELIGIBLE` | System CTD |
| 34 | `VENDOR_CTD` | `FLAG` | Vendor CTD |
| 35 | `RIL_CTD` | `FLAG` | RIL CTD |
| 36 | `VENDOR_REMARKS` | `ZMSG` | Vendor Remarks — Line 1 |
| 37 | `VENDOR_REMARKS2` | `ZMSG` | Vendor Remarks — Line 2 |
| 38 | `RIL_REMARKS` | `ZMSG` | RIL Remarks — Line 1 |
| 39 | `RIL_REMARKS2` | `ZMSG` | RIL Remarks — Line 2 |
| 40 | `REJ_REMARK` | `ZCTD_REMARKS` | Rejection Remarks |
| 41 | `CTD_RULEENG_REMARKS` | `ZCTD_RULEENG_REMARKS` | CTD Rule Engine Remarks |
| 42 | `DEL_IND` | `ZTRIP_DELIND` | Deletion Indicator (last column) |

> Activate in SE11 **before** transporting any code.

### 1.2 New Table Type — `ZSCM_CTD_REP_ALV_TT`

Create a new table type in SE11:

| Field | Value |
|-------|-------|
| **Table type name** | `ZSCM_CTD_REP_ALV_TT` |
| **Line type** | `ZSCM_CTD_REP_ALV_ST` |

---

## 2. New Function Module — `Z_SCM_CTD_GET_TRIP_REPORT`

### 2.1 FM Header

| Field | Value |
|-------|-------|
| **Function group** | `ZSCM_CTD_REPORT` (new, or existing group per namespace standards) |
| **Short text** | CTD Trip Report — Read-Only Data Fetch |
| **Processing type** | Normal function module |

### 2.2 Interface Definition

```abap
FUNCTION z_scm_ctd_get_trip_report.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FROM_DATE)   TYPE  DATS
*"     VALUE(IV_TO_DATE)     TYPE  DATS
*"     VALUE(IV_CALLER_TYPE) TYPE  CHAR1
*"     VALUE(IV_LIFNR)       TYPE  LIFNR OPTIONAL
*"     VALUE(ITR_LIFNR)      TYPE  ZSCM_VENDOR_RANGE_T OPTIONAL
*"     VALUE(ITR_TRUCK_NO)   TYPE  ZSCM_TRUCK_RANGE_T OPTIONAL
*"     VALUE(ITR_TRIP_NO)    TYPE  ZSCM_TRIPNO_RANGE_T OPTIONAL
*"     VALUE(ITR_TRIP_STATUS) TYPE ZSCM_TRIP_STATUS_RANGE_T OPTIONAL
*"  EXPORTING
*"     VALUE(ET_LINES)       TYPE  ZSCM_CTD_REP_ALV_TT
*"     VALUE(ET_RETURN)      TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
```

**Caller type contract:**

| `IV_CALLER_TYPE` | `IV_LIFNR` | Restriction applied |
|------------------|------------|---------------------|
| `V` | **Mandatory** (portal provides it) | Output restricted to `LIFNR = IV_LIFNR`; `ITR_LIFNR` is **ignored** |
| `R` | Not used | No automatic LIFNR restriction; `ITR_LIFNR` range applied if provided |

**`ITR_TRIP_STATUS` default:** If the caller passes `ITR_TRIP_STATUS` as initial (empty), the FM applies an internal range of `03` through `10` inclusive.

### 2.3 Type and Data Declarations

Declare all variables upfront per ABAP rules (no inline declarations; NW 7.31 compatible):

```abap
*   ============================================================
*   Type Definitions
*   ============================================================
  TYPES: BEGIN OF lty_hdr,
           lifnr        TYPE lifnr,
           truck_no     TYPE ytruck_no,
           trip_no      TYPE ztrip_no,
           trip_status  TYPE zctd_trip_st,
           area         TYPE yarea,
           del_ind      TYPE ztrip_delind,
           ctd_ruleeng_remarks TYPE zctd_ruleeng_remarks,
         END OF lty_hdr,

         BEGIN OF lty_itm,
           lifnr             TYPE lifnr,
           truck_no          TYPE ytruck_no,
           trip_no           TYPE ztrip_no,
           counter           TYPE zcounter,
           del_ind           TYPE ztrip_delind,
           shnumber          TYPE oig_shnum,
           shtype            TYPE oig_tdstyp,
           mvt_type          TYPE zctd_mvt_type,
           leg_type          TYPE zleg_type,
           business          TYPE zbusiness_id,
           sub_business      TYPE zsubusiness_id,
           material          TYPE matnr,
           area              TYPE yarea,
           source_region     TYPE zsource_region,
           source_tzone      TYPE zsource_tzone,
           dest_area         TYPE zdest_area,
           dest_region       TYPE zdest_region,
           dest_tzone        TYPE zdest_tzone,
           route             TYPE zctd_route,
           distance          TYPE distz,
           source_ent_date   TYPE zsource_ent_date,
           source_date       TYPE zsource_date,
           vend_source_date  TYPE zsource_date,
           dest_date         TYPE zdest_date,
           vend_dest_date    TYPE zdest_date,
           dest_exit_date    TYPE zdest_exit_date,
           ctd_eligible      TYPE zctd_eligible,
           vendor_ctd        TYPE flag,
           ril_ctd           TYPE flag,
           vendor_remarks    TYPE zmsg,
           vendor_remarks2   TYPE zmsg,
           ril_remarks       TYPE zmsg,
           ril_remarks2      TYPE zmsg,
           rej_remark        TYPE zctd_remarks,
         END OF lty_itm,

         BEGIN OF lty_lfa1,
           lifnr TYPE lifnr,
           name1 TYPE name1_gp,
         END OF lty_lfa1,

         BEGIN OF lty_t005u,
           bland TYPE regio,
           bezei TYPE bezei25,
         END OF lty_t005u,

         BEGIN OF lty_dd07v,
           domvalue_l TYPE domvalue_l,
           ddtext     TYPE ddtext,
         END OF lty_dd07v.

*   ============================================================
*   Data Declarations
*   ============================================================
  DATA: lt_hdr             TYPE TABLE OF lty_hdr,
        lw_hdr             TYPE lty_hdr,
        lt_itm             TYPE TABLE OF lty_itm,
        lw_itm             TYPE lty_itm,
        lt_lfa1            TYPE TABLE OF lty_lfa1,
        lw_lfa1            TYPE lty_lfa1,
        lt_t005u           TYPE TABLE OF lty_t005u,
        lw_t005u           TYPE lty_t005u,
        lt_dd07v           TYPE TABLE OF lty_dd07v,
        lt_dd07v_n         TYPE TABLE OF dd07v,
        lw_dd07v           TYPE lty_dd07v,
        lw_dd07v_raw       TYPE dd07v,
        lt_dd07v_tab       TYPE TABLE OF dd07v,
        lw_dd07v_tab       TYPE dd07v,
        lt_hdr_temp        TYPE TABLE OF lty_hdr,
        lw_hdr_temp        TYPE lty_hdr,
        lt_region_key      TYPE TABLE OF regio,
        lw_output          TYPE zscm_ctd_rep_alv_st,
        lw_return          TYPE bapiret2,
        ltr_trip_status    TYPE RANGE OF zctd_trip_st,
        lwr_trip_status    LIKE LINE OF ltr_trip_status,
        ltr_lifnr_int      TYPE RANGE OF lifnr,
        lwr_lifnr_int      LIKE LINE OF ltr_lifnr_int,
        lv_days            TYPE i,
        lv_message         TYPE string.

  CONSTANTS: lc_caller_vendor   TYPE char1 VALUE 'V',
             lc_caller_ril      TYPE char1 VALUE 'R',
             lc_area_domain     TYPE dd07l-domname VALUE 'YAREA',
             lc_trip_status_dom TYPE dd07l-domname VALUE 'ZCTD_TRIP_ST',
             lc_status_from     TYPE zctd_trip_st VALUE '03',
             lc_status_to       TYPE zctd_trip_st VALUE '10',
             lc_t               TYPE ddrefstruc-bool VALUE 'T',
             lc_msgty_error     TYPE symsgty VALUE 'E',
             lc_land_in         TYPE land1 VALUE 'IN'.
```

### 2.4 Step 1 — Validate Input Parameters

```abap
*   ============================================================
*   Step 1: Input Validation
*   ============================================================
  CLEAR: et_lines, et_return.

  IF iv_from_date IS INITIAL OR iv_to_date IS INITIAL.
    lw_return-type    = lc_msgty_error.
    lw_return-message = 'Trip Date From and Trip Date To are mandatory'(001).
    APPEND lw_return TO et_return.
    RETURN.
  ENDIF.

  IF iv_from_date > iv_to_date.
    lw_return-type    = lc_msgty_error.
    lw_return-message = 'Trip Date From cannot be greater than Trip Date To'(002).
    APPEND lw_return TO et_return.
    RETURN.
  ENDIF.

* Inclusive days = (To - From) + 1; maximum 90 inclusive calendar days
  lv_days = iv_to_date - iv_from_date + 1.
  IF lv_days > 90.
    lw_return-type    = lc_msgty_error.
    lw_return-message = 'Date range must not exceed 90 calendar days'(003).
    APPEND lw_return TO et_return.
    RETURN.
  ENDIF.

* When called from Vendor / Transporter portal: IV_LIFNR is mandatory
  IF iv_caller_type = lc_caller_vendor AND iv_lifnr IS INITIAL.
    lw_return-type    = lc_msgty_error.
    lw_return-message = 'Transporter LIFNR is mandatory for vendor caller type'(004).
    APPEND lw_return TO et_return.
    RETURN.
  ENDIF.
```

### 2.5 Step 2 — Build Effective Status Range

```abap
*   ============================================================
*   Step 2: Build Effective Trip Status Range
*   If caller passes no status range, default to 03-10 inclusive
*   ============================================================
  IF itr_trip_status[] IS INITIAL.
    lwr_trip_status-sign   = 'I'.
    lwr_trip_status-option = 'BT'.
    lwr_trip_status-low    = lc_status_from.
    lwr_trip_status-high   = lc_status_to.
    APPEND lwr_trip_status TO ltr_trip_status.
    CLEAR  lwr_trip_status.
  ELSE.
    ltr_trip_status[] = itr_trip_status[].
  ENDIF.
```

### 2.6 Step 3 — Build Effective LIFNR Range

```abap
*   ============================================================
*   Step 3: Build Effective LIFNR Range
*   Vendor caller: force IV_LIFNR; ignore ITR_LIFNR
*   RIL caller:    use ITR_LIFNR as-is
*   ============================================================
  IF iv_caller_type = lc_caller_vendor.
    lwr_lifnr_int-sign   = 'I'.
    lwr_lifnr_int-option = 'EQ'.
    lwr_lifnr_int-low    = iv_lifnr.
    APPEND lwr_lifnr_int TO ltr_lifnr_int.
    CLEAR  lwr_lifnr_int.
  ELSE.
    ltr_lifnr_int[] = itr_lifnr[].
  ENDIF.
```

### 2.7 Step 4 — Fetch Header Records from `ZSCE_CTD_HDR`

```abap
*   ============================================================
*   Step 4: Fetch Header Records
*   SELECT named fields only (DB rule: no SELECT *, type matches)
*   ============================================================
  SELECT lifnr truck_no trip_no trip_status area del_ind ctd_ruleeng_remarks
    FROM zsce_ctd_hdr CLIENT SPECIFIED
    INTO TABLE lt_hdr
    WHERE mandt        = sy-mandt
    AND   created_date >= iv_from_date
    AND   created_date <= iv_to_date
    AND   trip_status IN ltr_trip_status[].
  IF sy-subrc <> 0.
    lw_return-type    = lc_msgty_error.
    lw_return-message = 'No trip header records found for the given criteria'(005).
    APPEND lw_return TO et_return.
    RETURN.
  ENDIF.

* Apply LIFNR range (Vendor: forced single value; RIL: optional range)
  IF ltr_lifnr_int[] IS NOT INITIAL.
    DELETE lt_hdr WHERE lifnr NOT IN ltr_lifnr_int[].
  ENDIF.

* Apply optional filters (only if provided by RIL caller)
  IF itr_truck_no[] IS NOT INITIAL.
    DELETE lt_hdr WHERE truck_no NOT IN itr_truck_no[].
  ENDIF.

  IF itr_trip_no[] IS NOT INITIAL.
    DELETE lt_hdr WHERE trip_no NOT IN itr_trip_no[].
  ENDIF.

  IF lt_hdr IS INITIAL.
    lw_return-type    = lc_msgty_error.
    lw_return-message = 'No trip header records found after applying filters'(006).
    APPEND lw_return TO et_return.
    RETURN.
  ENDIF.

  SORT lt_hdr BY lifnr truck_no trip_no.
```

### 2.8 Step 5 — Fetch Item Records from `ZSCE_CTD_ITM`

```abap
*   ============================================================
*   Step 5: Fetch Item Records (FOR ALL ENTRIES — guard NOT INITIAL)
*   ============================================================
  lt_hdr_temp[] = lt_hdr[].
  SORT   lt_hdr_temp BY lifnr truck_no trip_no.
  DELETE ADJACENT DUPLICATES FROM lt_hdr_temp COMPARING lifnr truck_no trip_no.

  IF lt_hdr_temp IS NOT INITIAL.
    SELECT lifnr truck_no trip_no counter del_ind shnumber shtype mvt_type
           leg_type business sub_business material area source_region
           source_tzone dest_area dest_region dest_tzone route distance
           source_ent_date source_date vend_source_date dest_date
           vend_dest_date dest_exit_date ctd_eligible vendor_ctd ril_ctd
           vendor_remarks vendor_remarks2 ril_remarks ril_remarks2 rej_remark
      FROM zsce_ctd_itm CLIENT SPECIFIED
      INTO TABLE lt_itm
      FOR ALL ENTRIES IN lt_hdr_temp
      WHERE mandt    = sy-mandt
        AND lifnr    = lt_hdr_temp-lifnr
        AND truck_no = lt_hdr_temp-truck_no
        AND trip_no  = lt_hdr_temp-trip_no.
    IF sy-subrc = 0.
      SORT lt_itm BY lifnr truck_no trip_no counter.
    ENDIF.
  ENDIF.
```

### 2.9 Step 6 — Fetch Descriptions (LFA1, T005U, Domain YAREA, ZCTD_TRIP_ST)

```abap
*   ============================================================
*   Step 6: Fetch Descriptions — LFA1, T005U, Domain YAREA,
*           ZCTD_TRIP_ST (same approach as Z_SCM_CTD_ENRICHTRIPDETAILS)
*   ============================================================

* LFA1 — Transporter Name (deduplicate LIFNR before SELECT)
  SORT   lt_hdr_temp BY lifnr.
  DELETE ADJACENT DUPLICATES FROM lt_hdr_temp COMPARING lifnr.
  IF lt_hdr_temp IS NOT INITIAL.
    SELECT lifnr name1
      FROM lfa1 CLIENT SPECIFIED
      INTO TABLE lt_lfa1
      FOR ALL ENTRIES IN lt_hdr_temp
      WHERE mandt = sy-mandt
        AND lifnr = lt_hdr_temp-lifnr.
    IF sy-subrc = 0.
      SORT lt_lfa1 BY lifnr.
    ENDIF.
  ENDIF.

* T005U — Region Descriptions (collect unique regions; LAND1 = 'IN'; language = sy-langu)
  LOOP AT lt_itm INTO lw_itm.
    IF lw_itm-source_region IS NOT INITIAL.
      APPEND lw_itm-source_region TO lt_region_key.
    ENDIF.
    IF lw_itm-dest_region IS NOT INITIAL.
      APPEND lw_itm-dest_region TO lt_region_key.
    ENDIF.
  ENDLOOP.
  SORT   lt_region_key.
  DELETE ADJACENT DUPLICATES FROM lt_region_key.
  DELETE lt_region_key WHERE table_line IS INITIAL.
  IF lt_region_key IS NOT INITIAL.
    SELECT bland bezei
      FROM t005u CLIENT SPECIFIED
      INTO TABLE lt_t005u
      FOR ALL ENTRIES IN lt_region_key
      WHERE mandt = sy-mandt
        AND spras = sy-langu
        AND land1 = lc_land_in
        AND bland = lt_region_key-table_line.
    IF sy-subrc = 0.
      SORT lt_t005u BY bland.
    ENDIF.
  ENDIF.

* Area Description — Domain YAREA (same pattern as enrichment FM)
  CALL FUNCTION 'DD_DOMA_GET'
    EXPORTING
      domain_name   = lc_area_domain
      langu         = sy-langu
    TABLES
      dd07v_tab_a   = lt_dd07v_n
      dd07v_tab_n   = lt_dd07v_n
    EXCEPTIONS
      illegal_value = 1
      op_failure    = 2
      OTHERS        = 3.
  IF sy-subrc = 0.
    LOOP AT lt_dd07v_n INTO lw_dd07v_raw.
      CLEAR lw_dd07v.
      lw_dd07v-domvalue_l = lw_dd07v_raw-domvalue_l.
      lw_dd07v-ddtext     = lw_dd07v_raw-ddtext.
      APPEND lw_dd07v TO lt_dd07v.
    ENDLOOP.
    SORT lt_dd07v BY domvalue_l.
  ENDIF.

* Trip Status Description — Domain ZCTD_TRIP_ST
  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = lc_trip_status_dom
      text           = lc_t
      langu          = sy-langu
    TABLES
      dd07v_tab      = lt_dd07v_tab
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.
  IF sy-subrc = 0.
    SORT lt_dd07v_tab BY domvalue_l.
  ENDIF.
```

### 2.10 Step 7 — Build ALV Output Lines

**Logic:**
- For each header row loop; for each, attempt `READ TABLE lt_itm` for matching keys.
- If **one or more** item rows exist: emit one ALV line per item (header fields joined with item fields).
- If **no item rows** exist: emit **exactly one** ALV line with header fields; item columns blank; `DEL_IND` from header.

```abap
*   ============================================================
*   Step 7: Build ALV Output Lines
*   ============================================================
  LOOP AT lt_hdr INTO lw_hdr.

*   Collect items for this trip (all counters)
    REFRESH lt_hdr_temp.
    LOOP AT lt_itm INTO lw_itm
      WHERE lifnr    = lw_hdr-lifnr
        AND truck_no = lw_hdr-truck_no
        AND trip_no  = lw_hdr-trip_no.

      CLEAR lw_output.

*     Common header fields
      lw_output-lifnr               = lw_hdr-lifnr.
      lw_output-truck_no            = lw_hdr-truck_no.
      lw_output-trip_no             = lw_hdr-trip_no.
      lw_output-trip_status         = lw_hdr-trip_status.
      lw_output-ctd_ruleeng_remarks = lw_hdr-ctd_ruleeng_remarks.

*     Transporter Name (from LFA1)
      READ TABLE lt_lfa1 INTO lw_lfa1
        WITH KEY lifnr = lw_hdr-lifnr BINARY SEARCH.
      IF sy-subrc = 0.
        lw_output-vend_name = lw_lfa1-name1.
      ENDIF.

*     Trip Status Description
      READ TABLE lt_dd07v_tab INTO lw_dd07v_tab
        WITH KEY domvalue_l = lw_hdr-trip_status BINARY SEARCH.
      IF sy-subrc = 0.
        lw_output-status_desc = lw_dd07v_tab-ddtext.
      ENDIF.

*     Item-level fields
      lw_output-counter         = lw_itm-counter.
      lw_output-shnumber        = lw_itm-shnumber.
      lw_output-shtype          = lw_itm-shtype.
      lw_output-mvt_type        = lw_itm-mvt_type.
      lw_output-leg_type        = lw_itm-leg_type.
      lw_output-business        = lw_itm-business.
      lw_output-sub_business    = lw_itm-sub_business.
      lw_output-material        = lw_itm-material.
      lw_output-area            = lw_itm-area.
      lw_output-source_region   = lw_itm-source_region.
      lw_output-source_tzone    = lw_itm-source_tzone.
      lw_output-dest_area       = lw_itm-dest_area.
      lw_output-dest_region     = lw_itm-dest_region.
      lw_output-dest_tzone      = lw_itm-dest_tzone.
      lw_output-route           = lw_itm-route.
      lw_output-distance        = lw_itm-distance.
      lw_output-source_ent_date = lw_itm-source_ent_date.
      lw_output-source_date     = lw_itm-source_date.
      lw_output-vend_source_date = lw_itm-vend_source_date.
      lw_output-dest_date       = lw_itm-dest_date.
      lw_output-vend_dest_date  = lw_itm-vend_dest_date.
      lw_output-dest_exit_date  = lw_itm-dest_exit_date.
      lw_output-ctd_eligible    = lw_itm-ctd_eligible.
      lw_output-vendor_ctd      = lw_itm-vendor_ctd.
      lw_output-ril_ctd         = lw_itm-ril_ctd.
      lw_output-vendor_remarks  = lw_itm-vendor_remarks.
      lw_output-vendor_remarks2 = lw_itm-vendor_remarks2.
      lw_output-ril_remarks     = lw_itm-ril_remarks.
      lw_output-ril_remarks2    = lw_itm-ril_remarks2.
      lw_output-rej_remark      = lw_itm-rej_remark.

*     Source Area Description
      READ TABLE lt_dd07v INTO lw_dd07v
        WITH KEY domvalue_l = lw_itm-area BINARY SEARCH.
      IF sy-subrc = 0.
        lw_output-area_desc = lw_dd07v-ddtext.
      ENDIF.

*     Destination Area Description
      READ TABLE lt_dd07v INTO lw_dd07v
        WITH KEY domvalue_l = lw_itm-dest_area BINARY SEARCH.
      IF sy-subrc = 0.
        lw_output-dest_area_desc = lw_dd07v-ddtext.
      ENDIF.

*     Source Region Description
      READ TABLE lt_t005u INTO lw_t005u
        WITH KEY bland = lw_itm-source_region BINARY SEARCH.
      IF sy-subrc = 0.
        lw_output-source_reg_desc = lw_t005u-bezei.
      ENDIF.

*     Destination Region Description
      READ TABLE lt_t005u INTO lw_t005u
        WITH KEY bland = lw_itm-dest_region BINARY SEARCH.
      IF sy-subrc = 0.
        lw_output-dest_reg_desc = lw_t005u-bezei.
      ENDIF.

*     DEL_IND: item-level when header + item both exist
      lw_output-del_ind = lw_itm-del_ind.

      APPEND lw_output TO et_lines.
      CLEAR  lw_output.

    ENDLOOP.  " lt_itm for this trip

*   If no items found for this trip: emit one header-only row
    READ TABLE lt_itm INTO lw_itm
      WITH KEY lifnr    = lw_hdr-lifnr
               truck_no = lw_hdr-truck_no
               trip_no  = lw_hdr-trip_no
      BINARY SEARCH.
    IF sy-subrc <> 0.
      CLEAR lw_output.
      lw_output-lifnr               = lw_hdr-lifnr.
      lw_output-truck_no            = lw_hdr-truck_no.
      lw_output-trip_no             = lw_hdr-trip_no.
      lw_output-trip_status         = lw_hdr-trip_status.
      lw_output-ctd_ruleeng_remarks = lw_hdr-ctd_ruleeng_remarks.

      READ TABLE lt_lfa1 INTO lw_lfa1
        WITH KEY lifnr = lw_hdr-lifnr BINARY SEARCH.
      IF sy-subrc = 0.
        lw_output-vend_name = lw_lfa1-name1.
      ENDIF.

      READ TABLE lt_dd07v_tab INTO lw_dd07v_tab
        WITH KEY domvalue_l = lw_hdr-trip_status BINARY SEARCH.
      IF sy-subrc = 0.
        lw_output-status_desc = lw_dd07v_tab-ddtext.
      ENDIF.

*     Source area from header AREA field; description from domain
      lw_output-area = lw_hdr-area.
      READ TABLE lt_dd07v INTO lw_dd07v
        WITH KEY domvalue_l = lw_hdr-area BINARY SEARCH.
      IF sy-subrc = 0.
        lw_output-area_desc = lw_dd07v-ddtext.
      ENDIF.

*     DEL_IND: header-level when no items
      lw_output-del_ind = lw_hdr-del_ind.

      APPEND lw_output TO et_lines.
      CLEAR  lw_output.
    ENDIF.

  ENDLOOP.  " lt_hdr

  IF et_lines IS INITIAL.
    lw_return-type    = lc_msgty_error.
    lw_return-message = 'No data found for the given selection criteria'(007).
    APPEND lw_return TO et_return.
  ENDIF.

ENDFUNCTION.
```

---

## 3. Program `ZSCM_CTD_ENRICHTRIPDETAILS` — Changes

### 3.1 Overview of changes

| Include / Object | Change |
|-----------------|--------|
| `ZSCM_CTD_ENRICHTRIPDETAILSTOP` | Add global radio variable, new selection screen fields |
| `ZSCM_CTD_ENRICHTRIPDETAILSSEL` | Add radio-button block, new CTD Report selection fields |
| `ZSCM_CTD_ENRICHTRIPDETAILSC01` | Add CTD Report path (method `execute_report`) in class |
| Main program `ZSCM_CTD_ENRICHTRIPDETAILS` | Add AT SELECTION-SCREEN validation for report path |

### 3.2 TOP Include — `ZSCM_CTD_ENRICHTRIPDETAILSTOP`

Add after existing global declarations:

```abap
* CTD Report — global selection variables
DATA: gv_mode       TYPE char1,        " 'E' = Enrichment, 'R' = Report
      gt_rep_lines  TYPE zscm_ctd_rep_alv_tt,  " Report ALV output
      gt_rep_return TYPE bapiret2_t.            " Report messages
```

### 3.3 SEL Include — `ZSCM_CTD_ENRICHTRIPDETAILSSEL`

Add a new selection screen block **before** the existing enrichment parameters:

```abap
*----------------------------------------------------------------------
* Selection Screen — Radio Buttons: Program Mode
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b_mode WITH FRAME TITLE text-m01.
  PARAMETERS: p_enrich RADIOBUTTON GROUP mode DEFAULT 'X',
              p_report RADIOBUTTON GROUP mode.
SELECTION-SCREEN END OF BLOCK b_mode.

*----------------------------------------------------------------------
* Selection Screen — CTD Report Parameters (visible when p_report active)
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b_rep WITH FRAME TITLE text-m02.
  SELECT-OPTIONS: s_rfrom FOR zsce_ctd_hdr-created_date NO INTERVALS
                           MODIF ID rep,
                  s_rto   FOR zsce_ctd_hdr-created_date NO INTERVALS
                           MODIF ID rep,
                  s_rlifn FOR zsce_ctd_hdr-lifnr    MODIF ID rep,
                  s_rtrk  FOR zsce_ctd_hdr-truck_no MODIF ID rep,
                  s_rtrip FOR zsce_ctd_hdr-trip_no  MODIF ID rep,
                  s_rstat FOR zsce_ctd_hdr-trip_status MODIF ID rep.
SELECTION-SCREEN END OF BLOCK b_rep.
```

**Text symbols** (maintain in SE38 / text element editor):

| Symbol | Text |
|--------|------|
| `M01` | Program Mode |
| `M02` | CTD Report Parameters |
| `P_ENRICH` | Trip Enrichment Program |
| `P_REPORT` | CTD Report |
| `S_RFROM` | Trip Date (From) |
| `S_RTO` | Trip Date (To) |
| `S_RLIFN` | Transporter Code |
| `S_RTRK` | Vehicle Number |
| `S_RTRIP` | Trip Number |
| `S_RSTAT` | Trip Status |

> `MODIF ID rep` — used in `MODIFICATION` event to hide/show the report block based on active radio button. Implement in `AT SELECTION-SCREEN OUTPUT` in the main program or add `go_report->handle_screen_modification( )` method.

### 3.4 AT SELECTION-SCREEN Validations (main program)

Add these blocks in the main program after the existing `AT SELECTION-SCREEN` events:

```abap
*----------------------------------------------------------------------
* Validate CTD Report date range when p_report is active
*----------------------------------------------------------------------
AT SELECTION-SCREEN.
  IF p_report = abap_true.
    go_report->validate_report_dates( ).
  ENDIF.
```

> The method `validate_report_dates` is defined in §3.5.

### 3.5 C01 Include — Class Method Changes (`ZSCM_CTD_ENRICHTRIPDETAILSC01`)

#### 3.5.1 New method: `validate_report_dates`

Add to method definition section:

```abap
METHODS validate_report_dates.
```

Implementation:

```abap
METHOD validate_report_dates.
  DATA: lv_from  TYPE dats,
        lv_to    TYPE dats,
        lv_days  TYPE i,
        lw_range LIKE LINE OF s_rfrom.

* Extract single From/To values from ranges (report uses single values)
  READ TABLE s_rfrom INTO lw_range INDEX 1.
  IF sy-subrc = 0.
    lv_from = lw_range-low.
  ENDIF.

  READ TABLE s_rto INTO lw_range INDEX 1.
  IF sy-subrc = 0.
    lv_to = lw_range-low.
  ENDIF.

  IF lv_from IS INITIAL.
    MESSAGE 'Trip Date From is mandatory for CTD Report'(r01) TYPE 'E'.
  ENDIF.

  IF lv_to IS INITIAL.
    MESSAGE 'Trip Date To is mandatory for CTD Report'(r02) TYPE 'E'.
  ENDIF.

  IF lv_from > lv_to.
    MESSAGE 'Trip Date From cannot be greater than Trip Date To'(r03) TYPE 'E'.
  ENDIF.

* Inclusive days rule: (To - From) + 1 must not exceed 90
  lv_days = lv_to - lv_from + 1.
  IF lv_days > 90.
    MESSAGE 'Date range must not exceed 90 calendar days'(r04) TYPE 'E'.
  ENDIF.
ENDMETHOD.
```

#### 3.5.2 New method: `execute_report`

Add to method definition section:

```abap
METHODS execute_report.
```

Implementation — builds the FM import parameters from selection screen and calls the new FM:

```abap
METHOD execute_report.
  DATA: lv_from        TYPE dats,
        lv_to          TYPE dats,
        lw_range       LIKE LINE OF s_rfrom,
        ltr_trip_status TYPE RANGE OF zctd_trip_st,
        lwr_trip_status LIKE LINE OF ltr_trip_status.

* Extract From date
  READ TABLE s_rfrom INTO lw_range INDEX 1.
  IF sy-subrc = 0.
    lv_from = lw_range-low.
  ENDIF.

* Extract To date
  READ TABLE s_rto INTO lw_range INDEX 1.
  IF sy-subrc = 0.
    lv_to = lw_range-low.
  ENDIF.

* Pass s_rstat as-is to FM; FM defaults to 03-10 if initial
  CALL FUNCTION 'Z_SCM_CTD_GET_TRIP_REPORT'
    EXPORTING
      iv_from_date    = lv_from
      iv_to_date      = lv_to
      iv_caller_type  = 'R'
      itr_lifnr       = s_rlifn[]
      itr_truck_no    = s_rtrk[]
      itr_trip_no     = s_rtrip[]
      itr_trip_status = s_rstat[]
    IMPORTING
      et_lines        = gt_rep_lines
      et_return       = gt_rep_return.

ENDMETHOD.
```

#### 3.5.3 New method: `display_report`

Add to method definition section:

```abap
METHODS display_report.
```

Implementation — builds ALV field catalog and calls CL_SALV_TABLE (same ALV approach as existing report display):

```abap
METHOD display_report.
  DATA: lo_alv       TYPE REF TO cl_salv_table,
        lo_columns   TYPE REF TO cl_salv_columns_table,
        lo_column    TYPE REF TO cl_salv_column_table,
        lo_functions TYPE REF TO cl_salv_functions_list,
        lo_layout    TYPE REF TO cl_salv_layout,
        lw_key       TYPE salv_s_layout_key,
        lo_sorts     TYPE REF TO cl_salv_sorts,
        lo_exception TYPE REF TO cx_root,
        lv_message   TYPE string.

  IF gt_rep_lines IS INITIAL.
    MESSAGE 'No data found for the given CTD Report criteria'(r05) TYPE 'I'.
    RETURN.
  ENDIF.

  TRY.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lo_alv
      CHANGING
        t_table      = gt_rep_lines ).
  CATCH cx_salv_msg INTO lo_exception.
    lv_message = lo_exception->get_text( ).
    MESSAGE lv_message TYPE 'E'.
    RETURN.
  ENDTRY.

* Enable standard ALV functions (sort, filter, export)
  lo_functions = lo_alv->get_functions( ).
  lo_functions->set_all( abap_true ).

* Enable layout variant save
  lo_layout = lo_alv->get_layout( ).
  lw_key-report = sy-repid.
  lo_layout->set_key( lw_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

* Default sort: LIFNR, TRUCK_NO, TRIP_NO, COUNTER
  lo_sorts = lo_alv->get_sorts( ).
  TRY.
    lo_sorts->add_sort( columnname = 'LIFNR'    position = 1 ).
    lo_sorts->add_sort( columnname = 'TRUCK_NO' position = 2 ).
    lo_sorts->add_sort( columnname = 'TRIP_NO'  position = 3 ).
    lo_sorts->add_sort( columnname = 'COUNTER'  position = 4 ).
  CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
  ENDTRY.

* Column headers — long texts from text elements
  lo_columns = lo_alv->get_columns( ).
  lo_columns->set_optimize( abap_true ).

* Set column-level properties (example — extend for all columns)
  TRY.
    lo_column ?= lo_columns->get_column( 'LIFNR' ).
    lo_column->set_long_text( 'Transporter Code'(c01) ).

    lo_column ?= lo_columns->get_column( 'VEND_NAME' ).
    lo_column->set_long_text( 'Transporter Name'(c02) ).

    lo_column ?= lo_columns->get_column( 'DEL_IND' ).
    lo_column->set_long_text( 'Deletion Indicator'(c42) ).
* ... repeat for each ALV column listed in §1.1 ...

  CATCH cx_salv_not_found.
  ENDTRY.

  lo_alv->set_screen_status(
    pfstatus      = 'SALV_STANDARD'
    report        = 'SAPLSALV_MSG_DISPLAY_TOOL_IMPL' ).

  lo_alv->display( ).

ENDMETHOD.
```

#### 3.5.4 Extend existing `execute` method

The existing `execute` method drives enrichment processing. Add a branch at the start so `p_report` calls the new path:

```abap
METHOD execute.
  IF p_report = abap_true.
    me->execute_report( ).
    RETURN.
  ENDIF.
* ... existing enrichment logic follows unchanged ...
ENDMETHOD.
```

#### 3.5.5 Extend existing `display_results` method

Similarly branch so `p_report` calls `display_report`:

```abap
METHOD display_results.
  IF p_report = abap_true.
    me->display_report( ).
    RETURN.
  ENDIF.
* ... existing ALV display logic follows unchanged ...
ENDMETHOD.
```

---

## 4. Default Values — INITIALIZATION in Main Program

Add defaults for the CTD Report date fields in the `INITIALIZATION` block:

```abap
INITIALIZATION.
  CREATE OBJECT go_report.

* Existing authority check ...

* CTD Report: default Trip Date From = today - 30; To = today
  s_rfrom-sign   = 'I'.
  s_rfrom-option = 'EQ'.
  s_rfrom-low    = sy-datum - 30.
  APPEND s_rfrom.
  CLEAR  s_rfrom.

  s_rto-sign   = 'I'.
  s_rto-option = 'EQ'.
  s_rto-low    = sy-datum.
  APPEND s_rto.
  CLEAR  s_rto.

* CTD Report: default Trip Status 03-10
  s_rstat-sign   = 'I'.
  s_rstat-option = 'BT'.
  s_rstat-low    = '03'.
  s_rstat-high   = '10'.
  APPEND s_rstat.
  CLEAR  s_rstat.
```

---

## 5. ABAP Rules Compliance Checklist

| Rule | How applied in this change |
|------|---------------------------|
| **No inline declarations** (`DATA(x)`) | All variables declared upfront in DATA section (NW 7.31) |
| **No `NEW` / `VALUE` / `CORRESPONDING`** | Not used anywhere in new code |
| **No `SELECT *`** | All SELECTs name explicit fields; types match field order |
| **No SELECT in loop** | All selects use `FOR ALL ENTRIES` with `IS NOT INITIAL` guard |
| **No nested LOOPs** | Item lookup uses `READ TABLE … BINARY SEARCH` inside HDR loop |
| **`IS NOT INITIAL` not in OpenSQL WHERE** | Used in ABAP `IF` checks, not in `WHERE` clause |
| **`SY-SUBRC` checked immediately** | Every SELECT and READ TABLE is followed by `IF sy-subrc` |
| **Naming: `lv_`, `lt_`, `lw_`, `lty_`** | Applied throughout FM and method code |
| **Constants for fixed values** | `lc_caller_vendor`, `lc_status_from`, `lc_area_domain`, etc. |
| **No `sy-*` in CONSTANTS** | `sy-mandt`, `sy-datum`, `sy-langu` used directly, not in CONSTANTS |
| **OOP — no FORMs** | All new logic in class methods (`execute_report`, `display_report`, `validate_report_dates`) |
| **FM only for RFC/Dynpro** | `Z_SCM_CTD_GET_TRIP_REPORT` is correctly an FM (reusable, portal-callable) |
| **CL_SALV_TABLE** | Used for ALV display per project pattern; layout variant + export enabled |

---

## 6. Implementation Checklist

| # | Task | Object / Location |
|---|------|------------------|
| 1 | Create structure `ZSCM_CTD_REP_ALV_ST` in SE11 and activate | SE11 |
| 2 | Create table type `ZSCM_CTD_REP_ALV_TT` in SE11 and activate | SE11 |
| 3 | Create FM `Z_SCM_CTD_GET_TRIP_REPORT` (new function group if required) | SE37 |
| 4 | Add interface per §2.2 | FM editor |
| 5 | Add types, data, constants per §2.3 | FM code |
| 6 | Implement Steps 1–7 (§2.4–§2.10) | FM code |
| 7 | Add globals (`gv_mode`, `gt_rep_lines`, `gt_rep_return`) | TOP include |
| 8 | Add radio-button block and CTD Report parameters | SEL include |
| 9 | Add text symbols (M01, M02, P_ENRICH, P_REPORT, S_RFROM, etc.) | SE38 text elements |
| 10 | Add `AT SELECTION-SCREEN` call to `validate_report_dates` | Main program |
| 11 | Add `INITIALIZATION` defaults for report date/status | Main program |
| 12 | Add method definition and implementation `validate_report_dates` | C01 include |
| 13 | Add method definition and implementation `execute_report` | C01 include |
| 14 | Add method definition and implementation `display_report` | C01 include |
| 15 | Extend `execute` method — branch for `p_report` | C01 include |
| 16 | Extend `display_results` method — branch for `p_report` | C01 include |
| 17 | Set column long texts for all 42 ALV columns in `display_report` | C01 include |
| 18 | Syntax check (Code Inspector); Zero errors / warnings | SE38 / SCI |
| 19 | Unit test per §7 in DEV | DEV client |
| 20 | Transport to QA | STMS |

---

## 7. Test Checklist

| # | Test case | Expected result |
|---|-----------|-----------------|
| 1 | Open `ZCTDENRI`; both radios visible | Radio 1: Trip Enrichment Program (default); Radio 2: CTD Report |
| 2 | Select **Trip Enrichment Program**; execute | Existing enrichment behaviour; no change |
| 3 | Select **CTD Report**; do not fill dates; execute | Error: "Trip Date From is mandatory" |
| 4 | From > To | Error: "From cannot be greater than To" |
| 5 | Span = 91 inclusive days | Error: "must not exceed 90 calendar days" |
| 6 | Span = 90 inclusive days (e.g. 01-Jan to 31-Mar) | No error; proceeds to FM |
| 7 | Same From and To (1 day) | No error |
| 8 | Valid dates; data exists; default status 03–10 | ALV shows trips; headers + items; all 42 columns |
| 9 | Change status to single value e.g. 04 | Only status 04 rows returned |
| 10 | Header exists; no items for trip | One row per trip; item columns blank; `DEL_IND` from header |
| 11 | Header + items exist | One row per item; `DEL_IND` from item (last column) |
| 12 | Transporter filter applied | Only rows matching `LIFNR` |
| 13 | Save ALV layout variant; re-execute | Layout restored |
| 14 | Export to spreadsheet | Works via ALV standard export |
| 15 | FM called with `IV_CALLER_TYPE = 'V'` and `IV_LIFNR` blank | `ET_RETURN` has error; `ET_LINES` empty |
| 16 | FM called with `IV_CALLER_TYPE = 'V'` and valid `IV_LIFNR` | Only that vendor's data returned |
| 17 | FM called with `IV_CALLER_TYPE = 'R'`; no `ITR_LIFNR` | All vendors within date/status range returned |

---

## 8. Message / Text References

| Text symbol | Text |
|-------------|------|
| `001` (FM) | Trip Date From and Trip Date To are mandatory |
| `002` (FM) | Trip Date From cannot be greater than Trip Date To |
| `003` (FM) | Date range must not exceed 90 calendar days |
| `004` (FM) | Transporter LIFNR is mandatory for vendor caller type |
| `005` (FM) | No trip header records found for the given criteria |
| `006` (FM) | No trip header records found after applying filters |
| `007` (FM) | No data found for the given selection criteria |
| `R01` (program) | Trip Date From is mandatory for CTD Report |
| `R02` (program) | Trip Date To is mandatory for CTD Report |
| `R03` (program) | Trip Date From cannot be greater than Trip Date To |
| `R04` (program) | Date range must not exceed 90 calendar days |
| `R05` (program) | No data found for the given CTD Report criteria |
| `C01–C42` (program) | ALV column long texts (one per column per §1.1) |

---

## 9. References

- `CTD Process Report/Functional Specification - ZCTDENRI CTD Report and Transaction Enhancement.md`
- `CTD Process Report/ALV Output fields.txt`
- `CTD Process Report/ZSCE_CTD_HDR Table`
- `CTD Process Report/ZSCE_CTD_ITM Table`
- `CTD Process Report/FM - Z_SCM_CTD_ENRICHTRIPDETAILS.txt`
- `CTD Process Report/FM - Z_SCM_CTD_GETTRIPHDR.txt`
- `CTD Process Report/CTD Trip Status.txt`
- `ABAP RULES/00-main.mdc`, `02-naming.mdc`, `03-database.mdc`, `07-ui.mdc`

---

*End of Code Change Document*
