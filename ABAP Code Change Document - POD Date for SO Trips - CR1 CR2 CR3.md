# ABAP Code Change Document
## POD Date Enablement for SO Trips (CR1, CR2, CR3)

---

| Attribute | Detail |
|---|---|
| Document Type | Detailed ABAP Code Change Document |
| System | SAP ECC / NetWeaver 7.31 |
| CD Number | To be assigned |
| Transport Request | To be assigned |
| Date | 08.04.2026 |
| Functional Author | Gaurav Prabhu |
| Technical Author | Omkar More |

---

## 1. Objective

Implement POD-based date derivation for SO movement trips, ensuring:
- `DEST_DATE` of SO loaded leg comes from POD API,
- `DEST_EXIT_DATE` of SO loaded leg is same as POD date,
- `SOURCE_DATE` of immediate next empty leg is same POD date,
- SO trip is UI-blocked for confirm when POD is unavailable,
- Existing RTCA/RCCA architecture and LUW behavior remain stable.

---

## 2. Reference Objects (Latest Baseline)

- `FMs Tables - 09.04.2026/FM - Z_SCM_CTD_GETTRIPITM.txt`
- `FMs Tables - 09.04.2026/FM - Z_SCM_CTD_TRIPCONFIRM.txt`
- `FMs Tables - 09.04.2026/FM - Z_SCM_CTD_ENRICHTRIPDETAILS.txt`
- `FMs Tables - 09.04.2026/FM - Z_SCE_TRIP_STLMNT.txt`
- `FMs Tables - 09.04.2026/FM - Z_SCM_FETCH_SHORTAGE_DTLS_API.txt`

Note: ABAP rules are aligned to the project's NW 7.31 pattern used in existing change documents (explicit declarations, no inline declarations, no string templates, explicit `SELECT` fields, `SY-SUBRC` checks, no unintended LUW split).

---

## 3. Key Tables and Fields Impacted

### 3.1 Application Tables
- `ZSCE_CTD_ITM`
  - `DEST_DATE`
  - `DEST_EXIT_DATE`
  - `SOURCE_DATE` (next empty leg)
  - `VEND_SOURCE_DATE`, `VEND_DEST_DATE` (already used in TTC; unchanged by CR1.3)
- `ZSCE_CTD_HDR` (no direct new field for this CR set)
- `ZWSO2APIDTL` (API configuration)

### 3.2 Config / Master / External
- `ZWSO2APIDTL` (URL and API name for token/data fetch)
- External POD API:
  - `.../fetchpodv2?...&search_Text=<SHNUMBER>&bus_id=&service_cat=`

---

## 4. Existing Behavior (As-Is) - Confirmed from Latest FMs

## 4.1 TTC in `Z_SCM_CTD_TRIPCONFIRM` writes vendor dates (not system dates)

```abap
UPDATE zsce_ctd_itm CLIENT SPECIFIED
  SET vendor_remarks   = lv_vendor_remarks
      modified_by      = sy-uname
      modified_date    = sy-datum
      modified_time    = sy-uzeit
      vend_source_date = lw_upd-source_date
      vend_dest_date   = lw_upd-dest_date
 WHERE mandt    = sy-mandt
   AND lifnr    = lw_upd-lifnr
   AND truck_no = lw_upd-truck_no
   AND trip_no  = lw_upd-trip_no
   AND counter  = lw_upd-counter.
```

## 4.2 RTCA check compares vendor dates with system dates

```abap
IF lw_itm_date_check-vend_source_date NE lw_itm_date_check-source_date
OR lw_itm_date_check-vend_dest_date   NE lw_itm_date_check-dest_date.
  lv_auto_rtc_ok = abap_false.
  EXIT.
ENDIF.
```

## 4.3 Enrichment currently defaults SO destination date to `SY-DATUM`

```abap
IF <lfs_trip_items>-dest_date IS INITIAL.
  IF <lfs_trip_items>-mvt_type EQ 'SO'.
    <lfs_trip_items>-dest_date = sy-datum.
  ...
ENDIF.
```

## 4.4 Enrichment currently excludes trip when source/destination is blank

```abap
IF <lfs_trip_items>-source_date IS INITIAL OR
   <lfs_trip_items>-dest_date IS INITIAL.
  lv_exclude_trip = abap_true.
ENDIF.
```

## 4.5 Shipment Load FM hardcodes SO destination date

```abap
lw_trip_itm-dest_date = sy-datum. " Value to be sent from Fiori App'
```

---

## 5. CR1 - POD Fetch and Confirmation Flow

## CR1.1 New FM: `Z_SCM_CTD_GET_PODDATE`

### 5.1.1 Interface

```abap
IMPORTING
  IM_SHNUMBER TYPE OIG_SHNUM
EXPORTING
  EV_POD_DATE TYPE ZDEST_DATE
  EV_SUBRC    TYPE SY-SUBRC
```

### 5.1.2 Responsibilities
- Pure fetch helper FM:
  - reads config from `ZWSO2APIDTL`,
  - fetches token via existing token FM,
  - calls GET POD API,
  - parses JSON and returns POD date.
- No DB update and no `COMMIT WORK`.

### 5.1.3 API Pattern Reference
- Follow GET/HTTP/JSON handling structure from `Z_SCM_FETCH_SHORTAGE_DTLS_API`:
  - `CL_HTTP_CLIENT=>CREATE_BY_URL`
  - set request headers
  - `SEND` / `RECEIVE`
  - response `GET_CDATA`
  - JSON deserialize via `ZUI_CL_JSON`

### 5.1.4 Key Constants (Proposed)
- `PLATFORM = 'SCM'`
- `APINAME` (data fetch): e.g. `POD_DATE_DATAFETCH` (final value by middleware)
- `APINAME` (token): e.g. `POD_DATE_TOKEN` (final value by middleware)

---

## CR1.2 Changes in `Z_SCM_CTD_GETTRIPITM` (UI pre-check and enrichment)

### 5.2.1 Interface Change
Add export:

```abap
VALUE(EV_CONFIRM_BLOCKED) TYPE FLAG
```

### 5.2.2 Insertion Point
After existing:

```abap
SORT lt_trip_itm BY counter.
```

and before:

```abap
et_trip_itm = lt_trip_itm.
```

### 5.2.3 New Types/Data (inside FM)

```abap
TYPES: BEGIN OF lty_so_shn,
         shnumber TYPE oig_shnum,
         pod_date TYPE zdest_date,
       END OF lty_so_shn.

DATA: lt_so_shn      TYPE TABLE OF lty_so_shn,
      lw_so_shn      TYPE lty_so_shn,
      lw_itm_pod     TYPE zscm_ctd_tripitm_st,
      lw_itm_next    TYPE zscm_ctd_tripitm_st,
      lv_pod_date    TYPE zdest_date,
      lv_pod_subrc   TYPE sy-subrc.
```

### 5.2.4 POD Enrichment Logic (in-memory only)

```abap
LOOP AT lt_trip_itm INTO lw_itm_pod.
  IF lw_itm_pod-mvt_type  = 'SO'
 AND lw_itm_pod-leg_type  = 'L'
 AND lw_itm_pod-shnumber  IS NOT INITIAL
 AND lw_itm_pod-dest_date IS INITIAL.
    lw_so_shn-shnumber = lw_itm_pod-shnumber.
    APPEND lw_so_shn TO lt_so_shn.
    CLEAR lw_so_shn.
  ENDIF.
ENDLOOP.

SORT lt_so_shn BY shnumber.
DELETE ADJACENT DUPLICATES FROM lt_so_shn COMPARING shnumber.

LOOP AT lt_so_shn INTO lw_so_shn.
  CLEAR: lv_pod_date, lv_pod_subrc.
  CALL FUNCTION 'Z_SCM_CTD_GET_PODDATE'
    EXPORTING
      im_shnumber = lw_so_shn-shnumber
    IMPORTING
      ev_pod_date = lv_pod_date
      ev_subrc    = lv_pod_subrc.
  IF lv_pod_subrc = 0 AND lv_pod_date IS NOT INITIAL.
    lw_so_shn-pod_date = lv_pod_date.
    MODIFY lt_so_shn FROM lw_so_shn.
  ENDIF.
ENDLOOP.
```

### 5.2.5 Apply date to SO loaded leg + next empty leg

```abap
LOOP AT lt_trip_itm INTO lw_itm_pod.
  IF lw_itm_pod-mvt_type = 'SO'
 AND lw_itm_pod-leg_type = 'L'
 AND lw_itm_pod-shnumber IS NOT INITIAL
 AND lw_itm_pod-dest_date IS INITIAL.

    READ TABLE lt_so_shn INTO lw_so_shn
      WITH KEY shnumber = lw_itm_pod-shnumber
      BINARY SEARCH.
    IF sy-subrc = 0 AND lw_so_shn-pod_date IS NOT INITIAL.
      lw_itm_pod-dest_date      = lw_so_shn-pod_date.
      lw_itm_pod-dest_exit_date = lw_so_shn-pod_date.
      MODIFY lt_trip_itm FROM lw_itm_pod.

      READ TABLE lt_trip_itm INTO lw_itm_next
        WITH KEY counter = lw_itm_pod-counter + 1.
      IF sy-subrc = 0 AND lw_itm_next-leg_type = 'E'.
        lw_itm_next-source_date = lw_so_shn-pod_date.
        MODIFY lt_trip_itm FROM lw_itm_next.
      ENDIF.
    ENDIF.
  ENDIF.
ENDLOOP.
```

### 5.2.6 UI Block Flag

```abap
CLEAR ev_confirm_blocked.
LOOP AT lt_trip_itm INTO lw_itm_pod.
  IF lw_itm_pod-mvt_type  = 'SO'
 AND lw_itm_pod-leg_type  = 'L'
 AND lw_itm_pod-dest_date IS INITIAL.
    ev_confirm_blocked = 'X'.
    lw_return-type = 'W'.
    lw_return-message = 'Please complete Proof of Delivery before confirming this trip'.
    APPEND lw_return TO et_return.
    CLEAR lw_return.
    EXIT.
  ENDIF.
ENDLOOP.
```

---

## CR1.3 Changes in `Z_SCM_CTD_TRIPCONFIRM` (DB persistence during TTC)

### 5.3.1 Current TTC update (keep as-is)
- TTC updates `VEND_SOURCE_DATE`, `VEND_DEST_DATE` from UI payload.
- Do not change this behavior.

### 5.3.2 Insert new Step `1B` location
- After end of Step 1 processing loop (`ENDLOOP` around item updates).
- Before Step 2B/Step 3 commit path.

### 5.3.3 New types/data in FM

```abap
TYPES: BEGIN OF lty_so_leg_pod,
         lifnr    TYPE lifnr,
         truck_no TYPE ytruck_no,
         trip_no  TYPE ztrip_no,
         counter  TYPE zcounter,
         shnumber TYPE oig_shnum,
       END OF lty_so_leg_pod.

TYPES: BEGIN OF lty_so_shn_pod,
         shnumber TYPE oig_shnum,
         pod_date TYPE zdest_date,
       END OF lty_so_shn_pod.

DATA: lt_so_leg_pod    TYPE TABLE OF lty_so_leg_pod,
      lw_so_leg_pod    TYPE lty_so_leg_pod,
      lt_so_shn_pod    TYPE TABLE OF lty_so_shn_pod,
      lw_so_shn_pod    TYPE lty_so_shn_pod,
      lv_pod_date_ttc  TYPE zdest_date,
      lv_pod_subrc_ttc TYPE sy-subrc,
      lv_next_counter  TYPE zcounter.
```

### 5.3.4 Step 1B Logic

```abap
IF im_call_context = 'TTC'.
  SELECT lifnr truck_no trip_no counter shnumber
    FROM zsce_ctd_itm CLIENT SPECIFIED
    INTO TABLE lt_so_leg_pod
    WHERE mandt     = sy-mandt
      AND trip_no   = im_trip_no
      AND mvt_type  = 'SO'
      AND leg_type  = 'L'
      AND dest_date = '00000000'
      AND del_ind  <> 'X'.

  IF sy-subrc = 0.
    LOOP AT lt_so_leg_pod INTO lw_so_leg_pod.
      lw_so_shn_pod-shnumber = lw_so_leg_pod-shnumber.
      APPEND lw_so_shn_pod TO lt_so_shn_pod.
      CLEAR lw_so_shn_pod.
    ENDLOOP.
    SORT lt_so_shn_pod BY shnumber.
    DELETE ADJACENT DUPLICATES FROM lt_so_shn_pod COMPARING shnumber.

    LOOP AT lt_so_shn_pod INTO lw_so_shn_pod.
      CLEAR: lv_pod_date_ttc, lv_pod_subrc_ttc.
      CALL FUNCTION 'Z_SCM_CTD_GET_PODDATE'
        EXPORTING
          im_shnumber = lw_so_shn_pod-shnumber
        IMPORTING
          ev_pod_date = lv_pod_date_ttc
          ev_subrc    = lv_pod_subrc_ttc.
      IF lv_pod_subrc_ttc = 0 AND lv_pod_date_ttc IS NOT INITIAL.
        lw_so_shn_pod-pod_date = lv_pod_date_ttc.
        MODIFY lt_so_shn_pod FROM lw_so_shn_pod.
      ELSE.
        lw_return-type = 'W'.
        lw_return-message = 'Please complete Proof of Delivery before confirming this trip'.
        APPEND lw_return TO et_return.
        CLEAR lw_return.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_so_leg_pod INTO lw_so_leg_pod.
      READ TABLE lt_so_shn_pod INTO lw_so_shn_pod
        WITH KEY shnumber = lw_so_leg_pod-shnumber
        BINARY SEARCH.
      IF sy-subrc = 0 AND lw_so_shn_pod-pod_date IS NOT INITIAL.
        UPDATE zsce_ctd_itm CLIENT SPECIFIED
          SET dest_date      = lw_so_shn_pod-pod_date
              dest_exit_date = lw_so_shn_pod-pod_date
              modified_by    = sy-uname
              modified_date  = sy-datum
              modified_time  = sy-uzeit
          WHERE mandt    = sy-mandt
            AND lifnr    = lw_so_leg_pod-lifnr
            AND truck_no = lw_so_leg_pod-truck_no
            AND trip_no  = lw_so_leg_pod-trip_no
            AND counter  = lw_so_leg_pod-counter.

        lv_next_counter = lw_so_leg_pod-counter + 1.
        UPDATE zsce_ctd_itm CLIENT SPECIFIED
          SET source_date   = lw_so_shn_pod-pod_date
              modified_by   = sy-uname
              modified_date = sy-datum
              modified_time = sy-uzeit
          WHERE mandt    = sy-mandt
            AND lifnr    = lw_so_leg_pod-lifnr
            AND truck_no = lw_so_leg_pod-truck_no
            AND trip_no  = lw_so_leg_pod-trip_no
            AND counter  = lv_next_counter
            AND leg_type = 'E'.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDIF.
```

### 5.3.5 Why this does not conflict with TTC payload update
- Step 1 updates: `VEND_SOURCE_DATE`, `VEND_DEST_DATE`.
- Step 1B updates: `SOURCE_DATE`, `DEST_DATE`, `DEST_EXIT_DATE`.
- Different field sets, same LUW, committed together at Step 3.

---

## 6. CR2 - `Z_SCM_CTD_ENRICHTRIPDETAILS` changes

### 6.1 Replace SO default `SY-DATUM` with POD fetch

Current block:

```abap
IF <lfs_trip_items>-dest_date IS INITIAL.
  IF <lfs_trip_items>-mvt_type EQ 'SO'.
    <lfs_trip_items>-dest_date = sy-datum.
  ELSEIF <lfs_trip_items>-mvt_type EQ 'PO' OR <lfs_trip_items>-mvt_type EQ 'STO'.
    ...
  ENDIF.
ENDIF.
```

Proposed:

```abap
IF <lfs_trip_items>-dest_date IS INITIAL.
  IF <lfs_trip_items>-mvt_type EQ 'SO'.
    CLEAR: lv_pod_date_enrich, lv_pod_subrc_enrich.
    CALL FUNCTION 'Z_SCM_CTD_GET_PODDATE'
      EXPORTING
        im_shnumber = <lfs_trip_items>-shnumber
      IMPORTING
        ev_pod_date = lv_pod_date_enrich
        ev_subrc    = lv_pod_subrc_enrich.
    IF lv_pod_subrc_enrich = 0 AND lv_pod_date_enrich IS NOT INITIAL.
      <lfs_trip_items>-dest_date = lv_pod_date_enrich.
    ENDIF.
  ELSEIF <lfs_trip_items>-mvt_type EQ 'PO' OR <lfs_trip_items>-mvt_type EQ 'STO'.
    ...
  ENDIF.
ENDIF.
```

### 6.2 Modify trip exclusion rule

Current:

```abap
IF <lfs_trip_items>-source_date IS INITIAL OR
   <lfs_trip_items>-dest_date IS INITIAL.
  lv_exclude_trip = abap_true.
ENDIF.
```

Proposed:

```abap
IF <lfs_trip_items>-source_date IS INITIAL OR
   ( <lfs_trip_items>-dest_date IS INITIAL AND
     <lfs_trip_items>-mvt_type  NE 'SO' ).
  lv_exclude_trip = abap_true.
ENDIF.
```

This allows SO trip to remain processable when POD is still pending.

---

## 7. CR3 - `Z_SCE_TRIP_STLMNT` change

### 7.1 Required modification

Current:

```abap
lw_trip_itm-dest_date = sy-datum. " Value to be sent from Fiori App'
```

Change:
- Comment/remove this line for SO path only.

### 7.2 Reason
- If this remains, `DEST_DATE` is never initial for SO, so POD fetch logic in CR2 will not trigger.

---

## 8. Configuration Changes - `ZWSO2APIDTL`

Required entries per environment:
- POD data fetch API entry (`PLATFORM = 'SCM'`, `ACTIVE = 'X'`).
- POD token API entry (`PLATFORM = 'SCM'`, `ACTIVE = 'X'`).

Fields:
- `MANDT`, `PLATFORM`, `APINAME`, `URL`, `ACTIVE`,
- plus credential/token-related columns as currently maintained.

QA/PROD URL values pending from business/infra team.

---

## 9. UI Linkage (Reference)

`Z_SCM_CTD_GETTRIPITM` returns:
- `EV_CONFIRM_BLOCKED = 'X'` when any SO loaded leg still has blank POD-derived `DEST_DATE`.
- `ET_RETURN` message:
  - `Please complete Proof of Delivery before confirming this trip`

UI behavior (outside ABAP scope):
- Disable confirm button,
- Show message near trip header / relevant leg.

---

## 10. Technical Test Cases

1. SO trip with POD available:
   - GETTRIPITM enriches SO + next empty leg in-memory.
   - TRIPCONFIRM Step 1B persists dates to `ZSCE_CTD_ITM`.
2. SO trip with POD unavailable:
   - GETTRIPITM returns block flag and message.
   - UI blocks confirm.
3. Enrichment run with pending POD:
   - SO trip not excluded only for blank `DEST_DATE`.
4. Shipment load flow:
   - SO `DEST_DATE` not forced to `SY-DATUM`.
5. Regression:
   - PO/STO unchanged.
   - RTCA compare still uses `VEND_*` vs system dates.

---

## 11. Implementation Summary

- CR1 introduces POD API helper FM and integrates it in read path (`GETTRIPITM`) and confirm path (`TRIPCONFIRM`).
- CR2 aligns enrichment derivation/exclusion to allow SO pending POD state.
- CR3 removes conflicting SO date hardcoding in shipment settlement FM.
- No LUW split introduced; persistence remains under existing commit structure.

