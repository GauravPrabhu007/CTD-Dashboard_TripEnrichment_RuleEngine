# Code Change Document – Z_SCM_CTD_ENRICHTRIPDETAILS  
## Complete Requirements: Date Range, ALV (Executed + Excluded), Leg-Level Remarks, DB Update Only for Executed, Status 03/04

**Function Module:** Z_SCM_CTD_ENRICHTRIPDETAILS  
**Program:** ZSCM_CTD_ENRICHTRIPDETAILS  
**Tables:** ZSCE_CTD_HDR, ZSCE_CTD_ITM  
**Change Type:** Enhancement + Logic correction  
**Date:** 2026-02-25  
**Reference:** CD: 8086962 TR: RD2K9A5FGT | Technical: Omkar More | Functional: Gaurav Prabhu

---

### Executive summary (for ABAP team)

1. **Date:** Only trips with ZSCE_CTD_HDR.CREATED_DATE in From/To range.  
2. **No early exit:** If some trips fail validation, still process the rest; show all (executed + excluded) in ALV with remarks.  
3. **Leg-level remarks:** Loaded-leg errors (e.g. missing source/dest date) and empty-leg errors (route/distance) on **that leg** in ALV.  
4. **DB update only for executed:** Update ZSCE_CTD_HDR / ZSCE_CTD_ITM only when trip is executed (action 'S'); excluded trips → no DB update, status stays '03'.  
5. **Status:** Excluded = '03', Executed = '04' (in ALV and in ZSCE_CTD_HDR for executed only).  
6. **Fix Step 4 DELETE:** Use `WHERE trip_no IN ltr_trip_no_ex[]` (remove excluded), not NOT IN.

---

## 1. Summary of Requirements

| # | Requirement | Summary |
|---|-------------|--------|
| **1** | **Date range** | Only trips with **ZSCE_CTD_HDR.CREATED_DATE** between From Date and To Date (inclusive) may be considered for processing. |
| **2** | **Excluded trips & ALV** | When some trips are excluded due to validation: (a) show error/remark **in ALV** for that trip; (b) **do not** stop the run—process all other valid trips in the date range; (c) ALV must show **both** executed and excluded trips with respective remarks. |
| **3** | **Leg-level validation in ALV; DB only for executed** | Validation errors for **loaded legs** → displayed in ALV against **that loaded leg**; for **empty legs** → against **that empty leg**. In normal mode (simulate not selected): output shows **all** trips; **database update only for executed trips** (no update for excluded). |
| **4** | **Trip status in ZSCE_CTD_HDR and ALV** | **Excluded** trips: TRIP_STATUS remains **'03'** (no DB update). **Executed** trips: TRIP_STATUS set to **'04'**. Same status (03/04) displayed in ALV. |

---

## 2. Table References

### 2.1 ZSCE_CTD_HDR

| Field | Key | Description |
|-------|-----|-------------|
| MANDT | Yes | Client |
| LIFNR | Yes | Vendor |
| TRUCK_NO | Yes | Truck Number |
| TRIP_NO | Yes | Trip Number |
| **TRIP_STATUS** | | **'03' = Completed (excluded), '04' = Enriched (executed)** |
| AREA | | Area |
| CTD_RULEENG_REMARKS | | Remarks (e.g. exclusion reason) |
| CREATED_DATE | | **Used for date range filter (From/To Date)** |
| CREATED_BY, CREATED_TIME, MODIFIED_* | | Audit fields |

### 2.2 ZSCE_CTD_ITM

| Field | Key | Description |
|-------|-----|-------------|
| MANDT, LIFNR, TRUCK_NO, TRIP_NO | Yes | Header key |
| COUNTER | Yes | Leg counter |
| LEG_TYPE | | 'L' = Loaded, 'E' = Empty |
| SOURCE_*, DEST_*, ROUTE, etc. | | Leg details |

**DB update:** Only for legs of **executed** trips (TRIP_STATUS = '04'). No insert/update for excluded trips.

---

## 3. Current vs Required Behavior

| Area | Current behavior | Required behavior |
|------|------------------|--------------------|
| **Step 2 – Date range** | SELECT uses trip_status = '03'; then DELETE keeps created_date in range. **If IT_TRIP_NUMBERS** passed and any trip outside range → message to ET_RETURN and **RETURN** (no processing). | Only trips with **CREATED_DATE** in From/To range considered. Do **not** RETURN when some input trips are outside range; exclude those trips, add to ALV with remark, **continue** with in-range trips. |
| **Step 4 – Missing source/dest date** | Excluded trips added to LTR_TRIP_NO_EX, message to ET_RETURN, then **DELETE uses NOT IN** (wrong: deletes valid, keeps excluded). Excluded trips **never** in ET_ENRICHDET. | **Correct DELETE** to remove excluded trips (IN ltr_trip_no_ex). **Before** DELETE: copy excluded trip headers to LT_EXCLUDED_HEADERS. **At Step 9:** append one row **per leg** of excluded trip to ET_ENRICHDET with TRIP_STATUS = '03', REMARKS = text 014 (loaded-leg-level display). Continue processing valid trips. |
| **Step 8 – Empty leg route/distance** | Remark set only on **empty legs** (correct for leg-level). Trip still in lt_trip_headers; **DB is updated** for these excluded trips. | Keep leg-level remarks on empty legs. **Do not update DB** for excluded trips (action 'E'). |
| **Step 9 – ET_ENRICHDET** | Only legs of trips that reached enrichment. Step 4 excluded trips missing. | Add legs for Step 4 excluded trips (from LT_EXCLUDED_HEADERS → one row per leg with remark). Add Step 2 excluded trips (e.g. one row per trip or per leg with remark). |
| **Step 10 – DB update** | Updates ZSCE_CTD_HDR and ZSCE_CTD_ITM for **all** trips in lt_trip_headers (including Step 8 excluded). | Update **only** for **executed** trips (action 'S' / TRIP_STATUS '04'). Skip ZSCE_CTD_HDR and ZSCE_CTD_ITM for excluded trips so TRIP_STATUS remains '03'. |

---

## 4. Detailed Code Changes (Function Module)

### 4.1 Data declarations (after existing tables, ~line 250)

Add:

```abap
* Table for Step 4 excluded trips (for ALV output - one row per leg with remark)
  TYPES: BEGIN OF ty_excluded_leg,
           lifnr        TYPE lifnr,
           truck_no     TYPE ytruck_no,
           trip_no      TYPE ztrip_no,
           counter      TYPE zcounter,
           area         TYPE yarea,
           leg_type     TYPE zleg_type,
           remarks      TYPE zctd_ruleeng_remarks,
         END OF ty_excluded_leg.

  DATA: lt_excluded_headers   TYPE TABLE OF ty_trip_header,  " Step 4: excluded trip headers (before DELETE)
        lt_excluded_legs      TYPE TABLE OF ty_excluded_leg, " Step 4: one row per leg for ET_ENRICHDET
        lw_excluded_leg       TYPE ty_excluded_leg,
        lt_trip_no_out_range  TYPE TABLE OF zscm_tripno_st. " Step 2: trips outside date range (for ALV)
```

*Rationale:* LT_EXCLUDED_HEADERS holds header data for Step 4 excluded trips before they are removed. LT_EXCLUDED_LEGS is built from headers + items to produce one ALV row per leg with remark. LT_TRIP_NO_OUT_RANGE stores trip numbers that are outside date range (Step 2) for ALV.

---

### 4.2 Step 2 – Fetch and filter by CREATED_DATE (lines 304–355)

**4.2.1 Date filter (already correct)**  
- Keep: SELECT from ZSCE_CTD_HDR with trip_status = '03'; then  
  `DELETE lt_trip_headers WHERE NOT created_date >= i_from_date AND created_date <= i_to_date.`  
- This ensures only trips with **CREATED_DATE** in the From/To range are considered. No change needed if this is the only date filter applied.

**4.2.2 When IT_TRIP_NUMBERS is passed**

- **Current (lines 323–355):** Loop at IT_TRIP_NUMBERS; if trip not in lt_trip_headers (after date filter), append message to ET_RETURN. Then delete headers where trip_no NOT IN ltr_trip_no. Then **IF et_return[] IS NOT INITIAL. RETURN. ENDIF.**  
- **Required:**
  1. **Do not RETURN** when ET_RETURN has messages (remove lines 353–355).
  2. Build list of trips that **are** in range: e.g. from current LT_TRIP_HEADERS after date filter. If IT_TRIP_NUMBERS is not initial, restrict LT_TRIP_HEADERS to those in-range trip numbers (keep only headers where trip_no is in the in-range list).
  3. For each trip from IT_TRIP_NUMBERS that is **not** in the remaining LT_TRIP_HEADERS, store in LT_TRIP_NO_OUT_RANGE (trip_no) for later ALV. Optionally SELECT from ZSCE_CTD_HDR for those trip numbers to get LIFNR, TRUCK_NO for ALV rows.
  4. Continue with remaining LT_TRIP_HEADERS for further processing.

**Concrete change:** Delete or comment out:

```abap
  IF et_return[] IS NOT INITIAL.
    RETURN.
  ENDIF.
```

Replace with logic that: (a) builds in-range trip list from current lt_trip_headers; (b) if it_trip_numbers is passed, removes from lt_trip_headers any trip not in it_trip_numbers (so only requested + in-range trips remain); (c) fills lt_trip_no_out_range with requested trips that are not in lt_trip_headers (outside date range); (d) does **not** RETURN.

---

### 4.3 Step 4 – Source/destination date validation (lines 483–489)

**4.3.1 Fix DELETE logic (critical)**

- **Current (lines 486–488):**  
  `DELETE lt_trip_items   WHERE trip_no NOT IN ltr_trip_no_ex[].`  
  `DELETE lt_trip_headers WHERE trip_no NOT IN ltr_trip_no_ex[].`  
  This deletes **valid** trips (trip_no not in exclusion list) and keeps excluded. **Incorrect.**

- **Required:** Delete **excluded** trips, keep valid:

```abap
  IF ltr_trip_no_ex IS NOT INITIAL.
    " Copy excluded trip headers for ALV before deleting
    LOOP AT lt_trip_headers INTO lw_trip_header
      WHERE trip_no IN ltr_trip_no_ex[].
      APPEND lw_trip_header TO lt_excluded_headers.
    ENDLOOP.
    " Build one row per leg for excluded trips (for ALV with remark on each leg)
    LOOP AT lt_excluded_headers INTO lw_trip_header.
      LOOP AT lt_trip_items INTO lw_trip_item
        WHERE lifnr   = lw_trip_header-lifnr
          AND truck_no = lw_trip_header-truck_no
          AND trip_no  = lw_trip_header-trip_no.
        CLEAR lw_excluded_leg.
        MOVE-CORRESPONDING lw_trip_item TO lw_excluded_leg.
        lw_excluded_leg-remarks = 'Excluded - missing source or destination date'(014).
        APPEND lw_excluded_leg TO lt_excluded_legs.
      ENDLOOP.
    ENDLOOP.
    DELETE lt_trip_items   WHERE trip_no IN ltr_trip_no_ex[].
    DELETE lt_trip_headers WHERE trip_no IN ltr_trip_no_ex[].
  ENDIF.
```

- Use a work area `lw_excluded_leg` type `ty_excluded_leg`. Ensure REMARKS uses text symbol 014 or the same text as message 007.

**4.3.2 Keep**

- Return only when **no valid trips remain:** `IF lt_trip_items IS INITIAL.` … message 008, RETURN.

---

### 4.4 Step 9 – Build ET_ENRICHDET (before main loop ~line 1100)

**4.4.1 Append Step 4 excluded legs (one row per leg)**

- Before the loop `LOOP AT lt_enrichments ASSIGNING <lfs_enrichment>`, append rows to ET_ENRICHDET from LT_EXCLUDED_LEGS (and optionally from LT_EXCLUDED_HEADERS if structure differs). Each row must have: LIFNR, TRUCK_NO, TRIP_NO, COUNTER, AREA, LEG_TYPE, TRIP_STATUS = '03', REMARKS = 'Excluded - missing source or destination date' (or text 014). Map to ET_ENRICHDET structure (ZSCM_ENRICH_DET_ST). This gives **validation errors for loaded legs displayed against that leg** (each leg row has the remark).

**4.4.2 Append Step 2 excluded trips (outside date range)**

- For each entry in LT_TRIP_NO_OUT_RANGE, append one row to ET_ENRICHDET with TRIP_NO, REMARKS = 'Trip does not fall within the given date range'(011), TRIP_STATUS = '03'. If LIFNR/TRUCK_NO were read from ZSCE_CTD_HDR for these trips, fill them; otherwise leave initial or use a single representative row per trip.

---

### 4.5 Step 8 contd + Step 9 – DB tables only for executed trips (lines 1130–1146)

**Current:** For every leg in lt_enrichments, when i_test_run is initial, append to lt_ctd_itm_insert or lt_ctd_itm_update regardless of trip action.

**Required:** Add DB (insert/update) only for legs of **executed** trips (action 'S'):

```abap
*     --- Build DB insert/update tables (only for executed trips) ---
    IF i_test_run IS INITIAL.
      READ TABLE lt_trip_action INTO ls_trip_action
           WITH KEY trip_no = <lfs_enrichment>-trip_no BINARY SEARCH.
      IF sy-subrc = 0 AND ls_trip_action-action = 'S'.
        CLEAR lw_ctd_itm_update.
        MOVE-CORRESPONDING <lfs_enrichment> TO lw_ctd_itm_update.
        ...
        IF <lfs_enrichment>-is_new_leg = abap_true.
          APPEND lw_ctd_itm_update TO lt_ctd_itm_insert.
        ELSE.
          APPEND lw_ctd_itm_update TO lt_ctd_itm_update.
        ENDIF.
      ENDIF.
    ENDIF.
```

- So: **excluded** trips (action 'E') do **not** get any ZSCE_CTD_ITM insert/update. TRIP_STATUS in ZSCE_CTD_HDR remains '03' because we do not update the header for them (see 4.6).

---

### 4.6 Step 10 – Update ZSCE_CTD_HDR only for executed trips (lines 1208–1241)

**Current:** Loop at lt_trip_headers; for each trip read first enrichment row and update ZSCE_CTD_HDR with trip_status and ctd_ruleeng_remarks. So excluded trips (still in lt_trip_headers) get header updated to '03' + remarks.

**Required:** Update ZSCE_CTD_HDR **only** for trips with action 'S' (executed). For excluded trips, **do not** update the header so **TRIP_STATUS remains '03'** in the database.

- Option A: Build a list of executed trip numbers (e.g. from lt_trip_action where action = 'S'). In the header update loop, **skip** the UPDATE when trip_no is not in that list.
- Option B: Loop only over executed trips: e.g. read lt_trip_action where action = 'S', then for each such trip_no find lw_trip_header and perform UPDATE.

Example (Option A – minimal change):

```abap
*     Update ZSCE_CTD_HDR (only for executed trips; excluded remain '03')
    LOOP AT lt_trip_headers INTO lw_trip_header.
      READ TABLE lt_trip_action INTO ls_trip_action
           WITH KEY trip_no = lw_trip_header-trip_no BINARY SEARCH.
      IF sy-subrc <> 0 OR ls_trip_action-action <> 'S'.
        CONTINUE.  " Skip DB update for excluded trips
      ENDIF.
      CLEAR lw_ctd_hdr_update.
      READ TABLE lt_enrichments INTO lw_enrichment
        WITH KEY trip_no = lw_trip_header-trip_no.
      IF sy-subrc = 0.
        lw_ctd_hdr_update-trip_status = lc_status_enriched.  " '04' for executed
        lw_ctd_hdr_update-ctd_ruleeng_remarks = lw_enrichment-remarks.
      ELSE.
        lw_ctd_hdr_update-trip_status = lc_status_enriched.
      ENDIF.
      UPDATE zsce_ctd_hdr
        SET trip_status          = lw_ctd_hdr_update-trip_status
            ctd_ruleeng_remarks  = lw_ctd_hdr_update-ctd_ruleeng_remarks
            ...
```

- For **executed** trips: set TRIP_STATUS = '04' (lc_status_enriched). For **excluded** trips: no UPDATE, so TRIP_STATUS stays '03'.

---

### 4.7 ALV output – TRIP_STATUS and REMARKS

- ET_ENRICHDET already carries trip_status and remarks per leg. Ensure:
  - **Executed** trips: trip_status = '04' (set in Step 8/9 for action 'S').
  - **Excluded** trips (Step 4, Step 2, Step 8): trip_status = '03', with remarks on the relevant leg(s).
- Program ALV: ensure columns TRIP_STATUS and REMARKS (or equivalent) are displayed so 03/04 and exclusion reasons are visible.

---

## 5. Program ZSCM_CTD_ENRICHTRIPDETAILS

- ALV must be built from **ET_ENRICHDET** (all trips: executed + excluded).
- Include columns: **TRIP_STATUS** ('03' / '04') and **REMARKS** (leg-level validation messages).
- Optionally show **ET_RETURN** in a message area or second output so all validation messages are visible.

---

## 6. Implementation Checklist

| # | Task | Location / Note |
|---|------|------------------|
| 1 | Add types and data: ty_excluded_leg, lt_excluded_headers, lt_excluded_legs, lt_trip_no_out_range | Data declarations |
| 2 | Step 2: Remove RETURN when et_return has entries; build in-range list; fill lt_trip_no_out_range for out-of-range trips | After line 351 |
| 3 | Step 4: Before DELETE, copy excluded headers to lt_excluded_headers; build lt_excluded_legs (one row per leg, remark 014); change DELETE to IN ltr_trip_no_ex | Lines 484–489 |
| 4 | Step 9: Before main loop, append lt_excluded_legs to ET_ENRICHDET (TRIP_STATUS '03', REMARKS); append Step 2 excluded (lt_trip_no_out_range) to ET_ENRICHDET | Before ~1100 |
| 5 | Step 9/10 build: Add DB insert/update only when ls_trip_action-action = 'S' | Lines 1130–1146 |
| 6 | Step 10: Update ZSCE_CTD_HDR only for action = 'S'; excluded trips skipped so status remains '03' | Lines 1208–1241 |
| 7 | Program: Confirm ALV shows ET_ENRICHDET with TRIP_STATUS and REMARKS | Report / class |

---

## 7. Test Checklist

- [ ] **Date range:** Only trips with CREATED_DATE between From and To appear in processing and ALV.
- [ ] **Step 2:** Some trips outside date range → messages in ET_RETURN; those trips in ALV with remark; in-range trips processed and in ALV.
- [ ] **Step 4:** Some trips missing source/dest date → excluded; one row **per leg** in ALV with remark; valid trips processed; DELETE uses IN ltr_trip_no_ex.
- [ ] **Step 8:** Empty leg route/distance missing → remark on **that empty leg** in ALV; trip status '03' in ALV; no DB update for that trip.
- [ ] **Normal mode:** ALV shows all trips (executed + excluded); ZSCE_CTD_HDR updated only for executed (status '04'); ZSCE_CTD_ITM updated/inserted only for executed trips; excluded trips remain '03'.
- [ ] **Simulate:** No DB updates; ALV still shows all with correct status and remarks.

---

## 8. Message / Text References

- Text 011: 'Trip does not fall within the given date range'
- Text 014: 'Excluded - missing source or destination date'
- Text 016: 'Empty Leg Route / Distance missing'
- Message 004: Trip … does not fall within the given date range
- Message 007: Trip … excluded - missing source or destination date
- Message 009: Trip … excluded - Empty leg route/distance missing (with zones)

Use consistently in REMARKS and ET_RETURN.

---

*End of document*
