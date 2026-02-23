# Code Change Document – Z_SCM_CTD_ENRICHTRIPDETAILS  
## Empty Leg dest_date: Derive from Next Trip (ZSCE_CTD_HDR)

**Function Module:** Z_SCM_CTD_ENRICHTRIPDETAILS  
**Purpose:** For empty legs inserted when there is only one loaded leg OR after the last loaded leg, set `lw_enrichment_new-dest_date` to the CREATED_DATE of the next available trip (same LIFNR + TRUCK_NO, CREATED_DATE >= source_date), excluding the current trip.  
**Date:** 2026-02-23  
**Reference:** Empty leg determination (single loaded leg / after last loaded leg); table ZSCE_CTD_HDR (TRIP_NO, AREA, CREATED_DATE).

---

## 1. Summary of Changes

| # | Change | Location |
|---|--------|----------|
| 1 | Add local table for dest_date candidates | Data declarations (or inline in Step 7) |
| 2 | Replace dest_date logic – single loaded leg | Step 7: empty leg block when current is first leg (single-leg trip) |
| 3 | Replace dest_date logic – after last loaded leg | Step 7: empty leg block when current is last leg (multi-leg trip) |

**Scope:** Only the field **`lw_enrichment_new-dest_date`** for the two empty-leg cases above. All other empty-leg fields (source_tzone, dest_tzone, source_date, route, distance, etc.) remain unchanged.

**Behaviour:**
- **Relevant empty legs:** (1) Single loaded leg in trip → empty leg from dest back to source. (2) Last loaded leg of multi-leg trip → return empty leg from last leg’s dest to first leg’s source.
- **New dest_date logic:** From ZSCE_CTD_HDR (or from existing `lt_zsce_ctd_hdr`), consider records where LIFNR = `lw_enrichment_new-lifnr`, TRUCK_NO = `lw_enrichment_new-truck_no`, CREATED_DATE >= `lw_enrichment_new-source_date`. Exclude the current trip. Sort by CREATED_DATE ascending. Set **`lw_enrichment_new-dest_date` = CREATED_DATE of the first record**. If no record remains after exclusion, keep fallback **`lw_enrichment_new-dest_date = lw_enrichment_new-source_date`**.
- **Unchanged:** Empty leg inserted *between* two loaded legs continues to use `lw_enrichment_new-dest_date = <lfs_enrichment_prev>-source_ent_date`.

---

## 2. Exact Code Changes

**Reference implementation:** *FM - Z_SCM_CTD_ENRICHTRIPDETAILS New.txt*  
Step 7 (Determine and Insert Empty Legs). Existing type **lty_zsce_ctd_hdr** (lifnr, truck_no, trip_no, created_date) and table **lt_zsce_ctd_hdr** are already filled before the loop (SELECT from ZSCE_CTD_HDR by lifnr, truck_no for all enrichment combinations). Use this table to derive candidates; no new SELECT required if all trips for the vendor/truck are already in **lt_zsce_ctd_hdr**.

---

### 2.1 Data for dest_date candidates (optional: declare in Step 7 or in main data block)

If preferred to declare once in the main data block, add:

```abap
        lt_dest_date_candidates  TYPE TABLE OF lty_zsce_ctd_hdr,
```

Alternatively, use a local `DATA` inside each of the two empty-leg blocks (see below).

---

### 2.2 Replace dest_date logic – Single loaded leg (single-leg trip)

**Location:** Step 7, block where empty leg is created for **single-leg trip** (current leg is the first and only loaded leg). In *FM - Z_SCM_CTD_ENRICHTRIPDETAILS New.txt* this is approximately after the line:

`lw_enrichment_new-source_date  = <lfs_enrichment>-dest_exit_date.`

**Remove** the existing block:

```abap
              CLEAR lw_zsce_ctd_hdr.
              READ TABLE lt_zsce_ctd_hdr INTO lw_zsce_ctd_hdr WITH KEY lifnr    = <lfs_enrichment>-lifnr
                                                                       truck_no = <lfs_enrichment>-truck_no BINARY SEARCH.
              IF sy-subrc = 0 AND lw_zsce_ctd_hdr-created_date >= lw_enrichment_new-source_date.
               lw_enrichment_new-dest_date    = lw_zsce_ctd_hdr-created_date.
              ELSE.
               lw_enrichment_new-dest_date    = lw_enrichment_new-source_date.
              ENDIF.
```

**Insert** the following:

```abap
*             dest_date = first CREATED_DATE (asc) from other trips (same LIFNR/TRUCK_NO, created_date >= source_date), excluding current trip
              DATA lt_dest_date_candidates TYPE TABLE OF lty_zsce_ctd_hdr.
              CLEAR lt_dest_date_candidates.
              LOOP AT lt_zsce_ctd_hdr INTO lw_zsce_ctd_hdr
                WHERE lifnr        = lw_enrichment_new-lifnr
                  AND truck_no     = lw_enrichment_new-truck_no
                  AND created_date >= lw_enrichment_new-source_date.
                APPEND lw_zsce_ctd_hdr TO lt_dest_date_candidates.
              ENDLOOP.
              DELETE lt_dest_date_candidates WHERE trip_no = lw_enrichment_new-trip_no.
              SORT lt_dest_date_candidates BY created_date ASCENDING.
              READ TABLE lt_dest_date_candidates INDEX 1 INTO lw_zsce_ctd_hdr.
              IF sy-subrc = 0.
                lw_enrichment_new-dest_date = lw_zsce_ctd_hdr-created_date.
              ELSE.
                lw_enrichment_new-dest_date = lw_enrichment_new-source_date.
              ENDIF.
```

---

### 2.3 Replace dest_date logic – After last loaded leg (multi-leg trip)

**Location:** Step 7, block where empty leg is created **after the last loaded leg** (return empty leg: source = last leg’s dest, dest = first leg’s source). In *FM - Z_SCM_CTD_ENRICHTRIPDETAILS New.txt* this is approximately after:

`lw_enrichment_new-source_date  = <lfs_enrichment>-dest_exit_date.`

**Remove** the existing block:

```abap
            CLEAR lw_zsce_ctd_hdr.
            READ TABLE lt_zsce_ctd_hdr INTO lw_zsce_ctd_hdr WITH KEY lifnr    = <lfs_enrichment>-lifnr
                                                                     truck_no = <lfs_enrichment>-truck_no BINARY SEARCH.
            IF sy-subrc = 0 AND lw_zsce_ctd_hdr-created_date >= lw_enrichment_new-source_date.
             lw_enrichment_new-dest_date    = lw_zsce_ctd_hdr-created_date.
            ELSE.
             lw_enrichment_new-dest_date    = lw_enrichment_new-source_date.
            ENDIF.
```

**Insert** the same logic as in section 2.2 (same code block as above, with `lt_dest_date_candidates`, LOOP at `lt_zsce_ctd_hdr`, DELETE current trip, SORT by created_date, READ INDEX 1, set dest_date or fallback to source_date).

---

## 3. Logic Summary (for confirmation)

1. **Select:** From records already in **lt_zsce_ctd_hdr** (or from ZSCE_CTD_HDR), use those where **LIFNR** = `lw_enrichment_new-lifnr`, **TRUCK_NO** = `lw_enrichment_new-truck_no`, **CREATED_DATE** >= `lw_enrichment_new-source_date`. (Implementation above uses **lt_zsce_ctd_hdr** with a LOOP + WHERE.)
2. **Exclude current trip:** Remove from the candidate list the row where **TRIP_NO** = `lw_enrichment_new-trip_no` (the trip being processed).
3. **Sort:** Sort the resulting internal table by **CREATED_DATE** ascending.
4. **Set dest_date:** **lw_enrichment_new-dest_date** = **CREATED_DATE** of the first record. If the table is empty after step 2, **lw_enrichment_new-dest_date** = **lw_enrichment_new-source_date**.

---

## 4. Optional: Include AREA in type and SELECT

If AREA is required for reporting or downstream logic, extend **lty_zsce_ctd_hdr** with **area** and add it to the existing SELECT that fills **lt_zsce_ctd_hdr** (e.g. `SELECT lifnr truck_no trip_no area created_date FROM zsce_ctd_hdr ...`). The dest_date derivation above does not use AREA; this is optional.

---

## 5. Summary of Edits

| # | Section | Action |
|---|---------|--------|
| 1 | Data (optional) | Add `lt_dest_date_candidates TYPE TABLE OF lty_zsce_ctd_hdr` in main data or use local DATA in blocks. |
| 2 | Step 7 – single-leg empty leg | Replace READ TABLE + IF/ELSE dest_date block with LOOP/DELETE/SORT/READ logic. |
| 3 | Step 7 – last-leg empty leg | Same replacement as in step 2. |
| 4 | Optional | Add AREA to lty_zsce_ctd_hdr and to SELECT into lt_zsce_ctd_hdr if needed. |

---

## 6. Testing Suggestions

- **Single loaded leg:** Trip with one loaded leg and area gap → empty leg inserted; **dest_date** = CREATED_DATE of next trip (same LIFNR/TRUCK_NO, created_date >= empty leg source_date), or source_date if no such trip or only current trip.
- **Last loaded leg:** Multi-leg trip with area gap after last leg → return empty leg; **dest_date** = same rule as above; current trip must be excluded so dest_date is not the current trip’s created_date.
- **Between two loaded legs:** No change; dest_date remains **&lt;lfs_enrichment_prev&gt;-source_ent_date**.
- **No other trip:** After excluding current trip, no row left → **dest_date** = **source_date**.
