# Code Change Document – Z_SCM_CTD_ENRICHTRIPDETAILS  
## Empty Leg Destination Date: Derive from ZSCE_CTD_HDR (After Last Loaded Leg Only)

**Function Module:** Z_SCM_CTD_ENRICHTRIPDETAILS  
**Tables:** ZSCE_CTD_HDR, ZSCE_CTD_ITM  
**Change Type:** Logic correction (destination date for empty leg after last loaded leg)  
**Date:** 2026-03-05  
**Reference:** CD: 8086964 TR: RD2K9A5HKK | Technical: Omkar More | Functional: Gaurav Prabhu  
**Reference implementation:** *FM - Z_SCM_CTD_ENRICHTRIPDETAILS_05.03.2026.txt*

---

## 1. Executive Summary

For **empty legs that occur after the last loaded leg** (single loaded leg trip or multiple loaded leg trip), the **Destination Date** must be derived from the already-fetched internal table `lt_zsce_ctd_hdr` (ZSCE_CTD_HDR data) as follows:

1. **Before the main empty-leg loop (Step 7):** Sort `lt_zsce_ctd_hdr` **once** by LIFNR, TRUCK_NO, CREATED_DATE, TRIP_NO ascending.
2. **Inside the loop, per trip:** Use a single `READ TABLE … BINARY SEARCH` with `TRIP_NO = SPACE` (minimum value) as the fourth key field. Because no real trip has TRIP_NO = SPACE, the binary search always returns SY-SUBRC ≠ 0 and positions SY-TABIX at the **insertion point** — which is the first record where LIFNR, TRUCK_NO, and CREATED_DATE ≥ Source Date of the empty leg.
3. Read the record at SY-TABIX. If it belongs to the current trip (TRIP_NO = current), read the **next** record at SY-TABIX + 1.
4. At most **2 READ TABLE INDEX** calls. No `DO / ENDDO`, no nested `LOOP AT`, no `SORT` inside loop, no `DELETE` inside loop.
5. That record's **CREATED_DATE** = **Destination Date of the last empty leg**. If no eligible record exists, fallback to SY-DATUM.

**Sandwiched empty legs** (between two loaded legs within a trip) are **unchanged**: Destination Date = Source Entry Date of the next loaded leg (already in code at line 993 of reference FM).

---

## 2. Why trip_no = space Works as the Search Key

| Concept | Explanation |
|---------|-------------|
| Sort sequence | `lt_zsce_ctd_hdr` sorted by LIFNR, TRUCK_NO, CREATED_DATE, TRIP_NO ascending. |
| `SPACE` as minimum | Character field TRIP_NO with value SPACE (X'20') is less than any real alphanumeric trip number. |
| Binary search insertion point | When SY-SUBRC ≠ 0 after `READ TABLE … BINARY SEARCH`, SY-TABIX points to where the searched record **would be inserted** — i.e., the first position with key ≥ searched key. |
| Effect | Searching with (LIFNR=X, TRUCK_NO=Y, CREATED_DATE=threshold, TRIP_NO=SPACE) always fails to find an exact match (SY-SUBRC ≠ 0) and positions SY-TABIX at the **first record** for LIFNR=X, TRUCK_NO=Y with CREATED_DATE ≥ threshold. |
| Current trip exclusion | If the record at SY-TABIX is the current trip, read SY-TABIX+1. At most one trip has the current TRIP_NO, so at most **2 READ TABLE INDEX** calls are needed. |

---

## 3. Scope and Out of Scope

| In scope | Out of scope |
|----------|--------------|
| Empty leg **after** the last loaded leg — single loaded leg trip | Sandwiched empty legs (between two loaded legs) — dest_date = next loaded leg's source_ent_date — no change |
| Empty leg **after** the last loaded leg — last leg of a multi-leg trip | All other FM logic (Steps 1–6, 8–10) |
| Correct sort and binary-search-based derivation replacing global current-trip deletion | |

---

## 4. ABAP Rules Referenced

Code changes must conform to the following (see folder **ABAP RULES**):

| Rule file | Relevant constraint enforced |
|-----------|------------------------------|
| **03-database.mdc** | No SELECT inside LOOP; SY-SUBRC checked after every READ TABLE. |
| **12-documentation.mdc** | Double-quote comments; all Cursor-generated code wrapped in `" BEGIN: Cursor Generated Code` / `" END: Cursor Generated Code`. |
| **20-code-generation-checklist.mdc** | No nested LOOP AT inside LOOP AT; no SORT inside LOOP; no DELETE inside LOOP; no DO / ENDDO; SORT before BINARY SEARCH; variables declared upfront; no 7.40+ syntax. |

---

## 5. Current vs Required Behavior

| Aspect | Current behavior | Required behavior |
|--------|------------------|-------------------|
| **Global deletion of current trips** | After SELECT into `lt_zsce_ctd_hdr`, all trips in current run deleted once: `DELETE lt_zsce_ctd_hdr WHERE trip_no IN ltr_current_trip[]`. | **Remove this block.** Retain all trips in `lt_zsce_ctd_hdr`; current-trip exclusion handled inside the loop by the TRIP_NO ≠ current condition in the index read. |
| **SORT of lt_zsce_ctd_hdr** | Sorted by LIFNR, TRUCK_NO only (no CREATED_DATE, no TRIP_NO). | **Sort by LIFNR, TRUCK_NO, CREATED_DATE, TRIP_NO ASCENDING once, before the main loop.** |
| **Dest date derivation (last empty leg)** | Single READ TABLE BINARY SEARCH by LIFNR/TRUCK_NO; one row used; no CREATED_DATE filter; no per-trip exclusion. | BINARY SEARCH with TRIP_NO = SPACE to find insertion point; then at most 2 READ TABLE INDEX calls — no DO/ENDDO, no nested LOOP, no SORT, no DELETE inside loop. |

---

## 6. Table and Type Reference

**ZSCE_CTD_HDR** (fields used): LIFNR, TRUCK_NO, TRIP_NO, CREATED_DATE.

**Existing in FM:**

| Identifier | Type | Description |
|------------|------|-------------|
| `lty_zsce_ctd_hdr` | TYPE definition | lifnr, truck_no, trip_no, created_date |
| `lt_zsce_ctd_hdr` | TABLE OF lty_zsce_ctd_hdr | Filled before Step 7 by SELECT from ZSCE_CTD_HDR (FOR ALL ENTRIES by lifnr, truck_no) |
| `lw_zsce_ctd_hdr` | lty_zsce_ctd_hdr | Work area |

**New variable (add to main DATA section — upfront declaration per ABAP rules):**

| Identifier | Type | Purpose |
|------------|------|---------|
| `lv_tabix_hdr` | `TYPE sy-tabix` | Holds SY-TABIX (insertion point) after BINARY SEARCH for indexed READ calls |

---

## 7. Detailed Code Changes

### 7.1 Step 7 — Remove global deletion of current trips from lt_zsce_ctd_hdr

**Location:** Inside the block that fills `lt_zsce_ctd_hdr` (approx. lines 932–956 of *FM - Z_SCM_CTD_ENRICHTRIPDETAILS_05.03.2026.txt*).

**Remove** the following block entirely:

```abap
        " delete dates which are for current trip numbers
        IF ltr_current_trip IS NOT INITIAL.
          DELETE lt_zsce_ctd_hdr WHERE trip_no IN ltr_current_trip[].
        ENDIF.
```

**Rationale:** Removing current trips globally means Trip B (same LIFNR/TRUCK_NO, processed in the same run) would be lost when deriving the dest_date for Trip A's last empty leg. Current-trip exclusion is now handled per-trip inside the loop by checking `lw_zsce_ctd_hdr-trip_no <> lw_enrichment_new-trip_no` in the READ INDEX calls.

---

### 7.2 Step 7 — Change sort of lt_zsce_ctd_hdr (before main loop)

**Location:** After the SELECT into `lt_zsce_ctd_hdr` and removal of the DELETE block (section 7.1).

**Replace:**

```abap
        SORT lt_zsce_ctd_hdr BY lifnr truck_no.
```

**With:**

```abap
" BEGIN: Cursor Generated Code
" Sort by all 4 fields once: enables insertion-point binary search
" (lifnr, truck_no, created_date, trip_no) for last empty leg dest_date derivation
        SORT lt_zsce_ctd_hdr BY lifnr truck_no created_date trip_no ASCENDING.
" END: Cursor Generated Code
```

---

### 7.3 Step 7 — Add lv_tabix_hdr to main DATA section

**Location:** Main DATA declarations block (near `lt_zsce_ctd_hdr` / `lw_zsce_ctd_hdr` declarations).

**Add:**

```abap
" BEGIN: Cursor Generated Code
" Holds binary search insertion point for last empty leg dest_date indexed read
        lv_tabix_hdr TYPE sy-tabix,
" END: Cursor Generated Code
```

---

### 7.4 Step 7 — Replace dest_date logic: empty leg after last loaded leg (single-leg trip)

**Location:** Step 7, block where the empty leg is created for a **single loaded leg trip** (current leg is both the first and only loaded leg). In the reference FM, this is immediately after:

`lw_enrichment_new-source_date  = <lfs_enrichment>-dest_exit_date.`

**Remove** the existing dest_date block (approx. lines 1037–1046):

```abap
"* BEGIN: Cursor Generated Code <- get dest date by comparing source date 23.02.2026 ...
              CLEAR lw_zsce_ctd_hdr.
              READ TABLE lt_zsce_ctd_hdr INTO lw_zsce_ctd_hdr WITH KEY lifnr    = <lfs_enrichment>-lifnr
                                                                       truck_no = <lfs_enrichment>-truck_no BINARY SEARCH.
              IF sy-subrc = 0 AND lw_zsce_ctd_hdr-created_date >= lw_enrichment_new-source_date.
               lw_enrichment_new-dest_date    = lw_zsce_ctd_hdr-created_date.
              ELSE.
               lw_enrichment_new-dest_date    = sy-datum.
              ENDIF.
"* END: Cursor Generated Code ...
```

**Insert** the following in its place:

```abap
" BEGIN: Cursor Generated Code
" Dest date = CREATED_DATE of earliest next trip (same LIFNR/TRUCK_NO,
" CREATED_DATE >= source date of empty leg), excluding current trip.
" Pattern: SORT once outside loop (by lifnr, truck_no, created_date, trip_no).
" BINARY SEARCH with trip_no = SPACE positions SY-TABIX at insertion point =
" first eligible record (created_date >= source_date). At most 2 READ TABLE INDEX calls.
" No nested LOOP, no DO/ENDDO, no SORT, no DELETE inside loop.
              CLEAR: lw_zsce_ctd_hdr, lw_enrichment_new-dest_date.
              READ TABLE lt_zsce_ctd_hdr INTO lw_zsce_ctd_hdr
                WITH KEY lifnr        = lw_enrichment_new-lifnr
                         truck_no     = lw_enrichment_new-truck_no
                         created_date = lw_enrichment_new-source_date
                         trip_no      = space
                BINARY SEARCH.
              lv_tabix_hdr = sy-tabix.
              " First candidate: record at insertion point
              READ TABLE lt_zsce_ctd_hdr INTO lw_zsce_ctd_hdr INDEX lv_tabix_hdr.
              IF sy-subrc = 0 AND
                 lw_zsce_ctd_hdr-lifnr    = lw_enrichment_new-lifnr AND
                 lw_zsce_ctd_hdr-truck_no = lw_enrichment_new-truck_no.
                IF lw_zsce_ctd_hdr-trip_no <> lw_enrichment_new-trip_no.
                  " Eligible record: not the current trip
                  lw_enrichment_new-dest_date = lw_zsce_ctd_hdr-created_date.
                ELSE.
                  " Current trip is at this position; check the next record
                  READ TABLE lt_zsce_ctd_hdr INTO lw_zsce_ctd_hdr INDEX lv_tabix_hdr + 1.
                  IF sy-subrc = 0 AND
                     lw_zsce_ctd_hdr-lifnr    = lw_enrichment_new-lifnr AND
                     lw_zsce_ctd_hdr-truck_no = lw_enrichment_new-truck_no AND
                     lw_zsce_ctd_hdr-trip_no  <> lw_enrichment_new-trip_no.
                    lw_enrichment_new-dest_date = lw_zsce_ctd_hdr-created_date.
                  ENDIF.
                ENDIF.
              ENDIF.
              IF lw_enrichment_new-dest_date IS INITIAL.
                lw_enrichment_new-dest_date = sy-datum.
              ENDIF.
" END: Cursor Generated Code
```

---

### 7.5 Step 7 — Replace dest_date logic: empty leg after last loaded leg (multi-leg trip, last leg)

**Location:** Step 7, block where the **return empty leg** is created after the **last loaded leg** of a multi-leg trip (source = last leg's dest, dest = first leg's source). In the reference FM, this is after:

`lw_enrichment_new-source_date  = <lfs_enrichment>-dest_exit_date.`

**Remove** the existing dest_date block (approx. lines 1130–1139):

```abap
"* BEGIN: Cursor Generated Code <- get dest date by comparing source date 23.02.2026 ...
            CLEAR lw_zsce_ctd_hdr.
            READ TABLE lt_zsce_ctd_hdr INTO lw_zsce_ctd_hdr WITH KEY lifnr    = <lfs_enrichment>-lifnr
                                                                     truck_no = <lfs_enrichment>-truck_no BINARY SEARCH.
            IF sy-subrc = 0 AND lw_zsce_ctd_hdr-created_date >= lw_enrichment_new-source_date.
             lw_enrichment_new-dest_date    = lw_zsce_ctd_hdr-created_date.
            ELSE.
             lw_enrichment_new-dest_date    = sy-datum.
            ENDIF.
"* END: Cursor Generated Code ...
```

**Insert** the **identical code block** as in section 7.4 (BEGIN: Cursor Generated Code … END: Cursor Generated Code). Same logic: CLEAR, BINARY SEARCH with trip_no = space, READ INDEX lv_tabix_hdr, check trip_no, READ INDEX lv_tabix_hdr + 1 if needed, fallback to SY-DATUM.

---

### 7.6 Step 7 — Sandwiched empty leg: keep dest_date = source_ent_date of next loaded leg only

**Location:** The branch where the empty leg is inserted **between two loaded legs** of the same trip. The assignment already present:

`lw_enrichment_new-dest_date = <lfs_enrichment_prev>-source_ent_date.`

**Required:** This must be the **only** dest_date assignment in that branch. If the current code in the sandwiched branch overwrites dest_date with `lw_zsce_ctd_hdr-created_date` (as seen in approx. lines 1085–1101 of the reference FM), **remove that overwrite block** entirely so that `dest_date = <lfs_enrichment_prev>-source_ent_date` is the sole assignment.

---

## 8. Logic Summary — How the Insertion-Point Pattern Works

| Step | Action | Detail |
|------|--------|--------|
| **Once, before loop** | `SORT lt_zsce_ctd_hdr BY lifnr truck_no created_date trip_no ASCENDING.` | Sorted ascending on all 4 fields. |
| **1. Binary search** | `READ TABLE … WITH KEY lifnr=X truck_no=Y created_date=threshold trip_no=space BINARY SEARCH.` | trip_no=space is minimum; exact match never found (SY-SUBRC ≠ 0); SY-TABIX = first position where key ≥ (X, Y, threshold, space) = first record for (X,Y) with created_date ≥ threshold. |
| **2. Save position** | `lv_tabix_hdr = sy-tabix.` | Insertion point saved. |
| **3. First read** | `READ TABLE lt_zsce_ctd_hdr INDEX lv_tabix_hdr.` Check: lifnr=X, truck_no=Y, trip_no ≠ current. | If eligible → dest_date = created_date. Done. |
| **4. Second read (if needed)** | `READ TABLE lt_zsce_ctd_hdr INDEX lv_tabix_hdr + 1.` Check: lifnr=X, truck_no=Y, trip_no ≠ current. | Used only if step 3 landed on the current trip. At most 1 trip has trip_no = current, so this covers all cases. |
| **5. Fallback** | `IF dest_date IS INITIAL. dest_date = sy-datum. ENDIF.` | Used when no eligible next trip exists. |

---

## 9. Summary of Edits

| # | Location | Action |
|---|----------|--------|
| 1 | Main DATA section | Add `lv_tabix_hdr TYPE sy-tabix`. |
| 2 | Step 7 — after SELECT into lt_zsce_ctd_hdr | **Remove:** `DELETE lt_zsce_ctd_hdr WHERE trip_no IN ltr_current_trip[]`. |
| 3 | Step 7 — after SELECT into lt_zsce_ctd_hdr | **Change SORT** from `lifnr truck_no` to `lifnr truck_no created_date trip_no ASCENDING`. |
| 4 | Step 7 — single-leg empty leg | **Replace** existing READ TABLE + IF/ELSE dest_date block with: BINARY SEARCH (trip_no=space) + up to 2 READ INDEX calls + SY-DATUM fallback. |
| 5 | Step 7 — last-leg empty leg (multi-leg) | **Same replacement** as #4. |
| 6 | Step 7 — sandwiched empty leg | **Remove** any ZSCE_CTD_HDR-based dest_date overwrite; keep only `dest_date = <lfs_enrichment_prev>-source_ent_date`. |

---

## 10. ABAP Compliance Checklist

| Check | Status |
|-------|--------|
| No SELECT inside LOOP | Compliant — SELECT into `lt_zsce_ctd_hdr` is outside all loops |
| No nested LOOP AT inside LOOP AT | Compliant — no LOOP AT lt_zsce_ctd_hdr used; only READ TABLE INDEX |
| No SORT inside LOOP | Compliant — SORT done once before the main loop |
| No DELETE inside LOOP | Compliant — no DELETE; current trip excluded by trip_no ≠ check in READ INDEX |
| No DO / ENDDO | Compliant — insertion-point pattern uses at most 2 READ TABLE INDEX calls |
| SORT before BINARY SEARCH | Compliant — SORT by lifnr, truck_no, created_date, trip_no before loop |
| SY-SUBRC checked after READ TABLE | Compliant — checked after every READ TABLE INDEX call |
| Variables declared upfront | Compliant — `lv_tabix_hdr` declared in main DATA section |
| No 7.40+ syntax | Compliant — no inline declarations, no constructor operators, no string templates |
| Cursor Generated Code markers | Compliant — `" BEGIN: Cursor Generated Code` / `" END: Cursor Generated Code` |

---

## 11. Testing Suggestions

- **Single loaded leg trip:** One loaded leg, area gap → one empty leg after it; dest_date = CREATED_DATE of the earliest next trip (same LIFNR/TRUCK_NO, created_date ≥ empty leg source date), excluding current trip; or SY-DATUM if no such record.
- **Multiple trips, same LIFNR/TRUCK_NO (same run):** Process Trip A and Trip B for the same truck. Trip A's last empty leg dest_date = Trip B's CREATED_DATE (earliest next trip). Trip B's last empty leg dest_date = subsequent trip's CREATED_DATE or SY-DATUM. Each trip correctly excluded only from its own binary search result.
- **Current trip at insertion point:** Binary search positions at current trip (trip_no = current); second READ at INDEX+1 picks the next eligible record.
- **Multi-leg trip, last leg:** Return empty leg after the last loaded leg uses the same dest_date derivation.
- **Sandwiched empty leg:** No change; dest_date = Source Entry Date of the next loaded leg.
- **No next trip available:** No record for this LIFNR/TRUCK_NO with created_date ≥ source_date (or only the current trip) → dest_date = SY-DATUM.

---

## 12. Reference to Unchanged Code

| Element | Reference FM line (approx.) | Description |
|---------|------------------------------|-------------|
| Sandwiched empty leg dest_date | Line 993 | `lw_enrichment_new-dest_date = <lfs_enrichment_prev>-source_ent_date.` — do not modify |
| Type `lty_zsce_ctd_hdr` | Lines 176–182 | lifnr, truck_no, trip_no, created_date — no structural change needed |
| SELECT into `lt_zsce_ctd_hdr` | Lines 932–940 | FOR ALL ENTRIES by lifnr, truck_no — no change needed |
