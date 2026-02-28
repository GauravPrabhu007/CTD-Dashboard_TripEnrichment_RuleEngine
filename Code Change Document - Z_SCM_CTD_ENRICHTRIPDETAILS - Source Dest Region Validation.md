# Code Change Document – Z_SCM_CTD_ENRICHTRIPDETAILS  
## Source Region / Destination Region Validation for Loaded Legs

**Function Module:** Z_SCM_CTD_ENRICHTRIPDETAILS  
**Program:** ZSCM_CTD_ENRICHTRIPDETAILS  
**Tables:** ZSCE_CTD_HDR, ZSCE_CTD_ITM  
**Change Type:** Enhancement (new validation)  
**Date:** 2026-02-28  
**Reference:** CD: 8086962 TR: RD2K9A5FGT | Technical: Omkar More | Functional: Gaurav Prabhu

---

## Executive Summary

Add a **loaded-leg validation** after Source Region and Destination Region are determined (ADRC block): if **any loaded leg** in a trip has **Source Region/State** and/or **Destination Region/State** blank or initial, **exclude the entire trip** from further processing. Display **leg-level** validation messages in the ALV: on the **respective leg** where the validation failed, with remarks as below. If both are missing on the same leg, show **both messages concatenated** for that leg.

---

## 1. Summary of Requirements

| # | Requirement | Summary |
|---|-------------|--------|
| **1** | **When to validate** | Run validation **only after** Source Region and Destination Region are determined in the FM (i.e. after the ADRC fetch and the loop that populates `source_region` / `dest_region` in the enrichment table for loaded legs). |
| **2** | **Scope** | Validation applies **only to loaded legs** (legs with shipment number / `shnumber` not initial, or `leg_type = 'L'`). |
| **3** | **Trip exclusion** | If **any** loaded leg in a trip has Source Region **or** Destination Region (or both) initial, **exclude the entire trip** from further processing (no empty-leg logic, no DB update for that trip). |
| **4** | **Messages** | **Source Region/ State missing:** "Source Region/ State is missing". **Destination Region/ State missing:** "Destination Region/ State is missing". **Both missing on same leg:** both messages **concatenated** (e.g. with space) for that leg. |
| **5** | **Leg-level display in ALV** | Validation messages are **leg-level**: set **REMARKS** on the **specific leg row** in ET_ENRICHDET where the validation failed. The leg that failed shows the appropriate message(s); if multiple legs fail, each shows its own message(s). |
| **6** | **ET_RETURN and ALV** | Add BAPIRET2 entries to ET_RETURN for excluded trips (e.g. trip-level or per failing leg). Excluded trip legs appended to ET_ENRICHDET with TRIP_STATUS = '03' and REMARKS = leg-level message(s) so ALV shows them. |

---

## 2. When Validation Runs (Placement in FM)

| Step in FM | Description |
|------------|-------------|
| **Before** | Step 6: Build enrichment table; ADRC fetch; loop that sets `source_region` and `dest_region` for loaded legs from ADRC (by `adrna` / `adrnz` from OIGSS). |
| **New** | **After** the ADRC block (after region determination), **before** Step 7 (Determine and Insert Empty Legs). |
| **After** | Step 7 onwards: only trips that passed the region validation continue (excluded trips removed from `lt_enrichments` and `lt_trip_headers`). |

**Relevant code area in current FM:** After the block that updates `lt_enrichments` with `source_region` and `dest_region` from ADRC (loop over `lt_enrichments` with `lt_oigss` and `lt_adrc`), and before the empty-leg logic (Step 7).

---

## 3. Validation Logic (Detailed)

### 3.1 Condition per loaded leg

For each **loaded** leg (e.g. `shnumber IS NOT INITIAL` or `leg_type = lc_leg_loaded`):

- If `source_region` IS INITIAL → leg has "Source Region/ State is missing".
- If `dest_region` IS INITIAL → leg has "Destination Region/ State is missing".
- If **both** are initial → leg has **both messages concatenated** (e.g. `"Source Region/ State is missing"` & `" "` & `"Destination Region/ State is missing"`).

### 3.2 Trip-level exclusion

- If **any** loaded leg in the trip has at least one of the above (source and/or dest region missing), set **exclude trip** = true for that trip.
- Collect trip numbers to exclude in a range/internal table (e.g. `ltr_trip_no_ex_region`).

### 3.3 Output

- **ET_RETURN:** Add message(s) for excluded trip (e.g. "Trip & excluded - Source Region/ State or Destination Region/ State missing" or leg-specific if desired). Use new message number (e.g. ZCTD 0xx) and message type 'E'.
- **ET_ENRICHDET:** For each **leg** of an excluded trip:
  - **Legs that failed:** REMARKS = leg-level message (Source missing, Dest missing, or both concatenated).
  - **Other legs of same trip (optional):** Can have same trip-level remark or blank; recommended to show at least the failing leg(s) with leg-level remark.
- **Processing:** Remove excluded trips from `lt_enrichments` and `lt_trip_headers` so Step 7 (empty legs) and Step 10 (DB update) do not run for them.

---

## 4. Message Texts

| Usage | Text |
|-------|------|
| **Source Region missing** | "Source Region/ State is missing" |
| **Destination Region missing** | "Destination Region/ State is missing" |
| **Both missing (concatenated)** | "Source Region/ State is missing" & " " & "Destination Region/ State is missing" |
| **ET_RETURN (trip-level, optional)** | e.g. "Trip & excluded - Source Region/ State or Destination Region/ State missing" (or reference leg-level messages) |

Suggest creating text symbols (e.g. 040, 041) for the two leg-level messages and reusing in REMARKS and ET_RETURN.

---

## 5. Detailed Code Changes (Function Module)

### 5.1 Data declarations

Add (if not already present):

```abap
* Region validation - excluded trips (after ADRC region determination)
  DATA: ltr_trip_no_ex_region TYPE RANGE OF ztrip_no,
        lwr_trip_no_ex_region LIKE LINE OF ltr_trip_no_ex_region,
        lv_region_remarks     TYPE zctd_ruleeng_remarks.
```

Optional: internal table to hold excluded trip legs with leg-level remarks for appending to ET_ENRICHDET (if building in a separate loop before main output loop).

### 5.2 New validation block (after ADRC region update, before Step 7)

**Position:** After the loop that sets `source_region` and `dest_region` from ADRC (the block that ends with updating `lt_enrichments` with region from `lt_adrc`), and before "Step 7: Determine and Insert Empty Legs".

**Logic (pseudo-code):**

1. Clear `ltr_trip_no_ex_region`.
2. Loop at `lt_enrichments` by trip (e.g. AT NEW trip_no / group by trip_no).
   - For each trip, set `lv_exclude_trip_region = abap_false`.
   - Loop at legs of that trip.
     - For each leg where `shnumber IS NOT INITIAL` (loaded leg):
       - Build `lv_region_remarks`:
         - If `source_region` initial and `dest_region` initial: concatenate both messages.
         - Else if `source_region` initial: "Source Region/ State is missing".
         - Else if `dest_region` initial: "Destination Region/ State is missing".
       - If any message was set: assign to that leg's remarks (e.g. `<lfs_enrichment>-remarks = lv_region_remarks`), set `lv_exclude_trip_region = abap_true`.
   - At end of trip: if `lv_exclude_trip_region = abap_true`, append trip to `ltr_trip_no_ex_region`, append to ET_RETURN (message type E, e.g. "Trip ... excluded - Source Region/ State or Destination Region/ State missing").
3. If `ltr_trip_no_ex_region` is not initial:
   - **Before** deleting: for each leg of excluded trips, append to ET_ENRICHDET with REMARKS = that leg's remarks (leg-level), TRIP_STATUS = '03', and other key fields (lifnr, truck_no, trip_no, counter, etc.). This ensures **validation messages are displayed at the respective leg** in the ALV.
   - Delete from `lt_enrichments` where `trip_no IN ltr_trip_no_ex_region`.
   - Delete from `lt_trip_headers` where `trip_no IN ltr_trip_no_ex_region`.

### 5.3 Appending excluded legs to ET_ENRICHDET (leg-level remarks)

- When processing excluded trips (region validation):
  - Loop at `lt_enrichments` (before DELETE) where `trip_no IN ltr_trip_no_ex_region`.
  - For each such leg: MOVE-CORRESPONDING to output structure; set REMARKS = the leg's remarks (only for legs where remarks were set—i.e. the loaded leg(s) that failed); set TRIP_STATUS = '03'.
  - APPEND to ET_ENRICHDET.
- Then perform DELETE of excluded trips from `lt_enrichments` and `lt_trip_headers`.

### 5.4 Consistency with existing validations

- **Step 4 (date validation):** Excluded trips are removed from `lt_trip_items` / `lt_trip_headers` and appended to ET_ENRICHDET with remarks. Same pattern: exclude trip, append legs to ET_ENRICHDET with leg-level remarks, then delete from processing tables.
- **Step 8 (empty leg route/distance):** Leg-level remarks on empty legs. Here, leg-level remarks on **loaded** legs that failed region validation.
- **DB update:** Excluded trips (in `ltr_trip_no_ex_region`) are no longer in `lt_trip_headers` / `lt_enrichments`, so they are not updated in Step 10 (same as Step 4 excluded).

---

## 6. Implementation Checklist

| # | Task | Location / Note |
|---|------|------------------|
| 1 | Add data: `ltr_trip_no_ex_region`, `lwr_trip_no_ex_region`, `lv_region_remarks` (and optional table for excluded legs with remarks) | Data declarations |
| 2 | Create text symbols for "Source Region/ State is missing" and "Destination Region/ State is missing" | SE61 / text elements |
| 3 | Add new validation block **after** ADRC region-update loop, **before** Step 7 | After ~line 509 (end of ADRC update loop) |
| 4 | In validation: loop by trip; for each loaded leg check source_region/dest_region; build leg-level remarks (single or concatenated); set exclude flag; append trip to ltr_trip_no_ex_region; append to ET_RETURN | New block |
| 5 | Before DELETE: for each leg of excluded trips, append to ET_ENRICHDET with REMARKS = leg-level message, TRIP_STATUS = '03' | Same block |
| 6 | DELETE lt_enrichments / lt_trip_headers for trip_no IN ltr_trip_no_ex_region | Same block |
| 7 | Optional: if no valid trips remain after region exclusion, add message and RETURN | After new block |

---

## 7. Test Checklist

- [ ] **Loaded leg with Source Region missing:** Trip excluded; that leg appears in ALV with REMARKS = "Source Region/ State is missing"; ET_RETURN has message; no DB update for trip.
- [ ] **Loaded leg with Destination Region missing:** Trip excluded; that leg in ALV with REMARKS = "Destination Region/ State is missing".
- [ ] **Loaded leg with both missing:** Trip excluded; that leg in ALV with REMARKS = both messages concatenated.
- [ ] **Multiple loaded legs, one fails:** Trip excluded; only the failing leg shows the remark (leg-level display).
- [ ] **Validation runs only after region determination:** No region check on legs before ADRC block; after ADRC, only loaded legs with initial region trigger exclusion.
- [ ] **Other trips in run:** Trips that pass region validation continue to Step 7 and Step 10; ALV shows both executed and excluded trips with correct status and leg-level remarks.

---

## 8. Message / Text References

| Id | Text / Message |
|----|-----------------|
| **New text 040** | "Source Region/ State is missing" |
| **New text 041** | "Destination Region/ State is missing" |
| **New message (e.g. ZCTD 0xx)** | "Trip & excluded - Source Region/ State or Destination Region/ State missing" (trip-level in ET_RETURN) |

Use text 040/041 in REMARKS (leg-level); concatenate when both missing on same leg. Use new ZCTD message in ET_RETURN for excluded trips.

---

*End of document*
