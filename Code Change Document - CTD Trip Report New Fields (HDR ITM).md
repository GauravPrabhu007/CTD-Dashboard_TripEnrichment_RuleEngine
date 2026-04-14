# Code Change Document – CTD Trip Report: Additional HDR / ITM Fields

## Summary

| Item | Value |
|------|--------|
| **Scope** | Extend CTD Trip Report output with seven fields from `ZSCE_CTD_HDR` and `ZSCE_CTD_ITM` |
| **Program** | `ZSCM_CTD_ENRICHTRIPDETAILS` (Trip Report branch / `p_trip` only) |
| **Function module** | `Z_SCM_CTD_GET_TRIP_REPORT` |
| **DDIC** | `ZSCM_CTD_REP_ALV_ST`, table type `ZSCM_CTD_REP_ALV_TT` |
| **Tables** | `ZSCE_CTD_HDR`, `ZSCE_CTD_ITM` (read-only) |
| **Change type** | Enhancement — no change to `Z_SCM_CTD_ENRICHTRIPDETAILS` enrichment FM or Display/Update radiobutton flows |
| **Date** | 2026-04-14 |
| **Reference** | CD: TBD \| TR: TBD |
| **Functional author** | Gaurav Prabhu |
| **Technical author** | TBD |

---

## 1. Executive Summary

This change adds **fetch and ALV display** for:

**From `ZSCE_CTD_HDR`**

| Field | Purpose (short) |
|-------|------------------|
| `CREATED_DATE` | Trip header creation date (also used for date-range selection; must appear on the report line) |
| `RTCA_FLAG` | RTCA general flag |
| `RCCA_FLAG` | RCCA general flag |

**From `ZSCE_CTD_ITM`**

| Field | Purpose (short) |
|-------|------------------|
| `REJ_REMARK2` | Secondary rejection remarks |
| `ERUN_IDEAL_DAYS` | Empty run — ideal transit days |
| `ERUN_ACTUAL_DAYS` | Empty run — actual transit days |
| `ERUN_DELAY_DAYS` | Empty run — delay in days |

Downstream consumers of `Z_SCM_CTD_GET_TRIP_REPORT` (e.g. Transporter portal) receive the same extended line type once the structure is enhanced.

**Out of scope:** Any change to persistence, enrichment logic, or selection-screen behaviour except displaying new columns when Trip Report is selected.

---

## 2. Prerequisites and Dependencies

| # | Dependency |
|---|------------|
| 1 | Fields exist and are populated in `ZSCE_CTD_HDR` / `ZSCE_CTD_ITM` in target systems (transport order of table vs. report change as per project rules). |
| 2 | `ZSCM_CTD_REP_ALV_ST` is already in use as the line type for `ET_TRIP_DETAILS`; this change **appends** components — adjust any external API documentation that lists fixed column sets. |
| 3 | NW 7.31 compatibility: no inline declarations in touched ABAP if project standard requires it. |

---

## 3. DDIC Changes (SE11)

### 3.1 Structure `ZSCM_CTD_REP_ALV_ST` — append components

Add the following fields. Use **the same data elements as on the database tables** (verify in SE11 against `ZSCE_CTD_HDR` / `ZSCE_CTD_ITM` in the system).

| Field name | Reference table | Data element (expected) | Description |
|------------|-----------------|-------------------------|-------------|
| `CREATED_DATE` | `ZSCE_CTD_HDR` | `SYDATUM` | Trip header created on |
| `RTCA_FLAG` | `ZSCE_CTD_HDR` | `FLAG` | RTCA flag |
| `RCCA_FLAG` | `ZSCE_CTD_HDR` | `FLAG` | RCCA flag |
| `REJ_REMARK2` | `ZSCE_CTD_ITM` | `ZCTD_REMARKS` | Rejection remarks (line 2) |
| `ERUN_IDEAL_DAYS` | `ZSCE_CTD_ITM` | `ZSCM_ERUN_IDEAL_DAYS_DE` | Empty run ideal days |
| `ERUN_ACTUAL_DAYS` | `ZSCE_CTD_ITM` | `ZSCM_ERUN_ACTUAL_DAYS_DE` | Empty run actual days |
| `ERUN_DELAY_DAYS` | `ZSCE_CTD_ITM` | `ZSCM_ERUN_DELAY_DAYS_DE` | Empty run delay days |

**Field sequence:** Append in a consistent order (recommended):

1. `CREATED_DATE`, `RTCA_FLAG`, `RCCA_FLAG` — grouped as header attributes (place after existing header-level columns such as `CTD_RULEENG_REMARKS`, or immediately after `TRIP_NO` / status block per UX preference).
2. `REJ_REMARK2` — adjacent to existing `REJ_REMARK` if possible.
3. `ERUN_IDEAL_DAYS`, `ERUN_ACTUAL_DAYS`, `ERUN_DELAY_DAYS` — grouped as empty-run metrics.

> **Note:** If the project requires `DEL_IND` to remain physically last in the structure, insert the new fields **before** `DEL_IND` rather than after it.

Activate the structure. If `ZSCM_CTD_REP_ALV_TT` is defined as a table type of `ZSCM_CTD_REP_ALV_ST`, reactivate it after the structure change.

### 3.2 Text elements

Add English (and other languages per project policy) **data element texts** or **structure component texts** so ALV default headers are meaningful where automatic derivation is used.

---

## 4. Function Module `Z_SCM_CTD_GET_TRIP_REPORT`

### 4.1 Interface

No change to IMPORTING/EXPORTING parameters. `ET_TRIP_DETAILS` remains type `ZSCM_CTD_REP_ALV_TT` with an extended line type.

### 4.2 Local types

**`lty_hdr`**

- Add: `created_date TYPE sydatum` (or table-consistent type), `rtca_flag TYPE flag`, `rcca_flag TYPE flag`.

> If `created_date` is already present for filtering, only add the two flags.

**`lty_itm`**

- Add: `rej_remark2 TYPE zctd_remarks`, `erun_ideal_days TYPE zscm_erun_ideal_days_de`, `erun_actual_days TYPE zscm_erun_actual_days_de`, `erun_delay_days TYPE zscm_erun_delay_days_de`  
  (use exact types from table definition in SE11).

### 4.3 SELECT from `ZSCE_CTD_HDR`

Extend the field list to include **`RTCA_FLAG`** and **`RCCA_FLAG`**. Include **`CREATED_DATE`** if not already selected.

Example (illustrative — align names with DDIC):

```abap
SELECT lifnr truck_no trip_no trip_status area del_ind ctd_ruleeng_remarks
       created_date rtca_flag rcca_flag
  FROM zsce_ctd_hdr
  ...
```

### 4.4 SELECT from `ZSCE_CTD_ITM`

Extend the field list to include:

`rej_remark2`, `erun_ideal_days`, `erun_actual_days`, `erun_delay_days`.

### 4.5 Build loop — map to `lw_output` (type `ZSCM_CTD_REP_ALV_ST`)

**For each item line** (existing inner loop over items for a header):

- Assign from `lw_hdr`: `created_date`, `rtca_flag`, `rcca_flag` → corresponding `lw_output` components.
- Assign from `lw_itm`: `rej_remark2`, `erun_ideal_days`, `erun_actual_days`, `erun_delay_days`.

**For header-only rows** (no items — existing branch that emits one row per header):

- Assign `created_date`, `rtca_flag`, `rcca_flag` from `lw_hdr`.
- Clear or leave initial: `rej_remark2`, `erun_ideal_days`, `erun_actual_days`, `erun_delay_days` (standard: **CLEAR** item fields).

### 4.6 Remarks concatenation

Existing logic may concatenate vendor/RIL remark lines into single ALV columns. **Do not change** that behaviour as part of this CR unless a separate requirement asks for split columns. This CR only adds the new fields above.

---

## 5. Program `ZSCM_CTD_ENRICHTRIPDETAILS`

**Includes:** `ZSCM_CTD_ENRICHTRIPDETAILSC01` (class `LCL_REPORT`), method **`display_trip_report_alv`**.

### 5.1 Behaviour

- **Background (`CL_SALV_TABLE`):** After DDIC extension, new columns appear in the table. Optionally set medium/long texts via `get_column` for the new field names (same pattern as existing Trip Report columns).
- **Foreground (`CL_GUI_ALV_GRID`):** Field catalog is generated from `i_structure_name` = DDIC line type. Extend the **`LOOP AT lt_fcat_fe ASSIGNING`** (or equivalent) **`CASE fieldname`** block to set `seltext`, `coltext`, `tooltip` for:
  - `CREATED_DATE`
  - `RTCA_FLAG`
  - `RCCA_FLAG`
  - `REJ_REMARK2`
  - `ERUN_IDEAL_DAYS`
  - `ERUN_ACTUAL_DAYS`
  - `ERUN_DELAY_DAYS`

Use text symbols (e.g. `text-xxx`) for user-facing labels, consistent with neighbouring columns.

### 5.2 Trip Enrichment ALV

**No change** to `display_alv` / enrichment output (`Z_SCM_CTD_ENRICHTRIPDETAILS` FM) unless explicitly required in a separate CR.

---

## 6. Test Plan

| # | Scenario | Expected |
|---|----------|----------|
| 1 | Trip with multiple items; DB has all new HDR/ITM values | Each row shows header flags/date on every item line; item metrics and `REJ_REMARK2` per counter |
| 2 | Trip with **no** items (header-only row) | `CREATED_DATE` / flags from header; item fields empty |
| 3 | Item with blank `REJ_REMARK2` and zero/initial ERUN fields | ALV shows blank/zero as per DDIC display |
| 4 | Batch execution (`sy-batch` = `X`) | SALV lists new columns; job log without short dump |
| 5 | Portal caller (`IV_CALLER_TYPE` = `V`) if used | Same extended table returned |

---

## 7. Transport and Sequencing

1. Transport **DDIC** (`ZSCM_CTD_REP_ALV_ST` / `ZSCM_CTD_REP_ALV_TT`) first.  
2. Transport **FM** `Z_SCM_CTD_GET_TRIP_REPORT`.  
3. Transport **program** includes.

Ensure table definitions containing the new fields are already in the target client before relying on data in those columns.

---

## 8. Open Points / Clarifications (optional)

| Topic | Decision |
|-------|----------|
| Column order in ALV | Final order follows SE11 component sequence; UX may request drag-and-drop default via layout variant. |
| Flag display | Show as `X`/space, or use checkbox style in ALV — align with existing `VENDOR_CTD` / `RIL_CTD` handling. |
| `CREATED_DATE` label | Distinguish from item-level dates — e.g. “Trip Created On” vs “Leg Start Date”. |

---

## 9. Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-04-14 | Draft | Initial version — seven fields HDR/ITM |
