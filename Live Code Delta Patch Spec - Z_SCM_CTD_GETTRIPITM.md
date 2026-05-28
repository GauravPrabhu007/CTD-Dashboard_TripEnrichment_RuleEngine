# Live Code Delta Patch Spec - Z_SCM_CTD_GETTRIPITM

| Field | Detail |
|---|---|
| Function Module | `Z_SCM_CTD_GETTRIPITM` |
| Source baseline | Live RD2 source from `ZSCM_CTD_CHAIN_FG` (validated via `sap_mcp_rd2`) |
| Objective | Ensure SO POD date alignment for loaded leg and immediate next empty leg |
| Scope | Only this FM (separate spec for `Z_SCM_CTD_ENRICHTRIPDETAILS` will be provided next) |
| Date | 28-May-2026 |

---

## 1) Requirement handled in this patch

For SO loaded leg where POD date is fetched from API:

1. `DEST_DATE` must be POD date (`YYYYMMDD` format in ABAP date fields).
2. `DEST_EXIT_DATE` must be same as SO `DEST_DATE`.
3. Immediate next empty leg (same trip, `counter + 1`, `leg_type = 'E'`) must have:
   - `SOURCE_DATE = SO DEST_DATE`.

---

## 2) Live code observations (current RD2)

- POD API call is already present for `SO + L + shnumber + dest_date initial`.
- Current POD branch sets `dest_date`, but does **not** always set `dest_exit_date` in same branch.
- `dest_exit_date` assignment uses raw API string in one branch instead of converted `lv_dest_date`.
- No post-loop alignment to update next empty leg `source_date` from SO POD date.

---

## 3) Delta patch - exact changes

## 3.1 Data declarations (add one local variable)

### Existing

```abap
  DATA: lt_trip_itm TYPE zscm_ctd_tripitm_tt,
        ...
        lv_pod_date     TYPE char10,
        lv_dest_date    TYPE zdest_date.
```

### Change

```abap
  DATA: lt_trip_itm TYPE zscm_ctd_tripitm_tt,
        ...
        lv_pod_date     TYPE char10,
        lv_dest_date    TYPE zdest_date,
        lv_next_counter TYPE zcounter.
```

---

## 3.2 POD response handling - set both SO dates consistently

### Existing block (inside main `LOOP AT lt_trip_itm ASSIGNING <lfs_trip_item>`)

```abap
                CLEAR:lv_pod_date,lv_dest_date.
                IF   lw_api_response-data-poddate  IS NOT INITIAL.
                  IF <lfs_trip_item>-dest_date  IS INITIAL.
                     lv_pod_date = lw_api_response-data-poddate.
                     lv_dest_date = lv_pod_date+0(4) && lv_pod_date+5(2) && lv_pod_date+8(2).
                    <lfs_trip_item>-dest_date      =  lv_dest_date.
                  ELSEIF <lfs_trip_item>-dest_exit_date IS INITIAL.
                     lv_pod_date = lw_api_response-data-poddate.
                     lv_dest_date = lv_pod_date+0(4) && lv_pod_date+5(2) && lv_pod_date+8(2).
                    <lfs_trip_item>-dest_exit_date =  lw_api_response-data-poddate.
                  ENDIF.
```

### Replace with

```abap
                CLEAR: lv_pod_date, lv_dest_date.
                IF lw_api_response-data-poddate IS NOT INITIAL.
                  lv_pod_date  = lw_api_response-data-poddate.
                  lv_dest_date = lv_pod_date+0(4) && lv_pod_date+5(2) && lv_pod_date+8(2).

                  IF <lfs_trip_item>-dest_date IS INITIAL.
                    <lfs_trip_item>-dest_date = lv_dest_date.
                  ENDIF.

                  IF <lfs_trip_item>-dest_exit_date IS INITIAL.
                    <lfs_trip_item>-dest_exit_date = lv_dest_date.
                  ENDIF.
                ELSE.
```

### Why

- Uses a single converted value (`lv_dest_date`) for both date fields.
- Guarantees SO loaded leg has `dest_exit_date = dest_date` when POD is available.

---

## 3.3 New post-processing pass - align immediate next empty leg source date

### Insert position

Insert after existing main loop end and before:

```abap
      " Step 6: Populate output table
      et_trip_itm = lt_trip_itm.
```

### Insert code

```abap
      LOOP AT lt_trip_itm ASSIGNING <lfs_trip_item>.
        IF <lfs_trip_item>-mvt_type  = 'SO'
        AND <lfs_trip_item>-leg_type = 'L'
        AND <lfs_trip_item>-dest_date IS NOT INITIAL.

          lv_next_counter = <lfs_trip_item>-counter + 1.

          READ TABLE lt_trip_itm ASSIGNING FIELD-SYMBOL(<lfs_next_trip_item>)
            WITH KEY trip_no  = <lfs_trip_item>-trip_no
                     counter  = lv_next_counter.
          IF sy-subrc = 0
          AND <lfs_next_trip_item>-leg_type = 'E'.
            <lfs_next_trip_item>-source_date = <lfs_trip_item>-dest_date.
          ENDIF.
        ENDIF.
      ENDLOOP.
```

### Why

- Ensures immediate empty leg starts exactly on SO POD date in FM output (`ET_TRIP_ITM`).
- Matches requested behaviour without changing existing API interface.

---

## 4) Optional hardening (recommended)

Current POD trigger condition is:

```abap
AND <lfs_trip_item>-dest_date EQ 0.
```

Optional enhancement: also call API when `dest_date` is filled but `dest_exit_date` is initial (SO loaded leg), to auto-heal old records.

---

## 5) Impact and compatibility

- No DDIC structure changes.
- No API contract changes.
- No DB update introduced (FM remains read/derive for output table).
- Existing `confirm_blocked` flow remains unchanged.

---

## 6) Unit test checklist (for this FM)

1. SO+L row with blank `dest_date`, POD present:
   - `dest_date = POD (YYYYMMDD)`
   - `dest_exit_date = dest_date`
2. Next row is empty leg (`counter + 1`, same trip):
   - `source_date = previous SO dest_date`
3. POD missing:
   - existing error `075` and `confirm_blocked = 'X'` unchanged
4. Non-SO legs:
   - no behaviour regression.

