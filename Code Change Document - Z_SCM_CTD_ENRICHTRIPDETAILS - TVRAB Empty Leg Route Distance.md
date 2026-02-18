# Code Change Document – Z_SCM_CTD_ENRICHTRIPDETAILS  
## Replace TROLZ/TVRO with TVRAB for Empty Leg Route & Distance

**Function Module:** Z_SCM_CTD_ENRICHTRIPDETAILS  
**Purpose:** Replace route/distance fetch from TROLZ + TVRO with a single SELECT on table TVRAB for empty legs.  
**Date:** 2026-02-16  
**Reference:** Table TVRAB (KNANF, KNEND, ROUTE, DISTZ); screenshots TROLZ & TVRO Logic / Fetch Route & TROLZ & TVRO.

---

## 1. Summary of Changes

| # | Change | Location |
|---|--------|----------|
| 1 | Replace types | Remove ty_trolz, ty_tvro; add ty_tvrab_route_dist (route, distz). |
| 2 | Replace data declarations | Remove TROLZ/TVRO and zone range variables; add lw_tvrab. |
| 3 | Remove TROLZ/TVRO pre-fetch | Delete entire "Fetch route and distance for empty legs" block (zone init, collection loop, SELECT TROLZ, SELECT TVRO). |
| 4 | Empty leg – between two legs | Replace READ TABLE lt_trolz + lt_tvro with SELECT SINGLE from TVRAB (KNANF/KNEND); set route & distz on lw_enrichment_new. |
| 5 | Empty leg – single leg | Same replacement for single-leg return empty (dest_tzone → source_tzone). |
| 6 | Constants (optional) | Remove lc_vsbed, lc_tragr if no longer used. |

**Mapping:**
- **KNANF** = lv_prev_dest_zone (or &lt;lfs_enrichment&gt;-dest_tzone for single leg)
- **KNEND** = lv_curr_source_zone (or &lt;lfs_enrichment&gt;-source_tzone for single leg)
- **lw_enrichment_new-route** = TVRAB-ROUTE  
- **lw_enrichment_new-distance** = TVRAB-DISTZ  

---

## 2. TVRAB Table Reference (from Table TVRAB.txt)

- **Key:** MANDT, ROUTE, ABNUM  
- **KNANF** (KNOTA, CHAR 10) – Departure point  
- **KNEND** (KNOTZ, CHAR 10) – Destination point  
- **ROUTE** (CHAR 6) – Route  
- **DISTZ** (QUAN 13,3) – Distance  

---

## 3. Exact Code Changes

### 3.1 Types (approx. lines 113–128)

**Remove:**
```abap
  TYPES: BEGIN OF ty_trolz,
           azone TYPE azone,
           vsbed TYPE vsbed,
           tragr TYPE tragr,
           lzone TYPE lzone,
           route TYPE route_vl,
         END OF ty_trolz.

  TYPES: BEGIN OF ty_tvro,
           route TYPE route,
           distz TYPE distz,
         END OF ty_tvro.
```

**Add** (e.g. in place of ty_tvro, before ty_oigsi):
```abap
  TYPES: BEGIN OF ty_tvrab_route_dist,
           route TYPE route,
           distz TYPE distz,
         END OF ty_tvrab_route_dist.
```

---

### 3.2 Data Declarations (approx. lines 204–208, 243–246)

**Remove:**
- `lt_trolz`, `lt_trolz_temp`, `lw_trolz`, `lt_tvro`, `lw_tvro`
- `ltr_trolz_azones`, `lwr_trolz_azones`, `ltr_trolz_lzones`, `lwr_trolz_lzones`

**Add:**
```abap
        lw_tvrab                  TYPE ty_tvrab_route_dist,
```

---

### 3.3 Constants (optional, approx. lines 268–269)

**Remove** if no longer referenced:
```abap
             lc_vsbed            TYPE vsbed VALUE '03',
             lc_tragr            TYPE tragr VALUE '0006',
```

---

### 3.4 Step 7 – Remove TROLZ/TVRO Pre-Fetch Block (lines 710–782)

**Delete** from the line after `lv_lines_enrich = lines( lt_enrichments ).` down to and including the `ENDIF.` that closes `IF ltr_trolz_azones IS NOT INITIAL AND ltr_trolz_lzones IS NOT INITIAL.`

**Remove the following in full:**
- Comment `*   Fetch route and distance for empty legs`
- `lwr_trolz_azones-sign/option` and `lwr_trolz_lzones-sign/option` initialization
- Comment `*   Collect all possible TROLZ combinations for empty legs` and the entire **LOOP AT lt_enrichments** that fills `ltr_trolz_azones` / `ltr_trolz_lzones` (with READ TABLE lt_enrichments ASSIGNING &lt;lfs_enrichment_prev&gt;, IF/ELSEIF, APPEND to ranges)
- The entire **IF ltr_trolz_azones IS NOT INITIAL AND ltr_trolz_lzones IS NOT INITIAL** block:
  - SORT/DELETE ADJACENT DUPLICATES on both ranges
  - **SELECT** from **trolz** (azone, vsbed, tragr, lzone, route) INTO lt_trolz WHERE azone IN ltr_trolz_azones
  - IF sy-subrc = 0: DELETE lt_trolz (vsbed/tragr/lzone), SORT, lt_trolz_temp logic, DELETE/SORT ADJACENT DUPLICATES on lt_trolz_temp
  - **SELECT** from **tvro** (route, distz) INTO lt_tvro FOR ALL ENTRIES IN lt_trolz_temp
  - SORT lt_tvro BY route
  - All corresponding ENDIF

After deletion, Step 7 continues with **LOOP AT lt_enrichments ASSIGNING &lt;lfs_enrichment&gt;** (add current leg, decide insert empty, then fetch route/distance from TVRAB where needed).

---

### 3.5 Empty Leg – Between Two Loaded Legs (approx. lines 830–843)

**Replace:**
```abap
*           Fetch route from TROLZ
          READ TABLE lt_trolz INTO lw_trolz
            WITH KEY azone = lv_prev_dest_zone
                     lzone = lv_curr_source_zone BINARY SEARCH.
          IF sy-subrc = 0.
            lw_enrichment_new-route = lw_trolz-route.

*             Fetch distance from TVRO
            READ TABLE lt_tvro INTO lw_tvro
             WITH KEY route = lw_trolz-route BINARY SEARCH.
            IF sy-subrc = 0.
              lw_enrichment_new-distance = lw_tvro-distz.
            ENDIF.
          ENDIF.
```

**With:**
```abap
*           Fetch route and distance from TVRAB (KNANF = prev dest zone, KNEND = curr source zone)
          SELECT SINGLE route distz
            FROM tvrab CLIENT SPECIFIED
            INTO lw_tvrab
            WHERE mandt = sy-mandt
              AND knanf = lv_prev_dest_zone
              AND knend = lv_curr_source_zone.
          IF sy-subrc = 0.
            lw_enrichment_new-route   = lw_tvrab-route.
            lw_enrichment_new-distance = lw_tvrab-distz.
          ENDIF.
```

---

### 3.6 Empty Leg – Single Leg (approx. lines 868–881)

**Replace:**
```abap
*         Fetch route from TROLZ
        READ TABLE lt_trolz INTO lw_trolz
          WITH KEY azone = <lfs_enrichment>-dest_tzone
                   lzone = <lfs_enrichment>-source_tzone BINARY SEARCH.
        IF sy-subrc = 0.
          lw_enrichment_new-route = lw_trolz-route.

*           Fetch distance from TVRO
          READ TABLE lt_tvro INTO lw_tvro
           WITH KEY route = lw_trolz-route BINARY SEARCH.
          IF sy-subrc = 0.
            lw_enrichment_new-distance = lw_tvro-distz.
          ENDIF.
        ENDIF.
```

**With:**
```abap
*         Fetch route and distance from TVRAB (single leg: dest → source)
        SELECT SINGLE route distz
          FROM tvrab CLIENT SPECIFIED
          INTO lw_tvrab
          WHERE mandt = sy-mandt
            AND knanf = <lfs_enrichment>-dest_tzone
            AND knend = <lfs_enrichment>-source_tzone.
        IF sy-subrc = 0.
          lw_enrichment_new-route   = lw_tvrab-route.
          lw_enrichment_new-distance = lw_tvrab-distz.
        ENDIF.
```

---

## 4. Resulting Behaviour

- Empty leg **route** and **distance** are no longer derived from TROLZ + TVRO.
- They are read once per empty leg from **TVRAB** using:
  - **Between two legs:** KNANF = lv_prev_dest_zone, KNEND = lv_curr_source_zone.
  - **Single leg (return):** KNANF = &lt;lfs_enrichment&gt;-dest_tzone, KNEND = &lt;lfs_enrichment&gt;-source_tzone.
- **lw_enrichment_new-route** = TVRAB-ROUTE, **lw_enrichment_new-distance** = TVRAB-DISTZ.
- If no row is found in TVRAB, route/distance remain initial; existing validation (e.g. exclude trip / message 009 for missing empty leg route/distance) continues to apply.

---

## 5. Checklist

| # | Item |
|---|------|
| 1 | Remove ty_trolz, ty_tvro; add ty_tvrab_route_dist (route, distz). |
| 2 | Remove TROLZ/TVRO and zone range variables; add lw_tvrab. |
| 3 | Optionally remove lc_vsbed, lc_tragr. |
| 4 | Remove entire "Fetch route and distance for empty legs" block (zone init, TROLZ collection loop, TROLZ + TVRO SELECT). |
| 5 | Replace first TROLZ/TVRO read with SELECT SINGLE from TVRAB (lv_prev_dest_zone, lv_curr_source_zone) and assign route/distz. |
| 6 | Replace second TROLZ/TVRO read with SELECT SINGLE from TVRAB (dest_tzone, source_tzone) and assign route/distz. |

---

## 6. Testing Suggestions

- **Between two legs:** Ensure empty leg gets route and distance from TVRAB for the given KNANF/KNEND (prev dest zone, curr source zone).
- **Single leg:** Ensure return empty leg gets route and distance from TVRAB for dest_tzone → source_tzone.
- **No TVRAB row:** Confirm trip is still excluded / message 009 when route or distance is missing.
