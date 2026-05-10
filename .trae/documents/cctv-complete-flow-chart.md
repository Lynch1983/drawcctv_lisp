# CCTV AutoLISP - Complete Flow Chart & Function Call Reference

## 1. Module Load Order & Dependencies

```
M00_spatial_index.lsp        (no deps)
  |
M01_graph_algorithm.lsp      (deps: M02)
  |
M02_line_utils.lsp           (no deps)
  |
M03_mline_converter.lsp      (deps: M02)
  |
M04_duplicate_remover.lsp    (deps: M00, M02)
  |
M05_break_lines.lsp          (deps: M00, M02)
  |
M06_block_utils.lsp          (no deps)
  |
M07_device_projection.lsp    (deps: M01, M02, M06)
  |
M08_equivalent_points.lsp    (deps: M01, M07)
  |
M09_system_diagram.lsp       (deps: M06)
  |
M10_parameter_io.lsp         (no deps)
  |
M11_gui_handlers.lsp         (deps: M07, M08, M10, M12)
  |
M12_main.lsp                 (deps: M00-M11)
```

## 2. User Command Entry Points

```
+-------------------+------------------+------------------------------------------+
| Command           | Entry Function   | Description                              |
+-------------------+------------------+------------------------------------------+
| CCTV-Config       | c:CCTV-Config    | 8-step interactive configuration         |
| CCTV-Run          | c:CCTV-Run       | Execute full workflow                    |
| drawCCTV          | c:drawCCTV       | Legacy alias = main-run-workflow         |
| CCTV-Save         | c:CCTV-Save      | Save config to .txt file                 |
| CCTV-Load         | c:CCTV-Load      | Load config from .txt file               |
| CCTV-RoomPts      | c:CCTV-RoomPts   | Interactive room point picking           |
| CCTV-MLINEArea    | c:CCTV-MLINEArea | Window-select MLINE entities             |
| CCTV-EquivAdd     | c:CCTV-EquivAdd  | Add equivalent point pair                |
| CCTV-EquivClear   | c:CCTV-EquivClear| Clear all equiv pairs                    |
| CCTV-LayerProtect | c:CCTV-LayerProt | Manage protected layers                  |
| CCTV-ShowConfig   | c:CCTV-ShowConfig| Display current config                   |
| CCTV-Help         | c:CCTV-Help      | Show command reference                   |
| CCTV-Test         | c:CCTV-Test      | Run test suite (M13)                     |
+-------------------+------------------+------------------------------------------+
```

## 3. Main Workflow - Complete Function Call Chain

### `main-run-workflow` (M12:712)

```
main-run-workflow
|
+-- STEP 1: main-env-start (M12:44)
|   |-- Save 12 system variables to *main-saved-vars*
|   |-- Set: cmdecho=0, undo begin, osmode=0, attdia=0
|   |         PICKSTYLE=0, PEDITACCEPT=1, dynmode=0
|   |-- Set custom *error* handler -> main-workflow-error
|
+-- STEP 2: main-init (M12:234)
|   |-- graph-init (M01:31)
|   |   |-- Set *graph-nodes* = nil
|   |   |-- Set *graph-edges* = nil
|   |   |-- Set *graph-dist* = nil
|   |   |-- Set *graph-floyd-done* = nil
|   |   |-- Set *graph-node-count* = 0
|   |-- equiv-clear (M08)
|   |   |-- Set *equiv-pairs* = nil
|   |-- main-setup-temp-layer "TEMP_CCTV" color=3 (M12:167)
|   |   |-- vl-catch-all-apply
|   |       |-- tblsearch "LAYER" "TEMP_CCTV"
|   |       |-- command-s "_.layer" "_m" "TEMP_CCTV" "_c" "3" "TEMP_CCTV" ""
|   |-- main-bltc-add "TEMP_CCTV" (M12:146)
|   |   |-- Add to *main-bltc* list
|   |-- main-setup-temp-layer "TEMP_CCTV2" color=3
|   |-- main-bltc-add "TEMP_CCTV2"
|
+-- STEP 3: getpoint
|   |-- Get system diagram insertion point from user
|   |-- Default: (0.0 0.0 0.0) if nil
|
+-- STEP 4: main-gbtc (M12:100)
|   |-- vla-get-layers -> iterate all layers
|   |-- Save already-off layers to *main-hidden-layers*
|   |-- Turn OFF all layers NOT in *main-bltc*
|
+-- STEP 5: main-process-cable-trays (M12:349)
|   |
|   +-- IF *main-mline-set* exists (area selection mode):
|   |   |-- mline-convert-selection (M03)
|   |   |   |-- For each MLINE in selection set:
|   |   |       |-- mline-get-vertices (M03)
|   |   |       |   |-- entget MLINE -> extract vertex coordinates
|   |   |       |-- mline-convert-to-lines (M03)
|   |   |       |   |-- For each vertex pair:
|   |   |       |       |-- entmakex LINE on temp layer
|   |   |       |       |-- ssadd to result selection set
|   |   |       |-- mline-connect-endpoint (M03) x2 (start + end)
|   |   |           |-- mline-find-nearby-lines (M03)
|   |   |           |   |-- ssget nearby LINE entities
|   |   |           |-- For each nearby line:
|   |   |               |-- line-get-closest-point (M02)
|   |   |               |   |-- vl-catch-all-apply 'vlax-curve-getClosestPointTo
|   |   |               |-- If distance < threshold:
|   |   |                   |-- entmakex LINE (endpoint -> projection point)
|   |
|   +-- ELSE IF *main-cable-tray-layer* exists (layer mode):
|   |   |-- mline-process-all (M03)
|   |       |-- mline-get-all-on-layer (M03)
|   |       |   |-- ssget "x" MLINE on cable tray layer
|   |       |-- mline-convert-selection (M03)  [same as above]
|   |       |-- mline-connect-all-endpoints (M03)
|   |           |-- For each LINE in selection:
|   |               |-- mline-connect-endpoint (M03) x2
|   |
|   +-- ELSE: skip (no cable trays configured)
|
+-- STEP 6: dup-remove-all nil "TEMP_CCTV" (M04)
|   |-- ssget "x" LINE on TEMP_CCTV
|   |-- dup-remove-identical (M04)
|   |   |-- dup-build-line-records (M04)
|   |   |   |-- For each LINE:
|   |   |       |-- line-get-endpoints (M02) -> entget
|   |   |       |-- sp-box-from-pts (M00) -> bounding box
|   |   |       |-- dup-line-get-key-from-pts (M04) -> slope/intercept
|   |   |       |-- dup-endpoint-hash (M04) -> string hash
|   |   |       |-- Build record: (ent pts box key hash)
|   |   |-- For each record:
|   |       |-- Lookup hash in hash-table
|   |       |-- If duplicate found: entdel
|   |       |-- Else: add to hash-table
|   |-- dup-merge-colinear (M04)
|       |-- Group records by dup-key-to-hash (quantized slope/intercept)
|       |-- For each group:
|           |-- sp-build-index (M00) -> spatial index
|           |-- For each line in group:
|               |-- sp-get-candidates (M00) -> nearby lines
|               |-- For each candidate:
|                   |-- dup-keys-colinear-p (M04) -> same line?
|                   |-- sp-boxes-overlap-p (M00) -> bounding boxes overlap?
|                   |-- If both: dup-merge-two-lines (M04)
|                       |-- entmakex LINE (merged extent)
|                       |-- entdel both originals
|
+-- STEP 7: main-get-junction-blocks (M12:683)
|   |-- For each block name in *main-junction-blocks*:
|       |-- block-get-all-on-layer (M06)
|       |   |-- ssget "x" INSERT with matching block name
|       |-- ssadd each to result set
|   |-- Return selection set or nil
|
+-- STEP 8: main-process-room-points (M12:612)
|   |
|   +-- Branch 1: (null room-pts) AND junction-ss
|   |   |-- main-process-branch1 (M12:413)
|   |       |-- For each junction block entity:
|   |           |-- block-get-base-point (M06)
|   |           |   |-- block-get-name (M06) -> tblsearch block definition
|   |           |   |-- block-get-insertion-point (M06) -> entget
|   |           |   |-- block-get-scale (M06) -> entget X/Y scale
|   |           |   |-- block-get-rotation (M06) -> entget rotation
|   |           |   |-- Compute base point with scale/rotation transform
|   |           |-- device-project-to-graph (M07)
|   |           |   |-- device-find-nearest-line (M07)
|   |           |   |   |-- device-find-nearest-entity (M07)
|   |           |   |       |-- ssget "x" LINE on temp layers
|   |           |   |       |-- For each LINE:
|   |           |   |           |-- vlax-curve-getClosestPointTo
|   |           |   |           |-- Track minimum distance
|   |           |   |       |-- Return (nearest-ent nearest-pt min-dist)
|   |           |   |-- graph-add-node (M01) [EPHEMERAL - cleared later]
|   |           |   |-- Return (node-index proj-pt proj-dist)
|   |           |-- entmakex LINE (base-pt -> proj-pt) on TEMP_CCTV
|   |           |-- block-get-name-from-text (M06)
|   |           |   |-- block-find-nearest-text (M06)
|   |           |       |-- ssget nearby TEXT/MTEXT
|   |           |       |-- Return content of nearest text
|   |           |-- Add (proj-pt . proj-dist) to gjx-list
|   |           |-- Add name to gjx-name-list
|   |       |-- Return (gjx-list gjx-name-list)
|   |
|   +-- Branch 2: room-pts AND junction-ss
|   |   |-- main-process-branch2 (M12:468)
|   |       |-- For each room point:
|   |       |   |-- device-project-to-graph (M07) [same as Branch 1]
|   |       |   |-- entmakex LINE (room-pt -> proj-pt)
|   |       |   |-- First room point only: add to gjx-list as "RoomEntry"
|   |       |-- Connect room points sequentially:
|   |       |   |-- entmakex LINE (pt[i] -> pt[i+1]) for i=0..n-2
|   |       |-- Close polygon:
|   |       |   |-- entmakex LINE (pt[0] -> pt[n-1]) if n > 1
|   |       |-- For each junction block: [same as Branch 1]
|   |       |-- Return (gjx-list gjx-name-list)
|   |
|   +-- Branch 3: room-pts AND (null junction-ss)
|   |   |-- main-process-branch3 (M12:549)
|   |       |-- Same room point processing as Branch 2
|   |       |-- No junction processing
|   |       |-- Return (gjx-list gjx-name-list)
|   |
|   +-- Default: (null room-pts) AND (null junction-ss)
|       |-- Return (nil nil)
|
+-- STEP 9: main-process-equivalent-points (M12:636)
|   |-- ssget "x" LINE on TEMP_CCTV
|   |-- equiv-process-all (M08)
|   |   |-- For each pair in *equiv-pairs*:
|   |       |-- equiv-connect-pair (M08)
|   |           |-- device-project-to-graph pt1 (M07) -> proj1
|   |           |-- device-project-to-graph pt2 (M07) -> proj2
|   |           |-- graph-add-edge (M01) between proj1 and proj2
|   |               |-- graph-get-node-index for both endpoints
|   |               |-- Add to *graph-edges*
|   |-- dup-remove-all nil "TEMP_CCTV" (M04) [clean equiv duplicates]
|
+-- STEP 10: break-lines-all (M05)
|   |-- ssget "x" LINE on TEMP_CCTV + TEMP_CCTV2
|   |-- break-lines-in-set (M05)
|       |-- IF n > 50: Use spatial index
|       |   |-- break-build-spatial-index (M05)
|       |   |   |-- For each LINE:
|       |   |       |-- line-get-endpoints (M02)
|       |   |       |-- sp-box-from-pts (M00) -> bounding box
|       |   |       |-- sp-build-index (M00) -> spatial hash index
|       |   |-- For each line:
|       |       |-- break-collect-intersections-indexed (M05)
|       |       |   |-- sp-get-candidates (M00) -> nearby line indices
|       |       |   |-- For each candidate:
|       |       |       |-- sp-boxes-overlap-p (M00) -> quick reject
|       |       |       |-- lines-get-intersection (M02)
|       |       |           |-- vlax-ename->vla-object
|       |       |           |-- vla-IntersectWith
|       |       |           |-- vlax-safearray->list -> intersection points
|       |       |-- sp-remove-duplicate-points (M00)
|       |       |-- break-line-at-points (M05)
|       |           |-- sp-sort-points-by-distance (M00)
|       |           |-- break-create-segments-entmake (M05)
|       |           |   |-- For each segment:
|       |           |       |-- entmakex LINE on same layer
|       |           |-- entdel original line
|       |
|       |-- ELSE (n <= 50): Direct O(n^2) comparison
|           |-- break-collect-intersections (M05)
|               |-- For each other line:
|                   |-- lines-get-intersection (M02)
|           |-- break-line-at-points (M05) [same as above]
|
+-- STEP 11: main-build-graph (M12:381)
|   |-- ssget "x" LINE on TEMP_CCTV + TEMP_CCTV2
|   |-- graph-build-from-lines (M01)
|   |   |-- graph-init (M01)  *** CLEARS ALL PREVIOUS NODES/EDGES ***
|   |   |-- For each LINE entity:
|   |       |-- graph-get-line-points (M01)
|   |       |   |-- line-get-endpoints (M02)
|   |       |       |-- entget -> extract group 10 (start) and 11 (end)
|   |       |-- graph-add-node (M01) for start point
|   |       |   |-- graph-coord->key (M01) -> rtos 6-decimal string
|   |       |   |-- If key not in *graph-nodes*:
|   |       |       |-- Add to *graph-nodes*
|   |       |       |-- Increment *graph-node-count*
|   |       |-- graph-add-node (M01) for end point [same]
|   |       |-- graph-add-edge (M01)
|   |           |-- Get node indices for both endpoints
|   |           |-- Add to *graph-edges* with line length as weight
|   |-- graph-floyd-compute (M01)
|       |-- Build initial *graph-dist* matrix from *graph-edges*
|       |   |-- Diagonal = 0.0
|       |   |-- Direct edge = edge weight
|       |   |-- No edge = 1e30 (infinity)
|       |-- Floyd-Warshall triple loop (O(n^3))
|           |-- For k = 0 to n-1:
|               |-- For i = 0 to n-1:
|                   |-- For j = 0 to n-1:
|                       |-- dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j])
|       |-- Set *graph-floyd-done* = T
|
+-- STEP 12: Camera Processing Loop (M12:793-863)
|   |-- main-get-camera-blocks (M12:654)
|   |   |-- For each block name in *main-camera-blocks*:
|   |       |-- block-get-all-on-layer (M06) -> ssget INSERT
|   |       |-- Add to result selection set
|   |
|   |-- For each camera entity:
|       |-- block-get-base-point (M06) -> camera position
|       |-- block-get-name-from-text (M06) -> camera name
|       |
|       |-- device-find-nearest-line (M07)
|       |   |-- device-find-nearest-entity (M07)
|       |       |-- ssget "x" LINE on temp layers
|       |       |-- For each LINE:
|       |           |-- vlax-curve-getClosestPointTo
|       |       |-- Return (nearest-ent nearest-pt min-dist)
|       |
|       |-- For each junction in gjx-list:
|       |   |-- graph-get-node-index (M01)
|       |   |   |-- graph-coord->key -> rtos 6-decimal string
|       |   |   |-- Lookup in *graph-nodes* assoc list
|       |   |
|       |   |-- graph-distance-via-edge (M01)  *** EDGE-SPLIT METHOD ***
|       |       |-- line-get-endpoints (M02) -> get edge endpoints (ep1, ep2)
|       |       |-- graph-get-node-index ep1 -> ep1-node
|       |       |-- graph-get-node-index ep2 -> ep2-node
|       |       |-- graph-get-distance (M01) ep1-node -> jnx-node
|       |       |   |-- Lookup *graph-dist* matrix [i][j]
|       |       |-- graph-get-distance (M01) ep2-node -> jnx-node
|       |       |-- dist_via_ep1 = proj_dist + dist(proj, ep1) + Floyd(ep1, jnx)
|       |       |-- dist_via_ep2 = proj_dist + dist(proj, ep2) + Floyd(ep2, jnx)
|       |       |-- Return min(dist_via_ep1, dist_via_ep2)
|       |       |
|       |       |-- Formula:
|       |       |   total = proj_dist + min(
|       |       |     dist(proj_pt, ep1) + Floyd(ep1_node, jnx_node),
|       |       |     dist(proj_pt, ep2) + Floyd(ep2_node, jnx_node)
|       |       |   )
|       |
|       |-- Calculate total distance:
|       |   |-- use_bias = *main-room-bias* if "RoomEntry", else *main-junction-bias*
|       |   |-- total = (graph_dist + jnx_dist) * cable_coefficient + use_bias
|       |
|       |-- Track minimum total distance -> best junction
|       |-- Add to end-drawlist: (total_dist, block_name, cam_name, jnx_name)
|
+-- STEP 13: sysdiag-classify-by-junction (M09)
|   |-- Group end-drawlist items by junction name (4th element)
|   |-- Return list of groups: ((group1_items...) (group2_items...) ...)
|
+-- STEP 14: sysdiag-draw-classified (M09)
|   |-- For each group:
|       |-- sysdiag-draw-hjx (M09)
|       |   |-- For each camera in group:
|       |   |   |-- sysdiag-insert-block (M09)
|       |   |   |   |-- command-s "_.insert" camera_block
|       |   |   |   |-- entlast -> get inserted entity
|       |   |   |   |-- vla-GetBoundingBox -> block dimensions
|       |   |   |-- Draw vertical connection line
|       |   |   |-- sysdiag-draw-cable-line (M09)
|       |   |       |-- entmakex LINE (start -> end)
|       |   |       |-- command-s "_.text" (cable length label)
|       |   |-- Draw group header block
|       |   |-- Draw junction block
|
+-- STEP 15: main-cleanup (M12:250)
|   |-- main-cleanup-temp-layer "TEMP_CCTV" (M12:198)
|   |   |-- vl-catch-all-apply
|   |       |-- ssget "x" on layer
|   |       |-- For each entity: command-s "_.erase"
|   |-- main-cleanup-temp-layer "TEMP_CCTV2" [same]
|
+-- STEP 16: main-env-end (M12:74)
    |-- Restore 12 system variables from *main-saved-vars*
    |-- Restore *error* handler
    |-- command-s "_.undo" "_end"
    |-- Set cmdecho back
```

## 4. Data Flow Diagram

```
                         USER INPUT
                             |
            +----------------+----------------+
            |                |                |
       CCTV-Config      CCTV-Load       CCTV-RoomPts
            |                |                |
            v                v                v
   *main-camera-blocks*   param-load    *main-room-points*
   *main-junction-blocks*     |          *main-mline-set*
   *main-cable-tray-layer*   |
   *main-cable-coefficient*  v
   *main-junction-bias*   *main-* globals
   *main-room-bias*
            |
            v
     +--- CCTV-Run / drawCCTV ---+
     |                            |
     v                            v
  MLINE entities              LINE entities
  on cable tray layer         on temp layers
     |                            |
     v                            v
  mline-convert-to-lines    dup-remove-all
     |                            |
     v                            v
  LINE entities on temp -----> device-project-to-graph
  layers (TEMP_CCTV,             |
  TEMP_CCTV2)                    v
                           Projection LINEs
                           on TEMP_CCTV
                                 |
                                 v
                          equiv-process-all
                                 |
                                 v
                          dup-remove-all
                                 |
                                 v
                          break-lines-all
                                 |
                                 v
                     +--- graph-build-from-lines ---+
                     |                               |
                     v                               v
              *graph-nodes*                   *graph-edges*
              *graph-dist*                    *graph-floyd-done* = T
                     |                               |
                     +---------------+---------------+
                                     |
                                     v
                          Camera Processing Loop
                          device-find-nearest-line
                          graph-distance-via-edge
                                     |
                                     v
                              end-drawlist
                          ((dist blk cam jnx) ...)
                                     |
                                     v
                          sysdiag-classify-by-junction
                                     |
                                     v
                          sysdiag-draw-classified
                                     |
                                     v
                              SYSTEM DIAGRAM
                           (AutoCAD drawing)
```

## 5. Global Variable Lifecycle

```
Variable                     Set By              Used By                Cleared By
---------------------------  ------------------  --------------------   ----------------
*main-camera-blocks*         CCTV-Config         main-get-camera-       Never
                                                  blocks
*main-junction-blocks*       CCTV-Config         main-get-junction-     Never
                                                  blocks
*main-cable-tray-layer*      CCTV-Config         main-process-          Never
                                                  cable-trays
*main-room-points*           CCTV-RoomPts        main-process-room-     Never
                                                  points
*main-cable-coefficient*     CCTV-Config         Camera loop            Never
*main-junction-bias*         CCTV-Config         Camera loop            Never
*main-room-bias*             CCTV-Config         Camera loop            Never
*main-mline-set*             CCTV-MLINEArea      main-process-          Never
                                                  cable-trays
*main-bltc*                  CCTV-Config         main-gbtc              main-init (reset)
*main-hidden-layers*         main-gbtc           main-dktc              main-gbtc (reset)
*main-saved-vars*            main-env-start      main-env-end           main-env-end

*graph-nodes*                graph-add-node      graph-get-node-index   graph-init (clear)
*graph-edges*                graph-add-edge      graph-build-from-      graph-init (clear)
                                                  lines
*graph-dist*                 graph-floyd-compute graph-get-distance     graph-init (clear)
*graph-floyd-done*           graph-floyd-compute graph-get-distance     graph-init (clear)
*graph-node-count*           graph-add-node      graph-floyd-compute    graph-init (clear)

*equiv-pairs*                equiv-add-pair      equiv-process-all      equiv-clear

*sp-default-cell-size*       sp-set-cell-size    sp-build-index         Never
                                                  sp-get-candidates
```

## 6. Edge-Split Distance Calculation (ISSUE-5 Fix)

```
Camera projection point (proj_pt) lies ON an existing LINE edge.
The LINE edge has two endpoints (ep1, ep2) already in the Floyd matrix.

                    proj_pt
                   /       \
          d(proj,ep1)     d(proj,ep2)
                 /           \
          ep1_node ======= ep2_node    <- Floyd matrix has dist between these
              \                /
       Floyd(ep1, jnx)   Floyd(ep2, jnx)
                \            /
                 \          /
                  junction_node

Total distance from camera to junction:

  total = proj_dist + min(
    d(proj_pt, ep1) + Floyd(ep1_node, jnx_node),
    d(proj_pt, ep2) + Floyd(ep2_node, jnx_node)
  )

Where:
  proj_dist     = distance from camera base point to proj_pt
  d(proj, ep1)  = Euclidean distance from projection point to edge endpoint 1
  d(proj, ep2)  = Euclidean distance from projection point to edge endpoint 2
  Floyd(ep1, jnx) = shortest path distance from ep1 to junction (from matrix)
  Floyd(ep2, jnx) = shortest path distance from ep2 to junction (from matrix)

This avoids:
  1. Adding camera as a new node to the graph
  2. Re-running O(n^3) Floyd-Warshall after each camera projection
```

## 7. Three-Branch Room Point Logic

```
                    Has Room Points?
                       /        \
                     NO          YES
                     /              \
           Has Junctions?      Has Junctions?
              /      \           /        \
            YES       NO       YES        NO
            /          \       /            \
     Branch 1      Default  Branch 2     Branch 3
     (JNX only)   (nil nil) (Room+JNX)  (Room only)

  Branch 1: For each junction -> project to nearest line -> draw LINE
  Branch 2: Room polygon + room projections + junction projections
  Branch 3: Room polygon + room projections (no junctions)
  Default:  No room, no junctions -> return (nil nil)
```

## 8. Spatial Index Architecture (M00)

```
  Input: List of LINE entities
         |
         v
  For each LINE:
    |-- line-get-endpoints (M02) -> (pt1 pt2)
    |-- sp-box-from-pts (M00) -> (min-x min-y max-x max-y)
    |-- sp-spatial-hash (M00) -> cell keys like "cx_cy"
         |
         v
  Spatial Index: assoc list of (cell_key . (record_index ...))
         |
         v
  Query: sp-get-candidates (M00)
    |-- Input: bounding box
    |-- Output: list of record indices in nearby cells
    |-- Used by: M04 (duplicate detection), M05 (intersection detection)
```

## 9. Error Handling Strategy

```
  Level 1: Nil checks
    |-- All entget/entmakex calls check for nil return
    |-- All ssget calls check for nil result
    |-- All function parameters validated before use

  Level 2: vl-catch-all-apply
    |-- All vla-* operations (vla-GetBoundingBox, vla-IntersectWith, etc.)
    |-- All vlax-curve-* operations
    |-- File I/O operations (param-load, param-save)
    |-- ssget operations (may fail in certain contexts)

  Level 3: Custom *error* handler
    |-- main-workflow-error in main-run-workflow
    |-- Catches any unhandled errors
    |-- Calls main-cleanup + main-dktc + main-env-end before exiting
    |-- Prevents orphaned temp entities or sysvar changes
```

## 10. Performance Characteristics

```
  Operation                    Complexity    Optimized?
  ---------------------------  -----------   ----------
  dup-remove-identical         O(n)          Yes (endpoint hash)
  dup-merge-colinear           O(n*k)        Yes (spatial index + group hash)
  break-lines-in-set (>50)     O(n*k)        Yes (spatial index)
  break-lines-in-set (<=50)    O(n^2)        No (direct comparison)
  graph-build-from-lines       O(n)          Yes
  graph-floyd-compute          O(n^3)        No (inherent)
  graph-distance-via-edge      O(1)          Yes (Floyd matrix lookup)
  Camera loop (m cameras,      O(m*j)        Yes (edge-split, no Floyd rerun)
    j junctions)
  sysdiag-draw-classified      O(g*c)        Yes (grouped drawing)
```
