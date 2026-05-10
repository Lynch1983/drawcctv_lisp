# CCTV AutoLISP Project - Complete Flow & Logic Verification

## 1. Module Architecture

```
Load Order: M00 -> M01 -> M02 -> M03 -> M04 -> M05 -> M06 -> M07 -> M08 -> M09 -> M10 -> M11 -> M12

M00  spatial_index.lsp    Spatial hash, bounding box, point utilities
M01  graph_algorithm.lsp  Graph data structure, Floyd-Warshall, node/edge management
M02  line_utils.lsp       LINE entity operations, intersection, create/delete
M03  mline_converter.lsp  MLINE -> LINE conversion, endpoint connection
M04  duplicate_remover.lsp Remove duplicate/overlapping lines
M05  break_lines.lsp      Break lines at intersection points
M06  block_utils.lsp      Block info extraction, text recognition
M07  device_projection.lsp Device-to-graph projection, distance calculation
M08  equivalent_points.lsp Equivalent point pair management
M09  system_diagram.lsp   System diagram drawing, classification
M10  parameter_io.lsp     Configuration file read/write
M11  gui_handlers.lsp     User command interface
M12  main.lsp             Main workflow orchestration
```

## 2. User Entry Points

| Command | File | Function | Description |
|---------|------|----------|-------------|
| CCTV-Config | M11 | gui-configure-workflow | 8-step interactive configuration |
| CCTV-Run | M11 | main-run-workflow | Execute full workflow |
| drawCCTV | M12 | main-run-workflow | Legacy alias for CCTV-Run |
| CCTV-Save | M11 | main-save-parameters | Save config to .txt file |
| CCTV-Load | M11 | main-load-parameters | Load config from .txt file |
| CCTV-RoomPts | M11 | c:CCTV-RoomPts | Interactive room point picking |
| CCTV-MLINEArea | M11 | main-select-mline-area | Window-select MLINE entities |
| CCTV-EquivAdd | M11 | equiv-add-pair | Add equivalent point pair |
| CCTV-EquivClear | M11 | equiv-clear | Clear all equiv pairs |
| CCTV-LayerProtect | M11 | main-bltc-add/remove | Manage protected layers |
| CCTV-ShowConfig | M11 | c:CCTV-ShowConfig | Display current config |
| CCTV-Help | M11 | c:CCTV-Help | Show command reference |

## 3. Complete Workflow Call Chain (main-run-workflow)

```
main-run-workflow (M12:713)
|
+-- 1. main-env-start (M12:45)
|      Save sysvars: osmode, cmdecho, clayer, textstyle, cecolor,
|                    dimstyle, plinewid, attdia, PICKSTYLE, PEDITACCEPT,
|                    dynmode, nomutt
|      Set: cmdecho=0, undo begin, osmode=0, attdia=0,
|           PICKSTYLE=0, PEDITACCEPT=1, dynmode=0
|      Set custom *error* handler -> main-workflow-error
|
+-- 2. main-init (M12:235)
|      |-- graph-init (M01)         Clear *graph-nodes*, *graph-edges*, *graph-dist*
|      |-- equiv-clear (M08)        Clear *equiv-pairs*
|      |-- main-setup-temp-layer    Create TEMP_CCTV (color 3)
|      |-- main-bltc-add            Add TEMP_CCTV to protected layers
|      |-- main-setup-temp-layer    Create TEMP_CCTV2 (color 3)
|      |-- main-bltc-add            Add TEMP_CCTV2 to protected layers
|
+-- 3. getpoint                    Get system diagram insertion point
|
+-- 4. main-gbtc (M12:101)        Turn off layers NOT in *main-bltc*
|      |-- vla-get-layers           Iterate all layers
|      |-- Save already-off layers to *main-hidden-layers*
|      |-- Turn off non-protected layers
|
+-- 5. main-process-cable-trays (M12:350)
|      |
|      +-- IF *main-mline-set* exists (area selection):
|      |   |-- mline-convert-selection (M03)
|      |   |   |-- For each MLINE in selection:
|      |   |   |   |-- mline-get-vertices (M03)
|      |   |   |   |-- mline-convert-to-lines (M03) -> entmakex LINE on temp layer
|      |   |   |   |-- mline-connect-endpoint (M03) -> connect endpoints to nearby lines
|      |   |   |   |   |-- line-get-closest-point (M02)
|      |   |   |   |   |-- entmakex LINE (connection line)
|      |
|      +-- ELSE IF *main-cable-tray-layer* exists:
|      |   |-- mline-process-all (M03)
|      |   |   |-- ssget all MLINEs on cable tray layer
|      |   |   |-- For each MLINE:
|      |   |   |   |-- mline-get-vertices (M03)
|      |   |   |   |-- mline-convert-to-lines (M03)
|      |   |   |   |-- mline-connect-endpoint (M03)
|      |   |   |-- dup-remove-all (M04) on temp layer
|      |   |   |-- break-lines-all (M05) on temp layers
|      |   |   |-- Return T
|      |
|      +-- ELSE: skip (no cable trays configured)
|
+-- 6. dup-remove-all (M04)       Remove duplicate/overlapping lines on temp layer
|      |-- Build line records with endpoints, bounding boxes, keys
|      |-- dup-remove-identical: Endpoint hash O(n) lookup
|      |-- dup-merge-colinear: Spatial hash grouping + merge overlapping
|      |   |-- sp-build-index (M00)  Build spatial index
|      |   |-- sp-get-candidates (M00)  Query nearby lines
|      |   |-- dup-merge-two-lines: entmakex + entdel
|
+-- 7. main-get-junction-blocks (M12:684)
|      |-- For each block name in *main-junction-blocks*:
|      |   |-- block-get-all-on-layer (M06)  Find blocks by name
|      |   |-- ssadd to result set
|      |-- Return selection set or nil
|
+-- 8. main-process-room-points (M12:613)
|      |
|      +-- Branch 1: No room + has junctions -> main-process-branch1
|      |   |-- For each junction block:
|      |   |   |-- block-get-base-point (M06)
|      |   |   |-- device-project-to-graph (M07)
|      |   |   |   |-- device-find-nearest-line (M07)
|      |   |   |   |   |-- device-find-nearest-entity (M07)
|      |   |   |   |       |-- sp-spatial-hash (M00) or ssget
|      |   |   |   |       |-- vlax-curve-getClosestPointTo
|      |   |   |   |-- graph-add-node (M01)  [EPHEMERAL - cleared later]
|      |   |   |-- entmakex LINE (base-pt -> proj-pt) on temp layer
|      |   |   |-- block-get-name-from-text (M06)
|      |   |-- Return (gjx-list gjx-name-list)
|      |
|      +-- Branch 2: Has room + has junctions -> main-process-branch2
|      |   |-- For each room point:
|      |   |   |-- device-project-to-graph -> entmakex LINE
|      |   |   |-- Only first room point added to gjx-list as "RoomEntry"
|      |   |-- Connect room points in sequence: entmakex LINE (pt[i] -> pt[i+1])
|      |   |-- Connect first and last room point: entmakex LINE (pt[0] -> pt[n-1])
|      |   |-- For each junction block: (same as Branch 1)
|      |   |-- Return (gjx-list gjx-name-list)
|      |
|      +-- Branch 3: Has room + no junctions -> main-process-branch3
|      |   |-- Same room point processing as Branch 2
|      |   |-- No junction processing
|      |   |-- Return (gjx-list gjx-name-list)
|      |
|      +-- Default: No room + no junctions -> (nil nil)
|
+-- 9. main-process-equivalent-points (M12:637)
|      |-- ssget all LINEs on temp layer
|      |-- equiv-process-all (M08)
|      |   |-- For each pair in *equiv-pairs*:
|      |   |   |-- equiv-connect-pair (M08)
|      |   |       |-- device-project-to-graph pt1 (M07) -> proj1
|      |   |       |-- device-project-to-graph pt2 (M07) -> proj2
|      |   |       |-- graph-add-edge (M01) between proj1 and proj2
|      |-- dup-remove-all (M04)  Clean up equiv connection duplicates
|
+-- 10. break-lines-all (M05)     Break all lines at intersections
|      |-- break-lines-in-set (M05)
|      |   |-- IF n > 50: Use spatial index (M00)
|      |   |   |-- break-build-spatial-index -> sp-build-index (M00)
|      |   |   |-- For each line:
|      |   |       |-- break-collect-intersections-indexed
|      |   |       |   |-- sp-get-candidates (M00)
|      |   |       |   |-- sp-boxes-overlap-p (M00)
|      |   |       |   |-- lines-get-intersection (M02) -> vlax-curve
|      |   |       |-- break-line-at-points
|      |   |           |-- sp-sort-points-by-distance (M00)
|      |   |           |-- break-create-segments-entmake -> entmakex LINE
|      |   |           |-- entdel original line
|      |   |-- ELSE: Direct O(n^2) comparison
|
+-- 11. main-build-graph (M12:382)
|      |-- ssget all LINEs on temp layers
|      |-- graph-build-from-lines (M01)
|      |   |-- graph-init (M01)  *** CLEARS ALL PREVIOUS NODES ***
|      |   |-- For each LINE:
|      |   |   |-- graph-get-line-points (M01) -> line-get-endpoints (M02)
|      |   |   |-- graph-add-node (M01) for each endpoint
|      |   |   |-- graph-add-edge (M01) with line length as weight
|      |-- graph-floyd-compute (M01)
|      |   |-- Build initial distance matrix from edges
|      |   |-- Floyd-Warshall triple loop O(n^3)
|      |   |-- Set *graph-floyd-done* = T
|
+-- 12. Camera Processing Loop (M12:793-863)
|      |-- main-get-camera-blocks (M12:655)
|      |   |-- block-get-all-on-layer (M06) for each camera block name
|      |
|      |-- For each camera entity:
|          |-- block-get-base-point (M06)
|          |-- block-get-name-from-text (M06)
|          |-- device-project-to-graph (M07)
|          |   |-- device-find-nearest-line (M07)
|          |   |-- graph-add-node (M01)  [PERSISTENT - graph already built]
|          |
|          |-- For each junction in gjx-list:
|          |   |-- graph-get-node-index (M01)  Find node for junction point
|          |   |-- graph-get-distance (M01)    Floyd shortest path
|          |   |-- Calculate: total_dist = (graph_dist + proj_dist + jnx_dist) * coef + bias
|          |   |-- Track best (minimum total_dist) junction
|          |
|          |-- Add to end-drawlist: (total_dist block_name cam_name jnx_name)
|
+-- 13. sysdiag-classify-by-junction (M09:60)
|      |-- Group end-drawlist items by junction name (4th element)
|      |-- Return list of groups
|
+-- 14. sysdiag-draw-classified (M09)
|      |-- For each group:
|      |   |-- sysdiag-draw-hjx (M09)
|      |   |   |-- sysdiag-insert-block (M09) -> command-s "_.insert"
|      |   |   |-- vla-GetBoundingBox -> get block dimensions
|      |   |   |-- Draw connection lines between blocks
|      |   |   |-- sysdiag-draw-cable-line (M09) -> entmakex LINE + text label
|      |   |-- Draw group header
|
+-- 15. main-cleanup (M12:251)
|      |-- main-cleanup-temp-layer TEMP_CCTV
|      |   |-- ssget + erase all entities on layer
|      |-- main-cleanup-temp-layer TEMP_CCTV2
|
+-- 16. main-env-end (M12:75)
       |-- Restore all saved sysvars
       |-- Restore *error* handler
       |-- undo end
       |-- Set cmdecho back
```

## 4. Data Flow Diagram

```
User Input                     Processing                      Output
=========                      ===========                     ======

CCTV-Config -----> *main-camera-blocks*
                   *main-junction-blocks*
                   *main-cable-tray-layer*
                   *main-room-points*
                   *main-equiv-points*
                   *main-cable-coefficient*
                   *main-junction-bias*
                   *main-room-bias*

CCTV-Run --------> MLINE entities -----> LINE entities (temp layer)
                   |                        |
                   |               dup-remove-all (deduplicate)
                   |                        |
                   |               device-project-to-graph (projection lines)
                   |                        |
                   |               equiv-process-all (equiv connections)
                   |                        |
                   |               dup-remove-all (clean equiv duplicates)
                   |                        |
                   |               break-lines-all (break at intersections)
                   |                        |
                   |               graph-build-from-lines (build graph)
                   |                        |
                   |               graph-floyd-compute (shortest paths)
                   |                        |
                   |               Camera projection + distance calculation
                   |                        |
                   |               sysdiag-classify-by-junction
                   |                        |
                   |               sysdiag-draw-classified -----> System Diagram
                   |
                   main-cleanup (delete temp entities)
```

## 5. Global Variable Lifecycle

```
Variable                    Set By              Used By              Cleared By
--------                    ------              -------              ----------
*main-camera-blocks*        CCTV-Config         main-get-camera-     Never (persistent)
                                                blocks
*main-junction-blocks*      CCTV-Config         main-get-junction-   Never (persistent)
                                                blocks
*main-cable-tray-layer*     CCTV-Config         main-process-        Never (persistent)
                                                cable-trays
*main-room-points*          CCTV-RoomPts        main-process-room-   Never (persistent)
                                                points
*main-equiv-points*         CCTV-EquivAdd       (unused - see       Never
                                                *equiv-pairs*)
*main-cable-coefficient*    CCTV-Config         Camera loop          Never (persistent)
*main-junction-bias*        CCTV-Config         Camera loop          Never (persistent)
*main-room-bias*            CCTV-Config         Camera loop          Never (persistent)
*main-mline-set*            CCTV-MLINEArea      main-process-        Never (persistent)
                                                cable-trays
*main-bltc*                 CCTV-Config         main-gbtc            main-init (reset)
*main-hidden-layers*        main-gbtc           main-dktc            main-gbtc (reset)
*main-saved-vars*           main-env-start      main-env-end         main-env-end

*graph-nodes*               graph-add-node      graph-get-distance   graph-init (clear)
*graph-edges*               graph-add-edge      graph-build-from-    graph-init (clear)
                                                lines
*graph-dist*                graph-floyd-compute graph-get-distance   graph-init (clear)
*graph-floyd-done*          graph-floyd-compute graph-get-distance   graph-init (clear)
*graph-node-count*          graph-add-node      graph-floyd-compute  graph-init (clear)

*equiv-pairs*               equiv-add-pair      equiv-process-all    equiv-clear
```

## 6. Logic Issues Found

### ISSUE-1: *main-equiv-points* is never used (LOW)
- **Location**: M12:20 `(setq *main-equiv-points* nil)`
- **Problem**: This global variable is declared but never read or written by any function.
  Equivalent points are stored in `*equiv-pairs*` (M08), not `*main-equiv-points*`.
- **Impact**: Dead code, no functional impact.
- **Fix**: Remove the declaration, or use it to store the equiv points list.

### ISSUE-2: mline-process-all has its own dup/break but main-process-cable-trays does not (MEDIUM)
- **Location**: M03 `mline-process-all` calls `dup-remove-all` and `break-lines-all` internally
- **Problem**: When `*main-mline-set*` is used (area selection path), `mline-convert-selection` is called instead of `mline-process-all`. The convert-selection function does NOT call dup/break. But when using the layer-based path, `mline-process-all` DOES call dup/break internally.
- **Impact**: Inconsistent behavior between the two selection modes. The area selection path skips dedup/break inside M03, but the layer path does it inside M03 AND again in the main workflow.
- **Fix**: Remove dup/break from `mline-process-all` (M03) to match `mline-convert-selection`, since the main workflow calls them later anyway.

### ISSUE-3: Room point polygon closure may create zero-length lines (LOW)
- **Location**: M12:511-512 (Branch 2) and M12:592-593 (Branch 3)
- **Problem**: When there's only 1 room point, `(1- (length room-pts))` = 0, so the polygon closure line `(nth 0 room-pts)` -> `(nth 0 room-pts)` creates a zero-length line.
- **Impact**: Zero-length line added to graph, may cause issues with graph-add-edge (which checks `> 0.001`).
- **Fix**: Add check `(> (length room-pts) 1)` before drawing polygon closure line (already present for the last room point connection, but the sequential connections loop uses `(1- (length room-pts))` which is 0 for single point).

### ISSUE-4: equiv-process-all passes line-ss but device-project-to-graph receives nil (MEDIUM)
- **Location**: M12:642 `equiv-process-all ss` -> M08:78 `equiv-connect-pair (car pair) line-ss`
  -> M08:98 `device-project-to-graph pt1 line-ss nil`
- **Problem**: `equiv-process-all` receives the line-ss and passes it to `equiv-connect-pair`, which passes it to `device-project-to-graph`. But `device-project-to-graph` calls `device-find-nearest-line pt line-ss nil`, and when line-ss is not nil, it uses it directly. This is correct behavior.
- **Impact**: No bug. The line-ss contains all LINEs on temp layer, which is the correct set for finding nearest lines.
- **Status**: Verified correct.

### ISSUE-5: Camera loop calls device-project-to-graph AFTER graph is built (CORRECT)
- **Location**: M12:802
- **Analysis**: After `main-build-graph` (step 11), the graph is fully built with Floyd computed.
  Camera projection calls `device-project-to-graph` which calls `graph-add-node`.
  This adds new nodes to the graph, but Floyd is NOT re-computed.
- **Problem**: The new camera nodes are added to the graph but have no shortest-path distances computed.
  However, the camera code only uses `graph-get-distance(dev-node, jnx-node)` where
  `jnx-node` is found by `graph-get-node-index(tmp-jnx-pt)`.
- **Critical Detail**: `device-project-to-graph` adds a node at the projection point.
  This point is ON an existing edge. The shortest path from this new node to any
  junction node is NOT in the Floyd matrix (Floyd was computed before this node was added).
- **Impact**: `graph-get-distance` will return nil for the new camera node because
  the Floyd matrix doesn't include it. The camera will be reported as "NO JUNCTION FOUND".
- **Fix**: This is a **CRITICAL BUG**. After projecting cameras, need to either:
  (a) Re-run Floyd (expensive), or
  (b) Use the edge-splitting approach: when projecting a camera, split the nearest edge
      at the projection point, then compute distance as min-over-k(dist[cam][k] + dist[k][jnx])
      which can be done without full Floyd recompute.

### ISSUE-6: graph-get-node-index may return nil for junction points (MEDIUM)
- **Location**: M12:820 `(graph-get-node-index tmp-jnx-pt)`
- **Problem**: `graph-get-node-index` finds a node by exact point match. But after `graph-build-from-lines`,
  the junction projection points were added as nodes by `device-project-to-graph` in step 8,
  then CLEARED by `graph-init` in step 11, then rebuilt by `graph-build-from-lines`.
  The rebuilt nodes may have slightly different coordinates due to floating-point precision.
- **Impact**: Some junction points may not be found, causing cameras to not connect.
- **Fix**: Use tolerance-based node lookup in `graph-get-node-index`.

## 7. Issue Severity Summary

| ID | Severity | Description | Status |
|----|----------|-------------|--------|
| ISSUE-1 | LOW | *main-equiv-points* unused variable | Dead code |
| ISSUE-2 | MEDIUM | mline-process-all has internal dup/break but mline-convert-selection does not | Inconsistent |
| ISSUE-3 | LOW | Single room point creates zero-length polygon line | Edge case |
| ISSUE-4 | NONE | equiv-process-all line-ss passing | Verified correct |
| ISSUE-5 | CRITICAL | Camera nodes added after Floyd, distances unavailable | Must fix |
| ISSUE-6 | MEDIUM | graph-get-node-index uses exact match, may miss junctions | Should fix |

## 8. Recommended Fix Priority

1. **ISSUE-5 (CRITICAL)**: Camera projection after Floyd must handle distance calculation differently
2. **ISSUE-6 (MEDIUM)**: Add tolerance to graph-get-node-index
3. **ISSUE-2 (MEDIUM)**: Remove dup/break from mline-process-all
4. **ISSUE-3 (LOW)**: Guard against single room point polygon
5. **ISSUE-1 (LOW)**: Remove unused *main-equiv-points*
