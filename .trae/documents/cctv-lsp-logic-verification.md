# CCTV LISP Project Logic Verification & Execution Plan

## Summary

Verify the project logic flow from entry point to completion, identify logic issues, and create a corrected execution plan.

## Current State Analysis

### Entry Points (User Commands)

| Command | Module | Description |
|---------|--------|-------------|
| `CCTV-Config` | M11 | Interactive configuration (8 steps) |
| `CCTV-Run` | M11 | Execute workflow with current config |
| `CCTV-Save` | M11 | Save config to file |
| `CCTV-Load` | M11 | Load config from file |
| `CCTV-RoomPts` | M11 | Set room entry points |
| `CCTV-MLINEArea` | M11 | Select MLINE area |
| `CCTV-EquivAdd` | M11 | Add equivalent point pair |
| `CCTV-EquivClear` | M11 | Clear equivalent pairs |
| `CCTV-LayerProtect` | M11 | Layer visibility management |
| `CCTV-ShowConfig` | M11 | Display current config |
| `CCTV-Help` | M11 | Show help |
| `drawCCTV` | M12 | Legacy command = `main-run-workflow` |
| `CCTV-Test` | M13 | Run test suite |

### Current `main-run-workflow` Execution Order

```
1.  main-env-start()          -- Save sysvars, set working env
2.  main-init()               -- graph-init, equiv-clear, create temp layers
3.  getpoint()                -- Get system diagram insertion point
4.  main-gbtc()               -- Turn off non-protected layers
5.  main-process-cable-trays()
    ├── mline-convert-selection / mline-process-all  -- MLINE -> LINE
    ├── dup-remove-all()       -- Remove duplicates (1st time)
    └── break-lines-all()      -- Break at intersections (1st time)
6.  dup-remove-all()           -- Remove duplicates (2nd time) ⚠️ REDUNDANT
7.  main-get-junction-blocks() -- Get junction block entities
8.  main-process-room-points() -- Project junction/room points
    ├── device-project-to-graph()  -- Adds nodes to graph (will be cleared)
    └── entmakex LINE          -- Draw projection lines on temp layer
9.  main-process-equivalent-points()
    ├── equiv-process-all()    -- Connect equiv pairs via graph-add-edge
    ├── dup-remove-all()       -- Remove duplicates (3rd time) ⚠️
    └── graph-floyd-compute()  -- Floyd on INCOMPLETE graph ⚠️ BUG
10. break-lines-all()          -- Break at intersections (2nd time) ⚠️ REDUNDANT
11. main-build-graph()
    ├── graph-build-from-lines()  -- Calls graph-init() first! Clears all nodes
    └── graph-floyd-compute()     -- Floyd on COMPLETE graph ✅
12. Camera processing loop
    ├── device-project-to-graph()  -- Project cameras to graph
    ├── graph-get-distance()       -- Get shortest path distances
    └── Build end-drawlist
13. sysdiag-classify-by-junction()  -- Group by junction
14. sysdiag-draw-classified()       -- Draw system diagram
15. main-cleanup()               -- Delete temp entities
16. main-env-end()               -- Restore sysvars
```

### Logic Issues Found

#### BUG-1: `graph-floyd-compute` called on incomplete graph (Line 645)
- **Location**: `main-process-equivalent-points` (M12:645)
- **Problem**: Floyd-Warshall is computed before `graph-build-from-lines`, so the graph only has nodes from `device-project-to-graph` but no edges from LINE entities
- **Impact**: The Floyd result is incorrect/wasteful. It gets overwritten by the correct Floyd in `main-build-graph` (L399), so the final result is correct, but the intermediate computation is wasted
- **Fix**: Remove `graph-floyd-compute` from `main-process-equivalent-points`

#### BUG-2: Redundant `dup-remove-all` calls
- **Location**: `main-run-workflow` L747 calls `dup-remove-all` after `main-process-cable-trays` already called it internally (L374)
- **Impact**: Wasteful but not harmful. The second call finds no duplicates
- **Fix**: Remove the redundant call in `main-run-workflow`

#### BUG-3: Redundant `break-lines-all` calls
- **Location**: `main-run-workflow` L757 calls `break-lines-all` after `main-process-cable-trays` already called it (L376)
- **Impact**: Wasteful but not harmful. However, between the two calls, `main-process-room-points` added projection lines that may create new intersections that need breaking
- **Fix**: Keep the second `break-lines-all` call (it's needed for projection lines), but remove the one inside `main-process-cable-trays`

#### ISSUE-4: `device-project-to-graph` adds nodes that get cleared
- **Location**: `main-process-room-points` (M12:443,495,524,576)
- **Problem**: `device-project-to-graph` calls `graph-add-node`, but `graph-build-from-lines` later calls `graph-init` which clears all nodes
- **Impact**: No functional impact because the projection LINES are drawn on the temp layer and `graph-build-from-lines` rebuilds nodes from all LINE entities including projections
- **Fix**: This is actually correct by design. The projection lines are the persistent artifact; the graph nodes are ephemeral. No change needed, but add a clarifying comment

#### ISSUE-5: `main-process-equivalent-points` calls `dup-remove-all`
- **Location**: M12:644
- **Problem**: After connecting equivalent pairs, it removes duplicates. But the graph hasn't been fully built yet, so this is premature
- **Impact**: Low risk. The duplicates removed here are from equiv-pair connection lines. The final `main-build-graph` will work with whatever LINE entities exist
- **Fix**: Keep this call (it removes duplicate lines created by equiv connections)

### Corrected Execution Order

```
1.  main-env-start()
2.  main-init()                    -- graph-init, equiv-clear, temp layers
3.  getpoint()                     -- Diagram insertion point
4.  main-gbtc()                    -- Layer management
5.  main-process-cable-trays()
    ├── mline-convert-selection / mline-process-all
    └── (removed: dup-remove-all, break-lines-all from here)
6.  dup-remove-all()               -- Remove duplicates (once, after all MLINE conversion)
7.  main-get-junction-blocks()
8.  main-process-room-points()     -- Draw projection lines
9.  main-process-equivalent-points()
    ├── equiv-process-all()
    ├── dup-remove-all()            -- Clean up equiv connection duplicates
    └── (removed: graph-floyd-compute -- not needed here)
10. break-lines-all()              -- Break all intersections (once, after all lines created)
11. main-build-graph()
    ├── graph-build-from-lines()   -- Builds graph from ALL lines including projections
    └── graph-floyd-compute()      -- Floyd on complete graph
12. Camera processing loop
13. sysdiag-classify-by-junction()
14. sysdiag-draw-classified()
15. main-cleanup()
16. main-env-end()
```

## Proposed Changes

### File: /workspace/M12_main.lsp

#### Change 1: Remove redundant calls from `main-process-cable-trays`
- Remove `dup-remove-all` call at line 374
- Remove `break-lines-all` call at line 376
- Reason: These will be called later in the main workflow after all lines (including projections) are created

#### Change 2: Remove premature `graph-floyd-compute` from `main-process-equivalent-points`
- Remove `graph-floyd-compute` call at line 645
- Reason: Graph is not fully built at this point; Floyd will be computed correctly in `main-build-graph`

#### Change 3: Add clarifying doc to `main-process-room-points`
- Add note that `device-project-to-graph` nodes are ephemeral and will be rebuilt by `graph-build-from-lines`
- The projection LINES on temp layer are the persistent artifacts

### No changes needed in other files

All other modules (M00-M11) have correct internal logic. The issues are only in the orchestration within M12's `main-run-workflow`.

## Verification Steps

1. Bracket balance check on modified M12
2. ANSI encoding check
3. Cross-module call verification (no new calls added/removed)
4. Verify `main-process-cable-trays` still returns T
5. Verify `main-process-equivalent-points` still works without Floyd
6. Verify `main-build-graph` is the only place that calls `graph-floyd-compute`

## Assumptions & Decisions

- **Decision**: Keep `dup-remove-all` in `main-process-equivalent-points` because equiv connections may create duplicate lines
- **Decision**: Keep `break-lines-all` in main workflow (not in cable-trays) because projection lines create new intersections
- **Assumption**: The projection lines drawn by `main-process-room-points` are on `*main-temp-layer*` and will be included in `graph-build-from-lines`
- **Assumption**: `equiv-process-all` only needs `graph-add-edge` (not Floyd) to work correctly
