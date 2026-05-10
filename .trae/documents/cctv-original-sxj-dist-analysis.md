# 原始代码摄像机接入逻辑分析

## 原始代码核心流程

原始文件 [drawCCTV(界面版)2.lsp](file:///workspace/drawCCTV(界面版)2.lsp) 的摄像机接入逻辑分为 **两个阶段**：

### 阶段一：`sxj_dist` — 摄像机到线槽的接入点+距离（L2134-2266）

```
摄像机 base_pt (@p1)
    │
    ├─ Step 1: 在 1500mm 范围内搜索线槽 LINE (tmp_layer)
    │  (ssget "C" pt1 pt2 (cons 8 tmp_layer))
    │
    │  找到 → block_dist(@p1) → 找最近线槽投影点 block_dist_pt
    │         single_cctv_dist = distance(block_dist_pt, @p1)
    │         画线: LINE @p1 → block_dist_pt
    │         直接返回，**不经过钢管**
    │
    │  没找到 ↓
    │
    ├─ Step 2: 在 1500mm 范围内搜索钢管 (gg_layer, LWPOLYLINE+LINE)
    │  (ssget "C" pt1 pt2 (cons 8 gg_layer))
    │
    │  找到钢管 → 执行钢管路径计算（见下方详细分析）
    │  没找到 → block_dist(@p1) → 直接找最近线槽投影点（扩大搜索范围）
    │
    └─ 返回: block_dist_pt, single_cctv_dist
```

### 阶段二：`drawCCTV` 主循环 — 摄像机到接线盒距离（L3437-3477）

```
对每个摄像机:
    sxj_dist(base_pt) → single_cctv_pt, single_cctv_dist
    
    break_line(single_cctv_pt)  ← 在投影点打断线槽线
    
    对前3个最近接线盒 (min 3 gjx_list):
        sub1_tt1(single_cctv_pt, gjx_pt) → gjx_cctv_dis  ← Dijkstra最短路径
        
        tmp_dis = gjx_cctv_dis + single_cctv_dist + gjx_to_tray_dist
        tmp_dis = tmp_dis × cctv_coe + bias
        
        选最小 tmp_dis
    
    join_line  ← 恢复打断的线
```

---

## `sxj_dist` 钢管路径详细分析（L2153-2247）

这是原始代码最核心的部分，也是我们重构中遗漏的关键逻辑：

### Step 2A: 找最近钢管实体

```lisp
;; 在 1500mm 范围搜索钢管层 (LWPOLYLINE + LINE)
(setq &kw (ssget "C" pt1 pt2 (list (cons 0 "LWPOLYLINE,LINE") (cons 8 gg_layer))))

;; 遍历找最近点
(repeat (sslength &kw)
  (setq tmp_pts (vlax-curve-getClosestPointTo tmp_ent @p1))
  (setq tmp_dis (distance tmp_pts @p1))
  (if (< tmp_dis min_line)  ;; min_line 初始值 3000
    (setq start_ent tmp_ent)  ;; 最近的钢管实体
  )
)
```

### Step 2B: 沿钢管端点扩展相邻钢管（最多3段）

```lisp
;; 从 start_ent 开始，找其端点附近的其他钢管
(setq entlst (ssadd start_ent entlst))

;; 起点 zoom + ssget 找相邻钢管
(setq sta_pts (vlax-curve-getStartPoint start_ent))
(command-s "_zoom" "w" ...)  ;; zoom 到起点
(setq tmp_entset (ssget "c" pt3 pt4 (cons 8 gg_layer)))  ;; 找起点附近钢管
(if (ssname tmp_entset 0) (setq entlst (ssadd ... entlst)))  ;; 加入 entlst

;; 终点 zoom + ssget 找相邻钢管
(setq end_pts (vlax-curve-getEndPoint start_ent))
(command-s "_zoom" "w" ...)  ;; zoom 到终点
(setq tmp_entset (ssget "c" pt3 pt4 (cons 8 gg_layer)))  ;; 找终点附近钢管
(if (ssname tmp_entset 0) (setq entlst (ssadd ... entlst)))  ;; 加入 entlst
```

**关键**：原始代码用 `zoom` + `ssget` 的方式在端点附近搜索相邻钢管，最多收集 3 段钢管（start_ent + 起点1段 + 终点1段）。

### Step 2C: 找钢管→线槽的最佳出口点

```lisp
;; 对 entlst 中每段钢管的起点和终点，调用 block_dist 找最近线槽点
(repeat (sslength entlst)
  (setq tt_ent (ssname entlst i))
  (setq sta_tt_pts (vlax-curve-getStartPoint tt_ent))
  (setq end_tt_pts (vlax-curve-getEndPoint tt_ent))
  
  (block_dist sta_tt_pts)  ;; 找起点到线槽的最近点
  (setq sta_xc_pts block_dist_pt)
  (block_dist end_tt_pts)  ;; 找终点到线槽的最近点
  (setq end_xc_pts block_dist_pt)
  
  ;; 选距离线槽最近的端点作为出口
  (if (< (distance sta_xc_pts sta_tt_pts) min_dis)
    (setq min_pts sta_xc_pts  min_dis=...  min_ent=...  flag_min=i)
  )
  (if (< (distance end_xc_pts end_tt_pts) min_dis)
    (setq min_pts end_xc_pts  min_dis=...  min_ent=...  flag_min=i)
  )
)
```

### Step 2D: 计算钢管路径距离

**单段钢管**（flag_min=0，出口在 start_ent 上）：

```lisp
;; onept = 摄像机在 start_ent 上的投影点
;; finalpt = 出口点(min_pts)在 start_ent 上的投影点
;; dd_dist = 沿钢管曲线的距离
(setq single_cctv_dist 
  (+ (distance onept @p1)           ;; 摄像机→钢管投影点
     (distance min_pts finalpt)      ;; 线槽出口→钢管上出口投影点
     (dd_dist start_ent onept finalpt)  ;; 沿钢管曲线距离
  )
)
;; 画线: LINE onept → @p1 (摄像机→钢管)
;; 画线: LINE min_pts → finalpt (线槽出口→钢管出口)
```

**多段钢管**（flag_min≠0，出口在另一段钢管上）：

```lisp
;; onept = 摄像机在第一段钢管上的投影点
;; twopt = 第一段钢管端点在第二段钢管上的投影点（连接点）
;; finalpt = 出口点在第二段钢管上的投影点
(setq single_cctv_dist
  (+ (distance onept @p1)                    ;; 摄像机→钢管1投影点
     (distance min_pts finalpt)               ;; 线槽出口→钢管2出口投影点
     (dd_dist ent[0] onept twopt)             ;; 沿钢管1距离
     (dd_dist ent[flag_min] twopt finalpt)    ;; 沿钢管2距离
  )
)
```

---

## `sub1_tt1` + `sub1_main_p1` — Dijkstra 最短路径（L96-277）

原始代码**没有使用 Floyd-Warshall**，而是对每对 (摄像机投影点, 接线盒投影点) 运行 **Dijkstra 算法**：

```lisp
;; sub1_tt1 调用 sub1_main_p1
(setq ss1 (sub1_main_p1 pt1 pt2 t))
(setq gjx_cctv_dis (car ss1))  ;; 最短路径长度
```

`sub1_main_p1` 是一个基于 VLA 对象的 Dijkstra 实现：
- 从 pt1 附近的 LINE 开始 BFS
- 沿相连 LINE 扩展
- 到达 pt2 附近的 LINE 时返回最短路径长度
- **每次摄像机-接线盒对都重新运行 Dijkstra**

---

## 与当前重构代码的关键差异

| 方面 | 原始代码 | 当前重构代码 |
|------|---------|------------|
| **图算法** | 每对(摄像机,接线盒)运行 Dijkstra | Floyd-Warshall 预计算 + O(1)查表 |
| **钢管路径** | zoom+ssget 扩展最多3段钢管，沿曲线计算距离 | Dijkstra 在钢管子图上寻路 |
| **钢管曲线距离** | `dd_dist` 用 `vlax-curve-getDistAtPoint` 沿曲线计算 | 只用端点间直线距离 |
| **钢管→线槽出口** | `block_dist` 搜索最近线槽点（无距离限制） | `pipe-find-nearest-tray-node` 限制1500mm |
| **摄像机→钢管投影** | `vlax-curve-getClosestPointTo` 支持曲线 | `device-find-nearest-on-layer` 只搜索LINE |
| **接线盒选择** | 只比较前3个最近接线盒 | 遍历所有接线盒 |
| **钢管实体类型** | LWPOLYLINE + LINE | 仅 LINE |

---

## 当前重构代码需要修正的问题

### 问题1: 钢管只支持 LINE，不支持 LWPOLYLINE

原始代码 `sxj_dist` 搜索 `(cons 0 "LWPOLYLINE,LINE")`，而我们的 `device-find-nearest-on-layer` 只搜索 LINE。

**影响**：如果钢管层有 LWPOLYLINE（多段线），会被遗漏。

### 问题2: 钢管曲线距离用直线近似

原始代码用 `dd_dist`（`vlax-curve-getDistAtPoint`）沿钢管曲线计算真实距离。我们的钢管子图只用端点间直线距离。

**影响**：如果钢管有弯折（LWPOLYLINE），距离计算不准确。

### 问题3: 钢管扩展只限3段，我们的 Dijkstra 无限制

原始代码最多扩展3段钢管（start_ent + 2个相邻），我们的 `pipe-dijkstra` 在整个钢管子图上寻路，没有段数限制。

**影响**：如果钢管网络很复杂，Dijkstra 可能找到很长的绕行路径。但这是更正确的做法。

### 问题4: 钢管→线槽出口距离无限制 vs 1500mm

原始代码 `block_dist` 搜索最近线槽点时**没有距离限制**（搜索半径 var=250000，即500m×500m区域）。我们的 `pipe-find-nearest-tray-node` 限制1500mm。

**影响**：如果钢管端点距离线槽超过1500mm，我们的代码会找不到出口，而原始代码可以。

### 问题5: 原始代码对每对(摄像机,接线盒)运行 Dijkstra

原始代码的 `sub1_tt1` 对每个摄像机的前3个接线盒各运行一次 Dijkstra。这是 O(C×3×E×logV)。我们的 Floyd + 查表是 O(V³) 预计算 + O(C×J) 查表。

**影响**：我们的方案在接线盒多时更高效，但 Floyd 预计算在节点多时更慢。

---

## 建议修正方向

1. **钢管搜索支持 LWPOLYLINE** — `device-find-nearest-on-layer` 增加对 LWPOLYLINE 的支持
2. **钢管→线槽出口距离放宽** — `pipe-find-nearest-tray-node` 搜索半径从1500mm放宽，或去掉限制
3. **钢管曲线距离** — 钢管子图建图时，对 LWPOLYLINE 用 `vlax-curve-getDistAtPoint` 计算沿曲线距离
4. **保持 Dijkstra 钢管寻路** — 比原始的3段限制更正确，不需要改
