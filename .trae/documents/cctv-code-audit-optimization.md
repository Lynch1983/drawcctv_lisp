# CCTV AutoLISP 代码审计与优化计划

## 审计总结

对 M00-M13 全部模块进行代码审计，发现 **2个关键性能问题**、**3个一般性能问题**、**3个代码质量问题**。

---

## 当前状态分析

### 关键性能问题

#### ISSUE-A1: M01 `graph-update-matrix` 使 Floyd-Warshall 变为 O(n^4)

**位置**: [M01_graph_algorithm.lsp:418-440](file:///workspace/M01_graph_algorithm.lsp#L418-L440)

**问题**: `graph-update-matrix` 每次更新单个矩阵单元时，重建整个 n×n 矩阵（O(n²)）。Floyd-Warshall 主循环调用此函数最多 n³ 次，总复杂度 O(n⁴)。

**影响**: 100 节点图：100⁴ = 1亿次操作 vs 正确的 100³ = 100万次。**100倍性能差距**。

**修复方案**: 只重建受影响的行，而非整个矩阵。将 `graph-update-matrix` 从 O(n²) 降为 O(n)。

```lisp
;; 修复前: O(n^2) - 重建整个矩阵
(defun graph-update-matrix (row col val / i j result row-data)
  (setq result nil)
  (setq i 0)
  (foreach r *graph-dist*          ;; 遍历所有行
    (if (= i row)
      (progn
        (setq row-data nil)
        (setq j 0)
        (foreach c r               ;; 遍历所有列
          ...))))
  (setq *graph-dist* (reverse result)))

;; 修复后: O(n) - 只重建受影响的行
(defun graph-update-matrix (row col val / i new-row)
  (setq i 0)
  (setq new-row (mapcar '(lambda (c) (if (= i col) (progn (setq i (1+ i)) val) (progn (setq i (1+ i)) c))) (nth row *graph-dist*)))
  (setq i 0)
  (setq *graph-dist* (mapcar '(lambda (r) (if (= i row) (progn (setq i (1+ i)) new-row) (progn (setq i (1+ i)) r))) *graph-dist*)))
```

#### ISSUE-A2: M09 `sysdiag-classify-by-junction` O(n²) 分类算法

**位置**: [M09_system_diagram.lsp:60-105](file:///workspace/M09_system_diagram.lsp#L60-L105)

**问题**: 内层循环使用 `vl-remove`（O(n)）从列表中删除元素，外层 while 循环直到列表为空。总复杂度 O(n²)。此外，对每个元素使用 `vl-catch-all-apply` 包装 `nth` 调用是不必要的开销。

**修复方案**: 使用关联列表（hash-like）按 junction name 分组，O(n) 一次遍历完成。

```lisp
;; 修复后: O(n) 分组
(defun sysdiag-classify-by-junction (lst / groups jnx-name entry)
  (if (null lst)
    nil
    (progn
      (setq groups nil)
      (foreach item lst
        (setq jnx-name (nth 3 item))
        (setq entry (assoc jnx-name groups))
        (if entry
          (setq groups (subst (cons jnx-name (cons item (cdr entry))) entry groups))
          (setq groups (cons (list jnx-name item) groups))
        )
      )
      (mapcar 'cdr groups)
    )
  )
)
```

### 一般性能问题

#### ISSUE-B1: M12 branch1/2/3 使用 `device-project-to-graph` 产生无意义副作用

**位置**: [M12_main.lsp:438,490,519,571](file:///workspace/M12_main.lsp#L438)

**问题**: 三个分支函数调用 `device-project-to-graph`，该函数内部调用 `graph-add-node` 添加图节点。但这些节点是临时的——后续 `graph-build-from-lines` 调用 `graph-init` 会清空所有图数据。分支代码只使用返回值的 `(cadr ...)` 和 `(caddr ...)`，即投影点和距离，与 `device-find-nearest-line` 返回值的第2、3元素相同。

**修复方案**: 将 `device-project-to-graph` 替换为 `device-find-nearest-line`，消除 `graph-add-node` 的无意义调用。

**影响**: 每个 junction/room 点少一次 `graph-add-node` 调用。如果有 50 个 junction + 10 个 room 点，减少 60 次无用 assoc 查找和 cons 操作。

#### ISSUE-B2: M12 `main-cleanup-temp-layer` 逐个删除实体

**位置**: [M12_main.lsp:198-228](file:///workspace/M12_main.lsp#L198-L228)

**问题**: 使用 `command-s "_.erase" tmp ""` 逐个删除实体，每次调用 `command-s` 都有解释器开销。

**修复方案**: 改用 `entdel` 批量删除（`entdel` 是纯 AutoLISP 函数，比 `command-s` 快得多）。

```lisp
;; 修复后
(foreach ent (vl-remove-if 'null (mapcar '(lambda (i) (ssname clean-ss i))
  (vl-remove-if-not '(lambda (x) (< x (sslength clean-ss)))
    (read (strcat "(" (vl-prin1-to-string (sslength clean-ss)) ")")))))
  (entdel ent))
```

实际上更简洁的写法是直接遍历 ss：

```lisp
(setq i 0)
(repeat (sslength clean-ss)
  (entdel (ssname clean-ss i))
  (setq i (1+ i))
)
```

#### ISSUE-B3: M06 `block-get-base-point` 无缓存，重复计算

**位置**: [M06_block_utils.lsp:259-277](file:///workspace/M06_block_utils.lsp#L259-L277)

**问题**: 每次调用 `block-get-base-point` 都会调用 `block-find-largest-entity`，后者遍历块定义中的所有实体并计算面积。如果图纸中有 100 个相同类型的摄像头块，会重复计算 100 次相同的块定义。

**修复方案**: 添加 `*block-base-point-cache*` 缓存，按 `(block-name . scale-rotation-key)` 缓存 base point 偏移量。

```lisp
(setq *block-base-point-cache* nil)

(defun block-get-base-point (ent / block-name cache-key largest-ent center insertion scale rotation cached)
  (setq block-name (block-get-name ent))
  (if (null block-name)
    nil
    (progn
      (setq scale (block-get-scale ent))
      (setq rotation (block-get-rotation ent))
      (setq cache-key (strcat block-name "|" (rtos (car scale) 2 4) ","
                              (rtos (cadr scale) 2 4) ","
                              (rtos rotation 2 6)))
      (setq cached (assoc cache-key *block-base-point-cache*))
      (if cached
        (block-transform-point (cdr cached) (block-get-insertion-point ent) scale rotation)
        (progn
          (setq largest-ent (block-find-largest-entity block-name))
          (setq center (if largest-ent
                         (block-entity-get-center largest-ent)
                         (list 0.0 0.0 0.0)))
          (setq *block-base-point-cache*
            (cons (cons cache-key center) *block-base-point-cache*))
          (block-transform-point center (block-get-insertion-point ent) scale rotation)
        )
      )
    )
  )
)
```

### 代码质量问题

#### ISSUE-C1: M00 `sp-boxes-overlap-p` 使用 `t` 作为局部变量名

**位置**: [M00_spatial_index.lsp:125](file:///workspace/M00_spatial_index.lsp#L125)

**问题**: `(defun sp-boxes-overlap-p (box1 box2 tol / t)` 中 `t` 是 AutoLISP 的常量 `T` 的小写形式。虽然 AutoLISP 区分大小写，`t` 和 `T` 是不同的符号，但使用 `t` 作为变量名极易引起混淆，且在某些 AutoCAD 版本中可能有问题。

**修复**: 重命名为 `tol-val`。

#### ISSUE-C2: M06 `block-transform-point` 未声明局部变量

**位置**: [M06_block_utils.lsp:288-308](file:///workspace/M06_block_utils.lsp#L288-L308)

**问题**: `cos-r` 和 `sin-r` 未在局部变量列表中声明，会泄漏为全局变量。

**修复**: 添加到 `/` 后的局部变量列表。

#### ISSUE-C3: M01 `graph-get-distance` 未声明局部变量

**位置**: [M01_graph_algorithm.lsp:448-469](file:///workspace/M01_graph_algorithm.lsp#L448-L469)

**问题**: `d` 未在局部变量列表中声明（第 459 行 `(setq d (nth nodeB row))`），会泄漏为全局变量。

**修复**: 添加 `d` 到局部变量列表。

---

## 优化实施计划

### Step 1: 修复 M01 `graph-update-matrix` 性能 (ISSUE-A1)
- **文件**: M01_graph_algorithm.lsp
- **操作**: 重写 `graph-update-matrix`，只重建受影响的行
- **验证**: 运行 test-M01-graph-algorithm

### Step 2: 重写 M09 `sysdiag-classify-by-junction` (ISSUE-A2)
- **文件**: M09_system_diagram.lsp
- **操作**: 用 assoc-list 分组替换 O(n²) 算法
- **验证**: 运行 test-M09-system-diagram

### Step 3: 替换 M12 branch 中的 `device-project-to-graph` (ISSUE-B1)
- **文件**: M12_main.lsp
- **操作**: branch1/2/3 中 `device-project-to-graph` → `device-find-nearest-line`，调整返回值取值
- **验证**: 运行 test-M12（如有）

### Step 4: 优化 M12 `main-cleanup-temp-layer` (ISSUE-B2)
- **文件**: M12_main.lsp
- **操作**: `command-s "_.erase"` 循环 → `entdel` 循环
- **验证**: 功能测试

### Step 5: 添加 M06 block base point 缓存 (ISSUE-B3)
- **文件**: M06_block_utils.lsp
- **操作**: 添加 `*block-base-point-cache*` 和缓存逻辑
- **验证**: 运行 test-M06-block-utils

### Step 6: 修复代码质量问题 (ISSUE-C1/C2/C3)
- **文件**: M00_spatial_index.lsp, M06_block_utils.lsp, M01_graph_algorithm.lsp
- **操作**: 修复变量名和未声明局部变量
- **验证**: 语法检查

### Step 7: 同步更新 CCTV_AllInOne.lsp
- **文件**: CCTV_AllInOne.lsp
- **操作**: 将所有修改同步到合并文件
- **验证**: 括号平衡检查

---

## 优先级排序

| 优先级 | Issue | 影响 | 修复难度 |
|--------|-------|------|---------|
| P0 | A1: graph-update-matrix O(n^4) | 100倍性能差距 | 低 |
| P1 | A2: classify-by-junction O(n²) | 摄像头多时明显卡顿 | 低 |
| P1 | C3: graph-get-distance 变量泄漏 | 可能导致难以追踪的bug | 极低 |
| P2 | B1: device-project-to-graph 副作用 | 不必要的图节点创建 | 中 |
| P2 | C2: block-transform-point 变量泄漏 | 全局变量污染 | 极低 |
| P2 | C1: sp-boxes-overlap-p 变量名 | 可读性/兼容性 | 极低 |
| P3 | B2: main-cleanup-temp-layer | 清理速度 | 低 |
| P3 | B3: block-get-base-point 缓存 | 重复块多时有提升 | 中 |

## 假设与决策

- **不删除 M09 legacy 函数**：`sysdiag-draw`、`sysdiag-group-by-junction` 等标记为 legacy 的函数保留，避免破坏可能的用户自定义脚本
- **不修改 Floyd-Warshall 算法本身**：仅优化矩阵更新操作，不改变算法逻辑
- **缓存策略**：block base point 缓存不设过期机制，因为块定义在单次工作流中不会改变
- **ANSI 编码**：所有修改保持 ASCII only
