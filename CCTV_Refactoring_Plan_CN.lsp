;;;===============================================================
;;;                 CCTV系统重构方案文档
;;;===============================================================
;;; 项目名称: CCTV自动布线与系统图生成器
;;; 源文件: drawCCTV(界面版)2.lsp + graph_algorithm_module.lsp
;;; 目标: 模块化、ANSI编码、无中文的LISP代码
;;;===============================================================

;;;===============================================================
;;; 第一部分: 关键约束
;;;===============================================================

;;; [C1] 编码格式
;;;     - 所有代码文件必须使用ANSI编码保存（不是UTF-8）
;;;     - 代码中仅使用ASCII字符
;;;     - 注释仅使用英文
;;;     - 字符串字面量仅使用英文（中文输出文本使用常量定义）
;;;     - 原因: UTF-8编码在AutoCAD LISP中会导致乱码

;;; [C2] 禁止中文字符
;;;     - 注释中不能有中文
;;;     - 字符串字面量中不能有中文
;;;     - 需要输出的中文文本使用预定义常量
;;;     - 示例: (setq *text-camera-cable* "摄像机光电缆") ; 按需显示

;;; [C3] 模块化设计
;;;     - 每个模块独立文件
;;;     - 清晰的依赖链
;;;     - 最小化全局变量
;;;     - 每个模块有自己的测试函数

;;; [C4] 算法升级
;;;     - 用Floyd-Warshall算法替代BFS
;;;     - 使用规范的图数据结构
;;;     - 预计算所有最短路径

;;; [C5] 移除的功能
;;;     - 半定型图绘制（draw_BDXrec, draw_GArec等）
;;;     - M05中的圆弧/圆打断
;;;     - 冗余函数（sxj_dist_text1等）

;;;===============================================================
;;; 第二部分: 模块规格说明
;;;===============================================================

;;;---------------------------------------------------------------
;;; M01 - graph_algorithm.lsp (核心模块)
;;;---------------------------------------------------------------
;;; 功能: 图数据结构与最短路径算法
;;; 依赖: 无
;;;
;;; 全局变量:
;;;   *graph-nodes*      - 关联表: ((key . (index point)) ...)
;;;   *graph-edges*      - 关联表: (((nodeA . nodeB) . weight) ...)
;;;   *graph-adj*        - 邻接表: ((node . ((neighbor . weight) ...)) ...)
;;;   *graph-dist*       - 距离矩阵（Floyd-Warshall计算后）
;;;   *graph-node-count* - 节点数量
;;;
;;; 公开函数:
;;;   graph-init                    - 初始化/重置图
;;;   graph-build-from-lines        - 从线段选择集构建图
;;;   graph-floyd-compute           - 执行Floyd-Warshall算法
;;;   graph-get-distance            - 获取两节点间最短距离
;;;   graph-find-nearest-edge       - 查找距离点最近的边
;;;   graph-project-point           - 将点投影到图作为新节点
;;;   graph-assign-devices          - 摄像机与光交箱匹配
;;;
;;; 内部函数:
;;;   graph-coord->key              - 坐标转字符串键
;;;   graph-add-node                - 添加节点，返回索引
;;;   graph-add-edge                - 添加带权重的边
;;;   graph-get-line-points         - 从线实体提取端点
;;;
;;; 测试函数: test-M01-graph-algorithm

;;;---------------------------------------------------------------
;;; M02 - line_utils.lsp
;;;---------------------------------------------------------------
;;; 功能: 线段实体工具函数
;;; 依赖: 无
;;;
;;; 公开函数:
;;;   line-get-endpoints            - 获取LINE起点和终点
;;;   line-get-length               - 获取线段长度
;;;   line-get-points-list          - 获取所有顶点（LINE为2个点）
;;;   lines-get-intersection        - 获取两线交点
;;;   line-point-at-distance        - 获取距起点指定距离的点
;;;   line-get-closest-point        - 获取线上距离给定点最近的点
;;;
;;; 内部函数:
;;;   line-point-on-line-p          - 判断点是否在线段上
;;;
;;; 测试函数: test-M02-line-utils

;;;---------------------------------------------------------------
;;; M03 - mline_converter.lsp
;;;---------------------------------------------------------------
;;; 功能: 将MLINE转换为LINE实体
;;; 依赖: M02
;;;
;;; 公开函数:
;;;   mline-convert-to-lines        - 主转换函数
;;;   mline-get-vertices            - 从MLINE提取顶点
;;;   mline-connect-nearby          - 连接阈值范围内的端点
;;;
;;; 参数:
;;;   *mline-connect-threshold*     - 默认值: 1400 (mm)
;;;
;;; 测试函数: test-M03-mline-converter

;;;---------------------------------------------------------------
;;; M04 - duplicate_remover.lsp
;;;---------------------------------------------------------------
;;; 功能: 消除重复和重叠的线段
;;; 依赖: M02
;;;
;;; 公开函数:
;;;   dup-remove-all                - 主入口: 消除所有重复
;;;   dup-merge-colinear            - 合并重叠的共线线段
;;;   dup-remove-identical          - 删除完全相同的线段
;;;
;;; 内部函数:
;;;   dup-line-get-key              - 获取线段排序键（斜率、截距）
;;;   dup-lines-overlap-p           - 判断两线段是否重叠
;;;   dup-merge-two-lines           - 合并两条重叠线段
;;;
;;; 测试函数: test-M04-duplicate-remover

;;;---------------------------------------------------------------
;;; M05 - break_lines.lsp (简化版)
;;;---------------------------------------------------------------
;;; 功能: 在交叉点打断LINE实体
;;; 依赖: M02
;;;
;;; 注意: 本模块仅处理LINE实体，不支持ARC/CIRCLE/SPLINE
;;;
;;; 公开函数:
;;;   break-lines-all               - 在所有交叉点打断线段
;;;   break-lines-in-set            - 打断选择集中的线段
;;;   break-line-at-point           - 在指定点打断单条线
;;;
;;; 内部函数:
;;;   break-collect-intersections   - 收集所有交叉点
;;;   break-sort-points-by-distance - 按距离排序打断点
;;;   break-create-segments         - 从打断点创建线段
;;;
;;; 算法流程:
;;;   1. 对每条线，找出与其他所有线的交叉点
;;;   2. 按距线起点的距离排序这些点
;;;   3. 删除原线
;;;   4. 在相邻点之间创建新线段
;;;
;;; 测试函数: test-M05-break-lines

;;;---------------------------------------------------------------
;;; M06 - block_utils.lsp
;;;---------------------------------------------------------------
;;; 功能: 图块相关工具
;;; 依赖: 无
;;;
;;; 公开函数:
;;;   block-get-base-point          - 获取图块中心点
;;;   block-get-name                - 从附近文字获取图块名称
;;;   block-get-layer               - 获取图块所在图层
;;;   block-get-insertion           - 获取图块插入点
;;;
;;; 内部函数:
;;;   block-find-largest-entity     - 查找图块中最大的非文字实体
;;;   block-find-nearest-text       - 查找最近的文字实体
;;;   block-transform-point         - 对点应用图块变换
;;;
;;; 参数:
;;;   *block-name-search-radius*    - 默认值: 3000 (mm)
;;;
;;; 测试函数: test-M06-block-utils

;;;---------------------------------------------------------------
;;; M07 - device_projection.lsp
;;;---------------------------------------------------------------
;;; 功能: 将设备投影到图网络
;;; 依赖: M01, M02, M06
;;;
;;; 公开函数:
;;;   device-project-to-graph       - 将设备点投影到最近的图边
;;;   device-get-distance           - 计算设备到图的距离
;;;   device-find-route             - 查找设备到目标节点的路径
;;;
;;; 参数:
;;;   *device-search-radius*        - 默认值: 1500 (mm)
;;;   *pipe-layer-name*             - 管道路由图层（可选）
;;;
;;; 测试函数: test-M07-device-projection

;;;---------------------------------------------------------------
;;; M08 - equivalent_points.lsp
;;;---------------------------------------------------------------
;;; 功能: 处理等效连通点
;;; 依赖: M01, M07
;;;
;;; 公开函数:
;;;   equiv-add-pair                - 添加等效点对
;;;   equiv-process-all             - 处理所有点对
;;;   equiv-clear                   - 清除所有点对
;;;
;;; 内部函数:
;;;   equiv-connect-pair            - 将一对点连接到图
;;;
;;; 数据结构:
;;;   *equiv-pairs* - 列表，元素为 ((pt1 pt2) id)
;;;
;;; 测试函数: test-M08-equivalent-points

;;;---------------------------------------------------------------
;;; M09 - system_diagram.lsp
;;;---------------------------------------------------------------
;;; 功能: 生成系统图
;;; 依赖: M06
;;;
;;; 公开函数:
;;;   sysdiag-draw                  - 主绘制函数
;;;   sysdiag-set-insertion-point   - 设置系统图插入点
;;;   sysdiag-group-by-junction     - 按光交箱分组设备
;;;   sysdiag-sort-devices          - 按名称编号排序设备
;;;
;;; 内部函数:
;;;   sysdiag-draw-cable-line       - 绘制线缆
;;;   sysdiag-insert-device-block   - 插入设备图块
;;;   sysdiag-add-label             - 添加文字标注
;;;   sysdiag-draw-junction-box     - 绘制光交箱外框
;;;
;;; 参数:
;;;   *sysdiag-cable-length-unit*   - 默认值: 1000 (mm/单位)
;;;   *sysdiag-row-spacing*         - 默认值: 700 (mm)
;;;   *sysdiag-text-height*         - 默认值: 250 (mm)
;;;
;;; 测试函数: test-M09-system-diagram

;;;---------------------------------------------------------------
;;; M10 - parameter_io.lsp
;;;---------------------------------------------------------------
;;; 功能: 读写配置文件
;;; 依赖: 无
;;;
;;; 公开函数:
;;;   param-save                    - 保存参数到文件
;;;   param-load                    - 从文件加载参数
;;;   param-set-defaults            - 设置默认值
;;;
;;; 内部函数:
;;;   param-encode-list             - 列表编码为字符串
;;;   param-decode-list             - 字符串解码为列表
;;;
;;; 文件格式:
;;;   - 每行一个参数
;;;   - 格式: param_name=value1,value2,value3
;;;
;;; 测试函数: test-M10-parameter-io

;;;---------------------------------------------------------------
;;; M11 - gui_handlers.lsp
;;;---------------------------------------------------------------
;;; 功能: OpenDCL事件处理
;;; 依赖: M01-M10
;;;
;;; 公开函数:
;;;   gui-init                      - 初始化GUI
;;;   gui-show                      - 显示主窗体
;;;   gui-hide                      - 隐藏主窗体
;;;
;;; 事件处理器:
;;;   c:drawCCTV/Form1/TextButton16#OnClicked  - 选择摄像机图块
;;;   c:drawCCTV/Form1/TextButton17#OnClicked  - 移除摄像机图块
;;;   c:drawCCTV/Form1/TextButton2#OnClicked   - 选择名称图层
;;;   c:drawCCTV/Form1/TextButton9#OnClicked   - 选择光交箱图块
;;;   c:drawCCTV/Form1/TextButton3#OnClicked   - 选择电缆槽图层
;;;   c:drawCCTV/Form1/TextButton18#OnClicked  - 选择管道图层
;;;   c:drawCCTV/Form1/TextButton13#OnClicked  - 添加等效点
;;;   c:drawCCTV/Form1/TextButton12#OnClicked  - 设置机房引入点
;;;   c:drawCCTV/Form1/TextButton14#OnClicked  - 选择电缆槽区域
;;;   c:drawCCTV/Form1/TextButton15#OnClicked  - 选择摄像机区域
;;;   c:drawCCTV/Form1/TextButton5#OnClicked   - 选择光交箱区域
;;;   c:drawCCTV/Form1/TextButton21#OnClicked  - 执行主流程
;;;   c:drawCCTV/Form1/TextButton19#OnClicked  - 保存参数
;;;   c:drawCCTV/Form1/TextButton20#OnClicked  - 加载参数
;;;   c:drawCCTV/Form1/TextButton41#OnClicked  - 关闭窗体
;;;
;;; 测试函数: test-M11-gui-handlers（需在AutoCAD中手动测试）

;;;---------------------------------------------------------------
;;; M12 - main.lsp
;;;---------------------------------------------------------------
;;; 功能: 主入口与工作流协调
;;; 依赖: M01-M11
;;;
;;; 公开函数:
;;;   c:drawCCTV                    - 主命令入口
;;;   main-run-workflow             - 执行主工作流
;;;   main-run-junction-workflow    - 执行光交箱工作流
;;;
;;; 内部函数:
;;;   main-init                     - 初始化全局变量
;;;   main-cleanup                  - 清理临时实体
;;;   main-create-temp-layers       - 创建临时图层
;;;   main-process-cable-trays      - 处理电缆槽MLINE
;;;   main-process-devices          - 处理所有设备
;;;   main-generate-diagram         - 生成系统图
;;;
;;; 全局变量:
;;;   *main-camera-blocks*          - 摄像机图块名称列表
;;;   *main-camera-name-layers*     - 摄像机名称文字图层
;;;   *main-junction-blocks*        - 光交箱图块名称列表
;;;   *main-cable-tray-layer*       - 电缆槽图层名称
;;;   *main-pipe-layer*             - 管道图层名称（可选）
;;;   *main-room-points*            - 机房引入点列表
;;;   *main-equiv-points*           - 等效点对列表
;;;   *main-cable-coefficient*      - 线缆长度系数 (1.2)
;;;   *main-junction-bias*          - 光交箱距离偏置 (10000)
;;;   *main-room-bias*              - 机房引入距离偏置
;;;
;;; 工作流程:
;;;   1. 初始化并创建临时图层
;;;   2. 将MLINE转换为LINE
;;;   3. 消除重复线段
;;;   4. 在交叉点打断线段
;;;   5. 从线段构建图
;;;   6. 执行Floyd-Warshall算法
;;;   7. 处理等效点
;;;   8. 对每个摄像机:
;;;      a. 投影到图
;;;      b. 查找最近的光交箱
;;;      c. 计算最短路径
;;;      d. 选择最优光交箱
;;;   9. 按光交箱分组摄像机
;;;   10. 生成系统图
;;;   11. 清理
;;;
;;; 测试函数: test-M12-main

;;;---------------------------------------------------------------
;;; M13 - test_suite.lsp
;;;---------------------------------------------------------------
;;; 功能: 综合测试套件
;;; 依赖: M01-M12
;;;
;;; 公开函数:
;;;   test-all                      - 运行所有测试
;;;   test-module                   - 运行指定模块测试
;;;   test-report                   - 生成测试报告
;;;
;;; 内部函数:
;;;   test-assert-equal             - 断言相等
;;;   test-assert-true              - 断言为真
;;;   test-assert-not-nil           - 断言非空
;;;   test-log                      - 记录测试结果
;;;
;;; 测试输出格式:
;;;   [PASS] test-name - description
;;;   [FAIL] test-name - description : expected X got Y
;;;   Test Summary: N passed, M failed

;;;===============================================================
;;; 第三部分: 数据结构
;;;===============================================================

;;;---------------------------------------------------------------
;;; 图节点
;;;---------------------------------------------------------------
;;; 键: 字符串 "x,y"（坐标，6位小数精度）
;;; 值: (index point)
;;;   index: 整数，节点在图中的索引
;;;   point: (x y z) 坐标列表

;;;---------------------------------------------------------------
;;; 图边
;;;---------------------------------------------------------------
;;; 键: (nodeA . nodeB) - 节点索引的点对
;;; 值: 浮点数 - 边权重（距离）

;;;---------------------------------------------------------------
;;; 设备信息
;;;---------------------------------------------------------------
;;; 结构: (distance block-name device-name junction-name)
;;;   distance: 浮点数 - 加权总距离
;;;   block-name: 字符串 - 图块定义名
;;;   device-name: 字符串 - 从文字获取的设备名
;;;   junction-name: 字符串 - 连接的光交箱名

;;;---------------------------------------------------------------
;;; 等效点对
;;;---------------------------------------------------------------
;;; 结构: ((pt1 pt2) id)
;;;   pt1, pt2: (x y z) - 等效点坐标
;;;   id: 整数 - 点对标识

;;;===============================================================
;;; 第四部分: 优化说明
;;;===============================================================

;;; [O1] 图构建优化
;;;   - 原方案: 每次路径查询执行BFS搜索
;;;   - 优化后: Floyd-Warshall预计算所有路径
;;;   - 收益: 预计算后查询复杂度O(1)

;;; [O2] 线段打断优化
;;;   - 原方案: 在所有交叉点打断，包括圆弧
;;;   - 优化后: 仅打断LINE实体
;;;   - 收益: 代码更简单，执行更快

;;; [O3] 重复消除优化
;;;   - 原方案: 按斜率/截距排序后合并
;;;   - 优化后: 使用空间哈希进行初始分组
;;;   - 收益: 减少比较次数

;;; [O4] 设备匹配优化
;;;   - 原方案: 检查最近的3个光交箱
;;;   - 优化后: 使用预计算距离，检查全部
;;;   - 收益: 保证最优匹配

;;; [O5] 全局变量优化
;;;   - 原方案: 大量分散的全局变量
;;;   - 优化后: 分组管理，最小化作用域
;;;   - 收益: 更易调试，减少副作用

;;;===============================================================
;;; 第五部分: 文件加载顺序
;;;===============================================================

;;; 在AutoCAD或ACAD.LSP中按以下顺序加载:
;;;
;;; (load "M01_graph_algorithm.lsp")
;;; (load "M02_line_utils.lsp")
;;; (load "M03_mline_converter.lsp")
;;; (load "M04_duplicate_remover.lsp")
;;; (load "M05_break_lines.lsp")
;;; (load "M06_block_utils.lsp")
;;; (load "M07_device_projection.lsp")
;;; (load "M08_equivalent_points.lsp")
;;; (load "M09_system_diagram.lsp")
;;; (load "M10_parameter_io.lsp")
;;; (load "M11_gui_handlers.lsp")
;;; (load "M12_main.lsp")
;;; (load "M13_test_suite.lsp") ; 可选，仅测试时需要

;;;===============================================================
;;; 第六部分: 中文文本常量定义
;;;===============================================================

;;; 为避免编码问题，中文输出文本使用以下常量定义
;;; 在main.lsp或单独的配置文件中设置

;;; 示例:
;;; (setq *text-camera-cable* "摄像机光电缆")
;;; (setq *text-junction-cable* "汇聚箱线缆")
;;; (setq *text-room-entry* "机房引入点")
;;; (setq *text-meter* "m")

;;;===============================================================
;;; 第七部分: 实现优先级
;;;===============================================================

;;; 第一阶段（核心）:
;;;   M01 - graph_algorithm.lsp
;;;   M02 - line_utils.lsp
;;;
;;; 第二阶段（几何处理）:
;;;   M03 - mline_converter.lsp
;;;   M04 - duplicate_remover.lsp
;;;   M05 - break_lines.lsp
;;;
;;; 第三阶段（工具）:
;;;   M06 - block_utils.lsp
;;;   M10 - parameter_io.lsp
;;;
;;; 第四阶段（应用）:
;;;   M07 - device_projection.lsp
;;;   M08 - equivalent_points.lsp
;;;   M09 - system_diagram.lsp
;;;
;;; 第五阶段（集成）:
;;;   M12 - main.lsp
;;;   M11 - gui_handlers.lsp
;;;
;;; 第六阶段（验证）:
;;;   M13 - test_suite.lsp

;;;===============================================================
;;; 第八部分: 与原文件对照表
;;;===============================================================

;;; +-------------------+------------------------------------------+
;;; | 新模块            | 原文件对应函数                           |
;;; +-------------------+------------------------------------------+
;;; | M01               | graph_algorithm_module.lsp (全部)        |
;;; |                   | sub1_main_p1 (替代)                      |
;;; +-------------------+------------------------------------------+
;;; | M02               | graph-get-line-points                    |
;;; |                   | graph-get-polyline-points                |
;;; |                   | graph-get-arc-points                     |
;;; +-------------------+------------------------------------------+
;;; | M03               | sub1_mlpoint                             |
;;; +-------------------+------------------------------------------+
;;; | M04               | sx, hbzhx, hb_line, hb_arc               |
;;; |                   | line_data, arc_data                      |
;;; +-------------------+------------------------------------------+
;;; | M05               | break_with, BreakAll, break_obj          |
;;; |                   | get_interpts (仅LINE部分)                |
;;; +-------------------+------------------------------------------+
;;; | M06               | block_base                               |
;;; |                   | sub1_block_name                          |
;;; |                   | sub1_block_name_gjx                      |
;;; |                   | sub1_bxform2d                            |
;;; +-------------------+------------------------------------------+
;;; | M07               | block_dist, sxj_dist                     |
;;; |                   | graph-project-device-to-graph            |
;;; +-------------------+------------------------------------------+
;;; | M08               | dxd_dist, break_line, join_line          |
;;; +-------------------+------------------------------------------+
;;; | M09               | sub1_draw_sys                            |
;;; |                   | sub1_insert_block                        |
;;; |                   | sub1_fenlei                              |
;;; +-------------------+------------------------------------------+
;;; | M10               | create-parameter-file                    |
;;; |                   | read-parameter-file                      |
;;; |                   | split-string, hc-string                  |
;;; +-------------------+------------------------------------------+
;;; | M11               | 所有 c:drawCCTV/Form1/... 函数           |
;;; +-------------------+------------------------------------------+
;;; | M12               | drawCCTV, drawHJX                        |
;;; |                   | Berni_Start, Berni_End                   |
;;; |                   | gbtc, dktc, clean_creen                  |
;;; +-------------------+------------------------------------------+
;;; | 已移除            | draw_BDXrec (半定型图)                   |
;;; |                   | draw_GArec, draw_CKSrec                  |
;;; |                   | draw_PWrec, draw_JSQrec                  |
;;; |                   | draw_ISCSrec, draw_CWZ                   |
;;; |                   | sxj_dist_text1 (冗余)                    |
;;; +-------------------+------------------------------------------+

;;;===============================================================
;;; 文档结束
;;;===============================================================
