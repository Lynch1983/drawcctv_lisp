;;;子程序1：最短路径算法
;;;寻找连接两点的最近路线。前提 所有路线只在交点处交叉,起点和终点选择路线的端点.
;核心函数 (sub1_main_p1 起点 终点 是否显示搜索过程) ")
;返回值   (最短路线长度  最短路线途径实体表)")
;测试命令:tt\n")
;(defun z_timer (/ stime h m s)
;  (if (not zhf_time_dot)
;    (setq zhf_time_dot (getvar "date") h nil)
;    (progn
;      (setq stime (getvar "date"))
 ;     (setq stime (- stime zhf_time_dot))
 ;     (setq stime (* 86400.0 (- stime (fix stime))))
 ;     (setq h (fix (/ stime 3600)))
 ;     (setq m (fix (/ (- stime (* h 3600)) 60)))
 ;     (setq s (fix (- stime (* m 60) (* h 3600))))ml_p1
 ;     (setq zhf_time_dot nil)
 ;      
  ;    (strcat (if (> h 0)
 ;               (strcat (rtos h 2 0) "小时")""
 ;             )
 ;             (if (> m 0)
 ;               (strcat (rtos m 2 0) "分钟")""
 ;             )
 ;             (rtos s 2 0)
 ;             "秒"
;      )
;      ) 
;    ) 
;  )
;;;重画对象，如果stop对象暗显
(defun sub1_show (lst stop)      
  (mapcar '(lambda (x) (redraw (vlax-vla-object->ename x) 3))  ;;;3表示对象亮显
          lst
  )
  (if stop (progn(getpoint)
  (mapcar '(lambda (x) (redraw (vlax-vla-object->ename x) 4))   ;;;4表示对象暗显，2表示对象引出
          lst
  )))
)

;;;
(defun sub1_ss2lst (ss vla / re e)
  (if ss
    (repeat (setq n (sslength ss))
      (if vla
        (setq e (vlax-ename->vla-object (ssname ss (setq n (1- n)))))
        (setq e (ssname ss (setq n (1- n))))
      )
      (setq re (append re (list e)))
    )
  )
  re
)

;;;根据p点获取
(defun sub1_getss@ (p)
  (ssget "c"
         p
         (polar p (/ pi 4) (/ (getvar "viewsize") 5000))
         gllst    
  )
)
(defun sub1_getconnect (e)
  (vl-remove e
             (append (sub1_ss2lst (sub1_getss@ (vlax-curve-getStartpoint e)) t)
                     (sub1_ss2lst (sub1_getss@ (vlax-curve-getEndpoint e)) t)
             )
  )
)
(defun sub1_remove:same (lst / re)
  (foreach n lst
    (if        (member n re)
      ()
      (setq re (append re (list re)))
    )
  )
  re
)
(defun sub1_get:len (e)
	
	(if (= "test2" (cdr(assoc 8 (entget(vlax-vla-object->ename e)))))
  	    5000	    
  	;	(if (member (list (vlax-curve-getstartpoint e) (vlax-curve-getendpoint e)) dxd_ent_ptlst)
	 ; 		(- (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)) (cdr (assoc (list (vlax-curve-getstartpoint e) (vlax-curve-getendpoint e)) dxd_ent)))
  	;		(vlax-curve-getDistAtParam e (vlax-curve-getEndParam e))
	;	)
	  	
	  
	   (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e))
	)
)
;;;________________________________________________
;;;________________________________________________
;;;________________________________________________
;;;________________________________________________
(defun sub1_main_p1 (pt1 pt2 sub1_show / ss sse line path paths shortlen shortlst ss1 shortest)
  (setq count 0)
  (setq ss  (sub1_ss2lst (sub1_getss@ pt1) t)
        sse (sub1_ss2lst (sub1_getss@ pt2) t)
  )
  (if (and ss sse)
    (progn      
      (setq passed-ss ss
            path-ss   (mapcar '(lambda (x) (list x)) ss)
            dist-ss   (mapcar '(lambda (x) (list x (sub1_get:len x))) ss)
            dist-ss   (vl-sort dist-ss '(lambda (a b) (< (cadr a) (cadr b))))
            complete  nil
              
      )
      (mapcar '(lambda (x)
                 (if (member x sse)
                   (setq complete (append complete (list(list x (sub1_get:len x)))))
                 )
               )
              ss
      )
      (if complete
        (setq complete (vl-sort        complete
                                '(lambda (a b) (< (cadr a) (cadr b)))
                       )
              shortest (cadar complete)
        )
      )
      
      (if (and shortest (= shortest (distance pt1 pt2)))
        (progn
        (list (cadar complete) (list(caar complete)))
        )
        (progn
      (while (and dist-ss (> (length sse) (length complete)))
        (setq now     (car dist-ss)
              dist-ss (cdr dist-ss)
        )
        ;;;_____________________________
        ;;;_____________________________
        ;;;_____________________________
        (if sub1_show
          (progn
            (vlax-put (car now) 'color (+ 21 (* 10 (rem count 20))))
            (vla-update (car now))
          )
        )
        ;;;_____________________________
        ;;;_____________________________
        ;;;_____________________________
          (if (member (car now) sse)
            (progn
              (setq complete (append complete (list now)))
              ;;;__________________________________________________
              ;;;到达终点后剔出所有距离已经超出最小路由长度的未完成方向
              (setq complete
                     (vl-sort complete
                              '(lambda (a b) (< (cadr a) (cadr b)))
                     )
                    shortest (cadar complete)
                    dist-ss (mapcar '(lambda(x)(if (< (cadr x) shortest) x nil)) dist-ss)
                    dist-ss (vl-remove nil dist-ss)
              )
              ;;;__________________________________________________
              ;;;__________________________________________________
            )
            (progn
              (setq count (1+ count))
              (setq ss (sub1_getconnect (car now)))
              (mapcar '(lambda (x) (setq ss (vl-remove x ss)))
                      passed-ss
              )
              (setq passed-ss (append passed-ss ss)
                    path-ss   (append
                                path-ss
                                (mapcar '(lambda (x) (list x (car now))) ss)
                              )
                    dist-ss   (append
                                dist-ss
                                (mapcar
                                  '(lambda (x)
                                     (if (or (not shortest) (< (sub1_get:len x) shortest))(list x (+ (cadr now) (sub1_get:len x))))
                                   )
                                  ss
                                )
                              )
                    dist-ss (vl-remove nil dist-ss)
                    dist-ss   (vl-sort dist-ss
                                       '(lambda (a b) (< (cadr a) (cadr b)))
                              )
              )
          )
        )
        )
      ;;;_____________________________
      ;;;_____________________________
      ;;;_____________________________
        (if sub1_show
          (progn
            (mapcar '(lambda (x) (vlax-put x 'color 0)) passed-ss)
            (mapcar '(lambda (x) (vla-update x)) passed-ss)
          )
        )
      ;;;_____________________________
      ;;;_____________________________
      ;;;_____________________________
      (if complete
        (progn
          (setq
            complete (vl-sort complete
                              '(lambda (a b) (< (cadr a) (cadr b)))
                     )
            n (car complete)
            
          )          
          (setq        len (cadr n)
                n   (car n)
          )
          (while n
            (setq ss1 (append ss1 (list n)))
            (setq n (cadr (assoc n path-ss)))
          )          
          (list len (reverse ss1 ))
        )
        nil
      )
    )))
    nil
  )  
)
;;;________________________________________________
;;;________________________________________________
;;;________________________________________________
;;;________________________________________________
(defun sub1_tt1 (cctvpt gjxpt / pt1 pt2 ss2  complete zhf_time_dot cctv_length)
  ;(redraw)
  (setq pt1 cctvpt
        pt2 gjxpt
  )
  ;;;按照目前图形大小的1/40，在起点和终点画×
  ;(mapcar
    ';(lambda (pt)
       ;(grdraw (polar pt (* pi 0.25) (/ (getvar "viewsize") 40))
              ; (polar pt (* pi -0.75) (/ (getvar "viewsize") 40))
              ; 1
       ;)
       ;(grdraw (polar pt (* pi 0.75) (/ (getvar "viewsize") 40))
            ;   (polar pt (* pi -0.25) (/ (getvar "viewsize") 40))
            ;   1
      ; )
    ; )
   ; (list pt1 pt2)
 ; )
   (command-s "_zoom" "w" (mapcar '- ml_p1 '(10 10)) (mapcar '+ ml_p2 '(10 10)))
  (setq zhf_time_dot nil)
  ;(z_timer)
  (setq ss1 (sub1_main_p1 pt1 pt2 t))  
  (if ss1
    (progn
      (setq ss2 (ssadd))
      (mapcar '(lambda (x)
                 (setq ss2 (ssadd (vlax-vla-object->ename x) ss2))
               )
              (cadr ss1)
      )
      ;(princ (strcat "\n虚线显示最短路线, 共需" (itoa (sslength ss2)) "步,总长度为:"
                     ;(rtos (car ss1))
                     ;"  历时:"
                     ;(z_timer)
             ;)
      ;)
      (setq cctv_length (rtos (car ss1))) ;;;摄像机线缆长度
      (sub1_show (cadr ss1) nil)
      ;(setq ss-txtx (vl-remove nil (mapcar '(lambda (x) (vlax-vla-object->ename x)) (cadr ss1))))
      
    )
    ;(princ (strcat "\n两点间没有可连通路径,历时:" (z_timer)))
  )
  (setq gjx_cctv_dis (car ss1))
  (command-s "_u")
  (princ)
)


;;;;;;子程序2：1)根据多线绘制直线段
;;;;;;2)不同直线端点距离如果不超过1000mm，则认为其端点相连，增加直线连接
(defun sub1_mlpoint (mline_set / m n i var ent mline pts pts_fir pts_end line_pt_set xj_line line_tmp_fir line_tmp_end xjd &k1)
;(defun sub1_mlpoint (mline_set )
    (setvar "cmdecho" 0)
    (setvar "clayer" tmp_layer )
    (vl-load-com)
    (command-s "_zoom" "w" (mapcar '- ml_p1 '(10 10)) (mapcar '+ ml_p2 '(10 10)))
    (setq line_set (ssadd))
    (setq m 0 var 1400)  
    ;(setq mline_set (ssget (list (cons 0 "mline") (cons 8 wireway_layer))))
    (repeat (sslength mline_set)
      (setq mline (ssname mline_set m))
      (setq ent (entget mline))
      (setq pts nil pts_fir 0 pts_end 0 line_pt_set nil)
      (setq i 0 n 0)
      (repeat (length ent)
        (if (= (car (nth i ent)) 11)
          (setq pts (append pts (list (cdr (nth i ent)))))
        )
        (setq i (1+ i))
      )
      (if (/= pts null)
	(progn
	  (if (= (length pts) 2)
	    (progn (command-s "line" (nth 0 pts) (nth 1 pts) "") (setq line_set (ssadd (entlast) line_set))))
	  (if (> (length pts) 2)
	    (repeat (1- (length pts))
     	      (command-s "line" (nth n pts) (nth (1+ n) pts) "")
	      (setq line_set (ssadd (entlast) line_set))
              (setq n (1+ n))
	    )
	  )
        )
      )
      (setq m (1+ m))
     )
  	(breakobj)
  	(setq line_set (ssget "_c" (mapcar '- ml_p1 '(10 10)) (mapcar '+ ml_p2 '(10 10)) gllst))
    (setq m 0 i 0 xj_line nil)
    (repeat (sslength line_set)
      (setq line_tmp_fir (vlax-curve-getStartpoint (ssname line_set m)) i 0) 
      
       (setq xj_line (ssget "_c" (polar line_tmp_fir (* pi 0.75) var) (polar line_tmp_fir (* pi 1.75) var) gllst))
      
      (setq xj_line (ssdel (ssname line_set m) xj_line))
      (if xj_line
         (repeat (sslength xj_line)
	    (setq &k1 (ssname xj_line i))
	    (setq xjd (vlax-curve-getClosestPointTo &k1 (trans line_tmp_fir 1 0)))
	    (if (null (ACET-GEOM-INTERSECTWITH &k1 (ssname line_set m) 0))
	      (progn
		(command-s "line" line_tmp_fir xjd "")
		(if (= 0 (vlax-curve-getDistAtParam (entlast) (vlax-curve-getEndParam (entlast))))
		    ;(setq line_set (ssadd (entlast) line_set))
		    (command-s "erase" (entlast) "")
		  )
	      )
	    )
	    (setq i (1+ i))
         )
      )
      (setq m (1+ m) xj_line nil)
    )
    (setq m 0 i 0 xj_line nil)
    (repeat (sslength line_set)
      (setq line_tmp_end (vlax-curve-getEndpoint (ssname line_set m)) i 0)
      (setq xj_line (ssget "_c"
         (polar line_tmp_end (* pi 0.75) var)
         (polar line_tmp_end (* pi 1.75) var) 
         gllst    
      ))
      (if xj_line
         (repeat (sslength xj_line)
	    (setq &k1 (ssname xj_line i))
	    (setq xjd (vlax-curve-getClosestPointTo &k1 (trans line_tmp_end 1 0)))
	    (if (null (ACET-GEOM-INTERSECTWITH &k1 (ssname line_set m) 0))
	      (progn
		(command-s "line" line_tmp_end xjd "")
		(if (= 0 (vlax-curve-getDistAtParam (entlast) (vlax-curve-getEndParam (entlast))))
		    ;(setq line_set (ssadd (entlast) line_set))
		    (command-s "erase" (entlast) "")
		  )
	      )
	    )
	    (setq i (1+ i))
         )
      )
      (setq m (1+ m) xj_line nil)
    )
    (command-s "_u")
    (princ)
)



;消除重合线
;命令：SX
;移除几何上是多余的对象。例如：
;1.对象重复的副本将被删除。
;2.圆弧对象正好覆盖了圆的一部分，这个圆弧不能显现。此圆弧将被删除。
;3.两条直线其角度相同且部分重叠。这两条直线将合并为一条直线。
 
;;; *****消除重线 程序开始*****
(defun sx(/ n ss i ss1 ename elist etype);删线
   (setq ss (ssget "c" ml_p1 ml_p2 gllst))
   ;(setq ss line_set)
   (Berni_Start)
   ;(princ "\n★功能：删除重复的直线、圆、圆弧.")
  ;(setvar "clayer" tmp_layer)
   (cond
     (ss
       (setq n (sslength ss) i 0 ss1 ss ss (ssadd))
       (repeat n
       (setq ename (ssname ss1 i))
       (setq eList (entget ename))
       (setq eType (cdr (assoc 0 eList)))
       (cond
         ;((or (= eType "LINE") (= eType "ARC") (= eType "CIRCLE"))
	  ((= eType "LINE")
         (setq ss (ssadd ename ss))
          )
        )
       (setq i (1+ i))
        )
       (cond
        ((= (sslength ss) 0)
          (while
           (progn
             ;(setq ss (ssget '((0 . "LINE,ARC,CIRCLE"))))
	     (setq ss (ssget '((0 . "LINE"))))
              (not ss)
              )
          )
        )
       (T T)
       )
      )
      (T
       (while
         (progn
            ;(setq ss (ssget '((0 . "LINE,ARC,CIRCLE"))))
	   (setq ss (ssget '((0 . "LINE"))))
              (not ss)
            )
          )
         )
      )
     ;(princ "\n--->程序进行中，请稍候...")
     (hbzhx ss);合并

     ;(princ "\n消除重线完成！")

      (Berni_End)

  
     (vl-registry-delete "HKEY_CURRENT_USER\\Software\\Autodesk\\AutoCAD\\Yx_Zrw")
  	(princ)
    )
 
    ;合并重线
    (defun hbzhx(ss / precision i line_list arc_list ent obj e1 e2)
     (grtext -2 "正在整理数据")
      (setq precision 1e-8)
      (setq i 0
           line_list nil
           arc_list nil
      )
 
      (repeat (sslength ss)
        (setq ent (ssname ss i)
             i (1+ i)
        )
        (setq obj (vlax-ename->vla-object ent))
   
       (if (> (vlax-curve-getdistatparam obj (vlax-curve-getendparam obj)) precision);曲线长度大于精度precision
          (if (= "LINE" (cdr (assoc 0 (entget ent))))
                (setq line_list (cons (line_data ent) line_list))
              (setq arc_list (cons (arc_data ent) arc_list))
            )
          )
      )
 
      (setq line_list
         (vl-sort
           line_list
            '(lambda (e1 e2)
              (if (equal (car e1) (car e2) precision)
                (if (equal (cadr e1) (cadr e2) precision)
                 (if (equal (car (caddr e1)) (car (caddr e2)) precision);起点x坐标相等
                   (< (cadr (caddr e1)) (cadr (caddr e2)));斜率、截距、起点x坐标都相同时,按起点y坐标从小到大排序
                    (< (car (caddr e1)) (car (caddr e2)));斜率和截距都相同时,按起点x坐标从小到大排序
                     )
                   (< (cadr e1) (cadr e2));斜率相同时,再按截距从小到大排序
                 )
                  (< (car e1) (car e2));先按斜率从小到大排序
                )
              )
           )
      )
  
         半径    圆心x坐标    圆心y坐标    起点角    终点角    图元名
      (setq arc_list (vl-sort arc_list
                            '(lambda (e1 e2)
                               (if (equal (car e1) (car e2) precision);半径相等
                                 (if (equal (cadr e1) (cadr e2) precision);圆心x坐标相等
                                   (if (equal (caddr e1) (caddr e2) precision);圆心y坐标相等
                                     (< (cadddr e1) (cadddr e2));半径、圆心x坐标、圆心y坐标均相等时,按起点角从小到大排序
                                     (< (caddr e1) (caddr e2));半径、圆心x坐标相等时,按圆心y坐标从小到大排序
                                   )
                                   (< (cadr e1) (cadr e2));半径相等时,再按圆心x坐标从小到大排序
                                 )
                                 (< (car e1) (car e2));先按半径从小到大排序
                               )
                             )
                   )
      )
  
      (if line_list
          (hb_line line_list precision);合并直线
      )
      (if arc_list
          (progn
              (hb_arc arc_list precision);合并圆弧
          )
      )
  
      (grtext);使所有文本区域恢复为标准值
  )
  
  
  ;合并圆弧
  (defun hb_arc(arc_list precision / zongshu xuhao ssunnecessary 2pi arc_a biaoji bj pc sangl eangl ent arc_b sangl1 eangl1 ent1 tmplst arc_list_item ename obj)
      (setq zongshu (length arc_list)
          xuhao 0
          ssUnnecessary (ssadd)
          2pi (* 2 pi)
      )
      (princ (strcat "\n共处理了" (itoa zongshu) "个圆或圆弧图元"))
      (grtext -1 "合并圆弧")
  
      (while (> (length arc_list) 0)
          (cs_pross zongshu (setq xuhao (1+ xuhao)))
          (setq arc_a (car arc_list);a弧数据表
              arc_list (cdr arc_list)
              biaoji T
              bj (car arc_a)
              pc (list (cadr arc_a) (caddr arc_a));圆心
              sangl (cadddr arc_a)
              eangl (nth 4 arc_a)
              ent (last arc_a)
          )
          (while (and biaoji
                      (> (length arc_list) 0)
                 )
              (setq arc_b (car arc_list))
  
              (cond
                  ((and (equal bj (car arc_b) precision);同圆心同半径
                        (equal pc (list (cadr arc_b) (caddr arc_b)) precision)
                   )
  
                      (setq sangl1 (cadddr arc_b);b弧起点角
                          eangl1 (nth 4 arc_b);b弧终点角
                          ent1 (last arc_b)
                      )
  
                      (cond
                          ((= (get_dxf ent 0) "CIRCLE");a为圆
                              (setq tmpLst nil)
                              (foreach arc_list_item arc_list
                                  (cond
                                      ((and (equal bj (car arc_list_item) precision) (equal (car pc) (cadr arc_list_item) precision) (equal (cadr pc) (caddr arc_list_item) precision));同圆心等半径
                                          (setq ssUnnecessary (ssadd (last arc_list_item) ssUnnecessary))
                                          (cs_pross zongshu (setq xuhao (1+ xuhao)))
                                      )
                                      (T (setq tmpLst (append tmpLst (list arc_list_item))))
                                  )
                              )
                              (setq arc_list tmpLst)
                              (setq biaoji nil);结束内圈循环
                          )
                          ((= (get_dxf ent1 0) "CIRCLE");a为弧,且b为圆
                              (setq ssUnnecessary (ssadd ent ssUnnecessary))
                              (cs_pross zongshu (setq xuhao (1+ xuhao)))
                              (setq arc_list (cdr arc_list))
  
                              (setq tmpLst nil)
                              (foreach arc_list_item arc_list
                                  (cond
                                      ((and (equal bj (car arc_list_item) precision) (equal (car pc) (cadr arc_list_item) precision) (equal (cadr pc) (caddr arc_list_item) precision));同圆心等半径
                                          (setq ssUnnecessary (ssadd (last arc_list_item) ssUnnecessary))
                                          (cs_pross zongshu (setq xuhao (1+ xuhao)))
                                      )
                                      (T (setq tmpLst (append tmpLst (list arc_list_item))))
                                  )
                              )
                              (setq arc_list tmpLst)
                              (setq biaoji nil);结束内圈循环
                          )
                          ((and (= sangl eangl1) (= eangl sangl1));均为弧，且互补
                              (setq ename (entmakex (list '(0 . "CIRCLE") (list 10 (car pc) (cadr pc) 0) (cons 40 bj))))
                              (setq obj (Vlax-Ename->Vla-Object ename))
                              (Vlax-Put-Property obj 'Layer (cdr (assoc 8 (entget ent))) )
                              (Vlax-Put-Property obj 'Linetype (Vlax-Get (Vlax-Ename->Vla-Object ent) 'Linetype))
                              (Vlax-Put-Property obj 'LinetypeScale (Vlax-Get (Vlax-Ename->Vla-Object ent) 'LinetypeScale))
                              (Vlax-Put-Property obj 'Lineweight (Vlax-Get (Vlax-Ename->Vla-Object ent) 'Lineweight));线宽
                              (Vlax-Put-Property obj 'Color (Vlax-Get (Vlax-Ename->Vla-Object ent) 'Color))
                              (entdel ent)
  
                              (setq tmpLst nil)
                              (foreach arc_list_item arc_list
                                  (cond
                                      ((and (equal bj (car arc_list_item) precision) (equal (car pc) (cadr arc_list_item) precision) (equal (cadr pc) (caddr arc_list_item) precision));同圆心等半径
                                          (setq ssUnnecessary (ssadd (last arc_list_item) ssUnnecessary))
                                          (cs_pross zongshu (setq xuhao (1+ xuhao)))
                                     )
                                      (T
                                          (setq tmpLst (append tmpLst (list arc_list_item)))
                                      )
                                  )
                              )
                              (setq arc_list tmpLst)
                              (setq biaoji nil);结束内圈循环
                          )
                          ((and (BF-onArcP sangl1 sangl eangl) (BF-onArcP eangl1 sangl eangl) (>= (+ (/ (vlax-curve-getDistAtPoint (Vlax-Ename->Vla-Object ent) (polar (list (car pc) (cadr pc) 0) sangl1 bj)) bj) (Vlax-Get (Vlax-Ename->Vla-Object ent1) 'TotalAngle )) 2pi))
                              (setq ename (entmakex (list '(0 . "CIRCLE") (list 10 (car pc) (cadr pc) 0) (cons 40 bj))))
                              (setq obj (Vlax-Ename->Vla-Object ename))
                              (Vlax-Put-Property obj 'Layer (cdr (assoc 8 (entget ent))) )
                              (Vlax-Put-Property obj 'Linetype (Vlax-Get (Vlax-Ename->Vla-Object ent) 'Linetype))
                              (Vlax-Put-Property obj 'LinetypeScale (Vlax-Get (Vlax-Ename->Vla-Object ent) 'LinetypeScale))
                              (Vlax-Put-Property obj 'Lineweight (Vlax-Get (Vlax-Ename->Vla-Object ent) 'Lineweight));线宽
                              (Vlax-Put-Property obj 'Color (Vlax-Get (Vlax-Ename->Vla-Object ent) 'Color))
                              (entdel ent)
  
                              (setq tmpLst nil)
                              (foreach arc_list_item arc_list
                                  (cond
                                      ((and (equal bj (car arc_list_item) precision) (equal (car pc) (cadr arc_list_item) precision) (equal (cadr pc) (caddr arc_list_item) precision));同圆心等半径
                                          (setq ssUnnecessary (ssadd (last arc_list_item) ssUnnecessary))
                                          (cs_pross zongshu (setq xuhao (1+ xuhao)))
                                      )
                                      (T (setq tmpLst (append tmpLst (list arc_list_item))))
                                  )
                              )
                              (setq arc_list tmpLst)
                              (setq biaoji nil);结束内圈循环
                          )
                          ((and (BF-onArcP sangl1 sangl eangl) (BF-onArcP eangl1 sangl eangl));弧a包含弧b
                              (setq ssUnnecessary (ssadd ent1 ssUnnecessary))
                              (cs_pross zongshu (setq xuhao (1+ xuhao)))
                              (setq arc_list (cdr arc_list))
                          )
                          ((and (BF-onArcP sangl sangl1 eangl1) (BF-onArcP eangl sangl1 eangl1));弧b包含弧a
                              (Vlax-Put-Property (Vlax-Ename->Vla-Object ent) 'StartAngle sangl1)
                              (Vlax-Put-Property (Vlax-Ename->Vla-Object ent) 'EndAngle eangl1)
                              (setq sangl sangl1)
                              (setq eangl eangl1)
                              (setq ssUnnecessary (ssadd ent1 ssUnnecessary))
                              (cs_pross zongshu (setq xuhao (1+ xuhao)))
                              (setq arc_list (cdr arc_list))
                          )
  ;;;部分重叠
                          ((or (BF-onArcP sangl1 sangl eangl) (BF-onArcP eangl1 sangl eangl));弧b、弧a部分重叠
                              (setq ssUnnecessary (ssadd ent1 ssUnnecessary))
                              (cs_pross zongshu (setq xuhao (1+ xuhao)))
                              (setq arc_list (cdr arc_list))
                              (cond
                                  ((BF-onArcP sangl1 sangl eangl)
                                      (Vlax-Put-Property (Vlax-Ename->Vla-Object ent) 'EndAngle eangl1)
                                      (setq eangl eangl1)
                                  )
                                  (T
                                      (Vlax-Put-Property (Vlax-Ename->Vla-Object ent) 'StartAngle sangl1)
                                      (setq sangl sangl1)
                                  )
                              )
                          )
                          (T (setq biaoji nil));弧a、弧b不重叠
                      );cond
                  );cond-1    同圆心同半径
                  (T (setq biaoji nil));cond-2
              );cond
          );while
      );while
  
      (if (> (sslength ssUnnecessary) 0)
          (progn
              (princ (strcat "，删除了" (itoa (sslength ssUnnecessary)) "个重复圆或圆弧."))
              (command-s "erase" ssUnnecessary "")
          )
      )
  );hb_arc
  
  
  (defun line_data(ent / obj p1 p2 precision k b e1 e2)
      (setq precision 1e-8)
      (setq obj (vlax-ename->vla-object ent)
          p1 (vlax-curve-getstartpoint obj)
          p2 (vlax-curve-getendpoint obj)
      )
      (if (equal (car p1) (car p2) precision);斜率不存在
          (setq k nil
              b (car p1)
          );直线x=b
          (setq k (/ (- (cadr p2) (cadr p1))
                      (- (car p2) (car p1))
                  )
                b (- (cadr p1) (* (car p1) k))
          )
      )
  
      (setq p2 (vl-sort (list p1 p2)
                      '(lambda(e1 e2)
                         (if (equal (car e1) (car e2) precision);x坐标相等
                           (< (cadr e1) (cadr e2));x坐标相等时,再按y坐标从小到大排序
                           (< (car e1) (car e2));先按x坐标从小到大排序
                         )
                       )
             )
          p1 (car p2)
          p2 (cadr p2)
      )
  
      (list k;斜率
            b;截距
          (list (car p1) (cadr p1));左下点二维坐标表
          (list (car p2) (cadr p2));右上点二维坐标表
          ent;图元名
      )
  )
  
  
  (defun arc_data(ent / data bj pc sangl eangl 2pi)
      (setq 2pi (* 2 pi))
      (setq data (entget ent))
      (setq bj (cdr (assoc 40 data)))
      (setq pc (cdr (assoc 10 data)))
      (setq sangl (cdr (assoc 50 data)))
      (setq eangl (cdr (assoc 51 data)))
      (if sangl;圆弧
          nil
          (setq sangl 0.0
              eangl 2pi
          );圆
      )
  ;        半径 圆心x坐标 圆心y坐标 起点角 终点角 图元名
      (list bj (car pc) (cadr pc) sangl eangl ent)
  )
  
  
  ;合并直线
  (defun hb_line(line_list precision / zongshu i xuhao line_a biaoji k b p1 p2 ent lay line_b p3 p4 p5 e1 e2 data)
      (setq zongshu (length line_list);总数
          i 0;计数变量
          xuhao 0;序号
      )
      (princ (strcat "\n共处理了" (itoa zongshu) "个直线图元"))
      (grtext -1 "合并直线");将文本写入到模式状态行区域
  
      (while (> (length line_list) 0)
          (setq xuhao (1+ xuhao))
          (cs_pross zongshu xuhao)
          (setq line_a (car line_list);第一条直线a的数据表
              line_list (cdr line_list)
              biaoji T;标记
              k (car line_a)
              b (cadr line_a)
              p1 (caddr line_a)
              p2 (cadddr line_a)
              ent (last line_a)
              lay (cdr (assoc 8 (entget ent)));图层
          )
          (while (and biaoji
                     (> (length line_list) 0)
                 )
              (setq line_b (car line_list));第一条直线b的数据表
              (cond
                  ((and (equal k (car line_b) precision);共线
                      (equal b (cadr line_b) precision)
                  )
                      (setq p3 (caddr line_b);左下点
                          p4 (cadddr line_b);右上点
                          p5 (vl-sort (list p1 p2 p3 p4)
                             '(lambda (e1 e2)
                                (if (equal (car e1) (car e2) precision);x坐标相等
                                  (< (cadr e1) (cadr e2));x坐标相等时,按y坐标从小到大排序
                                  (< (car e1) (car e2));先按x坐标从小到大排序
                                )
                              )
                             )
                          p4 (cadr p5)
                      )
                      (if (or (equal p1 p4 precision);p4与某一条直线的左下点重合
                              (equal p3 p4 precision)
                          )
                          (progn
                              (setq p1 (car p5);四个点中的左下
                                  p2 (last p5);四个点中的右上
                                  line_list (cdr line_list)
                              )
                              (entdel (last line_b));保留直线a,删除直线b
                              (setq xuhao (1+ xuhao))
                              (cs_pross zongshu xuhao)
                              (setq i (1+ i))
                          )
                          (setq biaoji nil)
                      )
                  );共线
                  (T (setq biaoji nil))
              )
          )
  
          (setq data (entget ent)
              data (subst (cons 10 p1) (assoc 10 data) data)
              data (subst (cons 11 p2) (assoc 11 data) data)
          )
          (entmod data)
      )
  
      (if (> i 0)
          (progn
              (princ (strcat "，删除了" (itoa i) "条重复直线."))
          )
      )
  
      (princ)
  )
  
  
  (defun cs_pross(total i / cs_Text myI);total总数,i序号
      (setq cs_Text ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");42个字符
      (setq myI (fix (/ (* (strlen cs_Text) i) total));商的整数部分
            cs_Text (substr cs_Text 1 myI)
      )
      (grtext -2 cs_Text)
  )
  ;;; *****消除重线 程序结束*****
  
  
  ;;;name:BF-onArcP
  ;;;desc:确定角ang是否在ang1与ang2之间
  ;;;arg:ang1:圆弧起点角,ang2:圆弧终点角
  ;;;return:角ang在ang1与ang2之间返回T,否则返回nil
  ;;;example:(BF-onArcP (/ pi 2) 0 pi) 返回 T\n(BF-onArcP (/ pi 4) (/ pi 2) pi) 返回 nil
  (defun BF-onArcP(ang ang1 ang2)
      (cond
          ((> ang2 ang1)
              (>= ang2 ang ang1)
          )
          (T;ang2<=ang1
               (or (<= ang ang2) (>= ang ang1))
          )
      )
  )
  
  ;;;=====================================
  ;;;获取实体dxf组码内容
  ;;;(get_dxf ename code)
  (defun get_dxf (ename code / elist retVal)
      (setq elist (entget ename))
      (setq retVal (cdr (assoc code elist)))
      (cond
          (retVal)
          (T
              (princ "\n函数get_dxf的返回值为nil")
              (exit)
          )
      )
      retVal
  )
  
  ;;;初始化，读取系统变量
  (defun Berni_Start()
      (setq Berni_S_Lst (List (getvar "osmode");0
                              (getvar "cmdecho");1
                              (getvar "clayer");2
                              (getvar "textstyle");3
                              (getvar "cecolor");4
                              (getvar "dimstyle");5
                              (getvar "plinewid");6
                              (getvar "attdia");7
                              (getvar "PICKSTYLE");8
                              (getvar "PEDITACCEPT");9
                              (getvar "dynmode");10
                              (getvar "nomutt");11
                        );end list
      );end setq
      (setvar "cmdecho" 0);1
      (command-s "undo" "be")
      (setq old_error *error*)
      (setq *error* *error*_zrw)
      (setvar "osmode" 0);0
      (setvar "attdia" 0);7    INSERT 命令给出命令行提示而非使用对话框用于属性值的输入
      (setvar "PICKSTYLE" 0);8    控制编组选择和关联填充选择的使用。0 不使用编组选择和关联填充选择
      (setvar "PEDITACCEPT" 1);9    禁止在 PEDIT 中显示“选定的对象不是多段线”提示，选定对象将自动转换为多段线。
      (setvar "dynmode" 0);10
      (princ)
  )
  
  ;;;结束时，恢复系统变量
  (defun Berni_End()
      (setvar "osmode" (nth 0 Berni_s_Lst))
      (setvar "clayer" (nth 2 Berni_s_Lst))
      (setvar "textstyle" (nth 3 Berni_s_Lst))
      (setvar "cecolor" (nth 4 Berni_s_Lst))
      (setvar "plinewid" (nth 6 Berni_s_Lst))
      (setvar "attdia" (nth 7 Berni_s_Lst));控制 INSERT 命令是否使用对话框用于属性值的输入。0 给出命令行提示；1 使用对话框
       (setvar "PICKSTYLE" (nth 8 Berni_s_Lst));控制编组选择和关联填充选择的使用。0 不使用编组选择和关联填充选择；1 使用编组选择；2 使用关联填充选择；3 使用编组选择和关联填充选择
       (setvar "PEDITACCEPT" (nth 9 Berni_s_Lst));禁止在 PEDIT 中显示“选定的对象不是多段线”提示。 该提示后会显示“是否将其转换为多段线？”输入 y 将选定对象转换为多线段。 当该提示被禁止显示时，选定对象将自动转换
  ;为多段线。0 显示提示；1 抑制提示
       (setvar "dynmode" (nth 10 Berni_s_Lst))
      (setvar "nomutt" (nth 11 Berni_s_Lst));禁止显示通常情况下不禁止显示的消息（即不进行消息反馈）。 显示的消息为普通模式，但在脚本、AutoLISP 例程等运行期间将禁止消息显示。0 恢复普通模式的消息反馈；1 禁止不
  ;确定的消息反馈
       (setq *error* old_error)

    
       (command-s "_.undo" "_end")
      (setvar "cmdecho" (nth 1 Berni_s_Lst))
      (princ)
  )
  
  ;自定义错误处理函数
  (defun *error*_zrw(msg)
      (princ "\n出错: ")
      (princ msg)
      (princ ", 程序退出! ")
      (Berni_End)
  )
  


;;;=======================[ BreakObjects.lsp ]==============================
;;; Author: Copyright?2006-2008 Charles Alan Butler 
;;; Contact @  www.TheSwamp.org
;;; Version:  2.1  Nov. 20,2008
;;; Purpose: Break All selected objects
;;;    permitted objects are lines, lwplines, plines, splines,
;;;    ellipse, circles & arcs 
;;;                            
;;;  Function  c:MyBreak -       DCL for selecting the routines
;;;  Function  c:BreakAll -      Break all objects selected with each other
;;;  Function  c:BreakwObject  - Break many objects with a single object
;;;  Function  c:BreakObject -   Break a single object with other objects 
;;;  Function  c:BreakWith -     Break selected objects with other selected objects
;;;  Function  c:BreakTouching - Break objects touching selected objects
;;;  Function  c:BreakSelected - Break selected objects with any objects that touch it 
;;;  Revision 1.8 Added Option for Break Gap greater than zero
;;;  NEW r1.9  c:BreakWlayer -   Break objects with objects on a layer
;;;  NEW r1.9  c:BreakWithTouching - Break touching objects with selected objects
;;;  Revision 2.0 Fixed a bug when point to break is at the end of object
;;;  Revision 2.1 Fixed another bug when point to break is at the end of object
;;;
;;;
;;;  Function  break_with  - main break function called by all others and
;;;                          returns a list of new enames, see c:BreakAll
;;;                          for an example of using the return list
;;;
;;; Requirements: objects must have the same z-value
;;; Restrictions: Does not Break objects on locked layers 
;;; Returns:  none
;;;
;;;=====================================================================
;;;   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED     ;
;;;   WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR  ;
;;;   PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.            ;
;;;                                                                    ;
;;;  You are hereby granted permission to use, copy and modify this    ;
;;;  software without charge, provided you do so exclusively for       ;
;;;  your own use or for use by others in your organization in the     ;
;;;  performance of their normal duties, and provided further that     ;
;;;  the above copyright notice appears in all copies and both that    ;
;;;  copyright notice and the limited warranty and restricted rights   ;
;;;  notice below appear in all supporting documentation.              ;
;;;=====================================================================


;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;               M A I N   S U B R O U T I N E                   
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun break_with (ss2brk ss2brkwith self Gap / cmd intpts lst masterlist ss ssobjs
                   onlockedlayer ssget->vla-list list->3pair GetNewEntities oc
                   get_interpts break_obj GetLastEnt LastEntInDatabase ss2brkwithList
                  )
  ;; ss2brk     selection set to break
  ;; ss2brkwith selection set to use as break points
  ;; self       when true will allow an object to break itself
  ;;            note that plined will break at each vertex
  ;;
  ;; return list of enames of new objects
  
  (vl-load-com)
  
  ;(princ "\nCalculating Break Points, Please Wait.\n")

;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;                S U B   F U N C T I O N S                      
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ;;  return T if entity is on a locked layer
  (defun onlockedlayer (ename / entlst)
    (setq entlst (tblsearch "LAYER" (cdr (assoc 8 (entget ename)))))
    (= 4 (logand 4 (cdr (assoc 70 entlst))))
  )

  ;;  return a list of objects from a selection set
;|  (defun ssget->vla-list (ss)
    (mapcar 'vlax-ename->vla-object (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss ))))
  )|;
  (defun ssget->vla-list (ss / i ename allobj) ; this is faster, changed in ver 1.7
       (setq i -1)
       (while (setq  ename (ssname ss (setq i (1+ i))))
         (setq allobj (cons (vlax-ename->vla-object ename) allobj))
       )
       allobj
  )
  
  ;;  return a list of lists grouped by 3 from a flat list
  (defun list->3pair (old / new)
    (while (setq new (cons (list (car old) (cadr old) (caddr old)) new)
                 old (cdddr old)))
    (reverse new)
  )
  
;;=====================================
;;  return a list of intersect points  
;;=====================================
(defun get_interpts (obj1 obj2 / iplist)
  (if (not (vl-catch-all-error-p
             (setq iplist (vl-catch-all-apply
                            'vlax-safearray->list
                            (list
                              (vlax-variant-value
                                (vla-intersectwith obj1 obj2 acextendnone)
                              ))))))
    iplist
  )
)


;;========================================
;;  Break entity at break points in list  
;;========================================
;;   New as per version 1.8 [BrkGap] --- This subroutine has been re-written
;;  Loop through the break points breaking the entity
;;  If the entity is not a closed entity then a new object is created
;;  This object is added to a list. When break points don't fall on the current 
;;  entity the list of new entities are searched to locate the entity that the 
;;  point is on so it can be broken.
;;  "Break with a Gap" has been added to this routine. The problem faced with 
;;  this method is that sections to be removed may lap if the break points are
;;  too close to each other. The solution is to create a list of break point pairs 
;;  representing the gap to be removed and test to see if there i an overlap. If
;;  there is then merge the break point pairs into one large gap. This way the 
;;  points will always fall on an object with one exception. If the gap is too near
;;  the end of an object one break point will be off the end and therefore that 
;;  point will need to be replaced with the end point.
;;    NOTE: in ACAD2000 the (vlax-curve-getdistatpoint function has proven unreliable
;;  so I have used (vlax-curve-getdistatparam in most cases
(defun break_obj (ent brkptlst BrkGap / brkobjlst en enttype maxparam closedobj
                  minparam obj obj2break p1param p2param brkpt2 dlst idx brkptS
                  brkptE brkpt result GapFlg result ignore dist tmppt
                  #ofpts 2gap enddist lastent obj2break stdist
                 )
  (or BrkGap (setq BrkGap 0.0)) ; default to 0
  (setq BrkGap (/ BrkGap 2.0)) ; if Gap use 1/2 per side of break point
  
  (setq obj2break ent
        brkobjlst (list ent)
        enttype   (cdr (assoc 0 (entget ent)))
        GapFlg    (not (zerop BrkGap)) ; gap > 0
        closedobj (vlax-curve-isclosed obj2break)
  )
  ;; when zero gap no need to break at end points
  (if (zerop Brkgap)
    (setq spt (vlax-curve-getstartpoint ent)
          ept (vlax-curve-getendpoint ent)
          brkptlst (vl-remove-if '(lambda(x) (or (< (distance x spt) 0.0001)
                                                 (< (distance x ept) 0.0001)))
                                 brkptlst)
    )
  )
  (if brkptlst
    (progn
  ;;  sort break points based on the distance along the break object
  ;;  get distance to break point, catch error if pt is off end
  ;; ver 2.0 fix - added COND to fix break point is at the end of a
  ;; line which is not a valid break but does no harm
  (setq brkptlst (mapcar '(lambda(x) (list x (vlax-curve-getdistatparam obj2break
                                               ;; ver 2.0 fix
                                               (cond ((vlax-curve-getparamatpoint obj2break x))
                                                   ((vlax-curve-getparamatpoint obj2break
                                                     (vlax-curve-getclosestpointto obj2break x))))))
                            ) brkptlst))
  ;; sort primary list on distance
  (setq brkptlst (vl-sort brkptlst '(lambda (a1 a2) (< (cadr a1) (cadr a2)))))
  
  (if GapFlg ; gap > 0
    ;; Brkptlst starts as the break point and then a list of pairs of points
    ;;  is creates as the break points
    (progn
      ;;  create a list of list of break points
      ;;  ((idx# stpoint distance)(idx# endpoint distance)...)
      (setq idx 0)
      (foreach brkpt brkptlst
        
        ;; ----------------------------------------------------------
        ;;  create start break point, then create end break point    
        ;;  ((idx# startpoint distance)(idx# endpoint distance)...)  
        ;; ----------------------------------------------------------
        (setq dist (cadr brkpt)) ; distance to center of gap
        ;;  subtract gap to get start point of break gap
        (cond
          ((and (minusp (setq stDist (- dist BrkGap))) closedobj )
           (setq stdist (+ (vlax-curve-getdistatparam obj2break
                             (vlax-curve-getendparam obj2break)) stDist))
           (setq dlst (cons (list idx
                                  (vlax-curve-getpointatparam obj2break
                                         (vlax-curve-getparamatdist obj2break stDist))
                                  stDist) dlst))
           )
          ((minusp stDist) ; off start of object so get startpoint
           (setq dlst (cons (list idx (vlax-curve-getstartpoint obj2break) 0.0) dlst))
           )
          (t
           (setq dlst (cons (list idx
                                  (vlax-curve-getpointatparam obj2break
                                         (vlax-curve-getparamatdist obj2break stDist))
                                  stDist) dlst))
          )
        )
        ;;  add gap to get end point of break gap
        (cond
          ((and (> (setq stDist (+ dist BrkGap))
                   (setq endDist (vlax-curve-getdistatparam obj2break
                                     (vlax-curve-getendparam obj2break)))) closedobj )
           (setq stdist (- stDist endDist))
           (setq dlst (cons (list idx
                                  (vlax-curve-getpointatparam obj2break
                                         (vlax-curve-getparamatdist obj2break stDist))
                                  stDist) dlst))
           )
          ((> stDist endDist) ; off end of object so get endpoint
           (setq dlst (cons (list idx
                                  (vlax-curve-getpointatparam obj2break
                                        (vlax-curve-getendparam obj2break))
                                  endDist) dlst))
           )
          (t
           (setq dlst (cons (list idx
                                  (vlax-curve-getpointatparam obj2break
                                         (vlax-curve-getparamatdist obj2break stDist))
                                  stDist) dlst))
          )
        )
        ;; -------------------------------------------------------
        (setq idx (1+ IDX))
      ) ; foreach brkpt brkptlst
      

      (setq dlst (reverse dlst))
      ;;  remove the points of the gap segments that overlap
      (setq idx -1
            2gap (* BrkGap 2)
            #ofPts (length Brkptlst)
      )
      (while (<= (setq idx (1+ idx)) #ofPts)
        (cond
          ((null result) ; 1st time through
           (setq result (list (car dlst)) ; get first start point
                 result (cons (nth (1+(* idx 2)) dlst) result))
          )
          ((= idx #ofPts) ; last pass, check for wrap
           (if (and closedobj (> #ofPts 1)
                    (<= (+(- (vlax-curve-getdistatparam obj2break
                            (vlax-curve-getendparam obj2break))
                          (cadr (last BrkPtLst))) (cadar BrkPtLst)) 2Gap))
             (progn
               (if (zerop (rem (length result) 2))
                 (setq result (cdr result)) ; remove the last end point
               )
               ;;  ignore previous endpoint and present start point
               (setq result (cons (cadr (reverse result)) result) ; get last end point
                     result (cdr (reverse result))
                     result (reverse (cdr result)))
             )
           )
          )
          ;; Break Gap Overlaps
          ((< (cadr (nth idx Brkptlst)) (+ (cadr (nth (1- idx) Brkptlst)) 2Gap))
           (if (zerop (rem (length result) 2))
             (setq result (cdr result)) ; remove the last end point
           )
           ;;  ignore previous endpoint and present start point
           (setq result (cons (nth (1+(* idx 2)) dlst) result)) ; get present end point
           )
          ;; Break Gap does Not Overlap previous point 
          (t
           (setq result (cons (nth (* idx 2) dlst) result)) ; get this start point
           (setq result (cons (nth (1+(* idx 2)) dlst) result)) ; get this end point
          )
        ) ; end cond stmt
      ) ; while
      
      ;;  setup brkptlst with pair of break pts ((p1 p2)(p3 p4)...)
      ;;  one of the pair of points will be on the object that
      ;;  needs to be broken
      (setq dlst     (reverse result)
            brkptlst nil)
      (while dlst ; grab the points only
        (setq brkptlst (cons (list (cadar dlst)(cadadr dlst)) brkptlst)
              dlst   (cddr dlst))
      )
    )
  )
  ;;   -----------------------------------------------------

  ;; (if (equal  a ent) (princ)) ; debug CAB  -------------
 
  (foreach brkpt (reverse brkptlst)
    (if GapFlg ; gap > 0
      (setq brkptS (car brkpt)
            brkptE (cadr brkpt))
      (setq brkptS (car brkpt)
            brkptE brkptS)
    )
    ;;  get last entity created via break in case multiple breaks
    (if brkobjlst
      (progn
        (setq tmppt brkptS) ; use only one of the pair of breakpoints
        ;;  if pt not on object x, switch objects
        (if (not (numberp (vl-catch-all-apply
                            'vlax-curve-getdistatpoint (list obj2break tmppt))))
          (progn ; find the one that pt is on
            (setq idx (length brkobjlst))
            (while (and (not (minusp (setq idx (1- idx))))
                        (setq obj (nth idx brkobjlst))
                        (if (numberp (vl-catch-all-apply
                                       'vlax-curve-getdistatpoint (list obj tmppt)))
                          (null (setq obj2break obj)) ; switch objects, null causes exit
                          t
                        )
                   )
            )
          )
        )
      )
    )
    ;| ;; ver 2.0 fix - removed this code as there are cases where the break point
       ;; is at the end of a line which is not a valid break but does no harm
    (if (and brkobjlst idx (minusp idx)
             (null (alert (strcat "Error - point not on object"
                                  "\nPlease report this error to"
                                  "\n   CAB at TheSwamp.org"))))
      (exit)
    )
    |;
    ;; (if (equal (if (null a)(setq a (car(entsel"\nTest Ent"))) a) ent) (princ)) ; debug CAB  -------------

    ;;  Handle any objects that can not be used with the Break command-s
    ;;  using one point, gap of 0.000001 is used
    (setq closedobj (vlax-curve-isclosed obj2break))
    (if GapFlg ; gap > 0
      (if closedobj
        (progn ; need to break a closed object
          (setq brkpt2 (vlax-curve-getPointAtDist obj2break
                     (- (vlax-curve-getDistAtPoint obj2break brkptE) 0.00001)))
          (command-s "._break" obj2break "_non" (trans brkpt2 0 1)
                   "_non" (trans brkptE 0 1))
          (and (= "CIRCLE" enttype) (setq enttype "ARC"))
          (setq BrkptE brkpt2)
        )
      )
      ;;  single breakpoint ----------------------------------------------------
      ;|(if (and closedobj ; problems with ACAD200 & this code
               (not (setq brkptE (vlax-curve-getPointAtDist obj2break
                       (+ (vlax-curve-getDistAtPoint obj2break brkptS) 0.00001))))
          )
        (setq brkptE (vlax-curve-getPointAtDist obj2break
                       (- (vlax-curve-getDistAtPoint obj2break brkptS) 0.00001)))
        
      )|;
      (if (and closedobj 
               (not (setq brkptE (vlax-curve-getPointAtDist obj2break
                       (+ (vlax-curve-getdistatparam obj2break
                            ;;(vlax-curve-getparamatpoint obj2break brkpts)) 0.00001))))
                            ;; ver 2.0 fix
                            (cond ((vlax-curve-getparamatpoint obj2break brkpts))
                                  ((vlax-curve-getparamatpoint obj2break
                                      (vlax-curve-getclosestpointto obj2break brkpts))))) 0.00001)))))
        (setq brkptE (vlax-curve-getPointAtDist obj2break
                       (- (vlax-curve-getdistatparam obj2break
                            ;;(vlax-curve-getparamatpoint obj2break brkpts)) 0.00001)))
                            ;; ver 2.0 fix
                            (cond ((vlax-curve-getparamatpoint obj2break brkpts))
                                  ((vlax-curve-getparamatpoint obj2break
                                      (vlax-curve-getclosestpointto obj2break brkpts))))) 0.00001)))
       )
    ) ; endif
    
    ;; (if (null brkptE) (princ)) ; debug
    
    (setq LastEnt (GetLastEnt))
    (command-s "._break" obj2break "_non" (trans brkptS 0 1) "_non" (trans brkptE 0 1))
    (and *BrkVerbose* (princ (setq *brkcnt* (1+ *brkcnt*))) (princ "\r"))
    (and (= "CIRCLE" enttype) (setq enttype "ARC"))
    (if (and (not closedobj) ; new object was created
             (not (equal LastEnt (entlast))))
        (setq brkobjlst (cons (entlast) brkobjlst))
    )
  )
  )
  ) ; endif brkptlst
  
) ; defun break_obj

;;====================================
;;  CAB - get last entity in datatbase
(defun GetLastEnt ( / ename result )
  (if (setq result (entlast))
    (while (setq ename (entnext result))
      (setq result ename)
    )
  )
  result
)
;;===================================
;;  CAB - return a list of new enames
(defun GetNewEntities (ename / new)
  (cond
    ((null ename) (alert "Ename nil"))
    ((eq 'ENAME (type ename))
      (while (setq ename (entnext ename))
        (if (entget ename) (setq new (cons ename new)))
      )
    )
    ((alert "Ename wrong type."))
  )
  new
)

  
  ;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;;         S T A R T  S U B R O U T I N E   H E R E              
  ;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   
    (setq LastEntInDatabase (GetLastEnt))
    (if (and ss2brk ss2brkwith)
    (progn
      (setq oc 0
            ss2brkwithList (ssget->vla-list ss2brkwith))
      (if (> (* (sslength ss2brk)(length ss2brkwithList)) 5000)
        (setq *BrkVerbose* t)
      )
      (and *BrkVerbose*
           (princ (strcat "Objects to be Checked: "
            (itoa (* (sslength ss2brk)(length ss2brkwithList))) "\n")))
      ;;  CREATE a list of entity & it's break points
      (foreach obj (ssget->vla-list ss2brk) ; check each object in ss2brk
        (if (not (onlockedlayer (vlax-vla-object->ename obj)))
          (progn
            (setq lst nil)
            ;; check for break pts with other objects in ss2brkwith
            (foreach intobj  ss2brkwithList
              (if (and (or self (not (equal obj intobj)))
                       (setq intpts (get_interpts obj intobj))
                  )
                (setq lst (append (list->3pair intpts) lst)) ; entity w/ break points
              )
              (and *BrkVerbose* (princ (strcat "Objects Checked: " (itoa (setq oc (1+ oc))) "\r")))
            )
            (if lst
              (setq masterlist (cons (cons (vlax-vla-object->ename obj) lst) masterlist))
            )
          )
        )
      )

      
      (and *BrkVerbose* (princ "\nBreaking Objects.\n"))
      (setq *brkcnt* 0) ; break counter
      ;;  masterlist = ((ent brkpts)(ent brkpts)...)
      (if masterlist
        (foreach obj2brk masterlist
          (break_obj (car obj2brk) (cdr obj2brk) Gap)
        )
      )
      )
  )
;;==============================================================
   (and (zerop *brkcnt*) (princ "\nNone to be broken."))
   (setq *BrkVerbose* nil)
  (GetNewEntities LastEntInDatabase) ; return list of enames of new objects
)
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      E N D   O F    M A I N   S U B R O U T I N E             
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;           M A I N   S U B   F U N C T I O N S                 
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ;;======================
  ;;  Redraw ss with mode 
  ;;======================
  (defun ssredraw (ss mode / i num)
    (setq i -1)
    (while (setq ename (ssname ss (setq i (1+ i))))
      (redraw (ssname ss i) mode)
    )
  )

  ;;===========================================================================
  ;;  get all objects touching entities in the sscross                         
  ;;  limited obj types to "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"
  ;;  returns a list of enames
  ;;===========================================================================
  (defun gettouching (sscros / ss lst lstb lstc objl)
    (and
      (setq lstb (vl-remove-if 'listp (mapcar 'cadr (ssnamex sscros)))
            objl (mapcar 'vlax-ename->vla-object lstb)
      )
      (setq
        ss (ssget "_A" (list (cons 0 "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE")
                             (cons 410 (getvar "ctab"))))
      )
      (setq lst (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
      (setq lst (mapcar 'vlax-ename->vla-object lst))
      (mapcar
        '(lambda (x)
           (mapcar
             '(lambda (y)
                (if (not
                      (vl-catch-all-error-p
                        (vl-catch-all-apply
                          '(lambda ()
                             (vlax-safearray->list
                               (vlax-variant-value
                                 (vla-intersectwith y x acextendnone)
                               ))))))
                  (setq lstc (cons (vlax-vla-object->ename x) lstc))
                )
              ) objl)
         ) lst)
    )
    lstc
  )



;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;          E N D   M A I N    F U N C T I O N S                 
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



;;===============================================
;;   Break all objects selected with each other  
;;===============================================
(defun BreakAll (/ cmd ss NewEnts AllEnts tmp)

  (command-s "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  ;(or Bgap (setq Bgap 0)) ; default
  ;(initget 4) ; no negative numbers
  ;(if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    ;(setq Bgap tmp)
  ;)
  ;;  get objects to break
   (setq ss (ssget "c" ml_p1 ml_p2 gllst))
  (if (/= ss nil)
     (setq NewEnts (Break_with ss ss nil Bgap) ; ss2break ss2breakwith (flag nil = not to break with self)
           ; AllEnts (append NewEnts (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
           )
  )
  (setvar "CMDECHO" cmd)
  (command-s "_.undo" "_end")
  (princ)
)


;;===========================================
;;  Break a single object with other objects 
;;===========================================
(defun BreakObject (/ cmd ss1 ss2 tmp)

  (command-s "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )

  ;;  get objects to break
  (prompt "\nSelect single object to break: ")
  (if (and (setq ss1 (ssget "+.:E:S" '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (redraw (ssname ss1 0) 3))
           (not (prompt "\n***  Select object(s) to break with & press enter:  ***"))
           (setq ss2 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (redraw (ssname ss1 0) 4)))
     (Break_with ss1 ss2 nil Bgap) ; ss2break ss2breakwith (flag nil = not to break with self)
  )

  (setvar "CMDECHO" cmd)
  (command-s "_.undo" "_end")
  (princ)
)

;;==========================================
;;  Break many objects with a single object 
;;==========================================
(defun BreakWobject (/ cmd ss1 ss2 tmp)

  (command-s "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )
  ;;  get objects to break
  (prompt "\nSelect object(s) to break & press enter: ")
  (if (and (setq ss1 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (ssredraw ss1 3))
           (not (prompt "\n***  Select single object to break with:  ***"))
           (setq ss2 (ssget "+.:E:S" '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (ssredraw ss1 4))
      )
    (break_with ss1 ss2 nil Bgap) ; ss1break ss2breakwith (flag nil = not to break with self)
  )

  (setvar "CMDECHO" cmd)
  (command-s "_.undo" "_end")
  (princ)
)


;;==========================================
;;  Break objects with objects on a layer   
;;==========================================
;;  New 08/01/2008
(defun BreakWlayer (/ cmd ss1 ss2 tmp lay)

  (command-s "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )
  ;;  get objects to break
  (prompt "\n***  Select single object for break layer:  ***")
  
  (if (and (setq ss2 (ssget "+.:E:S" '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (setq lay (assoc 8 (entget (ssname ss2 0))))
           (setq ss2 (ssget "_X" (list
                                   '(0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE")
                                   lay (cons 410 (getvar "ctab")))))
           (not (prompt "\nSelect object(s) to break & press enter: "))
           (setq ss1 (ssget (list
                              '(0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE")
                              (cons 8 (strcat "~" (cdr lay))))))
      )
    (break_with ss1 ss2 nil Bgap) ; ss1break ss2breakwith (flag nil = not to break with self)
  )

  (setvar "CMDECHO" cmd)
  (command-s "_.undo" "_end")
  (princ)
)


;;======================================================
;;  Break selected objects with other selected objects  
;;======================================================
(defun BreakWith (/ cmd ss1 ss2 tmp)

  (command-s "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )
  ;;  get objects to break
  (prompt "\nBreak selected objects with other selected objects.")
  (prompt "\nSelect object(s) to break & press enter: ")
  (if (and (setq ss1 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (ssredraw ss1 3))
           (not (prompt "\n***  Select object(s) to break with & press enter:  ***"))
           (setq ss2 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (ssredraw ss1 4))
      )
    (break_with ss1 ss2 nil Bgap) ; ss1break ss2breakwith (flag nil = not to break with self)
  )

  (setvar "CMDECHO" cmd)
  (command-s "_.undo" "_end")
  (princ)
)



;;=============================================
;;  Break objects touching selected objects    
;;=============================================

(defun BreakTouching (/ cmd ss1 ss2 tmp)

  (command-s "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq ss1 (ssadd))
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )
  ;;  get objects to break
  (prompt "\nBreak objects touching selected objects.")
  (if (and (not (prompt "\nSelect object(s) to break & press enter: "))
           (setq ss2 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (mapcar '(lambda (x) (ssadd x ss1)) (gettouching ss2))
      )
    (break_with ss1 ss2 nil Bgap) ; ss1break ss2breakwith (flag nil = not to break with self)
  )

  (setvar "CMDECHO" cmd)
  (command-s "_.undo" "_end")
  (princ)
)



;;=================================================
;;  Break touching objects with selected objects   
;;=================================================
;;  New 08/01/2008
(defun BreakWithTouching (/ cmd ss1 ss2 tmp)

  (command-s "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq ss1 (ssadd))
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )

  ;;  get objects to break
  (prompt "\nBreak objects touching selected objects.")
  (prompt "\nSelect object(s) to break with & press enter: ")
  (if (and (setq ss2 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (setq tlst (gettouching ss2))
      )
    (progn
      (setq tlst (vl-remove-if '(lambda (x)(ssmemb x ss2)) tlst)) ;  remove if in picked ss
      (mapcar '(lambda (x) (ssadd x ss1)) tlst) ; convert to a selection set
      (break_with ss1 ss2 nil Bgap) ; ss1break ss2breakwith (flag nil = not to break with self)
    )
  )

  (setvar "CMDECHO" cmd)
  (command-s "_.undo" "_end")
  (princ)
)


;;==========================================================
;;  Break selected objects with any objects that touch it   
;;==========================================================


(defun BreakSelected (/ cmd ss1 ss2 tmp)
  
  (command-s "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq ss1 (ssadd))
  (or Bgap (setq Bgap 0)) ; default
  (initget 4) ; no negative numbers
  (if (setq tmp (getdist (strcat "\nEnter Break Gap.<"(rtos Bgap)"> ")))
    (setq Bgap tmp)
  )
  ;;  get objects to break
  (prompt "\nBreak selected objects with any objects that touch it.")
  (if (and (not (prompt "\nSelect object(s) to break with touching & press enter: "))
           (setq ss2 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (mapcar '(lambda (x) (ssadd x ss1)) (gettouching ss2))
      )
    (break_with ss2 ss1 nil Bgap) ; ss2break ss1breakwith (flag nil = not to break with self)
  )

  (setvar "CMDECHO" cmd)
  (command-s "_.undo" "_end")
  (princ)
)

;; ***************************************************
;;     Function to create a dcl support file if it    
;;       does not exist                               
;;     Usage : (create_dcl "file name")               
;;     Returns : T if successful else nil             
;; ***************************************************
(defun create_Breakdcl (fname / acadfn dcl-rev-check)
  ;;=======================================
  ;;      check revision date Routine          
  ;;=======================================
  (defun dcl-rev-check (fn / rvdate ln lp)
    ;;  revision flag must match exactly and must
    ;;  begin with //
    (setq rvflag "//  Revision Control 05/12/2008@14:11" )
    (if (setq fn (findfile fn))
      (progn ; check rev date
        (setq lp 5) ; read 4 lines
        (setq fn (open fn "r")) ; open file for reading
        (while (> (setq lp (1- lp)) 0)
          (setq ln (read-line fn)) ; get a line from file
          (if (vl-string-search rvflag ln)
            (setq lp 0)
          )
        )
        (close fn) ; close the open file handle
        (if (= lp -1)
          nil ; no new dcl needed
          t ; flag to create new file
        )
      )
      t ; flag to create new file
    )
  )
  (if (null(wcmatch (strcase fname) "*`.DCL"))
    (setq fname (strcat fname ".DCL"))
  )
  (if (dcl-rev-check fname)
    ;; create dcl file in same directory as ACAD.PAT  
    (progn
      (setq acadfn (findfile "ACAD.PAT")
            fn (strcat (substr acadfn 1 (- (strlen acadfn) 8))fname)
            fn (open fn "w")
      )
      (foreach x (list
                   "// WARNING file will be recreated if you change the next line"
                   rvflag
                   "//BreakAll.DCL"
                   "BreakDCL : dialog { label = \"[ Break All or Some by CAB  v1.8 ]\";"
                   "  : text { label = \"--=<  Select type of Break Function needed  >=--\"; "
                   "           key = \"tm\"; alignment = centered; fixed_width = true;}"
                   "    spacer_1;"
                   "    : button { key = \"b1\"; mnemonic = \"T\";  alignment = centered;"
                   "               label = \"Break all objects selected with each other\";} "
                   "    : button { key = \"b2\"; mnemonic = \"T\"; alignment = centered;"
                   "               label = \"Break selected objects with other selected objects\";}"
                   "    : button { key = \"b3\"; mnemonic = \"T\";  alignment = centered;"
                   "               label = \" Break selected objects with any  objects that touch it\";}"
                   "    spacer_1;"
                   "  : row { spacer_0;"
                   "    : edit_box {key = \"gap\" ; width = 8; mnemonic = \"G\"; label = \"Gap\"; fixed_width = true;}"
                   "    : button { label = \"Help\"; key = \"help\"; mnemonic = \"H\"; fixed_width = true;} "
                   "    cancel_button;"
                   "    spacer_0;"
                   "  }"
                   "}"
                  ) ; endlist
        (princ x fn)
        (write-line "" fn)
      ) ; end foreach
      (close fn)
      (setq acadfn nil)
      (alert (strcat "\nDCL file created, please restart the routine"
               "\n again if an error occures."))
      t ; return True, file created
    )
    t ; return True, file found
  )
) ; end defun


;;==============================
;;     BreakAll Dialog Routine  
;;==============================
(defun c:MyBreak(/ dclfile dcl# RunDCL BreakHelp cmd txt2num)
   ;;  return number or nil
  (defun txt2num (txt / num)
    (if txt
    (or (setq num (distof txt 5))
        (setq num (distof txt 2))
        (setq num (distof txt 1))
        (setq num (distof txt 4))
        (setq num (distof txt 3))
    )
    )
    (if (numberp num)
      num
    )
  )
  (defun mydonedialog (flag)
    (setq DCLgap (txt2num (get_tile "gap")))
    (done_dialog flag)
  )
  (defun RunDCL (/ action)
    (or DCLgap (setq DCLgap 0)) ; error trap value
    (action_tile "b1" "(mydonedialog 1)")
    (action_tile "b2" "(mydonedialog 2)")
    (action_tile "b3" "(mydonedialog 3)")
    (action_tile "gap" "(setq DCLgap (txt2num value$))")
    (set_tile "gap" (rtos DCLgap))
    (action_tile "help" "(BreakHelp)")
    (action_tile "cancel" "(done_dialog 0)")
    (setq action (start_dialog))
    (or DCLgap (setq DCLgap 0)) ; error trap value
    (setq DCLgap (max DCLgap 0)) ; nu negative numbers
    
    (cond
      ((= action 1) ; BreakAll
         (command-s "_.undo" "_begin")
  ;;  get objects to break
  (prompt "\nSelect objects to break with each other & press enter: ")
  (if (setq ss (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
     (setq NewEnts (Break_with ss ss nil DCLgap) ; ss2break ss2breakwith (flag nil = not to break with self)
           ; AllEnts (append NewEnts (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
           )
  )
  (command-s "_.undo" "_end")
  (princ)
       )
      
      ((= action 2) ; BreakWith
         ;;  get objects to break
  (prompt "\nBreak selected objects with other selected objects.")
  (prompt "\nSelect object(s) to break & press enter: ")
  (if (and (setq ss1 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (ssredraw ss1 3))
           (not (prompt "\n***  Select object(s) to break with & press enter:  ***"))
           (setq ss2 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (not (ssredraw ss1 4))
      )
    (break_with ss1 ss2 nil DCLgap) ; ss1break ss2breakwith (flag nil = not to break with self)
  )

       )
      ((= action 3) ; BreakSelected
  (setq ss1 (ssadd))
  ;;  get objects to break
  (prompt "\nBreak selected objects with any objects that touch it.")
  (if (and (not (prompt "\nSelect object(s) to break with touching & press enter: "))
           (setq ss2 (ssget '((0 . "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
           (mapcar '(lambda (x) (ssadd x ss1)) (gettouching ss2))
      )
    (break_with ss2 ss1 nil DCLgap) ; ss2break ss1breakwith (flag nil = not to break with self)
  )
       )
    )
  )
  (defun BreakHelp ()
    (alert
      (strcat
        "BreakAll.lsp				       (c) 2007-2008 Charles Alan Butler\n\n"
        "This LISP routine will break objects based on the routine you select.\n"
        "It will not break objects on locked layers and objects must have the same z-value.\n"
        "Object types are limited to LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE\n"
        "BreakAll -      Break all objects selected with each other\n"
        "BreakwObject  - Break many objects with a single object\n"
        "BreakObject -   Break a single object with many objects \n"
        "BreakWith -     Break selected objects with other selected objects\n"
        "BreakTouching - Break objects touching selected objects\n"
        "BreakSelected - Break selected objects with any objects that touch it\n"
        " The Gap distance is the total opening created.\n"
        "You may run each routine by entering the function name at the command-s line.\n"
        "For updates & comments contact Charles Alan Butler AKA CAB at TheSwamp.org.\n")
    )
  )
  
  ;;================================================================
  ;;                    Start of Routine                            
  ;;================================================================
  (vl-load-com)
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq dclfile "BreakAll.dcl")
  (cond
    ((not (create_Breakdcl dclfile))
     (prompt (strcat "\nCannot create " dclfile "."))
    )
    ((< (setq dcl# (load_dialog dclfile)) 0)
     (prompt (strcat "\nCannot load " dclfile "."))
    )
    ((not (new_dialog "BreakDCL" dcl#))
     (prompt (strcat "\nProblem with " dclfile "."))
    )
    ((RunDCL))      ; No DCL problems: fire it up
  )
  (and cmd (setvar "CMDECHO" cmd))
  (princ)
)
;(prompt "Break routines loaded, Enter Mybreak to run.")
;(princ)

;;;获取图块基点
(defun sub1_bxform2d (pt ins sx sy rot / x y xr yr)
  (setq x (* (car pt) sx)
        y (* (cadr pt) sy)
  )
  (setq xr (- (* x (cos rot)) (* y (sin rot))))
  (setq yr (+ (* x (sin rot)) (* y (cos rot))))
  (list (+ (car ins) xr) (+ (cadr ins) yr) 0.0)
)
(defun block_base(single / cm os obj btr ent p1 p2 ins sx sy rot best_area best_pt dx dy cname)
  ;;;找到图块的中点位置（优先取最大面积非文字图元中心）
  (setq cm (getvar "cmdecho") os (getvar "osmode"))
  (setvar "cmdecho" 0) (setvar "osmode" 0)
  (command-s "_undo" "be")

  (setq obj (vlax-ename->vla-object single))
  (setq ins (cdr (assoc 10 (entget single))))
  (setq sx  (vla-get-XScaleFactor obj))
  (setq sy  (vla-get-YScaleFactor obj))
  (setq rot (vla-get-Rotation obj))

  (setq btr (vla-item (vla-get-blocks (vla-get-document obj))
                      (vla-get-Effectivename obj)))
  (setq best_area -1.0 best_pt nil)

  (vlax-for ent btr
    (if (vlax-method-applicable-p ent 'GetBoundingBox)
      (progn
        (setq cname (vla-get-ObjectName ent))
        (if (and (/= cname "AcDbText") (/= cname "AcDbMText"))
          (progn
            (vla-GetBoundingBox ent 'p1 'p2)
            (setq p1 (vlax-safearray->list p1)
                  p2 (vlax-safearray->list p2))
            (setq dx (abs (- (car p2) (car p1))))
            (setq dy (abs (- (cadr p2) (cadr p1))))
            (if (> (* dx dy) best_area)
              (progn
                (setq best_area (* dx dy))
                (setq best_pt (sub1_bxform2d (list (/ (+ (car p1) (car p2)) 2.0)
                                                   (/ (+ (cadr p1) (cadr p2)) 2.0)
                                                   0.0)
                                              ins sx sy rot))
              )
            )
          )
        )
      )
    )
  )

  (if best_pt
    (setq block_base1 best_pt)
    (progn
      (vla-GetBoundingBox obj 'p1 'p2)
      (setq p1 (vlax-safearray->list p1)
            p2 (vlax-safearray->list p2))
      (setq block_base1 (list (/ (+ (car p1) (car p2)) 2.0)
                              (/ (+ (cadr p1) (cadr p2)) 2.0)
                              0.0))
    )
  )

  (command-s "_.undo" "_end")
  (setvar "osmode" os) (setvar "cmdecho" cm)
  (princ)
)

(defun sub1_block_name (bbase_cctv / @p1 var pt1 pt2 pt3 pt4 &kw &k1 &kw #k1 @p2 dis1 i &k2 #k2 @p3 dis2 &k1_p1 &k1_p2 &k2_p1 &k2_p2)
  (setvar "cmdecho" 0)
  (setq @p1 bbase_cctv)
  (setq sub1_block_name1 "12345")
  (setq var 3000)
  (setq	pt1 (polar @p1 (/ pi 4) var)
	pt2 (polar @p1 (/ (* pi 3) 4) var)
	pt3 (polar @p1 (/ (* pi 5) 4) var)
	pt4 (polar @p1 (/ (* pi 7) 4) var)
  )
  (if cctv_name_list
    (setq &kw (ssget "CP" (list pt1 pt2 pt3 pt4) (list (cons 8 cctv_name_list) (cons 0 "TEXT"))))
  )
    ;(setq &kw (ssget "CP" (list pt1 pt2 pt3 pt4) (list (cons 0 "TEXT"))))

  (if (/= &kw nil)
  (progn  
  (if (> (sslength &kw) 1) 		;如果有选择了文字    
    (progn
      (setq &k1	 (ssname &kw 0))		;取得第一个图元
	    (vla-GetBoundingBox (vlax-ename->vla-object &k1) '&k1_p1 '&k1_p2)
      (setq
	    &k1_p1 (vlax-safearray->list &k1_p1)
	    &k1_p2 (vlax-safearray->list &k1_p2);取得文字属性列表
	    @p2	 (list (/ (+ (car &k1_p1) (car &k1_p2)) 2) (/ (+ (cadr &k1_p1) (cadr &k1_p2)) 2) 0)	;取得文字坐标
	    dis1 (distance @p1 @p2)	;取得文字坐标与点的距离
	    i	 1
      )
      (repeat (- (sslength &kw) 1)	;循环判断
	(setq &k2  (ssname &kw i))	;取得下一个文字
	      (vla-GetBoundingBox (vlax-ename->vla-object &k2) '&k2_p1 '&k2_p2)
	(setq
	      &k2_p1 (vlax-safearray->list &k2_p1)
	      &k2_p2 (vlax-safearray->list &k2_p2);取得文字属性列表
	      @p3   (list (/ (+ (car &k2_p1) (car &k2_p2)) 2) (/ (+ (cadr &k2_p1) (cadr &k2_p2)) 2) 0)
	      dis2 (distance @p1 @p3)
	)
	(if (< dis2 dis1)
	(progn
	  (setq &k1 &k2)
	  (setq dis1 dis2)
	  )
	)				;如果这个文字距离比第一个近就选择这个
	(setq i (+ i 1))
      )
      ;(setq &k2 (ssadd) &k2 (ssadd &k1 &k2));加入选择集
      ;(sssetfirst nil &k2);亮显最近的文字
      	(setq sub1_block_name1 (cdr (assoc 1 (entget &k1))))
    )
    )
    (if (= (sslength &kw) 1)
        (progn
    	  (setq &k1 (ssname &kw 0))
     	  (setq sub1_block_name1 (cdr (assoc 1 (entget &k1))))
	)
    )
  )
  )
  (if (= &kw nil) (command-s "CIRCLE" @p1 500))
  (prin1)
)

;;;获取摄光交箱图块名称
(defun sub1_block_name_gjx (bbase_gjx / @p1 var pt1 pt2 pt3 pt4 &kw &k1 #k1 @p2 dis1 i &k2 #k2 @p3 dis2)
  (setvar "cmdecho" 0)
  (setq @p1 bbase_gjx)
  (setq sub1_block_name2 "45678")
  (setq var 5000)
  (setq	pt1 (polar @p1 (/ pi 4) var)
	pt2 (polar @p1 (/ (* pi 3) 4) var)
	pt3 (polar @p1 (/ (* pi 5) 4) var)
	pt4 (polar @p1 (/ (* pi 7) 4) var)
  )
  (setq &kw (ssget "CP" (list pt1 pt2 pt3 pt4) (list (cons 0 "TEXT"))))
  (if (/= &kw nil)
  (progn  
  (if (> (sslength &kw) 1) 		;如果有选择了文字    
    (progn
      (setq &k1	 (ssname &kw 0))		;取得第一个图元
	    (vla-GetBoundingBox (vlax-ename->vla-object &k1) '&k1_p1 '&k1_p2)
      (setq
	    &k1_p1 (vlax-safearray->list &k1_p1)
	    &k1_p2 (vlax-safearray->list &k1_p2);取得文字属性列表
	    @p2	 (list (/ (+ (car &k1_p1) (car &k1_p2)) 2) (/ (+ (cadr &k1_p1) (cadr &k1_p2)) 2) 0)	;取得文字坐标
	    dis1 (distance @p1 @p2)	;取得文字坐标与点的距离
	    i	 1
      )
      (repeat (- (sslength &kw) 1)	;循环判断
	(setq &k2  (ssname &kw i))	;取得下一个文字
	      (vla-GetBoundingBox (vlax-ename->vla-object &k2) '&k2_p1 '&k2_p2)
	(setq
	      &k2_p1 (vlax-safearray->list &k2_p1)
	      &k2_p2 (vlax-safearray->list &k2_p2);取得文字属性列表
	      @p3   (list (/ (+ (car &k2_p1) (car &k2_p2)) 2) (/ (+ (cadr &k2_p1) (cadr &k2_p2)) 2) 0)
	      dis2 (distance @p1 @p3)
	)
	(if (< dis2 dis1)
	(progn
	  (setq &k1 &k2)
	  (setq dis1 dis2)
	  )
	)				;如果这个文字距离比第一个近就选择这个
	(setq i (+ i 1))
      )
      ;(setq &k2 (ssadd) &k2 (ssadd &k1 &k2));加入选择集
      ;(sssetfirst nil &k2);亮显最近的文字
      	(setq sub1_block_name2 (cdr (assoc 1 (entget &k1))))
    )
    )  
  ;(if (and (= (sslength &kw) 1) (vl-string-search gjx_name (cdr (assoc 1 (entget (ssname &kw 0))))))
  (if (= (sslength &kw) 1) 
	(progn
    	(setq &k1 (ssname &kw 0))
   	(setq sub1_block_name2 (cdr (assoc 1 (entget &k1))))
	)
    )
  )
 )
  (prin1)
)

;获取光交箱离指定块最近的线槽距离及线槽上的点
(defun block_dist (bbase_gjx / @p1 var pt1 pt2 pt3 pt4 &kw &k1 #k1 @p2 dis1 i &k2 #k2 @p3 dis2)    
  (setvar "cmdecho" 0)
  
  (setq @p1 bbase_gjx)
  (setq var 250000 block_dist_pt nil)
  (setq	pt1 (polar @p1 (/ pi 4) var)
	pt2 (polar @p1 (/ (* pi 3) 4) var)
	pt3 (polar @p1 (/ (* pi 5) 4) var)
	pt4 (polar @p1 (/ (* pi 7) 4) var)
  )
  (setq &kw (ssget "CP" (list pt1 pt2 pt3 pt4) gllst))
  (if (/= &kw nil)
  (progn  
  (if (> (sslength &kw) 1) 		;如果有选择了文字    
    (progn  
      (setq &k1	 (ssname &kw 0)		      			;取得第一个线槽
	    #k1	 (entget &k1)					;取得线槽属性列表
	    @p2 (vlax-curve-getClosestPointTo &k1 (trans @p1 1 0))		;取得线槽距离指定点最近点坐标
	    
	    dis1 (distance @p1 @p2)				;取得文字坐标与点的距离
	    i	 1
      )
      (repeat (- (sslength &kw) 1)	;循环判断
	(setq &k2  (ssname &kw i)	;取得下一个线槽
	      #k2  (entget &k2)
	      @p3  (vlax-curve-getClosestPointTo &k2 (trans @p1 1 0))
	      dis2 (distance @p1 @p3)
	)
	(if (< dis2 dis1) 
	(progn
	  (setq &k1 &k2)
	  (setq dis1 (distance @p1 (vlax-curve-getClosestPointTo &k1 (trans @p1 1 0))))
	  )
	)				;如果这个文字距离比第一个近就选择这个
	(setq i (+ i 1))
      )
      (setq block_dist_pt (vlax-curve-getClosestPointTo &k1 (trans @p1 1 0)))
    )
 )
  (if (= (sslength &kw) 1) (setq block_dist_pt (vlax-curve-getClosestPointTo (ssname &kw 0) (trans @p1 1 0))))  
  ;(setq ppy (entlast))
  ;(command-s "erase" ppy "")
  )
  )
  (prin1)
)

;;计算多段线两个点的距离
(defun dd_dist (ent pt1 pt2)
  	(if (and (vlax-curve-getDistAtPoint ent pt1) (vlax-curve-getDistAtPoint ent pt2))
  		(abs(- (vlax-curve-getDistAtPoint ent pt1) (vlax-curve-getDistAtPoint ent pt2)))
	  	1500
	)
)


;;;获取摄像机离指定块最近的线槽距离及线槽上的点
(defun sxj_dist (bbase_gjx / var vargg min_line min_dis min_ent i &kw gg_pts  pt1 pt2 pt3 pt4 @p1 entlst
		 start_ent tmp_ent sta_pts end_pts tmp_entset tt_ent sta_tt_pts end_tt_pts min_pts onept twopt finalpt)
  (vl-load-com)
  
  (setq var 1500 vargg 20 min_line 3000 min_dis 100000 min_ent nil i 0 &kw nil gg_pts nil entlst (ssadd))
  
  (setq @p1 bbase_gjx min_entlst () i 0)
  (setq	pt1 (polar @p1 (/ pi 4) 1500)
	pt2 (polar @p1 (/ (* pi 5) 4) 1500)
  )
  (setq &kw (ssget "C" pt1 pt2 (list (cons 0 "LINE") (cons 8 tmp_layer))))
  (if &kw
    (progn
      (block_dist @p1)
      (setq single_cctv_dist (distance block_dist_pt @p1))
      (setvar "clayer" tmp_layer_xc)
      (command-s "LINE" @p1 block_dist_pt "")
    )
    (progn
      (if gg_layer
	(progn
          (setq &kw (ssget "C" pt1 pt2 (list (cons 0 "LWPOLYLINE,LINE") (cons 8 gg_layer))))
	 (if &kw
	  (progn
	  (repeat (sslength &kw)
	    (setq tmp_ent (ssname &kw i))
	   
	    	(setq tmp_pts (vlax-curve-getClosestPointTo tmp_ent (trans @p1 1 0)))
	    	(setq tmp_dis (distance tmp_pts @p1))
	    	(if (< tmp_dis min_line)
	       		(progn
            	  	(setq min_line tmp_dis)
           	  	(setq start_ent tmp_ent)
               		)
	    	)
	    
	    (setq i (1+ i))
          )  
	  (setq i 0)
          (setq entlst (ssadd start_ent entlst))
	  	
	  (setq sta_pts (vlax-curve-getStartPoint start_ent))
	  (setq end_pts (vlax-curve-getEndPoint start_ent))
	  (setq pt3 (polar sta_pts (* pi 1.25) (/ (getvar "viewsize") 5000)))
	  (setq pt4 (polar sta_pts (/ pi 4) (/ (getvar "viewsize") 5000)))
	  (command-s "_zoom" "w" (mapcar '- pt4 '(10 10)) (mapcar '+ pt3 '(10 10)))
	  (setq tmp_entset (ssget "c" pt3 pt4 (list (cons 0 "*LINE") (cons 8 gg_layer))))
	  (command-s "_u")
	  (setq tmp_entset (ssdel start_ent tmp_entset))
	  (if (ssname tmp_entset 0) (setq entlst (ssadd (ssname tmp_entset 0) entlst)))
	  (setq pt3 (polar end_pts (* pi 1.25) (/ (getvar "viewsize") 5000)))
	  (setq pt4 (polar end_pts (/ pi 4) (/ (getvar "viewsize") 5000)))
	  (command-s "_zoom" "w" (mapcar '- pt4 '(10 10)) (mapcar '+ pt3 '(10 10)))
	  (setq tmp_entset (ssget "c" pt3 pt4 (list (cons 0 "*LINE") (cons 8 gg_layer))))
	  (command-s "_u")
	  (setq tmp_entset (ssdel start_ent tmp_entset))
	  (if (ssname tmp_entset 0) (setq entlst (ssadd (ssname tmp_entset 0) entlst)))
          (repeat (sslength entlst)
          	(setq tt_ent (ssname entlst i))
          	(setq sta_tt_pts (vlax-curve-getStartPoint tt_ent))
          	(setq end_tt_pts (vlax-curve-getEndPoint tt_ent))
     
          	(block_dist sta_tt_pts)
          	(setq sta_xc_pts block_dist_pt)
          	(block_dist end_tt_pts)
          	(setq end_xc_pts block_dist_pt)
          	(if (< (distance sta_xc_pts sta_tt_pts) min_dis)
            	(progn
              		(setq min_pts sta_xc_pts)
	      		(setq min_dis (distance sta_xc_pts sta_tt_pts))
              		(setq min_ent (ssname entlst i))
	      		(setq flag_min i)
            	)
          	)
          	(if (< (distance end_xc_pts end_tt_pts) min_dis)
            	(progn
              		(setq min_pts end_xc_pts)
	      		(setq min_dis (distance end_xc_pts end_tt_pts))
              		(setq min_ent (ssname entlst i))
	      		(setq flag_min i)
            	)
          	)
         (setq i (1+ i))
        )
       (setq block_dist_pt min_pts)
       (setq onept (vlax-curve-getclosestpointto (ssname entlst 0) @p1))
       (if (= flag_min 0)
         (progn
           (setq finalpt (vlax-curve-getclosestpointto (ssname entlst 0) min_pts))
           (setq single_cctv_dist (+ (distance onept @p1) (distance min_pts finalpt) (dd_dist (ssname entlst 0) onept finalpt)))
	   (setvar "clayer" tmp_layer_xc)
           (command-s "line" onept @p1 "")
           (command-s "line" min_pts finalpt "")
         ) 
         (progn
	   (setq onept_sta (vlax-curve-getStartPoint (ssname entlst 0)))
	   (setq onept_end (vlax-curve-getEndPoint (ssname entlst 0)))
	   (setq onept_sta_dis (vlax-curve-getclosestpointto (ssname entlst flag_min) onept_sta))
	   (setq onept_end_dis (vlax-curve-getclosestpointto (ssname entlst flag_min) onept_end))
	   (if (< (distance onept_sta_dis onept_sta) (distance onept_end_dis onept_end))
	   	(setq twopt onept_sta_dis)
	     	(setq twopt onept_end_dis)
	   )
           (setq finalpt (vlax-curve-getclosestpointto (ssname entlst flag_min) min_pts))
	    
           ;(setq single_cctv_dist (+ (distance onept @p1) (distance min_pts finalpt) (dd_dist (ssname entlst 0) onept twopt) (dd_dist (ssname entlst flag_min) twopt finalpt)))
	   (setq single_cctv_dist (+ (distance onept @p1) (distance min_pts finalpt) (dd_dist (ssname entlst 0) onept twopt) (dd_dist (ssname entlst flag_min) twopt finalpt)))
	   ;(command-s "line" twopt finalpt "")
	 
	   (setvar "clayer" tmp_layer_xc)
           (command-s "line" onept @p1 "")
           (command-s "line" min_pts finalpt "")
         )
       )
      )  
     (progn
       (block_dist @p1)
        (setq single_cctv_dist (distance block_dist_pt @p1))
        (setvar "clayer" tmp_layer_xc)
        (command-s "LINE" @p1 block_dist_pt "")
     )
     )
    )
   (progn
      (block_dist @p1)
      (setq single_cctv_dist (distance block_dist_pt @p1))
      (setvar "clayer" tmp_layer_xc)
      (command-s "LINE" @p1 block_dist_pt "")
    )
  )
  )
 )
)


;;;过点打断线
(defun break_line (pt / ss_lst i line line_pt line_pte pt1 pt2 pt3 pt4)
  (vl-load-com)
  (setq ss_break pt var 20)
  (setq	pt1 (polar ss_break (/ pi 4) var)
	pt2 (polar ss_break (/ (* pi 3) 4) var)
	pt3 (polar ss_break (/ (* pi 5) 4) var)
	pt4 (polar ss_break (/ (* pi 7) 4) var)
  )
  (setq ss_lst (ssget "_c" pt1 pt3 gllst)
        i 0 line_pt_lst ())
  (repeat (sslength ss_lst)
      (setq line (ssname ss_lst i))
      (setq line_pt (vlax-curve-getStartPoint line))
      (setq line_pte (vlax-curve-getEndPoint line))
      (setq line_pt_lst (append (list (list line_pt line_pte)) line_pt_lst))
      (setq tmp_layer_breakline (cdr (assoc 8 (entget line))))
      (command-s "break" line ss_break ss_break)
      (setq i (1+ i))
  )
)
  
;;;过点合并线	 
(defun join_line (line_pt_lst / ss_lst i pt1 pt2 pt3 pt4)
  (setq var 20)
  (setq	pt1 (polar ss_break (/ pi 4) var)
	pt2 (polar ss_break (/ (* pi 3) 4) var)
	pt3 (polar ss_break (/ (* pi 5) 4) var)
	pt4 (polar ss_break (/ (* pi 7) 4) var)
  )
  (setq ss_lst (ssget "_c" pt1 pt3 gllst)
	i 0)
  (setvar "clayer" tmp_layer_breakline)
  (repeat (length line_pt_lst)
  
    (command-s "line" (car(car line_pt_lst)) (cadr(car line_pt_lst)) "")
    (setq line_pt_lst (cdr line_pt_lst))
   
  )
  ;(repeat (sslength ss_lst)
    (command-s "erase" ss_lst "")
    ;(setq i (1+ i))
  ;)
  ;(command-s "_.undo" "_end")
)

;;;end_drawlist按所属光交箱分类
(defun sub1_fenlei (lst / i end_drawlist tmp_lst tmp_gjx)
  (setq i 0 end ())
  (setq end_drawlist lst)
  (while end_drawlist
    (setq tmp_lst (list (car end_drawlist)))
    (setq tmp_gjx (nth 3 (car tmp_lst)))
    (setq end_drawlist (cdr end_drawlist))
    (repeat (length end_drawlist)   
      (if (= (nth 3 (nth i end_drawlist)) tmp_gjx)
	(progn
	  (setq tmp_lst (cons (nth i end_drawlist) tmp_lst))
	  (setq end_drawlist (vl-remove (nth i end_drawlist) end_drawlist))
	)
	(setq i (1+ i))
      )     
    )
    (setq end (append (list tmp_lst) end))
    (setq i 0)
  )
)

;;插入摄像机图块
(defun sub1_insert_block (pts name / tmp_block p1 p2 gj_pts gj_jidian)
   (command-s "insert" name pts "" "" 0)
   (setq tmp_block (entlast))
   (vla-GetBoundingBox (vlax-ename->vla-object tmp_block) 'p1 'p2)
   (setq p1 (vlax-safearray->list p1) p2 (vlax-safearray->list p2))
   (setq gj_heigh (- (cadr p2) (cadr p1)))
   (setq gj_length (- (car p2) (car p1)))
   (setq gj_pts (polar p1 (/ pi 2) (/ gj_heigh 2)))
   (setq gj_jidian (cdr (assoc 10 (entget tmp_block))))
   (command-s "move" (entlast) "" gj_pts gj_jidian)
   (prin1)
)

(defun sub1_vl-string->number(string_nu / nu0 nu1 nu2 nu2 number num)  ;;;子程序实现字符串中找出数字
   (setq num (vl-string->list string_nu) nu0 (vl-string->list ".0123456789"))   ;;;num为输入字符串转为的列表，nu0是数字小数点的列?
   (setq nu2(length num))
   (repeat nu2
     (setq nu1(car num))     ?;;提取输入字符串第一个字符
     (setq nu3 (member nu1 nu0) num (cdr num)   ;;;num减少一个输入字符串,nu3如为数字通过赋值nu0关键元素以后的元素保证不为0
   )  
   (if(/= nu3 nil)
    (setq number(cons nu1 number))
   )
   )
   (vl-list->string (reverse number))
)

(defun sub1_draw_sys (htd lst / cm os  htd_pt1 htd_pt2 htd_pt3 htd_p1 end_p1 tmp_drawblock tmp_drawblockname tmp_textpts tmp_textstr tmp_namepts i m)
   (setq cm (getvar "cmdecho") os (getvar "osmode"))
   (setvar "cmdecho" 0) (setvar "osmode" 0)
   (setvar "clayer" "0")
   (setq end lst i 0 m 0)
   (repeat (length end)
      (setq tmp_draw (nth m end))
      (setq tmp_draw (vl-sort tmp_draw '(lambda (e1 e2) (< (atof(sub1_vl-string->number(caddr e1))) (atof(sub1_vl-string->number(caddr e2)))))))
      (setq htd_p1 (polar htd (/ pi -16.4) 1019))
      (repeat (length tmp_draw)
        (setq end_p1 (polar htd_p1 0 6000))
        (command-s "pline" htd_p1 end_p1 "")
        (setq tmp_drawblock (nth i tmp_draw))
        (setq tmp_drawblockname (nth 1 tmp_drawblock))

        (setq tmp_textpts (polar htd_p1 0.2 250))
        (setq tmp_textstr (strcat "摄像机光电缆" "-" (rtos (fix (/ (nth 0 tmp_drawblock) 1000)) 2 0) "m"))
	
        (sub1_insert_block end_p1 tmp_drawblockname)
	(setq tmp_namepts (polar end_p1 0 (+ gj_length 100)))
        (command-s "text" tmp_textpts 250 0 tmp_textstr)
	(command-s "text" tmp_namepts 250 0 (nth 2 tmp_drawblock))
        (setq i (1+ i))
	(if (> (+ gj_heigh 100) 700)
	  (setq htd_p1 (polar htd_p1 (* pi 1.5) (+ gj_heigh 100)))
          (setq htd_p1 (polar htd_p1 (* pi 1.5) 700))
	)
     )
     (setq htd_pt1 (polar htd (* pi 1.5) (+ (- (cadr htd) (cadr htd_p1)) 200)))
     (setq htd_pt2 (polar htd_pt1 0 1000))
     (setq htd_pt3 (polar htd_pt2 (* pi 0.5) (+ (- (cadr htd) (cadr htd_p1)) 200)))
     (command-s "pline" htd htd_pt1 htd_pt2 htd_pt3 "c")
     (command-s "text" (polar htd (* pi 0.7) 400) 250 0 (nth 3 (car tmp_draw)))
     (setq m (1+ m) i 0)
     (setq htd (polar htd 0 12000))
   )
   (setq gj_heigh nil)
   (setvar "cmdecho" cm) (setvar "osmode" os)
)

;;;打断所有线
(defun breakobj ( / temp_xjdlst m n i temp_L1 temp_L2 temp_xjd ss)
   (vl-load-com)
   (setq ss (ssget "c" ml_p1 ml_p2 (list (cons 0 "LINE") (cons 8 "test1"))))
   (setq m 0 n 0 i 0 temp_xjdlst ())   
   (repeat (sslength ss)
     (setq temp_L1 (ssname ss m))
     (setq temp_xjdlst (list (vlax-curve-getStartpoint temp_L1) (vlax-curve-getEndpoint temp_L1)))
     (repeat (sslength ss)
        (if (/= m n) (setq temp_L2 (ssname ss n)))
        (setq temp_xjd (ACET-GEOM-INTERSECTWITH temp_L1 temp_L2 0))
        (if (and temp_xjd (null (member (car temp_xjd) temp_xjdlst))) (setq temp_xjdlst (cons (car temp_xjd) temp_xjdlst)))
        (setq n (1+ n))
     )
     (setq temp_xjdlst (mapcar '(lambda (x) (list (vlax-curve-getdistatpoint temp_L1 x) x)) temp_xjdlst))
     (setq temp_xjdlst (vl-sort temp_xjdlst '(lambda (e1 e2) (< (car e1) (car e2)))))
     (setq temp_xjdlst (mapcar '(lambda (x) (cadr x)) temp_xjdlst))
     (setvar "clayer" tmp_layer)
     (repeat (1- (length temp_xjdlst))
        (command-s "line" (nth i temp_xjdlst) (nth (1+ i) temp_xjdlst) "")
        (setq i (1+ i))
     )
     (setq m (1+ m) i 0 n 0)
   )
   (repeat (sslength ss)
     (command-s "erase" (ssname ss i) "")
     (setq i (1+ i))
   )
  (prin1)
)

;;关闭未选图层
(defun gbtc(/ tcmb layers)
   (vl-load-com)
   (command-s "_undo" "be")
   
  (setq layers (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
  (setq hiddenLayers '()) 
  (vlax-for layer layers
    (if (=  (vla-get-LayerOn  layer) ':vlax-false)
      (progn
        (setq layerName (vla-get-name  layer))
        (setq hiddenLayers (cons layerName hiddenLayers))
      )
    )
  )
   (setq tcmb nil)
   (vlax-for layer layers
       (setq tcmb (cons (list (vla-get-name layer) layer) tcmb))
   )
   
  
   (if (/= bltc nil)
	(progn	    
	    (foreach ent bltc
	       (setq tcmb (vl-remove (assoc ent tcmb) tcmb))   
            )
            (foreach tcm tcmb
              (vla-put-LayerOn (cadr tcm) :vlax-false)
            )
        )
   )
   (command-s "_undo" "e")
   (princ)
)



;;打开关闭图层
(defun dktc(/ tcmb)
   (vl-load-com)
   (command-s "_undo" "be")

   (setq tcmb nil)
   (vlax-for layer (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object)))
   (if (and (= (vla-get-LayerOn layer) ':vlax-false) (null(member (vla-get-name layer) hiddenLayers)))
     
      (vla-put-LayerOn layer :vlax-true)
      
   )
   )
  (command-s "_undo" "e")
  (princ)
)





(vl-load-com)
(command "opendcl")
;(dcl_project_load "D:\\OPENDCLsoft\\drawCCTV.odcl" T)
 
(setq dclfile '("YWt6AzzXAADO6hAYBuKTIyMSa7lrQJWGGXP4RHfb3izG+kHpuA6BEiElKz3/2Mp7ZaX2PA+18wF8"
"SnjSuBfuP89ra3dX7xJqdO+kedkU1To0JVyL+SjrbNvTOut5jO65Opzf6+40pEA6Fzp2kLLHqaCj"
"DdGRsgQZA5kQuHbGoaOjp4vgh7BHTZmy4rjnBU3ugRVxvHGeTwZkCM9BeZ5eonl+Eal/Us+meQi8"
"uCg2PmVyPHj4Hv4DEBjZ2V5w05It5GMXHaGMkZyusQ5NRZEP+EyShQAIq15FUXtTklGgDvjJtq9u"
"RV17NwjCe6WfE7mPLUGFQfuTjuiGLwCpCKiYXkVVv8A3zoF5pycrrr5hDFeO4UseJZLlgnHFM7Pd"
"DuSDEFmnl4gLBh6INoBqxXnUGcGc86adhmGAohzxF8F3S+CdcUa5oR7hkoVSkYsBDvAdhcFdhol6"
"geiexZoHSAYAZwUoQoVQwICyl7dxRZPBBfD9o5rKqbrwpuE+kxINBoLZeoHunk/8GXYA6Z7Zd8A8"
"jo16qSQggk9HAPgepQUAvQyI13+PGFSXhH3SDYo9UfJMB+H+7wdaIHuIoqWQDKBogawgPKDANYQG"
"sFexc2WIz+W7pqGd0llwzUFPprOAQ5iXWykHUQdulJnQ5c8J7kCLJ4xqhWNinUyim55noFkpRTH3"
"qQrtlamYx+6B20d3CHGgpqEP8olk8MBAguP3qbAMGgHrystQgQgZ0bOBpvHuSW9VxjnQgaNdEQ5t"
"B8gweSGdEZgl2oGRWwZhv6FaZlKRjS1AEl+xnZy5VIAQeTcBZeBFYbHN4V6GmqKLqPmCw+JYucEx"
"jv7Jg1ur3iLjVBVViRoPCAjk46+w8I3Bs76lMc6NDXTkVZUYCBv833oAtM5VpI7wDxKHT4iRXtIJ"
"1ZtUieXJKCoHSteLXIWqvlMwYo+96qdzeaYDsU4gkfrZpsHdMt3hHAkx/KOeB8MleN591lulcPB7"
"pnCNy8p/xKbZI/LAWaYRjXrkaMX4VtWh7rCMVfwBM47VXwnQmTaFkbHDS/Xur7ywgREMgxPWjZ7z"
"moSBRrH9uZvLF/idJNFfAJfHVRmBh6PbHUlQpCn3qAfznmwEZo98io5UAKMbrNA1jTAlILNFdCCz"
"xO+dAW+TnKwQBVLRCKVxFMtlQNFxk7iR8eKk8C7wLTewVwdXSTz/6tUelDAJboecj26ahUzvkU6G"
"7ZF27xFt6inABUYo3IDsOIM310EarUi6zBI0V20vtPY2PDpACqlfVsouS9m4uLAxwKy9ghxzUyr4"
"Fs4Jv9W/4PmsZNL3Fs5vrro8S11SnqqsHDIW3fKkwk44y2dAP6ZgIRMRQWLjxAKdgsP9RYXctlZT"
"A8Bbvse6AURW3abN4v5Z73fZZ12k+owLs/ijn6PN5La6W4HJR5AOjhTwpfbyNYp/EjHrIYZZmUeg"
"mbFTlG0hDRhDjkDQQEw7hLbxho/rsJJ54OFawSnkyjGDsmqHwyS2i1uEoiuOiMcl3t6xy738hSLo"
"nRsAK8zJXbCE4cXf4bfRvCqMyTeFKjthWMbvnsBMBs8lbrUEz3cbIBK8JiN3b3tl/ym9r8lwnaIQ"
"eySiDS4khZdIdLE84C/lb7RMVNDyLw+9VnbjLwnxcgvBp0VM2p3CqAuOTBTRmxuqE9YuOBM8VrlW"
"YGYovVag7B8qpvLUAx+MMIkFI/nwlrRU4mMMz5Sx+isDKPvMFF08Jygb6s9UHSZ7NCeLIVYeK4O5"
"WdkFAAiX282IsqFE3w0VGYPy8KIEZ8KiZFiihH40DRV0wqLM04wMlXKax8t/31LJdBhMabJTTen3"
"JmPJiCV7otT3i3IgsBuvKlzZZHqI9FkgxkcLwrLitDzgLXFf6psq1Uwwy2Qw/yww82qfaskpr5tq"
"hMNQMCOu/y1CRM5g31ZErjTZC4uRg2tIa4VX8sAe6q45ilt4IHAAvkdqXoQM8CFhXoRs9JFnXoRM"
"mQW4brSkt2hB5AV8dEuJ3bmV2bTZx68lV9/H0f/JqYpvs4Nw6uhMuKoiLLjmJWdLiXvwa0dIU7jP"
"f88UODDTPVK118p0MMtCK0WAt3djR2v9XOSmu9cf/0lqGzDbc9k+7C0Vb7UMVGtO3dHo8U7ADwix"
"rs2xgA9a9B1yowyEzN7wK1iba89y1cD+JugrwAyC9bJNqQ8oC2IF2oFpfT2jBIzUFqjo8RIBo5jm"
"zd7wKVilYc/2KYIQFW4B2x7YugfPAtg6wrAKRe8S4h7BXYPeXYeFa/OSTZpHOnKDbMfR9c9Fdh0P"
"8ohOHy5NvyW3g4E8zvWP0QzC4E4Q5Q2C3Y+sZF+SwjW6QEp8OOpXGTtiI2BMOYU3hWl22G0KPfx3"
"xeHqB/yQD5GDZIan4TnC85T6pMPGhOJtONJxllB9q/ppUMAdSuqmIRAF0QphkGSBj1HBCUzuSD7S"
"HiBxmcQ1zkUwpoNxjsxtkw13iCN8UYXUK6adHoipJrIc4JYUuszg1tRCtvq3/eotFkDMyNJ8w/1P"
"jqKgb0SEgj+5rEYICJkSF56NXdkoX+olYamQObiPXEyYHcWWtGYpFscawjl9Z9UnrhasKG/1Emrx"
"AQFwz1aAo2wGJy31J6ianrT3292bTc+KYnBP6GD0UChvNSH3LrCgGIKmJeLFXxU1xjH7R8Vvysjf"
"sucWHxMzREUcZxXFfM+2pPTdmUBKRhS4bktGHN2/vfdAeKqeP+PIyg+kYsJmTThxXOjNyNM/CVqf"
"qEi0h37EkzbseytoqVqaXegfKvBeDPIUL1pg7ShWXWB7Z/yb5eVtG3ac1FfpkIRVBgup6d/KkKxP"
"PW5DpoVv9EImD9DkcXFoodvn/bafbkOmLqiW6qf9M2dxqL/1HxFlb7WnpcpcBRUs8Bu63V6gJ8Dy"
"gs4NYsvzCQB432LhAhv2sWmR8tDy3WWRSqef9hsYHuldaIfRN83z3cUDs2PxdPQJXFC9Jt3vlrCU"
"/h1zonNG9I+wG3abkhM/s+t71BHNdn8P1oUVqkeFFdJigSERaoNlU4mmnuK0F1tHMIfULSgaEdxE"
"GRHc1rQnuYP3qlY8zVtMHGdByHfPtvGLy4ScrURx1eURarsHpvpncEdGGoPeCyM7dSU1zhENqXEe"
"p6QYCEjRleq4qCwG0mdVUIyk9bOSZtG85rxMxoIgF6hMoZEFs3yUVFhvtagripvx3pvR7mQOqfao"
"lX3RaJ3XFNvKagRjUFP7IDdaFL7b82FcmfkOSFtutcw3q2TJyiedWFvc/kuetMV1LS9BsLlQMmZx"
"Ud+/Ety/DuF1Y4gwiC2HSdsSyYHqJ0jLJuJUIaDs9i0R/ukteXbZzjgzWwy0MZuh6sGipOYrVhvD"
"XdfcQ18pdkoZaMIssNZm3xSfhdsyENPCkZnZgBE0J4Py6icO2qQwa68eayxB/fGUo+RyFdjHSmfB"
"zuxKp+LD5NlwjkKibClRCzdldNvTi7S1F1ydL34hTWttnuuP1Ey1RnWU0utXCfwmWBxMMWOeBEQv"
"00oJY8dov+Y38g5sCZh0bk2/Zhb+5jdoPK6dfxxY5jHEslG5dcXc9+S+HJd2EAvJXOjLxMAcfTvS"
"0JhHqz85923e6aMrRzRKb3y6TAiJ9fa7CCnCbc3PxdIzLsQYcqkqObaQHFv9vgipWhHFjhymoyJI"
"7dKjUybZwx5LECgUtfa2+ekNbXTC4nSmXwFkwiQLqtnZ/anFyp45LiAxWXUHIBM/VrXV/IvmdZjr"
"J+/3JmPR9BpoK6FDTKrTFbOO5Z4bKuRONGo5qlPF9jkM7y/lb7RMVNvyLxj3YZwuZyuFvJrjkInE"
"cGOv6ytG1Zocq7F31Yh0kurI0qT6Lugre5XQaLJUzYx0D9ZnUcvDgw6KBonT1wV5CYh7H2ucbiZL"
"Hwcn+4trItKkc9sn2yWjT8DIbCfLf99S1SqbGNgyENgUKDM+f9UUFDDz8+PhAtuZixnctlde4gRv"
"Sp+qPXabRusq6OXnSsfn1Ef7RdQrpvDmrforAE1fDrETadXQPQcn6/tC1ahd6hvr1DLM2rdaLbPU"
"XMXEFSEJSFbVCqKnYydrPVa1VCFHGruY3Z6WJfmJI6qtYW+TuUdy2xvSnbYn62xKRxr2GxynlW71"
"Hac0qaPdAPccUuUOzKJHGh8pS4u1D0tHGhibw3VR+PAm9SbSXZxUC3qQ62E/Y09zLmbnc95dTuxl"
"PjkeMOI1OaL6jJT2Utaubw/dXWLCZsuqDQtAOsuqu2cPdBXXnSI+ahXX/Sn9wr1L/rwklOkGBX8U"
"1/nTL26FrlZFRkjLqr3jLlcoNUM5iizLqgMbtvQU190JNaiCxxXXpU0x4FXDmBHPAngvQwvgQpYY"
"hhN1ApN9T3PeQAW10eERlNf1zToNyzkamc0k6zPu5lufTFyMTwX2xYMoKkMLryRmL1NMYonGCjKo"
"LN4dKrpNmdepT3HZJHyQvNUHO4veLKgTeSCiGyXvbErXjPYbGP4MS0U6Bc3IeaAqzaNypWMiR0t4"
"roQrviKjcFbcU1UWE+QQhZYcc0MQDQ1LNhoT0NQrZzfNVD3LYUUh4//i+d2VgI2taHq0Ca6ztbc8"
"l6RjOwdbbd1TQxzXDThSL+vKEN3oUtaMGW0Ye8HCQeXNhlwBSB0CWndh907MokdxxIc2OwxgAnH1"
"TBgmmp7ui32Bh0H3tzLm"))
(dcl_project_import dclfile)
(dcl_form_show drawCCTV_form1)
;将字符串根据其中的","分开成不同的字符串，并列入新的列表中
(defun split-string (str / start end index substring lst)
  (setq lst '()) ; 新建一个空列表
  (setq start 1)
  (setq end (strlen str))
  (while (< start end)
    (setq index (vl-string-position (ascii ",") str start nil))
    (if (null index)
      (progn
        (setq substring (substr str start))
	(setq start (strlen str))	
      )
      (progn
	(setq substring (substr str start (1+(- index start))))
	(setq start (+ index 1))
      )
     )
    (setq substring (vl-string-subst "" "," substring))
    (setq lst (cons substring lst)) ; 将子字符串添加到列表
  )
  lst
 )
(defun clean_creen (/ clean_set i tmp)
	;(command-s "-osmode" "0")
       	(setq i 0)
       	(if (or (/= (tblsearch "Layer" "test1") nil) (/= (tblsearch "Layer" "cc_xc11") nil) (/= (tblsearch "Layer" "test2") nil))
	  (setq clean_set (ssget "x" (list (cons 8 (strcat "test1,cc_xc11,test2"))))))
       	(if clean_set
	 (repeat (sslength clean_set)
	   (setq tmp (ssname clean_set i))
	   (command-s "erase" tmp "")
	   (setq i (1+ i))
	 )
       )
  )

(defun hc-string (str / tmplst str)
	(setq tmplst str)
  	(setq str (car tmplst) tmplst (cdr tmplst))
   	(while tmplst
      	   (setq str (strcat str "," (car tmplst)))
      	   (setq tmplst (cdr tmplst))
  	 )
  	str
  )


(Berni_Start)

(setq bltc ())
(defun c:drawCCTV/Form1/TextButton16#OnClicked (/ cctv_lst cctv_tmplst cctv_list cctv_tk_list tmp_sxjblock )

  (setq cctv_lst (ssget  '((0 . "INSERT"))) cctv_tmplst () cctv_list nil cctv_tk_list ())
  
  (if (/= cctv_lst nil)
    (progn
  	(repeat (sslength cctv_lst)
      	   (setq tmp_sxjblock (cdr (assoc 2 (entget (ssname cctv_lst 0)))))
	   (if (null (member (cdr (assoc 8 (entget (ssname cctv_lst 0)))) bltc)) (setq bltc (cons (cdr (assoc 8 (entget (ssname cctv_lst 0)))) bltc)))
      	   (if (null (member tmp_sxjblock cctv_tmplst)) (setq cctv_tmplst (cons tmp_sxjblock cctv_tmplst)))    
      	   (ssdel (ssname cctv_lst 0) cctv_lst)
   	)
   	(setq cctv_list (car cctv_tmplst) cctv_tmplst (cdr cctv_tmplst))
   	(while cctv_tmplst
      	   (setq cctv_list (strcat cctv_list "," (car cctv_tmplst)))
      	   (setq cctv_tmplst (cdr cctv_tmplst))
   	)
  	(setq cctv_tk_list (split-string cctv_list))
        (setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox3))
        (if allstr
	  (progn
             (if cctv_tk_list
	       (repeat (length cctv_tk_list)
	          (if (null (member (car cctv_tk_list) allstr)) (setq allstr (cons (car cctv_tk_list) allstr)))
		 (setq cctv_tk_list (cdr cctv_tk_list))
	       )
	       (setq allstr cctv_tk_list)
	     )
	  )
	  (setq allstr cctv_tk_list)
	) 
  	(dcl-Control-SetList drawCCTV/Form1/ListBox3 allstr)
    )
  )
)

(defun c:drawCCTV/Form1/ListBox3#OnDblClicked (/ )
  (setq choose_str (car(dcl-ListBox-GetSelectedItems drawCCTV/Form1/ListBox3)))
  (setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox3))
)


(defun c:drawCCTV/Form1/TextButton17#OnClicked (/ )
  	(setq allstr (vl-remove choose_str allstr))
        ;(setq bltc (vl-remove choose_str bltc))
	(dcl-Control-SetList drawCCTV/Form1/ListBox3 allstr)
)

(defun c:drawCCTV/Form1/TextButton2#OnClicked (/ cctv_name_layer cctv_name_tmplst cctv_name_list )
	(setq cctv_name_layer (ssget '((0 . "TEXT")))  cctv_name_tmplst () cctv_name_list nil)
  
        (if cctv_name_layer
	  (progn
	     (repeat (sslength cctv_name_layer)
      	        (if (null (member (cdr (assoc 8 (entget (ssname cctv_name_layer 0)))) cctv_name_tmplst))
		  (setq cctv_name_tmplst (cons (cdr (assoc 8 (entget (ssname cctv_name_layer 0)))) cctv_name_tmplst)))
	        (if (null (member (cdr (assoc 8 (entget (ssname cctv_name_layer 0)))) bltc))
		  (setq bltc (cons (cdr (assoc 8 (entget (ssname cctv_name_layer 0)))) bltc)))
      	       
      	        (ssdel (ssname cctv_name_layer 0) cctv_name_layer)
   	     )

   	     (setq cctv_name_list (car cctv_name_tmplst) cctv_name_tmplst (cdr cctv_name_tmplst))
   	     (while cctv_name_tmplst
      	        (setq cctv_name_list (strcat cctv_name_list "," (car cctv_name_tmplst)))
      	        (setq cctv_name_tmplst (cdr cctv_name_tmplst))
  	     )
	  )
	)
  	(setq cctv_name_list (split-string cctv_name_list))
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox1))
        (if allstr
	  (progn
             (if cctv_name_list
	       (repeat (length cctv_name_list)
	         (if (null (member (car cctv_name_list) allstr)) (setq allstr (cons (car cctv_name_list) allstr)))
		 (setq cctv_name_list (cdr cctv_name_list))
	       )
	       (setq allstr cctv_name_list)
	     )
	  )
	  (setq allstr cctv_name_list)
	) 
  	(dcl-Control-SetList drawCCTV/Form1/ListBox1 allstr)
)

(defun c:drawCCTV/Form1/ListBox1#OnDblClicked (/ )
  	(setq choose_str (car(dcl-ListBox-GetSelectedItems drawCCTV/Form1/ListBox1)))
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox1))
)

(defun c:drawCCTV/Form1/TextButton1#OnClicked (/ )
  	(setq allstr (vl-remove choose_str allstr))
  	(setq bltc (vl-remove choose_str bltc))
	(dcl-Control-SetList drawCCTV/Form1/ListBox1 allstr)
)

(defun c:drawCCTV/Form1/TextButton9#OnClicked (/ gk_lst gk_tmplst tmp_gkblock )
  	(setq gk_lst (ssget  '((0 . "INSERT"))) gk_tmplst () gjx_ksh nil tmp_gkblock nil)
  	(if gk_lst
	  (progn
	     (repeat (sslength gk_lst)
      	   	(setq tmp_gkblock (cdr (assoc 2 (entget (ssname gk_lst 0)))))
	   	(if (null (member (cdr (assoc 8 (entget (ssname gk_lst 0)))) bltc)) (setq bltc (cons (cdr (assoc 8 (entget (ssname gk_lst 0)))) bltc)))
      	   	(if (null (member tmp_gkblock gk_tmplst)) (setq gk_tmplst (cons tmp_gkblock gk_tmplst)))    
      	   	(ssdel (ssname gk_lst 0) gk_lst)
   	     )

   	     (setq gjx_ksh (car gk_tmplst) gk_tmplst (cdr gk_tmplst))
   	     (while gk_tmplst
      	        (setq gjx_ksh (strcat gjx_ksh "," (car gk_tmplst)))
      	        (setq gk_tmplst (cdr gk_tmplst))
  	     )
	  )
	)
  	(setq gjx_ksh (split-string gjx_ksh))
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox5))
        (if allstr
	  (progn
             (if gjx_ksh
	       (repeat (length gjx_ksh)
	          (if (null (member (car gjx_ksh) allstr)) (setq allstr (cons (car gjx_ksh) allstr)))
		 (setq gjx_ksh (cdr gjx_ksh))
	       )
	       (setq allstr gjx_ksh)
	     )
	  )
	  (setq allstr gjx_ksh)
	) 
  	(dcl-Control-SetList drawCCTV/Form1/ListBox5 allstr) 	
)

(defun c:drawCCTV/Form1/ListBox5#OnDblClicked (/ )
  	(setq choose_str (car(dcl-ListBox-GetSelectedItems drawCCTV/Form1/ListBox5)))
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox5))
)

(defun c:drawCCTV/Form1/TextButton10#OnClicked (/ )
  	(setq allstr (vl-remove choose_str allstr))
  	;(setq bltc (vl-remove choose_str bltc))
	(dcl-Control-SetList drawCCTV/Form1/ListBox5 allstr)
)

(defun c:drawCCTV/Form1/TextButton3#OnClicked (/ ent_xc wireway_layer_tmplst wireway_layer)
  	(setq ent_xc (ssget '((0 . "MLINE"))))
  	(if ent_xc
	  (progn
	     (repeat (sslength ent_xc)
	     (if (null (member (cdr (assoc 8 (entget (ssname ent_xc 0)))) wireway_layer_tmplst))
	       	(setq wireway_layer_tmplst (cons (cdr (assoc 8 (entget (ssname ent_xc 0)))) wireway_layer_tmplst)))
	     (if (null (member (cdr (assoc 8 (entget (ssname ent_xc 0)))) bltc))
	       (setq bltc (cons (cdr (assoc 8 (entget (ssname ent_xc 0)))) bltc)))
      	       
      	        (ssdel (ssname ent_xc 0) ent_xc)
   	     )

   	     (setq wireway_layer (car wireway_layer_tmplst) wireway_layer_tmplst (cdr wireway_layer_tmplst))
   	     (while wireway_layer_tmplst
      	        (setq wireway_layer (strcat wireway_layer "," (car wireway_layer_tmplst)))
      	        (setq wireway_layer_tmplst (cdr wireway_layer_tmplst))
  	     )
	  )
	)
  	(setq wireway_layer (split-string wireway_layer))
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox2))
        (if allstr
	  (progn
             (if wireway_layer
	       (repeat (length wireway_layer)
	         (if (null (member (car wireway_layer) allstr)) (setq allstr (cons (car wireway_layer) allstr)))
		 (setq wireway_layer (cdr wireway_layer))
	       )
	       (setq allstr wireway_layer)
	     )
	  )
	  (setq allstr wireway_layer)
	) 
  	(dcl-Control-SetList drawCCTV/Form1/ListBox2 allstr)
)


(defun c:drawCCTV/Form1/ListBox2#OnDblClicked (/ )
  	(setq choose_str (car(dcl-ListBox-GetSelectedItems drawCCTV/Form1/ListBox2)))
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox2))
)

(defun c:drawCCTV/Form1/TextButton4#OnClicked (/ )
  	(setq allstr (vl-remove choose_str allstr))
  	(setq bltc (vl-remove choose_str bltc))
	(dcl-Control-SetList drawCCTV/Form1/ListBox2 allstr)
)

(defun c:drawCCTV/Form1/TextButton18#OnClicked (/ ent_gg gg_layer)
	(setq ent_gg (entsel))
  	(if (/= ent_gg nil)
	  (progn
	    (setq gg_layer (cdr (ASSOC 8 (entget (car ent_gg)))))
	    (setq bltc (cons gg_layer bltc))
	    (setq ent_gg_lst (list gg_layer))
	  )
	)
  	
  	(dcl-Control-SetList drawCCTV/Form1/ListBox6 ent_gg_lst)
)

(defun c:drawCCTV/Form1/ListBox6#OnDblClicked (/ )
  	(setq choose_str (car(dcl-ListBox-GetSelectedItems drawCCTV/Form1/ListBox6)))
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox6))
)

(defun c:drawCCTV/Form1/TextButton8#OnClicked (/ )
  	(setq allstr (vl-remove choose_str allstr))
  	(setq bltc (vl-remove choose_str bltc))
	(dcl-Control-SetList drawCCTV/Form1/ListBox6 allstr)
)


(defun c:drawCCTV/Form1/TextButton13#OnClicked (/ dxd_text dxd_1 dxd_2)
  	(if (null (dcl-Control-GetList drawCCTV/Form1/ListBox4)) (setq dxd_n 0 dxd_list nil))
  	(dcl-Form-hide drawCCTV_form1)
  	(setq dxd_1 (getpoint "\n输入第一个等效点："))
  	(setq dxd_2 (getpoint "\n输入第二个等效点："))
  	(setq dxd_text (dcl-Control-GetList drawCCTV/Form1/ListBox4))
  	(setq dxd_n (1+ dxd_n))
  	(setq dxd_list (cons (list (list dxd_1 dxd_2) dxd_n) dxd_list))	
  	(setq dxd_text (cons (strcat "第" (itoa dxd_n) "对等效点") (reverse dxd_text)))
  	(dcl-Form-show drawCCTV_form1)
  	(dcl-Control-SetList drawCCTV/Form1/ListBox4 (reverse dxd_text))
)

(defun c:drawCCTV/Form1/ListBox4#OnDblClicked (/ )
  	(setq choose_str (car(dcl-ListBox-GetSelectedItems drawCCTV/Form1/ListBox4))) 
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox4))
)


(defun c:drawCCTV/Form1/TextButton6#OnClicked (/ )
  	(setq allstr (vl-remove choose_str allstr))
  	(setq choose_n (atoi(sub1_vl-string->number choose_str)))
  	(setq n 0)
  	(repeat (length dxd_list)
		(if (= (cadr (nth n dxd_list))	choose_n) (setq dxd_list (vl-remove (nth n dxd_list) dxd_list)))
	  	(setq n (1+ n))
	)
	
        
	(dcl-Control-SetList drawCCTV/Form1/ListBox4 allstr)
)


(defun c:drawCCTV/Form1/TextButton12#OnClicked (/ tt_jifang text_jifang)
	(dcl-Form-hide drawCCTV_form1)
  	(setq jifang (list(getpoint "\n输入机房本层引入位置：")))  
  	(while (car jifang)
	   (setq tt_jifang (getpoint "\n输入机房本层其它引入位置："))
	   (setq jifang (cons tt_jifang jifang))
	)
  	(setq jifang (reverse jifang))
        (setq jifang (vl-remove nil jifang))
  	
  	(setq text_jifang (list(strcat (itoa (length jifang)) "个机房接入点")))
  	(dcl-Form-show drawCCTV_form1)
	(dcl-Control-SetList drawCCTV/Form1/ListBox7 text_jifang)
  	

)



(dcl-Control-SetText drawCCTV/Form1/TextBox1 "25")
(setq jf_bias (atof(dcl-Control-GetText drawCCTV/Form1/TextBox1)))
(setq jf_bias (* jf_bias 1000))

(setq tmp_layer "test1") 
(if (= (tblsearch "Layer" tmp_layer) nil) (command-s "Layer" "new" tmp_layer "color" "3" tmp_layer "")) (setq bltc (cons tmp_layer bltc))
(setq tmp_layer_xc "cc_xc11")
(if (= (tblsearch "Layer" tmp_layer_xc) nil) (command-s "Layer" "new" tmp_layer_xc "color" "4" tmp_layer_xc "")) (setq bltc (cons tmp_layer_xc bltc))
(setq cctv_coe 1.2)

(defun c:drawCCTV/Form1/TextButton14#OnClicked (/ wireway_layer_tmplst)
	(setq wireway_layer_tmplst (dcl-Control-GetList drawCCTV/Form1/ListBox2))
  	(setq wireway_layer (car wireway_layer_tmplst) wireway_layer_tmplst (cdr wireway_layer_tmplst))
   	(while wireway_layer_tmplst
      	        (setq wireway_layer (strcat wireway_layer "," (car wireway_layer_tmplst)))
      	        (setq wireway_layer_tmplst (cdr wireway_layer_tmplst))
  	)
  	(if wireway_layer
	  (progn
	    	(dcl-Form-Hide drawCCTV_form1)
	  	(setq mline_set (ssget "c" (setq ml_p1 (getpoint)) (setq ml_p2 (getcorner ml_p1)) (list (cons 0 "mline") (cons 8 wireway_layer))))
	    	(dcl-Form-show drawCCTV_form1)
	  )
	  (alert "未明确电缆槽图层")
	)
)

(defun c:drawCCTV/Form1/TextButton15#OnClicked (/ cctv_name_tmplst)
  	(setq cctv_name_tmplst (dcl-Control-GetList drawCCTV/Form1/ListBox3))
  	(setq cctv_list (car cctv_name_tmplst) cctv_name_tmplst (cdr cctv_name_tmplst))
   	(while cctv_name_tmplst
      	        (setq cctv_list (strcat cctv_list "," (car cctv_name_tmplst)))
      	        (setq cctv_name_tmplst (cdr cctv_name_tmplst))
  	)
  	(if cctv_list
	  (progn
	    	(dcl-Form-Hide drawCCTV_form1)	
	  	(setq cctv_set (ssget (list (cons 2 cctv_list))))
	    	(dcl-Form-show drawCCTV_form1)
	  )
	  (alert "未明确摄像机图块")
	)
)

(defun c:drawCCTV/Form1/TextButton5#OnClicked (/ )
	(setq gk_tmplst (dcl-Control-GetList drawCCTV/Form1/ListBox5))
  	(setq gjx_ksh (car gk_tmplst) gk_tmplst (cdr gk_tmplst))
   	(while gk_tmplst
      	   (setq gjx_ksh (strcat gjx_ksh "," (car gk_tmplst)))
      	   (setq gk_tmplst (cdr gk_tmplst))
  	)
  	(if gjx_ksh
	  (progn
	    	(dcl-Form-Hide drawCCTV_form1)
	  	(setq gjx_set (ssget (list (cons 2 gjx_ksh))))
	    	(dcl-Form-show drawCCTV_form1)
	  )
	  (alert "未明确汇聚箱图块")
	)
)

;(defun c:drawCCTV/Form1/TextButton7#OnClicked (/)
;   	(clean_creen)
        
;)


(defun c:drawCCTV/Form1/TextButton21#OnClicked (/)
	(clean_creen)
  	(drawCCTV)
)

(defun c:drawCCTV/Form1/TextButton7#OnClicked (/)
  	(dktc)
)
(defun drawkong (/)
  	(prin1)

)
(defun c:drawCCTV/Form1/TextButton23#OnClicked (/)
  	(clean_creen)
  	(drawkong)
)

(defun sxj_dist_text1 (bbase_gjx / var vargg min_line min_dis min_ent i &kw gg_pts  pt1 pt2 pt3 pt4 @p1 entlst
		 start_ent tmp_ent sta_pts end_pts tmp_entset tt_ent sta_tt_pts end_tt_pts min_pts onept twopt finalpt)
  (vl-load-com)
  
  (setq var 1500 vargg 20 min_line 3000 min_dis 100000 min_ent nil i 0 &kw nil gg_pts nil entlst (ssadd))
  
  (setq @p1 bbase_gjx min_entlst () i 0)
  (setq	pt1 (polar @p1 (/ pi 4) 1500)
	pt2 (polar @p1 (/ (* pi 5) 4) 1500)
  )
  (setq &kw (ssget "C" pt1 pt2 (list (cons 0 "LINE") (cons 8 tmp_layer))))
  (if &kw
    (progn
      (block_dist @p1)
      (setq single_cctv_dist (distance block_dist_pt @p1))
      (setvar "clayer" tmp_layer)
      (command-s "LINE" @p1 block_dist_pt "")
    )
    (progn
      (if gg_layer
	(progn
          (setq &kw (ssget "C" pt1 pt2 (list (cons 0 "LWPOLYLINE,LINE") (cons 8 gg_layer))))
	 (if &kw
	  (progn
	  (repeat (sslength &kw)
	    (setq tmp_ent (ssname &kw i))
	   
	    	(setq tmp_pts (vlax-curve-getClosestPointTo tmp_ent (trans @p1 1 0)))
	    	(setq tmp_dis (distance tmp_pts @p1))
	    	(if (< tmp_dis min_line)
	       		(progn
            	  	(setq min_line tmp_dis)
           	  	(setq start_ent tmp_ent)
               		)
	    	)
	    
	    (setq i (1+ i))
          )  
	  (setq i 0)
          (setq entlst (ssadd start_ent entlst))
	  	
	  (setq sta_pts (vlax-curve-getStartPoint start_ent))
	  (setq end_pts (vlax-curve-getEndPoint start_ent))
	  (setq pt3 (polar sta_pts (* pi 1.25) (/ (getvar "viewsize") 5000)))
	  (setq pt4 (polar sta_pts (/ pi 4) (/ (getvar "viewsize") 5000)))
	  (command-s "_zoom" "w" (mapcar '- pt4 '(10 10)) (mapcar '+ pt3 '(10 10)))
	  (setq tmp_entset (ssget "c" pt3 pt4 (list (cons 0 "*LINE") (cons 8 gg_layer))))
	  (command-s "_u")
	  (setq tmp_entset (ssdel start_ent tmp_entset))
	  (if (ssname tmp_entset 0) (setq entlst (ssadd (ssname tmp_entset 0) entlst)))
	  (setq pt3 (polar end_pts (* pi 1.25) (/ (getvar "viewsize") 5000)))
	  (setq pt4 (polar end_pts (/ pi 4) (/ (getvar "viewsize") 5000)))
	  (command-s "_zoom" "w" (mapcar '- pt4 '(10 10)) (mapcar '+ pt3 '(10 10)))
	  (setq tmp_entset (ssget "c" pt3 pt4 (list (cons 0 "*LINE") (cons 8 gg_layer))))
	  (command-s "_u")
	  (setq tmp_entset (ssdel start_ent tmp_entset))
	  (if (ssname tmp_entset 0) (setq entlst (ssadd (ssname tmp_entset 0) entlst)))
          (repeat (sslength entlst)
          	(setq tt_ent (ssname entlst i))
          	(setq sta_tt_pts (vlax-curve-getStartPoint tt_ent))
          	(setq end_tt_pts (vlax-curve-getEndPoint tt_ent))
     
          	(block_dist sta_tt_pts)
          	(setq sta_xc_pts block_dist_pt)
          	(block_dist end_tt_pts)
          	(setq end_xc_pts block_dist_pt)
          	(if (< (distance sta_xc_pts sta_tt_pts) min_dis)
            	(progn
              		(setq min_pts sta_xc_pts)
	      		(setq min_dis (distance sta_xc_pts sta_tt_pts))
              		(setq min_ent (ssname entlst i))
	      		(setq flag_min i)
            	)
          	)
          	(if (< (distance end_xc_pts end_tt_pts) min_dis)
            	(progn
              		(setq min_pts end_xc_pts)
	      		(setq min_dis (distance end_xc_pts end_tt_pts))
              		(setq min_ent (ssname entlst i))
	      		(setq flag_min i)
            	)
          	)
         (setq i (1+ i))
        )
       (setq block_dist_pt min_pts)
       (setq onept (vlax-curve-getclosestpointto (ssname entlst 0) @p1))
       (if (= flag_min 0)
         (progn
           (setq finalpt (vlax-curve-getclosestpointto (ssname entlst 0) min_pts))
           (setq single_cctv_dist (+ (distance onept @p1) (distance min_pts finalpt) (dd_dist (ssname entlst 0) onept finalpt)))
	   (setvar "clayer" tmp_layer)
           (command-s "line" onept @p1 "")
           (command-s "line" min_pts finalpt "")
         ) 
         (progn
	   (setq onept_sta (vlax-curve-getStartPoint (ssname entlst 0)))
	   (setq onept_end (vlax-curve-getEndPoint (ssname entlst 0)))
	   (setq onept_sta_dis (vlax-curve-getclosestpointto (ssname entlst flag_min) onept_sta))
	   (setq onept_end_dis (vlax-curve-getclosestpointto (ssname entlst flag_min) onept_end))
	   (if (< (distance onept_sta_dis onept_sta) (distance onept_end_dis onept_end))
	   	(setq twopt onept_sta_dis)
	     	(setq twopt onept_end_dis)
	   )
           (setq finalpt (vlax-curve-getclosestpointto (ssname entlst flag_min) min_pts))
	   ;(setvar "clayer" tmp_layer_xc)
	   ;(command-s "line" onept twopt "")
           ;(setq single_cctv_dist (+ (distance onept @p1) (distance min_pts finalpt) (dd_dist (ssname entlst 0) onept twopt) (dd_dist (ssname entlst flag_min) twopt finalpt)))
	   (setq single_cctv_dist (+ (distance onept @p1) (distance min_pts finalpt) (dd_dist (ssname entlst 0) onept twopt) (dd_dist (ssname entlst flag_min) twopt finalpt)))
	   ;(command-s "line" twopt finalpt "")
	 
	   (setvar "clayer" tmp_layer)
           (command-s "line" onept @p1 "")
           (command-s "line" min_pts finalpt "")
         )
       )
      )  
     (progn
       (block_dist @p1)
        (setq single_cctv_dist (distance block_dist_pt @p1))
        (setvar "clayer" tmp_layer)
        (command-s "LINE" @p1 block_dist_pt "")
     )
     )
    )
   (progn
      (block_dist @p1)
      (setq single_cctv_dist (distance block_dist_pt @p1))
      (setvar "clayer" tmp_layer)
      (command-s "LINE" @p1 block_dist_pt "")
    )
  )
  )
 )
)

;;等效点
(defun dxd_dist (/  dxd_nn single_dxd_ent single_dxd_ent_ptlst dxd_length dxd_tmp_zhongzhi dxd_tmp dxd_tmp_qishi dxd_tmp_xc_qishi dxd_tmp_qishi_length)    
  	(setvar "cmdecho" 0)
  	
  	(setq dxd_nn 0)
  	(setq dxd_ent nil dxd_ent_ptlst nil single_dxd_ent nil single_dxd_ent_ptlst nil)
	(repeat (length dxd_list)
	  	(setq dxd_length 0)
  		(setq dxd_tmp (car(nth dxd_nn dxd_list)))
	  	(setq dxd_tmp_qishi (car dxd_tmp))
	  	(sxj_dist dxd_tmp_qishi)
	  	(setq dxd_tmp_xc_qishi block_dist_pt)
	  	(setq dxd_tmp_qishi_length single_cctv_dist)
	  	;(breakobj)
	  	(setq dxd_tmp_zhongzhi (cadr dxd_tmp))
	  	(sxj_dist dxd_tmp_zhongzhi)
	  	(setq dxd_tmp_xc_zhongzhi block_dist_pt)
	  	(setq dxd_tmp_zhongzhi_length single_cctv_dist)
		(setq dxd_length (distance dxd_tmp_qishi dxd_tmp_zhongzhi))
		(break_line dxd_tmp_xc_qishi)
	  	(break_line dxd_tmp_xc_zhongzhi)

	  	(setvar "clayer" tmp_layer)
	  	(command-s "LINE" dxd_tmp_xc_qishi dxd_tmp_xc_zhongzhi "")
	  	;(command "line" dxd_tmp_xc_qishi dxd_tmp_xc_zhongzhi "")
	  	(sub1_tt1 dxd_tmp_xc_qishi dxd_tmp_xc_zhongzhi)    ;;判断等笑点是不是在一个平面层上
	  	;(command-s "LINE" dxd_tmp_xc_qishi dxd_tmp_xc_zhongzhi "")
	  	(if gjx_cctv_dis
		  	(if (<= dxd_tmp_qishi_length dxd_tmp_zhongzhi_length)
			     (progn
			        (setvar "clayer" tmp_layer)
			  	(command-s "LINE" dxd_tmp_qishi dxd_tmp_xc_qishi "")
				;(setq dxd_length (+ tmp_length (distance dxd_tmp_qishi dxd_tmp_xc_qishi)))
			     )
			     (progn
			        (setvar "clayer" tmp_layer)
			  	(command-s "LINE" dxd_tmp_zhongzhi dxd_tmp_xc_zhongzhi "")
				;(setq dxd_length (+ tmp_length (distance dxd_tmp_zhongzhi dxd_tmp_xc_zhongzhi)))
			     )
			)
	  	
	  		(progn
			  	(setvar "clayer" tmp_layer)
			  	(command-s "LINE" dxd_tmp_qishi dxd_tmp_xc_qishi "")
			  	(command-s "LINE" dxd_tmp_zhongzhi dxd_tmp_xc_zhongzhi "")
			  	;(setq dxd_length (+ tmp_length (distance dxd_tmp_qishi dxd_tmp_xc_qishi) (distance dxd_tmp_zhongzhi dxd_tmp_xc_zhongzhi)))
			)
	  	)
	  	(setvar "clayer" "test2")
	  	(command-s "LINE" dxd_tmp_qishi dxd_tmp_zhongzhi "")
	  	(setvar "clayer" tmp_layer)
	  	(setq single_dxd_ent (cons (list dxd_tmp_qishi dxd_tmp_zhongzhi) dxd_length))
	  	;(setq single_dxd_ent (cons (entlast) dxd_length))
		(setq single_dxd_ent_ptlst  (list dxd_tmp_qishi dxd_tmp_zhongzhi))
	  	;(setq single_dxd_ent_ptlst  (entlast))
	  	
	  	(setq dxd_ent (cons single_dxd_ent dxd_ent))
	  	(setq dxd_ent_ptlst (cons single_dxd_ent_ptlst dxd_ent_ptlst))
		;(setq ttt (list dxd_tmp_qishi dxd_tmp_zhongzhi))
	  	;(if (member (list (getpoint) (getpoint)) dxd_ent_ptlst) (princ "yes") (princ "no"))
	  	(setq dxd_nn (1+ dxd_nn))
	)
  	(breakobj)
)

(defun is-point-on-line (pt1 pt2 ent / pt3 pt4 ent11  )
  (command-s "LINE" (car ent) (cadr ent) "")
  (setq ent11 (entlast))
  (setq pt3 (vlax-curve-getClosestPointTo ent11 pt1))
  (setq pt4 (vlax-curve-getClosestPointTo ent11 pt2))
  (setq dis1 (distance pt1 pt3))
  (setq dis2 (distance pt2 pt4))
  (command-s "erase" (entlast) "")
  
  (if (and (< dis1 1) (< dis2 1))
       (setq point_flag T)
       (setq point_flag nil)
  )
)





(defun create-parameter-file ()
  	(setq file-path (strcat (getvar "DWGPREFIX") "param-drawCCTV.txt")) ; 设置文件路径为程序所在目录下的parameters.txt

  	(setq file (open file-path "w")) ; 以写入模式打开文件
	; 将cctv_list写入文件
  	(if (dcl-Control-GetList drawCCTV/Form1/ListBox3)
	  	(write-line (hc-string (dcl-Control-GetList drawCCTV/Form1/ListBox3)) file)
	  	(write-line "123"  file)
	)
	; 将cctv_name_list写入文件
  	(if (dcl-Control-GetList drawCCTV/Form1/ListBox1)
  		(write-line (hc-string (dcl-Control-GetList drawCCTV/Form1/ListBox1)) file)
	  	(write-line "123"  file)
	)
  	; 将gjx_ksh写入文件
  	(if (dcl-Control-GetList drawCCTV/Form1/ListBox5)
  		(write-line (hc-string (dcl-Control-GetList drawCCTV/Form1/ListBox5)) file)
	  	(write-line "123"  file)
	)
  	; 将wireway_layer写入文件
  	(if (dcl-Control-GetList drawCCTV/Form1/ListBox2)
		(write-line (hc-string (dcl-Control-GetList drawCCTV/Form1/ListBox2)) file)
	  	(write-line "123"  file)
	)
  	; 将gg_layer写入文件
  	(if (dcl-Control-GetList drawCCTV/Form1/ListBox6)
  		(write-line (hc-string (dcl-Control-GetList drawCCTV/Form1/ListBox6)) file)
	  	(write-line "123"  file)
	)

  	; 将bltc写入文件
  	(if bltc
  		(write-line (hc-string bltc) file)
	  	(write-line "123"  file)
	)
 
  
  	(close file) ; 关闭文件
)

(defun read-parameter-file ()
  	(setq file-path (strcat (getvar "DWGPREFIX") "param-drawCCTV.txt"))
  	(setq file (open file-path "r")) 	; 打开文档以供读取
  	(setq cctv_list (read-line file)) 	; 读取第一行内容并赋值给参数 line1
  	(setq cctv_name_list (read-line file)) 	; 读取第二行内容并赋值给参数 line2
  	(setq gjx_ksh (read-line file))
  	(setq wireway_layer (read-line file))
  	(setq gg_layer (read-line file))
  	(setq bltc (read-line file))
  	(close file) ; 关闭文档
  	(if (/= cctv_list "123")
	  (progn
		(setq cctv_list (split-string cctv_list))
	  	(dcl-Control-SetList drawCCTV/Form1/ListBox3 cctv_list)
	    	
	  )
	  	(setq cctv_list nil)
	)
  	(if (/= cctv_name_list "123")
	  (progn
		(setq cctv_name_list (split-string cctv_name_list))
	  	(dcl-Control-SetList drawCCTV/Form1/ListBox1 cctv_name_list)
	    	
	    )
	  	(setq cctv_name_list nil)
	)
  	(if (/= gjx_ksh "123")
	  (progn
		(setq gjx_ksh (split-string gjx_ksh))
	  	(dcl-Control-SetList drawCCTV/Form1/ListBox5 gjx_ksh)
	    	
	    )
	  	(setq gjx_ksh nil)
	)
  	(if (/= wireway_layer "123")
	  (progn
		(setq wireway_layer (split-string wireway_layer))
	  	(dcl-Control-SetList drawCCTV/Form1/ListBox2 wireway_layer)
	    	
	    )
	  	(setq wireway_layer nil)
	)
  	(if (/= gg_layer "123")
	  (progn
		(setq gg_layer (split-string gg_layer))
	  	(dcl-Control-SetList drawCCTV/Form1/ListBox6 gg_layer)
	    	
	    )
	  	(setq gg_layer nil)
	)
  	(if (/= bltc "123")
	  (progn
		(setq bltc (split-string bltc))	  	
	    	
	    )
	  	(setq bltc nil)
	)
  	;(close file) ; 关闭文件
)



(defun c:drawCCTV/Form1/TextButton19#OnClicked (/)
	(create-parameter-file)
)

(defun c:drawCCTV/Form1/TextButton20#OnClicked (/)
  	(read-parameter-file)
)


(defun drawCCTV (/  jf_bias dxd_ent_ptlst dxd_ent)
	(Berni_Start)
  	(vl-load-com)
  	;(setq jifang nil)
  	(setq tmp_layer "test1") 
	(if (= (tblsearch "Layer" tmp_layer) nil) (command-s "Layer" "new" tmp_layer "color" "3" tmp_layer "")) (setq bltc (cons tmp_layer bltc))
  	 
	(if (= (tblsearch "Layer" "test2") nil) (command-s "Layer" "new" "test2" "color" "3" "test2" "")) (setq bltc (cons "test2" bltc))
	(setq tmp_layer_xc "cc_xc11")
  	(if (= (tblsearch "Layer" tmp_layer_xc) nil) (command-s "Layer" "new" tmp_layer_xc "color" "4" tmp_layer_xc "")) (setq bltc (cons tmp_layer bltc))
  	
  	(setq gllst (list (cons 0 "LINE") '(8 . "test1,test2")))
  	
  	(setq cctv_list (dcl-Control-GetList drawCCTV/Form1/ListBox3))
  	(setq cctv_list (hc-string cctv_list))
  	(setq gjx_ksh (dcl-Control-GetList drawCCTV/Form1/ListBox5))
  	(setq gjx_ksh (hc-string gjx_ksh))
  	(setq wireway_layer (dcl-Control-GetList drawCCTV/Form1/ListBox2))
  	(setq wireway_layer (hc-string wireway_layer))
  	(setq gg_layer (dcl-Control-GetList drawCCTV/Form1/ListBox6))
  	(setq gg_layer (hc-string gg_layer))
  	
  	(setq cctv_name_list (dcl-Control-GetList drawCCTV/Form1/ListBox1))
  	(setq cctv_name_list (hc-string cctv_name_list))
  	
  	(setq gjx_bias 10000)
  	(setq cctv_coe 1.2)
  	(if (car jifang)  (setq jf_bias (* (atof(dcl-Control-GetText drawCCTV/Form1/TextBox1)) 1000)))
  	(setq chksty (tblsearch "style" "宋体"))
        (if (null chksty) (command-s "style" "宋体" "@万能字体" "" "" "" "" "" ""))
  	(setvar 'textstyle "宋体")
        (command-s "celtype" "bylayer")
        (command-s "celweight" 0)
  	(setq draw_pts (getpoint "\n绘制系统图插入点："))
  	(gbtc) 		;;;沿电缆槽绘制直线
  	(sub1_mlpoint mline_set)  
	(sx)    	;;;删除重复和覆盖的直线部分
        ;(breakall)
  	(setq m_i 0 m_n 0 cctv_list_info nil gjx_list nil gjx_name_list nil y 0 jifang_ptt ())
  	(if (null gjx_ksh) (setq gjx_set nil))
  	
  	(if (and (/= gjx_set nil) (null (car jifang)))     ;;;第一种情况无机房引入位置，有光交箱
	  	(progn
	    		(repeat (sslength gjx_set)
	      (setq single_gjx (ssname gjx_set m_i))
	        		(block_base single_gjx)
	            (setq single_gjx_base block_base1)
	            (block_dist single_gjx_base)
	        		(setvar "clayer" tmp_layer)
	        		(command-s "LINE" single_gjx_base block_dist_pt "") ;(setq line_set (ssadd (entlast) line_set))
	            (setq single_gjx_pt block_dist_pt)
	            (setq single_gjx_dist (distance single_gjx_pt single_gjx_base))
	            (sub1_block_name_gjx single_gjx_base)
	            (setq single_gjx_name sub1_block_name2)
	        		(setq gjx_list (append (list (cons single_gjx_pt single_gjx_dist)) gjx_list))  ;光交箱坐标及离线槽距离的列表
	            (setq gjx_name_list (append (list single_gjx_name) gjx_name_list))		;光交箱名字列表
	            (setq m_i (1+ m_i))
	    		)
	    		(setq m_i 0)
	    		(setq gjx_list (REVERSE gjx_list))
	    		(setq gjx_name_list (REVERSE gjx_name_list))
   	    )
	)
  	(if (and (/= gjx_set nil) (/= (car jifang) nil))     ;;;第二种情况有机房引入位置，有光交箱
	  	(progn
	    		(repeat (length jifang)
	        		(setq tmp_jifang (nth y jifang))
	      (block_dist tmp_jifang)
	        		(setvar "clayer" tmp_layer)
	          (command-s "LINE" tmp_jifang block_dist_pt "") (setq jifang_ptt (cons block_dist_pt jifang_ptt));(setq line_set (ssadd (entlast) line_set))
	      (if (= y 0)
		        (progn
	            	      (setq gjx_list (append (list (cons block_dist_pt (distance tmp_jifang block_dist_pt))) gjx_list))
	    	    	      (setq gjx_name_list (append (list "机房引入点") gjx_name_list))
		  			)
				)
	        		(setq y (1+ y))
	    		)
	    	(setq y 0)
	    	(repeat (1- (length jifang))
		 	(command-s "LINE" (nth y jifang) (nth (1+ y) jifang) "")
	         	(setq y (1+ y))
	    	)
	    	(command-s "LINE" (nth 0 jifang) (nth y jifang) "")
	    	(repeat (sslength gjx_set)
      (setq single_gjx (ssname gjx_set m_i))
	        	(block_base single_gjx)
	      		(setq single_gjx_base block_base1)
	      		(block_dist single_gjx_base)
	        	(setvar "clayer" tmp_layer)
	        	(command-s "LINE" single_gjx_base block_dist_pt "") ;(setq line_set (ssadd (entlast) line_set))
	      		(setq single_gjx_pt block_dist_pt)
	      		(setq single_gjx_dist (distance single_gjx_pt single_gjx_base))
	      		(sub1_block_name_gjx single_gjx_base)
	      		(setq single_gjx_name sub1_block_name2)
	      	
	    	 
	        	(setq gjx_list (append (list (cons single_gjx_pt single_gjx_dist)) gjx_list))  ;光交箱坐标及离线槽距离的列表
	      	
	      		(setq gjx_name_list (append (list single_gjx_name) gjx_name_list))		;光交箱名字列表
	      		(setq m_i (1+ m_i))
	   	)
	    
	    	(setq m_i 0)
	    	(setq gjx_list (REVERSE gjx_list))
	    	(setq gjx_name_list (REVERSE gjx_name_list))
   	    	)
	)
  	(if (and (null gjx_set) (/= (car jifang) nil))     ;;;第三种情况有机房引入位置，无光交箱
	   	(progn
	    		(repeat (length jifang)
	       		(setq tmp_jifang (nth y jifang))
	       		(block_dist tmp_jifang)
	       		(setvar "clayer" tmp_layer)
	       		(command-s "LINE" tmp_jifang block_dist_pt "") (setq jifang_ptt (cons block_dist_pt jifang_ptt));(setq line_set (ssadd (entlast) line_set))
	       		(if (= y 0)
		  	(progn
	            		(setq gjx_list (append (list (cons block_dist_pt (distance tmp_jifang block_dist_pt))) gjx_list))
	    	    		(setq gjx_name_list (append (list "机房引入点") gjx_name_list))
		  	)
			)
	       		(setq y (1+ y))
   	    		)
	    (setq y 0)
	    (repeat (1- (length jifang))
		 (command-s "LINE" (nth y jifang) (nth (1+ y) jifang) "")
	         (setq y (1+ y))
	    )
	    (command-s "LINE" (nth 0 jifang) (nth y jifang) "")
	   )
	  )
	
        
  	(setq tmp_dis 0 end_dis 1000000 end_drawlist ())
	(command-s "layer" "off" wireway_layer "")
  	(if (dcl-Control-GetList drawCCTV/Form1/ListBox4) (dxd_dist))
  	(setvar "clayer" tmp_layer)
  	(sx)
  	(breakobj)
  	(setq mline_set (ssget "c" ml_p1 ml_p2 (list (cons 0 "mline") (cons 8 wireway_layer))))
  	(if (/= (sslength cctv_set) nil)
	  (progn
	     (repeat (sslength cctv_set)
	    	(setq single_cctv (ssname cctv_set m_i))
	    	(block_base single_cctv)
	    	(setq single_cctv_base block_base1)
	    	(sub1_block_name single_cctv_base)
	    	(setq single_cctv_name sub1_block_name1)
	    	;(block_dist single_cctv_base)
	        (sxj_dist single_cctv_base)
	    	(setq single_cctv_pt block_dist_pt)
	    	;(setq single_cctv_dist (distance single_cctv_pt single_cctv_base))
	    	(break_line single_cctv_pt)
	        (setq line_pt_lst1 line_pt_lst)
	        (setq tmp_gjx_dis ())
		;(repeat (length gjx_name_list)
	       (repeat (min 3 (length gjx_name_list))
      (setq tmp_gjx_list (mapcar '(lambda (x) (list (distance (car x) single_cctv_pt) (car x) (cdr x))) gjx_list))
      (setq tmp_gjx_list (vl-sort tmp_gjx_list '(lambda (e1 e2) (< (car e1) (car e2)))))
      (setq tmp_gjx_list (mapcar '(lambda (x) (cdr x)) tmp_gjx_list))		
	    		(sub1_tt1 single_cctv_pt (car(nth m_n tmp_gjx_list)))
			
		 	
			  
	    		(if gjx_cctv_dis (setq tmp_dis (+ gjx_cctv_dis single_cctv_dist (cadr (nth m_n tmp_gjx_list)))) (setq tmp_dis (+ 1000000 single_cctv_dist (cadr (nth m_n tmp_gjx_list)))))
		  	(if (member (car (nth m_n tmp_gjx_list)) jifang_ptt) (setq tmp_dis (+ (* tmp_dis cctv_coe) jf_bias)) (setq tmp_dis (+ (* tmp_dis cctv_coe) gjx_bias)))
		  	(if (< tmp_dis end_dis)
			  (progn
			    (setq end_dis tmp_dis)
			    (setq m_ii (VL-POSITION (cons (car (nth m_n tmp_gjx_list)) (cadr (nth m_n tmp_gjx_list))) gjx_list))
			    (setq gjx_lj (nth m_ii gjx_name_list))
			  )
			 )
		  	(setq m_n (1+ m_n))
		       (break_line single_cctv_pt)
	        (setq line_pt_lst1 line_pt_lst)
		)
	       (setq m_n 0)
	       (join_line line_pt_lst1)
	       (setq end_drawlist (append (list (list end_dis (cdr(assoc 2 (entget single_cctv))) single_cctv_name gjx_lj)) end_drawlist))
	       (setq m_i (1+ m_i))
	       (setq end_dis 1000000)	;;;重置距离初设值	       
	    )
	    ；(setq end_drawlist (REVERSE end_drawlist))
	  )
	)
	(command-s "layer" "on" wireway_layer "")
  	(dktc)
	(sub1_fenlei end_drawlist)
	(setq end_drawlist end)
	
	(sub1_draw_sys draw_pts end_drawlist)
	
	(Berni_end)
  	(setq jifang nil)
  	(setq gjx_set nil)
  	(prin1)
)

(defun c:drawCCTV/Form1/TextButton25#OnClicked (/)
	(setq gk_lst (ssget  '((0 . "INSERT"))) gk_tmplst () gjx_ksh nil tmp_gkblock nil)
  	(if gk_lst
	  (progn
	     (repeat (sslength gk_lst)
      	   	(setq tmp_gkblock (cdr (assoc 2 (entget (ssname gk_lst 0)))))
	   	(if (null (member (cdr (assoc 8 (entget (ssname gk_lst 0)))) bltc)) (setq bltc (cons (cdr (assoc 8 (entget (ssname gk_lst 0)))) bltc)))
      	   	(if (null (member tmp_gkblock gk_tmplst)) (setq gk_tmplst (cons tmp_gkblock gk_tmplst)))    
      	   	(ssdel (ssname gk_lst 0) gk_lst)
   	     )

   	     (setq gjx_ksh (car gk_tmplst) gk_tmplst (cdr gk_tmplst))
   	     (while gk_tmplst
      	        (setq gjx_ksh (strcat gjx_ksh "," (car gk_tmplst)))
      	        (setq gk_tmplst (cdr gk_tmplst))
  	     )
	  )
	)
  	(setq gjx_ksh (split-string gjx_ksh))
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox5))
        (if allstr
	  (progn
             (if gjx_ksh
	       (repeat (length gjx_ksh)
	          (if (null (member (car gjx_ksh) allstr)) (setq allstr (cons (car gjx_ksh) allstr)))
		 (setq gjx_ksh (cdr gjx_ksh))
	       )
	       (setq allstr gjx_ksh)
	     )
	  )
	  (setq allstr gjx_ksh)
	) 
  	(dcl-Control-SetList drawCCTV/Form1/ListBox9 allstr)
)

(defun c:drawCCTV/Form1/ListBox9#OnDblClicked (/ )
  	(setq choose_str (car(dcl-ListBox-GetSelectedItems drawCCTV/Form1/ListBox9)))
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox9))
)

(defun c:drawCCTV/Form1/TextButton26#OnClicked (/ )
  	(setq allstr (vl-remove choose_str allstr))
  	;(setq bltc (vl-remove choose_str bltc))
	(dcl-Control-SetList drawCCTV/Form1/ListBox9 allstr)
)

(defun c:drawCCTV/Form1/TextButton24#OnClicked (/ cctv_name_layer cctv_name_tmplst cctv_name_list )
	(setq cctv_name_layer (ssget '((0 . "TEXT")))  cctv_name_tmplst () cctv_name_list nil)
  
        (if cctv_name_layer
	  (progn
	     (repeat (sslength cctv_name_layer)
      	        (if (null (member (cdr (assoc 8 (entget (ssname cctv_name_layer 0)))) cctv_name_tmplst))
		  (setq cctv_name_tmplst (cons (cdr (assoc 8 (entget (ssname cctv_name_layer 0)))) cctv_name_tmplst)))
	        (if (null (member (cdr (assoc 8 (entget (ssname cctv_name_layer 0)))) bltc))
		  (setq bltc (cons (cdr (assoc 8 (entget (ssname cctv_name_layer 0)))) bltc)))
      	       
      	        (ssdel (ssname cctv_name_layer 0) cctv_name_layer)
   	     )

   	     (setq cctv_name_list (car cctv_name_tmplst) cctv_name_tmplst (cdr cctv_name_tmplst))
   	     (while cctv_name_tmplst
      	        (setq cctv_name_list (strcat cctv_name_list "," (car cctv_name_tmplst)))
      	        (setq cctv_name_tmplst (cdr cctv_name_tmplst))
  	     )
	  )
	)
  	(setq cctv_name_list (split-string cctv_name_list))
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox1))
        (if allstr
	  (progn
             (if cctv_name_list
	       (repeat (length cctv_name_list)
	         (if (null (member (car cctv_name_list) allstr)) (setq allstr (cons (car cctv_name_list) allstr)))
		 (setq cctv_name_list (cdr cctv_name_list))
	       )
	       (setq allstr cctv_name_list)
	     )
	  )
	  (setq allstr cctv_name_list)
	) 
  	(dcl-Control-SetList drawCCTV/Form1/ListBox8 allstr)
)

(defun c:drawCCTV/Form1/ListBox8#OnDblClicked (/ )
  	(setq choose_str (car(dcl-ListBox-GetSelectedItems drawCCTV/Form1/ListBox8)))
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox8))
)

(defun c:drawCCTV/Form1/TextButton11#OnClicked (/ )
  	(setq allstr (vl-remove choose_str allstr))
  	(setq bltc (vl-remove choose_str bltc))
	(dcl-Control-SetList drawCCTV/Form1/ListBox8 allstr)
)

(defun c:drawCCTV/Form1/TextButton35#OnClicked (/)
	(dcl-Form-Hide drawCCTV_form1)
  	(setq jifang (list(getpoint "\n输入机房本层引入位置：")))  
  	(while (car jifang)
	   (setq tt_jifang (getpoint "\n输入机房本层其它引入位置："))
	   (setq jifang (cons tt_jifang jifang))
	)
  	(setq jifang (reverse jifang))
        (setq jifang (vl-remove nil jifang))
  	
  	(setq text_jifang (list(strcat (itoa (length jifang)) "个机房接入点")))
  	(dcl_form_show drawCCTV_form1)
	(dcl-Control-SetList drawCCTV/Form1/ListBox10 text_jifang)
  	
)

(dcl-Control-SetText drawCCTV/Form1/TextBox2 "25")

(defun c:drawCCTV/Form1/TextButton28#OnClicked (/ sub10_dxd_1 sub10_dxd_2 sub10_dxd_text )
	(if (null (dcl-Control-GetList drawCCTV/Form1/ListBox11)) (setq sub10_dxd_n 0 sub10_dxd_list nil))
  	(dcl-Form-Hide drawCCTV_form1)
  	(setq sub10_dxd_1 (getpoint "\n输入第一个等效点："))
  	(setq sub10_dxd_2 (getpoint "\n输入第二个等效点："))
  	(setq sub10_dxd_text (dcl-Control-GetList drawCCTV/Form1/ListBox11))
  	(setq sub10_dxd_n (1+ sub10_dxd_n))
  	(setq sub10_dxd_list (cons (list (list sub10_dxd_1 sub10_dxd_2) sub10_dxd_n) sub10_dxd_list))	
  	(setq sub10_dxd_text (cons (strcat "第" (itoa sub10_dxd_n) "对等效点") (reverse sub10_dxd_text)))
  	(dcl_form_show drawCCTV_form1)
  	(dcl-Control-SetList drawCCTV/Form1/ListBox11 (reverse sub10_dxd_text))
)

(defun sub10_dxd_dist (/  dxd_nn single_dxd_ent single_dxd_ent_ptlst dxd_length dxd_tmp_zhongzhi dxd_tmp dxd_tmp_qishi dxd_tmp_xc_qishi dxd_tmp_qishi_length)    
  	(setvar "cmdecho" 0)
  	
  	(setq dxd_nn 0)
  	(setq dxd_ent nil dxd_ent_ptlst nil single_dxd_ent nil single_dxd_ent_ptlst nil)
	(repeat (length sub10_dxd_list)
	  	(setq dxd_length 0)
  		(setq dxd_tmp (car(nth dxd_nn sub10_dxd_list)))
	  	(setq dxd_tmp_qishi (car dxd_tmp))
	  	(if (<  (distance dxd_tmp_qishi (car jifang)) 10000)
		  (progn
      (setvar "clayer" tmp_layer)
      (command-s "LINE" dxd_tmp_qishi (car jifang) "")
		  )
		)
	  	(sxj_dist dxd_tmp_qishi)
	  	(setq dxd_tmp_xc_qishi block_dist_pt)
	  	(setq dxd_tmp_qishi_length single_cctv_dist)
	  	
	  	(setq dxd_tmp_zhongzhi (cadr dxd_tmp))
	  	(if (<  (distance dxd_tmp_zhongzhi (car jifang)) 10000)
		  (progn
      (setvar "clayer" tmp_layer)
      (command-s "LINE" dxd_tmp_zhongzhi (car jifang) "")
		  )
		)
	  	(sxj_dist dxd_tmp_zhongzhi)
	  	(setq dxd_tmp_xc_zhongzhi block_dist_pt)
	  	(setq dxd_tmp_zhongzhi_length single_cctv_dist)
		(setq dxd_length (distance dxd_tmp_qishi dxd_tmp_zhongzhi))
		(break_line dxd_tmp_xc_qishi)
	  	(break_line dxd_tmp_xc_zhongzhi)
		

	  	(setvar "clayer" tmp_layer)
	  	;(breakobj)
	  	(setq gjx_cctv_dis nil)
	  	;(command-s "LINE" dxd_tmp_xc_qishi dxd_tmp_xc_zhongzhi "")
	  	;(command "line" dxd_tmp_xc_qishi dxd_tmp_xc_zhongzhi "")
	  	(sub1_tt1 dxd_tmp_xc_qishi dxd_tmp_xc_zhongzhi)
	  	(if gjx_cctv_dis
		  	(if (<= dxd_tmp_qishi_length dxd_tmp_zhongzhi_length)
			     (progn
			        (setvar "clayer" tmp_layer)
			  	(command-s "LINE" dxd_tmp_qishi dxd_tmp_xc_qishi "")
				;(setq dxd_length (+ tmp_length (distance dxd_tmp_qishi dxd_tmp_xc_qishi)))
			     )
			     (progn
			        (setvar "clayer" tmp_layer)
			  	(command-s "LINE" dxd_tmp_zhongzhi dxd_tmp_xc_zhongzhi "")
				;(setq dxd_length (+ tmp_length (distance dxd_tmp_zhongzhi dxd_tmp_xc_zhongzhi)))
			     )
			)
	  	
	  		(progn
			  	(setvar "clayer" tmp_layer)
			  	(command-s "LINE" dxd_tmp_qishi dxd_tmp_xc_qishi "")
			  	(command-s "LINE" dxd_tmp_zhongzhi dxd_tmp_xc_zhongzhi "")
			  	;(setq dxd_length (+ tmp_length (distance dxd_tmp_qishi dxd_tmp_xc_qishi) (distance dxd_tmp_zhongzhi dxd_tmp_xc_zhongzhi)))
			)
	  	)
	  	(setvar "clayer" "test2")
	  	(command-s "LINE" dxd_tmp_qishi dxd_tmp_zhongzhi "")
	  	(setvar "clayer" tmp_layer)
	  	(setq single_dxd_ent (cons (list dxd_tmp_qishi dxd_tmp_zhongzhi) dxd_length))
	  	;(setq single_dxd_ent (cons (entlast) dxd_length))
		(setq single_dxd_ent_ptlst  (list dxd_tmp_qishi dxd_tmp_zhongzhi))
	  	;(setq single_dxd_ent_ptlst  (entlast))
	  	
	  	(setq dxd_ent (cons single_dxd_ent dxd_ent))
	  	(setq dxd_ent_ptlst (cons single_dxd_ent_ptlst dxd_ent_ptlst))
		;(setq ttt (list dxd_tmp_qishi dxd_tmp_zhongzhi))
	  	;(if (member (list (getpoint) (getpoint)) dxd_ent_ptlst) (princ "yes") (princ "no"))
	  	(setq dxd_nn (1+ dxd_nn))
	)
  	(breakobj)
)

(defun c:drawCCTV/Form1/ListBox11#OnDblClicked (/ )
  	(setq choose_str (car(dcl-ListBox-GetSelectedItems drawCCTV/Form1/ListBox11))) 
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox11))
)



(defun c:drawCCTV/Form1/TextButton29#OnClicked (/ )
  	(setq allstr (vl-remove choose_str allstr))
  	(setq choose_n (atoi(sub1_vl-string->number choose_str)))
  	(setq n 0)
  	(repeat (length sub10_dxd_list)
		(if (= (cadr (nth n sub10_dxd_list))	choose_n) (setq sub10_dxd_list (vl-remove (nth n sub10_dxd_list) sub10_dxd_list)))
	  	(setq n (1+ n))
	)
	
        
	(dcl-Control-SetList drawCCTV/Form1/ListBox11 allstr)
)

(defun c:drawCCTV/Form1/ListBox11#OnDblClicked (/)
  	(setq choose_str (car(dcl-ListBox-GetSelectedItems drawCCTV/Form1/ListBox11))) 
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox11))
)

(defun c:drawCCTV/Form1/TextButton30#OnClicked (/)
  	(clean_creen)
  	(drawkong)
)

(defun c:drawCCTV/Form1/TextButton32#OnClicked (/)
  	(dktc)
)

(defun c:drawCCTV/Form1/TextButton36#OnClicked (/ ent_xc wireway_layer_tmplst wireway_layer)
  	(setq ent_xc (ssget '((0 . "MLINE"))))
  	(if ent_xc
	  (progn
	     (repeat (sslength ent_xc)
	     (if (null (member (cdr (assoc 8 (entget (ssname ent_xc 0)))) wireway_layer_tmplst))
	       	(setq wireway_layer_tmplst (cons (cdr (assoc 8 (entget (ssname ent_xc 0)))) wireway_layer_tmplst)))
	     (if (null (member (cdr (assoc 8 (entget (ssname ent_xc 0)))) bltc))
	       (setq bltc (cons (cdr (assoc 8 (entget (ssname ent_xc 0)))) bltc)))
      	       
      	        (ssdel (ssname ent_xc 0) ent_xc)
   	     )

   	     (setq wireway_layer (car wireway_layer_tmplst) wireway_layer_tmplst (cdr wireway_layer_tmplst))
   	     (while wireway_layer_tmplst
      	        (setq wireway_layer (strcat wireway_layer "," (car wireway_layer_tmplst)))
      	        (setq wireway_layer_tmplst (cdr wireway_layer_tmplst))
  	     )
	  )
	)
  	(setq wireway_layer (split-string wireway_layer))
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox12))
        (if allstr
	  (progn
             (if wireway_layer
	       (repeat (length wireway_layer)
	         (if (null (member (car wireway_layer) allstr)) (setq allstr (cons (car wireway_layer) allstr)))
		 (setq wireway_layer (cdr wireway_layer))
	       )
	       (setq allstr wireway_layer)
	     )
	  )
	  (setq allstr wireway_layer)
	) 
  	(dcl-Control-SetList drawCCTV/Form1/ListBox12 allstr)
)

(defun c:drawCCTV/Form1/ListBox12#OnDblClicked (/ )
  	(setq choose_str (car(dcl-ListBox-GetSelectedItems drawCCTV/Form1/ListBox12)))
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox12))
)

(defun c:drawCCTV/Form1/TextButton31#OnClicked (/ )
  	(setq allstr (vl-remove choose_str allstr))
  	(setq bltc (vl-remove choose_str bltc))
	(dcl-Control-SetList drawCCTV/Form1/ListBox12 allstr)
)

(defun c:drawCCTV/Form1/TextButton38#OnClicked (/ ent_gg gg_layer)
	(setq ent_gg (entsel))
  	(if (/= ent_gg nil)
	  (progn
	    (setq gg_layer (cdr (ASSOC 8 (entget (car ent_gg)))))
	    (setq bltc (cons gg_layer bltc))
	    (setq ent_gg_lst (list gg_layer))
	  )
	)
  	
  	(dcl-Control-SetList drawCCTV/Form1/ListBox13 ent_gg_lst)
)

(defun c:drawCCTV/Form1/ListBox13#OnDblClicked (/ )
  	(setq choose_str (car(dcl-ListBox-GetSelectedItems drawCCTV/Form1/ListBox13)))
  	(setq allstr (dcl-Control-GetList drawCCTV/Form1/ListBox13))
)

(defun c:drawCCTV/Form1/TextButton37#OnClicked (/ )
  	(setq allstr (vl-remove choose_str allstr))
  	(setq bltc (vl-remove choose_str bltc))
	(dcl-Control-SetList drawCCTV/Form1/ListBox13 allstr)
)




(defun sub10_create-parameter-file ()
  	(setq file-path (strcat (getvar "DWGPREFIX") "param-drawHJX.txt")) ; 设置文件路径为程序所在目录下的parameters.txt

  	(setq file (open file-path "w")) ; 以写入模式打开文件
	; 将gjx_ksh写入文件
  	(if (dcl-Control-GetList drawCCTV/Form1/ListBox9)
	  	(write-line (hc-string (dcl-Control-GetList drawCCTV/Form1/ListBox9)) file)
	  	(write-line "123"  file)
	)
	; 将cctv_name_list写入文件
  	(if (dcl-Control-GetList drawCCTV/Form1/ListBox8)
  		(write-line (hc-string (dcl-Control-GetList drawCCTV/Form1/ListBox8)) file)
	  	(write-line "123"  file)
	)
 
	; 将wireway_layer写入文件
  	(if (dcl-Control-GetList drawCCTV/Form1/ListBox12)
		(write-line (hc-string (dcl-Control-GetList drawCCTV/Form1/ListBox12)) file)
	  	(write-line "123"  file)
	)
  	; 将gg_layer写入文件
  	(if (dcl-Control-GetList drawCCTV/Form1/ListBox13)
  		(write-line (hc-string (dcl-Control-GetList drawCCTV/Form1/ListBox13)) file)
	  	(write-line "123"  file)
	)

  	; 将bltc写入文件
  	(if bltc
  		(write-line (hc-string bltc) file)
	  	(write-line "123"  file)
	)
 
  
  	(close file) ; 关闭文件
)

(defun sub10_read-parameter-file ()
  	(setq file-path (strcat (getvar "DWGPREFIX") "param-drawHJX.txt"))
  	(setq file (open file-path "r")) 	; 打开文档以供读取
  	(setq gjx_ksh (read-line file)) 	; 读取第一行内容并赋值给参数 line1
  	(setq cctv_name_list (read-line file)) 	; 读取第二行内容并赋值给参数 line2
  	
  	(setq wireway_layer (read-line file))
  	(setq gg_layer (read-line file))
  	(setq bltc (read-line file))
  	(close file) ; 关闭文档
  	(if (/= gjx_ksh "123")
	  (progn
		(setq gjx_ksh (split-string gjx_ksh))
	  	(dcl-Control-SetList drawCCTV/Form1/ListBox9 gjx_ksh)
	    	
	    )
	  	(setq gjx_ksh nil)
	)
  	(if (/= cctv_name_list "123")
	  (progn
		(setq cctv_name_list (split-string cctv_name_list))
	  	(dcl-Control-SetList drawCCTV/Form1/ListBox8 cctv_name_list)
	    	
	    )
	  	(setq cctv_name_list nil)
	)
  	
  	(if (/= wireway_layer "123")
	  (progn
		(setq wireway_layer (split-string wireway_layer))
	  	(dcl-Control-SetList drawCCTV/Form1/ListBox12 wireway_layer)
	    	
	    )
	  	(setq wireway_layer nil)
	)
  	(if (/= gg_layer "123")
	  (progn
		(setq gg_layer (split-string gg_layer))
	  	(dcl-Control-SetList drawCCTV/Form1/ListBox13 gg_layer)
	    	
	    )
	  	(setq gg_layer nil)
	)
  	(if (/= bltc "123")
	  (progn
		(setq bltc (split-string bltc))	  	
	    	
	    )
	  	(setq bltc nil)
	)
)

(defun c:drawCCTV/Form1/TextButton33#OnClicked (/)
  	(sub10_create-parameter-file)
)

(defun c:drawCCTV/Form1/TextButton34#OnClicked (/)
	(sub10_read-parameter-file)
)

(defun c:drawCCTV/Form1/TextButton40#OnClicked (/ wireway_layer_tmplst)
	(setq wireway_layer_tmplst (dcl-Control-GetList drawCCTV/Form1/ListBox12))
  	
  	(setq wireway_layer (car wireway_layer_tmplst) wireway_layer_tmplst (cdr wireway_layer_tmplst))
   	(while wireway_layer_tmplst
      	        (setq wireway_layer (strcat wireway_layer "," (car wireway_layer_tmplst)))
      	        (setq wireway_layer_tmplst (cdr wireway_layer_tmplst))
  	)
  	(if wireway_layer
		(progn
		  	(dcl-Form-Hide drawCCTV_form1)
	  		(setq mline_set (ssget "c" (setq ml_p1 (getpoint)) (setq ml_p2 (getcorner ml_p1)) (list (cons 0 "mline") (cons 8 wireway_layer))))
		  	(dcl_form_show drawCCTV_form1)
		)
	  (alert "未明确电缆槽图层")
	)
)


(defun c:drawCCTV/Form1/TextButton39#OnClicked (/ )
	(setq gk_tmplst (dcl-Control-GetList drawCCTV/Form1/ListBox9))
  	
  	(setq gjx_ksh (car gk_tmplst) gk_tmplst (cdr gk_tmplst))
   	(while gk_tmplst
      	   (setq gjx_ksh (strcat gjx_ksh "," (car gk_tmplst)))
      	   (setq gk_tmplst (cdr gk_tmplst))
  	)
  	(if gjx_ksh
	  (progn
		(dcl-Form-Hide drawCCTV_form1)
	    	(setq gjx_set (ssget (list (cons 2 gjx_ksh))))
		(dcl_form_show drawCCTV_form1)
	   )
	  (alert "未明确汇聚箱图块")
	)
)

(defun c:drawCCTV/Form1/TextButton27#OnClicked (/)
	(clean_creen)
  	(drawHJX)
)

(defun sub10_draw_sys (htd lst / cm os  htd_pt1 htd_pt2 htd_pt3 htd_p1 end_p1 tmp_drawblock tmp_drawblockname tmp_textpts tmp_textstr tmp_namepts i m)
   (setq cm (getvar "cmdecho") os (getvar "osmode"))
   (setvar "cmdecho" 0) (setvar "osmode" 0)
   (setvar "clayer" "0")
   (setq end lst i 0 m 0)
   (setq htd_p1 (polar htd (/ pi -16.4) 1019))
   (setq end (vl-sort end '(lambda (e1 e2) (< (cadr e1) (cadr e2)))))
   (repeat (length end)
      (setq tmp_draw (nth m end))
      ;(setq tmp_draw (vl-sort tmp_draw '(lambda (e1 e2) (< (atof(sub1_vl-string->number(caddr e1))) (atof(sub1_vl-string->number(caddr e2)))))))
      
      ;(setq tmp_draw (vl-sort tmp_draw '(lambda (e1 e2) (< (atof(sub1_vl-string->number(caddr e1))) (atof(sub1_vl-string->number(caddr e2)))))))
        (setq end_p1 (polar htd_p1 0 6000))
        (command-s "pline" htd_p1 end_p1 "")
        (setq tmp_drawblock tmp_draw)
        (setq tmp_drawblockname (cdr(assoc 2 (entget(nth 2 tmp_drawblock)))))
        (setq tmp_textpts (polar htd_p1 0.2 250))
        (setq tmp_textstr (strcat "汇聚箱线缆" "-" (rtos (fix (/ (nth 0 tmp_drawblock) 1000)) 2 0) "m"))
	
     	
        (sub1_insert_block end_p1 tmp_drawblockname)
     	(setq tmp_namepts (polar end_p1 0 (+ gj_length 100)))
        (command-s "text" tmp_textpts 250 0 tmp_textstr)
     	
	(command-s "text" tmp_namepts 250 0 (nth 1 tmp_drawblock))
        (setq i (1+ i))
	(if (> (+ gj_heigh 100) 550)
	  (setq htd_p1 (polar htd_p1 (* pi 1.5) (+ gj_heigh 550)))
          (setq htd_p1 (polar htd_p1 (* pi 1.5) 100))
	)
        (setq m (1+ m) i 0)
     )
     (setq htd_pt1 (polar htd (* pi 1.5) (+ (- (cadr htd) (cadr htd_p1)))))
     (setq htd_pt2 (polar htd_pt1 0 1000))
     (setq htd_pt3 (polar htd_pt2 (* pi 0.5) (+ (- (cadr htd) (cadr htd_p1)) )))
     (if (> (- (cadr htd_pt1) (cadr htd)) 18800)
     
     	(command-s "pline" htd htd_pt1 htd_pt2 htd_pt3 "c")
        (command-s "pline" htd (polar htd (* pi 1.5) 18800) (polar (polar htd (* pi 1.5) 18800) 0 1000) (polar (polar (polar htd (* pi 1.5) 18800) 0 1000) (* pi 0.5) 18800) "c")
     )
     ;(command-s "text" (polar htd (* pi 0.7) 400) 250 0 (nth 3 (car tmp_draw)))
     (draw_BDXrec (polar htd (* pi 1.5) 200))
     ;(setq htd (polar htd 0 12000))
	(setq chksty (tblsearch "style" "@宋体"))
   (setq gj_heigh nil)
  	
   (setvar "cmdecho" cm) (setvar "osmode" os)
)

(defun draw_singlerec (text text1 htd / htd_1 htd_2 htd_3)

	(setq htd_1 (polar htd (- (* 0.5 pi)) 500))
	(setq htd_2 (polar htd_1 0 2200))
	(setq htd_3 (polar htd_2 (* 0.5 pi) 250))
	(command-s "rectangle" htd  htd_2)
	(command-s "text" (polar htd_1 1.8 40) 350 0 text)
	(command-s "pline" htd_3 (polar htd_3 0 4000) "")
	(command-s "text" (polar htd_3 1 80) 250 0 text1)
	(prin1)
 )

(defun draw_CKSrec (htd / htd_1 htd_2 htd_3 htd_4 htd_5 htd_6 htd_7)
	
	(setq htd_1 (polar htd (- (* 0.5 pi)) 1200))
	(setq htd_2 (polar htd_1 0 4500))
  	(command-s "rectangle" htd  htd_2)
  	(setq htd_3 (polar htd 0 2000))
  	(command-s "text" (polar htd_3 (- (* 0.5 pi)) 300) 250 0 "车控室")
	(setq htd_4 (polar (polar htd (- (* 0.5 pi)) 500) 0 300))
  	(setq htd_5 (polar htd_4 (* 0.5 pi) -400))
	(setq htd_6 (polar htd_5 0 2200))
  	(setq htd_7 (polar htd_6 (* 0.5 pi) 200))
	(command-s "rectangle" htd_4  htd_6)
	(command-s "text" (polar htd_5 1.8 40) 250 0 "后备操作终端")
	(command-s "pline" htd_7 (polar htd_7 0 2700) "")
	(command-s "text" (polar htd_7 1 80) 250 0 "N+F-95m")
  
	(prin1)
 )

(defun draw_PWrec (htd / htd_1 htd_2 htd_3 htd_4 htd_5 htd_6 htd_7)
	
	(setq htd_1 (polar htd (- (* 0.5 pi)) 1200))
	(setq htd_2 (polar htd_1 0 4500))
  	(command-s "rectangle" htd  htd_2)
  	(setq htd_3 (polar htd 0 2000))
  	(command-s "text" (polar htd_3 (- (* 0.5 pi)) 300) 250 0 "站长室")
	(setq htd_4 (polar (polar htd (- (* 0.5 pi)) 500) 0 300))
  	(setq htd_5 (polar htd_4 (* 0.5 pi) -400))
	(setq htd_6 (polar htd_5 0 2200))
  	(setq htd_7 (polar htd_6 (* 0.5 pi) 200))
	(command-s "rectangle" htd_4  htd_6)
	(command-s "text" (polar htd_5 1.8 40) 250 0 "票务操作终端")
	(command-s "pline" htd_7 (polar htd_7 0 2700) "")
	(command-s "text" (polar htd_7 1 80) 250 0 "N+F-95m")
  
	(prin1)
 )

(defun draw_JSQrec (htd / htd_1 htd_2 htd_3 htd_4 htd_5 htd_6 htd_7 htd_8)
	;(setq htd (getpoint))
	(setq htd_1 (polar htd (- (* 0.5 pi)) 500))
	(setq htd_2 (polar htd_1 0 3000))
	(setq htd_3 (polar htd_2 (* 0.5 pi) 250))
	(command-s "rectangle" htd  htd_2)
	(command-s "text" (polar htd_1 1.8 40) 350 0 "上行站台监视器")
	(command-s "pline" htd_3 (polar htd_3 0 350) "")
	(setq htd_4 (polar htd 0 3350))
	(setq htd_5 (polar htd_4 (- (* 0.5 pi)) 500))
	(setq htd_6 (polar htd_5 0 1500))
  	(setq htd_7 (polar htd_6 (* 0.5 pi) 250))
  	(command-s "rectangle" htd_4  htd_6)
	(command-s "text" (polar htd_5 1.8 40) 350 0 "光端机")
  	(command-s "pline" htd_7 (polar htd_7 0 2000) "")
  	(command-s "text" (polar htd_7 0.4 200) 250 0 "G4+F1-225m")

  	(setq htd_8 (polar htd (- (* 0.5 pi)) 1000))
  	(setq htd_1 (polar htd_8 (- (* 0.5 pi)) 500))
	(setq htd_2 (polar htd_1 0 3000))
	(setq htd_3 (polar htd_2 (* 0.5 pi) 250))
	(command-s "rectangle" htd_8  htd_2)
	(command-s "text" (polar htd_1 1.8 40) 350 0 "下行站台监视器")
	(command-s "pline" htd_3 (polar htd_3 0 350) "")
	(setq htd_4 (polar htd_8 0 3350))
	(setq htd_5 (polar htd_4 (- (* 0.5 pi)) 500))
	(setq htd_6 (polar htd_5 0 1500))
  	(setq htd_7 (polar htd_6 (* 0.5 pi) 250))
  	(command-s "rectangle" htd_4  htd_6)
	(command-s "text" (polar htd_5 1.8 40) 350 0 "光端机")
  	(command-s "pline" htd_7 (polar htd_7 0 2000) "")
  	(command-s "text" (polar htd_7 0.4 200) 250 0 "G4+F1-225m")
  
	(prin1)
 )

(defun draw_ISCSrec (htd / htd_1 htd_2 htd_3 htd_4 htd_5 htd_6 htd_7)
	
	(setq htd_1 (polar htd (- (* 0.5 pi)) 500))
	(setq htd_2 (polar htd_1 0 2000))
	(setq htd_3 (polar htd_2 (* 0.5 pi) 250))
	(command-s "rectangle" htd  htd_2)
	(command-s "text" (polar htd_1 1.8 40) 350 0 "综合监控")
	
	(setq htd_4 (polar htd 0 2000))
	(setq htd_5 (polar htd_4 (- (* 0.5 pi)) 500))
	(setq htd_6 (polar htd_5 0 1000))
  	(setq htd_7 (polar htd_6 (* 0.5 pi) 250))
  	(command-s "rectangle" htd_4  htd_6)
	(command-s "text" (polar htd_5 1.8 40) 350 0 "EDF")
  	(command-s "pline" htd_7 (polar htd_7 0 3200) "")
  	(command-s "text" (polar htd_7 0.4 200) 250 0 "N-25m×2")
  
	(prin1)
 )

(defun draw_GArec (htd / htd_1 htd_2 htd_3 htd_4 htd_5 htd_6 htd_7 htd_8 htd_9 htd_10 htd_11 htd_12 htd_13 htd_14 htd_15 htd_16 htd_17 htd_18)
	
  	;;画大框并写警务室
	(setq htd_1 (polar htd (- (* 0.5 pi)) 4300))
	(setq htd_2 (polar htd_1 0 8500))
  	(command-s "rectangle" htd  htd_2)
  	(setq htd_3 (polar htd 0 4500))
  	(command-s "text" (polar htd_3 (- (* 0.5 pi)) 300) 250 0 "警务室")
  	;;画各终端设备
  	(setq htd_4 (polar(polar htd_3 pi 2300) (- (* 0.5 pi)) 500))
  	(setq htd_5 (polar htd_4 (- (* 0.5 pi)) 400))
  	(setq htd_6 (polar htd_5 0 2000))
  	(setq htd_7 (polar htd_6 (* 0.5 pi) 200))
	(command-s "rectangle" htd_4  htd_6)
	(command-s "text" (polar htd_5 1.8 100) 250 0 "控制键盘×2")
  	(command-s "pline" htd_7 (polar htd_7 0 5000) "")
  	(command-s "text" (polar htd_7 0.008 2600) 250 0 "N-95m×2")

  	(setq htd_4 (polar htd_4 (- (* 0.5 pi)) 1000))
  	(setq htd_5 (polar htd_4 (- (* 0.5 pi)) 400))	
  	(setq htd_6 (polar htd_5 0 2000))
  	(setq htd_7 (polar htd_6 (* 0.5 pi) 200))
  	(command-s "rectangle" htd_4  htd_6)
  	(command-s "text" (polar htd_5 1.8 40) 250 0 "电视墙")
  	(command-s "pline" htd_7 (polar htd_7 0 1000) "")
  	(command-s "text" (polar htd_7 1 80) 250 0 "HDMI")
  	(setq htd_8 (polar (polar htd_7 0 1000) (* 0.5 pi) 200))
  	(setq htd_9 (polar htd_8 (- (* 0.5 pi)) 400))
  	(setq htd_10 (polar htd_9 0 1300))
  	(command-s "rectangle" htd_8  htd_10)
  	(setq htd_11 (polar htd_9 (* 0.5 pi) 200))
  	(command-s "text" (polar htd_9 1.8 40) 250 0 "解码器组")
	(setq htd_12 (polar htd_10 (* 0.5 pi) 200))
  	(command-s "pline" htd_12 (polar htd_12 0 2700) "")
  	(command-s "text" (polar htd_12 0.02 300) 250 0 "N-95m")
	(setq htd_13 (polar htd_4 pi 1500))
  	(setq htd_14 (polar htd_13 (- (* 0.5 pi)) 400))	
  	(setq htd_15 (polar htd_14 0 700))
	(command-s "rectangle" htd_13  htd_15)
	(command-s "text" (polar htd_14 1 120) 250 0 "PDU")
  	(setq htd_16 (polar htd_15 (* 0.5 pi) 200))
  	(command-s "pline" htd_16 (polar htd_16 0 800) "")
  	(setq htd_17 (polar htd_13 0 350))
  	(command-s "pline" htd_17 (polar htd_17 (* 0.5 pi) 800) (polar (polar htd_17 (* 0.5 pi) 800) 0 1150) "")

  	(setq htd_4 (polar htd_4 (- (* 0.5 pi)) 1000))
  	(setq htd_5 (polar htd_4 (- (* 0.5 pi)) 400))	
  	(setq htd_6 (polar htd_5 0 2000))
  	(setq htd_7 (polar htd_6 (* 0.5 pi) 200))
  	(command-s "rectangle" htd_4  htd_6)
  	(command-s "text" (polar htd_5 1.8 40) 250 0 "液晶监视器")
  	(command-s "pline" htd_7 (polar htd_7 0 1650) (polar (polar htd_7 0 1650) (* 0.5 pi) 800) "")
  	(command-s "text" (polar htd_7 1 80) 250 0 "HDMI")

  	(setq htd_18 (polar htd_15 pi 200))
  	(command-s "pline" htd_18 (polar htd_18 (- (* 0.5 pi)) 300) (polar (polar htd_18 (- (* 0.5 pi)) 300) 0 4200) (polar (polar (polar htd_18 (- (* 0.5 pi)) 300) 0 4200) (* 0.5 pi) 300)"")
  	(setq htd_18 (polar htd_18 pi 150))
  	(command-s "pline" htd_18 (polar htd_18 (- (* 0.5 pi)) 800) (polar (polar htd_18 (- (* 0.5 pi)) 800) 0 1150) "")
  	(setq htd_18 (polar htd_18 pi 150))
  	(command-s "pline" htd_18 (polar htd_18 (- (* 0.5 pi)) 1200) (polar (polar htd_18 (- (* 0.5 pi)) 1200) 0 8350) "")
  	(command-s "text" (polar (polar htd_12 (- (* 0.5 pi)) 1400) 0 300) 250 0 "F-95m")

  	(setq htd_4 (polar htd_4 (- (* 0.5 pi)) 1000))
  	(setq htd_5 (polar htd_4 (- (* 0.5 pi)) 400))	
  	(setq htd_6 (polar htd_5 0 2200))
  	(setq htd_7 (polar htd_6 (* 0.5 pi) 200))
  	(command-s "rectangle" htd_4  htd_6)
  	(command-s "text" (polar htd_5 1.8 40) 250 0 "视频操作终端×2")
  	(command-s "pline" htd_7 (polar htd_7 0 4800)  "")
  	(command-s "text" (polar (polar htd_12 (- (* 0.5 pi)) 2000) 0 300) 250 0 "N+F-95m×2")
  
	(prin1)
 )

(defun draw_CWZ (htd / htd_1 htd_2)
	
	
	(command-s "text" htd 250 0 "预留接入线网安防集成平台")
	
	(setq htd_1 (polar htd 0 3800))
	(setq htd_2 (polar htd_1 (* 0.5 pi) 125))

  	(command-s "pline" htd_2 (polar htd_2 0 3200) "")

  
	(prin1)
 )

;;半定型部分
(defun draw_BDXrec (htd / htd_1 htd_2 htd_3)
	
  	(setq htd_1 (polar htd pi 9200))
  	(draw_GArec htd_1)
  	(setq htd_1 (polar htd pi 5200))
  	(setq htd_1 (polar htd_1 (- (* 0.5 pi)) 5000))
  	(draw_CKSrec htd_1)
  	(setq htd_1 (polar htd pi 5200))
  	(setq htd_1 (polar htd_1 (- (* 0.5 pi)) 6900))
	(draw_PWrec htd_1)
  	(setq htd_1 (polar htd pi 6850))
  	(setq htd_1 (polar htd_1 (- (* 0.5 pi)) 8800))
  	(draw_JSQrec htd_1)

  	(setq htd_1 (polar htd pi 6200))
  	(setq htd_1 (polar htd_1 (- (* 0.5 pi)) 11000))
  	(draw_singlerec "传输系统" "20m单模双芯跳纤LC-LC"  htd_1)
  
  	(setq htd_1 (polar htd pi 6200))
  	(setq htd_1 (polar htd_1 (- (* 0.5 pi)) 12000))
  	(draw_ISCSrec htd_1)

  	(setq htd_1 (polar htd pi 6200))
  	(setq htd_1 (polar htd_1 (- (* 0.5 pi)) 13000))
  	(draw_singlerec "站厅电梯" "G4-120m"  htd_1)
  	(setq htd_1 (polar htd pi 6200))
  	(setq htd_1 (polar htd_1 (- (* 0.5 pi)) 14000))
  	(draw_singlerec "B出入口电梯" "G4-120m"  htd_1)
    	(setq htd_1 (polar htd pi 6200))
  	(setq htd_1 (polar htd_1 (- (* 0.5 pi)) 15000))
  	(draw_singlerec "D出入口电梯" "G4-120m"  htd_1)
      	(setq htd_1 (polar htd pi 6200))
  	(setq htd_1 (polar htd_1 (- (* 0.5 pi)) 16000))
  	(draw_singlerec "交流配电屏" "F3-40m×4"  htd_1)
      	(setq htd_1 (polar htd pi 6200))
  	(setq htd_1 (polar htd_1 (- (* 0.5 pi)) 17000))
  	(draw_singlerec "综合接地箱" "J-25m×3"  htd_1)
  	(setq htd_1 (polar htd pi 7000))
  	(setq htd_1 (polar htd_1 (- (* 0.5 pi)) 18000))
  	(draw_CWZ htd_1)
	(prin1)
 )

(defun drawHJX (/  jf_bias dxd_ent_ptlst dxd_ent)
	(Berni_Start)
  	(vl-load-com)
  	;(setq jifang nil)
  	(setq tmp_layer "test1") 
	(if (= (tblsearch "Layer" tmp_layer) nil) (command-s "Layer" "new" tmp_layer "color" "3" tmp_layer "")) (setq bltc (cons tmp_layer bltc))
  	(if (= (tblsearch "Layer" "test2") nil) (command-s "Layer" "new" "test2" "color" "3" "test2" "")) (setq bltc (cons "test2" bltc))
	(setq tmp_layer_xc "cc_xc11")
  	(if (= (tblsearch "Layer" tmp_layer_xc) nil) (command-s "Layer" "new" tmp_layer_xc "color" "4" tmp_layer_xc "")) (setq bltc (cons tmp_layer bltc))
  	
  	(setq gllst (list (cons 0 "LINE") '(8 . "test1,test2")))
  	;;;相关信息获取	
  	(setq gjx_ksh (dcl-Control-GetList drawCCTV/Form1/ListBox9))
  	(setq gjx_ksh (hc-string gjx_ksh))
  	(setq wireway_layer (dcl-Control-GetList drawCCTV/Form1/ListBox12))
  	(setq wireway_layer (hc-string wireway_layer))
  	(setq gg_layer (dcl-Control-GetList drawCCTV/Form1/ListBox13))
  	(setq gg_layer (hc-string gg_layer))
  	
  	(setq cctv_name_list (dcl-Control-GetList drawCCTV/Form1/ListBox8))
  	(setq cctv_name_list (hc-string cctv_name_list))
  	
  	(setq gjx_bias 10000)
  	(setq cctv_coe 1.2)
  	(setq jf_bias (* (atof(dcl-Control-GetText drawCCTV/Form1/TextBox2)) 1000))
  	
  	(setq chksty (tblsearch "style" "宋体"))
        (if (null chksty) (command-s "style" "宋体" "@万能字体" "" "" "" "" "" ""))
  	(setvar 'textstyle "宋体")
        (command-s "celtype" "bylayer")
        (command-s "celweight" 0)
  	(setq draw_pts (getpoint "\n绘制系统图插入点："))


	(gbtc)
;;;沿电缆槽绘制直线
  	(sub1_mlpoint mline_set)  
	(sx)    ;;;删除重复和覆盖的直线部分
        ;(breakall)
  	
	(setq m_i 0 m_n 0 cctv_list_info nil gjx_list nil gjx_name_list nil y 0 jifang_ptt () jifang_dis ())
  
	    (repeat (length jifang)
	       (setq tmp_jifang (nth y jifang))
	       (block_dist tmp_jifang)
	       (setvar "clayer" tmp_layer)
	       (command-s "LINE" tmp_jifang block_dist_pt "")
	       (setq jifang_ptt (cons block_dist_pt jifang_ptt))
	       (setq jifang_dis (cons (distance block_dist_pt tmp_jifang) jifang_dis))
	       
	       (setq y (1+ y))
   	    )
	    (setq y 0)

	   (setq HJX_set gjx_set)

	
        
  	(if (dcl-Control-GetList drawCCTV/Form1/ListBox11) (sub10_dxd_dist))
  	(setvar "clayer" tmp_layer)
  	(sx)
  	(breakobj)
        ;(breakall)
  	;(dktc)
	(setq tmp_dis 0 end_dis 1000000 end_drawlist ())
	(command-s "layer" "off" wireway_layer "")
  	(if (/= (sslength HJX_set) nil)
	  (progn
	     (repeat (sslength HJX_set)
	    	(setq single_HJX (ssname HJX_set m_i))
	    	(block_base single_HJX)
	    	(setq single_HJX_base block_base1)
	    	(sub1_block_name single_HJX_base)
	    	(setq single_HJX_name sub1_block_name1)
	        ;(setq gjx_name_list (append (list single_HJX_name) gjx_name_list))
	    	;(block_dist single_cctv_base)
	        (sxj_dist single_HJX_base)
	    	(setq single_HJX_pt block_dist_pt)
	    	;(setq single_cctv_dist (distance single_cctv_pt single_cctv_base))
	    	(break_line single_HJX_pt)
	        (setq line_pt_lst1 line_pt_lst)
	        
	        ;(setq tmp_gjx_dis ())
		;(repeat (length gjx_name_list)
	       (repeat (length jifang)
      (setq tmp_jifang (nth m_n jifang_ptt))		
	    		(sub1_tt1 single_HJX_pt tmp_jifang)
		 	
	    		(if gjx_cctv_dis (setq tmp_dis (+ (* (+ gjx_cctv_dis single_cctv_dist) cctv_coe)  jf_bias))
			  (setq tmp_dis (+ 1000000 single_cctv_dist )))
		  	;(if (member (car (nth m_n tmp_gjx_list)) jifang_ptt) (setq tmp_dis (+ (* tmp_dis cctv_coe) jf_bias)) (setq tmp_dis (+ (* tmp_dis cctv_coe) HJX_bias)))
		  	(if (< tmp_dis end_dis)
			  (progn
			    (setq end_dis tmp_dis)
			    ;(setq m_ii (VL-POSITION (cons (car (nth m_n tmp_gjx_list)) (cadr (nth m_n tmp_gjx_list))) gjx_list))
			    
			  )
			 )
		  	(setq m_n (1+ m_n))
		       
	        
		)
	       (setq m_n 0)
	       (join_line line_pt_lst1)
	       (setq end_drawlist (append (list (list end_dis single_HJX_name single_HJX)) end_drawlist))
	       (setq m_i (1+ m_i))
	       (setq end_dis 1000000)	;;;重置距离初设值
	       (setq gjx_cctv_dis nil)
	    )
	    ；(setq end_drawlist (REVERSE end_drawlist))
	  )
	)
	(command-s "layer" "on" wireway_layer "")
  	(dktc)
	;(sub1_fenlei end_drawlist)
	;(setq end_drawlist end)
	
	(sub10_draw_sys draw_pts end_drawlist)
	
	(Berni_end)
  	(setq jifang nil)

  	(setq gjx_set nil)
  	(prin1)
 )
(defun c:drawCCTV/Form1/TextButton41#OnClicked (/)
  	 (Dcl_Form_Close drawCCTV_form1)
)

(defun c:drawCCTV/Form1/TextButton22#OnClicked (/)
	  (Dcl_Form_Close drawCCTV_form1)
)


(dcl-Control-GetEventInvoke drawCCTV/Form1)


(defun vl-string->number(string_nu / nu0 nu1 nu2 nu2 number num)  ;;;子程序实现字符串中找出数字
   (setq num (vl-string->list string_nu) nu0 (vl-string->list ".0123456789"))   ;;;num为输入字符串转为的列表，nu0是数字小数点的列?
   (setq nu2(length num))
   (repeat nu2
     (setq nu1(car num))     ?;;提取输入字符串第一个字符
     (setq nu3 (member nu1 nu0) num (cdr num)   ;;;num减少一个输入字符串,nu3如为数字通过赋值nu0关键元素以后的元素保证不为0
   )  
   (if(/= nu3 nil)
   (setq number(cons nu1 number))
   )
)
(vl-list->string (reverse number))
)

(defun c:drawCCTV/Form1/TextButton42#OnClicked (/ test_sets total n text drawpts textstr)
  	(setq test_sets (ssget  '((0 . "*text"))))
  	(setq n 0 total 0)
  	
  	(repeat (sslength test_sets)
	  	(setq text (cdr(assoc 1 (entget (ssname test_sets n)))))
		(if (vl-string-search "摄像机光电缆-" text)
		  (progn
      (setq num (atoi(vl-string->number text)))
		    	(setq total (+ total num))
		  )
		)
	  	(setq n (1+ n))
	)
  	(princ "工程量表插入点")
  	(setq drawpts (getpoint))
  	(setq textstr (strcat "摄像机光电缆-" (itoa total)))
  	(command-s "text" drawpts 250 0 textstr)
 
)
