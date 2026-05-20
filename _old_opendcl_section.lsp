;; Extracted from old drawCCTV -- OpenDCL section
;; Lines 2494-4430

(vl-load-com)
(command "opendcl")
;(dcl_project_load "D:\\OPENDCLsoft\\drawCCTV.odcl" T)
 
(setq dclfile '("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"
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
