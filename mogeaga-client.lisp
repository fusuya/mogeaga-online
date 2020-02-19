
(defparameter *game-state* :title)
(defparameter *cursor* 0)
(defparameter *input-time* 0)
(defparameter *stream* nil)
(defparameter *command* nil)
(defparameter *name* nil)
(defparameter *addr* nil) ;;接続先アドレス
(defparameter *id* 0)
(defparameter *port* 24336)

(defun lookup (key alist)
  (cdr (assoc key alist :test #'equal)))

(defun init-keystate ()
  (with-slots (left right down up z x c enter shift) *keystate*
    (setf left nil right nil down nil up nil
	  z nil x nil c nil enter nil shift nil)))

(defun init-parameter ()
  (setf *game-state* :title
	*cursor* 0
	*input-time* 0
	*stream* nil
	*command* nil
	*id* 0)
  (init-keystate))


    

(defun chomp (line)
  (let ((last-char-pos
         (position-if (lambda (c) (and (not (equal c #\Return)) (not (equal c #\Linefeed))))
                      line :from-end t)))
    (if last-char-pos
        (subseq line 0 (1+ last-char-pos))
      "")))

;;名前とアドレスを読み込む
(defun set-name-and-addr ()
  (with-open-file (in "./setting.txt" :if-does-not-exist nil)
    (if in
	(loop :for line = (read-line in nil)
	     :for i from 0
	   :while line
	   :do (case i
		 (0 (setf *name* (chomp line)))
		 (1 (setf *addr* (chomp line))))))))

;;ブ ラシ生成
(defun set-brush ()
  (setf *brush* (make-array 7 :initial-contents
                              (list
                                (create-solid-brush (encode-rgb 128 0 255))
                                (create-solid-brush (encode-rgb 255 0 0))
                                (create-solid-brush (encode-rgb 1 255 0))
                                (create-solid-brush (encode-rgb 0 0 255))
                                (create-solid-brush (encode-rgb 255 255 0))
                                (create-solid-brush (encode-rgb 0 255 255))
                                (create-solid-brush (encode-rgb 255 0 255))))))

(defun set-font ()
  (setf *font140* (create-font "MSゴシック" :height 140)
	*font90* (create-font "MSゴシック" :height 90)
	*font70* (create-font "MSゴシック" :height 70)
        *font40* (create-font "MSゴシック" :height 40)
	*font30* (create-font "MSゴシック" :height 22 :width 9)
        *font20* (create-font "MSゴシック" :height 25 :width 12 :weight (const +fw-bold+))
	*font10* (create-font "MSゴシック" :height 16 :width 8)))

(defun delete-font ()
  (delete-object *font140*)
  (delete-object *font40*)
  (delete-object *font30*)
  (delete-object *font90*)
  (delete-object *font70*)
  (delete-object *font10*)
  (delete-object *font20*))

(defun delete-object-array (arr)
  (loop for i across arr
     do (delete-object i)))

(defun delete-brush ()
  (delete-object-array *brush*))

(defun delete-images ()
  (delete-object *p-img*)
  (delete-object *objs-img*)
  (delete-object *p-atk-img*)
  (delete-object *hammer-img*)
  (delete-object *anime-monsters-img*)
  (delete-object *buki-img*))

(defun load-images ()
  (setf *objs-img* (load-image "./img/objs-img2.bmp" :type :bitmap
			     :flags '(:load-from-file :create-dib-section))
	*p-img* (load-image "./img/p-ido-anime.bmp" :type :bitmap
			     :flags '(:load-from-file :create-dib-section))
	*p-atk-img* (load-image "./img/p-atk-anime.bmp" :type :bitmap
			     :flags '(:load-from-file :create-dib-section))
	*hammer-img* (load-image "./img/hammer-anime.bmp" :type :bitmap
			     :flags '(:load-from-file :create-dib-section))
	*anime-monsters-img* (load-image "./img/monsters.bmp" :type :bitmap
					 :flags '(:load-from-file :create-dib-section))
	*buki-img* (load-image "./img/buki-anime.bmp" :type :bitmap
			     :flags '(:load-from-file :create-dib-section))))


;;キー押したとき
(defun moge-keydown (wparam)
  (with-slots (left right down up z x c q enter shift) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
        (:left (setf left t))
	(:shift (setf shift t))
        (:right (setf right t))
        (:down (setf down t))
        (:up (setf up t))
        (:return (setf enter t))
        (:keyz (setf z t))
        (:keyx (setf x t))
	(:keyc (setf c t))
        (:keyq ;; quit
	 (setf q t))))))
          ;;(send-message hwnd (const +wm-close+) nil nil))))))

;;キー話したとき
(defun moge-keyup (wparam)
  (with-slots (left right down up z x q c enter shift) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
        (:left (setf left nil))
	(:shift (setf shift nil))
        (:right (setf right nil))
        (:down (setf down nil))
        (:up (setf up nil))
        (:return (setf enter nil))
        (:keyx (setf x nil))
	(:keyc (setf c nil))
	(:keyq (setf q nil))
        (:keyz (setf z nil))))))



;;transparent-blt
(defun trans-blt (x y w-src h-src w-dest h-dest)
  (transparent-blt *hmemdc* x y *hogememdc* 0 0 :width-source w-src
		   :height-source h-src
		   :width-dest w-dest :height-dest h-dest
		   :transparent-color (encode-rgb 0 255 0)))

(defun new-trans-blt (x y x-src y-src w-src h-src w-dest h-dest)
  (transparent-blt *hmemdc* x y *hogememdc* x-src y-src :width-source w-src
		   :height-source h-src
		   :width-dest w-dest :height-dest h-dest
		   :transparent-color (encode-rgb 0 255 0)))


;;pの受けたダメージ表示
(defun render-dmg (p color)
  (let ((dmg (lookup "dmg" p)))
    (when dmg
      (let ((x (lookup "x" dmg))
	    (y (lookup "y" dmg))
	    (num (lookup "dmg-num" dmg)))
	(select-object *hmemdc* *font20*)
      
      (set-bk-mode *hmemdc* :transparent)
      ;;縁取り
      (set-text-color *hmemdc* (encode-rgb 0 0 0))
      (text-out *hmemdc* (format nil "~d" num) (- x 2) y)
      (text-out *hmemdc* (format nil "~d" num) (+ x 2) y)
      (text-out *hmemdc* (format nil "~d" num) x (- y 2))
      (text-out *hmemdc* (format nil "~d" num) x (+ y 2))
      ;;
      (set-text-color *hmemdc* color)
      (text-out *hmemdc* (format nil "~d" num) x y)))))

;;アニメ表示
(defun render-enemy (obj anime-num)
  (let* ((x (lookup "x" obj)) (y (lookup "y" obj))
	 (w (lookup "w" obj)) (h (lookup "h" obj)) (dead (lookup "dead" obj))
	 (img (lookup "img" obj))
	 (moto-w (or (lookup "moto-w" obj) (lookup "w" obj)))
         (moto-h (or (lookup "moto-h" obj) (lookup "h" obj))))
    (when (null dead) ;;死んでなかったら表示
      (select-object *hogememdc* *anime-monsters-img*)
      (new-trans-blt x y (* moto-w img) (* moto-h anime-num)
		     moto-w moto-h w h))))



;;敵表示
(defun render-enemies (mes)
  (let ((enemies (lookup "enemies" mes)))
    (loop for e in enemies
       do (let ((anime-img (lookup "anime-img" e)))
	    (render-enemy e anime-img)))))




;;現在の方向
(defun p-dir-num (dir)
  (cond
    ((string= dir "UP") +up+)
    ((equal dir "DOWN") +down+)
    ((equal dir "RIGHT") +right+)
    ((equal dir "LEFT") +left+)))


(defun render-player-name (name x y)
  (let* ((len (length name))
	 (mid (+ x 12)) (namex (- mid (* len 8)))
	 (namey (- y 18)))
    (select-object *hmemdc* *font10*)
    (set-text-color *hmemdc* (encode-rgb 255 255 255))
    (set-bk-mode *hmemdc* :transparent)
    (text-out *hmemdc* (format nil "~a" name) namex namey)))

;;攻撃時の描画
(defun render-p-atk (obj atk-img)
  (let* ((x (lookup "x" obj)) (y (lookup "y" obj)) (name (lookup "name" obj))
	 (w (lookup "w" obj)) (h (lookup "h" obj)) (dir1 (lookup "dir" obj))
	 (moto-w (or (lookup "moto-w" obj) (lookup "w" obj)))
         (moto-h (or (lookup "moto-h" obj) (lookup "h" obj)))
	 (img (lookup "img" obj)) (buki (lookup "buki" obj))
	 (xb (lookup "x" buki)) (yb (lookup "y" buki)))
    (render-player-name name x y)
    (let ((dir (p-dir-num dir1)))
      (cond
	((eq dir +down+)
	 (select-object *hogememdc* *p-atk-img*)
	 (new-trans-blt x y  (* w img) (* h dir) moto-w moto-h w h)
	 (select-object *hogememdc* atk-img)
	 (new-trans-blt xb yb (* w img) (* h dir) moto-w moto-h w h))
	(t
	 (select-object *hogememdc* atk-img)
	 (new-trans-blt xb yb (* w img) (* h dir) moto-w moto-h w h)
	 (select-object *hogememdc* *p-atk-img*)
	 (new-trans-blt x y (* w img) (* h dir) moto-w moto-h w h))))))



  
;;プレイヤー表示
(defun render-player (obj)
  (let ((atk-now (lookup "atk-now" obj))
	(hammer-now (lookup "hammer-now" obj)))
    (cond
      (atk-now
       (render-p-atk obj *buki-img*))
      (hammer-now
       (render-p-atk obj *hammer-img*))
      (t
       (let ((x (lookup "x" obj)) (y (lookup "y" obj)) (name (lookup "name" obj))
	     (w (lookup "w" obj)) (h (lookup "h" obj)) (dir (lookup "dir" obj))
	     (moto-w (or (lookup "moto-w" obj) (lookup "w" obj)))
             (moto-h (or (lookup "moto-h" obj) (lookup "h" obj)))
	     (img (lookup "img" obj)))
	 (render-player-name name x y)
	 (select-object *hogememdc* *p-img*)
	 (new-trans-blt x y (* moto-w img) (* moto-h (p-dir-num dir))
			moto-w moto-h w h))))))



;;*objs-img*の描画
(defun render-objs-img (obj)
  (let ((x (lookup "x" obj)) (y (lookup "y" obj))
	(w (lookup "w" obj)) (h (lookup "h" obj))
	(moto-w (or (lookup "moto-w" obj) (lookup "w" obj)))
        (moto-h (or (lookup "moto-h" obj) (lookup "h" obj)))
	(img (lookup "img" obj)))
    (select-object *hogememdc* *objs-img*)
    (new-trans-blt x y (* moto-w img) 0
		   moto-w moto-h w h)))


;;プレイヤーのステータス表示
(defun render-p-status (p)
  (let* ((num 10) (name (lookup "name" p))
	 (level (lookup "level" p)) (hp (lookup "hp" p))
	 (maxhp (lookup "maxhp" p)) (str (lookup "str" p))
	 (def (lookup "def" p)) (exp (lookup "exp" p))
	 (lvupexp (lookup "lvup-exp" p)) (hammer (lookup "hammer" p)))
    (macrolet ((hoge (n)
		 `(incf ,n 25)))
      (select-object *hmemdc* *font30*)
      (set-text-color *hmemdc* (encode-rgb 255 255 255))
      (set-bk-mode *hmemdc* :transparent)
      (text-out *hmemdc* (format nil "~a" name) (+ *map-w* 10) num)
      (text-out *hmemdc* (format nil "Lv:~2d" level) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "HP:~2d/~2d" hp maxhp) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "攻:~2d" str) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "防:~2d" def) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "exp") (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "~3d/~3d" exp lvupexp) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "ハンマー") (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "残り:~d回" hammer) (+ *map-w* 10) (hoge num)))))
      ;;;(text-out *hmemdc* (format nil "モゲアーガの塔 ~2,'0d階" (stage *p*)) 10 (+ *map-h* 10)))))
      ;;(text-out *hmemdc* (format nil "~2,'0d:~2,'0d:~2,'0d:~2,'0d" h m s ms) 200 (+ *map-h* 10))))))

(defun render-other-p-status (p x)
  (let* ((num (+ *map-h* 10)) (name (lookup "name" p))
	 (level (lookup "level" p)) (hp (lookup "hp" p))
	 (maxhp (lookup "maxhp" p)) (str (lookup "str" p)))
    (macrolet ((hoge (n)
		 `(incf ,n 25)))
      (select-object *hmemdc* *font30*)
      (set-text-color *hmemdc* (encode-rgb 255 255 255))
      (set-bk-mode *hmemdc* :transparent)
      (text-out *hmemdc* (format nil "~a" name) (+ 10 (* x 100)) num)
      (text-out *hmemdc* (format nil "Lv:~2d" level) (+ 10 (* x 100)) (hoge num))
      (text-out *hmemdc* (format nil "HP:~2d/~2d" hp maxhp) (+ 10 (* x 100)) (hoge num))
      (text-out *hmemdc* (format nil "攻:~2d" str) (+ 10 (* x 100)) (hoge num)))))

;;プレイヤーたち描画
(defun render-players (players now-stage)
  (let ((x 0))
    (dolist (player players)
      (if (= *id* (lookup "id" player))
	  (render-p-status player)
	  (progn
	   (render-other-p-status player x)
	   (incf x)))
      (when (= (lookup "stage" player) now-stage)
	(render-player player)))))

(defun render-objects (objs)
  (loop for obj in objs
        do (render-objs-img obj)))

;;ブロック描画
(defun render-block (mes)
  (let ((blocks (lookup "blocks" mes)))
    (loop for blo in blocks
       do (render-objs-img blo))))

;;床描画
(defun render-yuka (mes)
  (let ((yuka (lookup "yuka" mes)))
    (loop for obj in yuka
       do (render-objs-img obj))))

;;鍵とか描画
(defun render-item (mes)
  (let ((item (lookup "item" mes)))
    (loop for obj in item 
       do (render-objs-img obj))))




;;バックグラウンド
(defun render-background ()
  (select-object *hmemdc* (get-stock-object :black-brush))
  (rectangle *hmemdc* 0 0 *change-screen-w* *change-screen-h*))



;;HPバー表示
(defun render-hpbar (e hp maxhp)
  (let* ((len (floor (* (/ hp maxhp) *hpbar-max*)))
	 (x (lookup "x" e)) (y (lookup "y" e))
	 (hp-w (+ x len)))
    ;;残りHP
    (select-object *hmemdc* (aref *brush* +green+))
    (rectangle *hmemdc* x  (- y 15) hp-w y)
    ;;減ったHP
    (select-object *hmemdc* (aref *brush* +red+))
    (rectangle *hmemdc* hp-w (- y 15) (+ hp-w (- *hpbar-max* len)) y)))

;;ダメージ表示
(defun render-damage (e color)
  (with-slots (dmg) e
    (when dmg
      (select-object *hmemdc* *font20*)
      
      (set-bk-mode *hmemdc* :transparent)
      ;;縁取り
      (set-text-color *hmemdc* (encode-rgb 0 0 0))
      (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (- (x dmg) 2) (y dmg))
      (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (+ (x dmg) 2) (y dmg))
      (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (x dmg) (- (y dmg) 2))
      (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (x dmg) (+ (y dmg) 2))
      ;;
      (set-text-color *hmemdc* color)
      (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (x dmg) (y dmg))
      )))

;;全てのダメージ表示
(defun render-all-damage (mes)
  (let ((players (lookup "players" mes))
	(enemies (lookup "enemies" mes)))
    (dolist (p players)
      (render-dmg p (encode-rgb 255 147 122)))
    ;;(render-hpbar *p*)
    (dolist (e enemies)
      (let ((maxhp (lookup "maxhp" e))
	    (hp (lookup "hp" e)) (dead (lookup "dead" e)))
      (render-dmg e (encode-rgb 255 255 255))
      (when (and (/= maxhp hp)
		 (null dead))
	(render-hpbar e hp maxhp))))))

(defvar *backgrounds* nil)

;;マップを表示
(defun render-map (mes)
  (let ((backgrounds (lookup "backgrounds" mes)))
    (if backgrounds
        (setf *backgrounds* (coerce backgrounds 'vector))))

  (let* ((players (lookup "players" mes))
	 (mine (find *id* players :key #'(lambda (x) (lookup "id" x))))
	 (now-stage (lookup "stage" mine))
	 (donjon (lookup "donjon" mine)))
    (render-background)
    (render-objects (lookup "yuka" (aref *backgrounds* (1- now-stage))))
    (render-objects (lookup "blocks" (aref *backgrounds* (1- now-stage)))) ; hard-block
    (render-block donjon) ; soft-block
    (render-item donjon)
    (render-enemies donjon)
    (render-players players now-stage)
    (render-all-damage donjon)
    (render-events (lookup "events" mes))))

(defun render-events (events)
  (dolist (e events)
    (if (string= (first e) "se")
        (play-sound (second e) '(:filename :async)))))

;;test
(defun render-test ()
  (select-object *hogememdc*  *anime-monsters-img*)
  (transparent-blt *hmemdc* 0 0 *hogememdc* 0 32 :width-source 32
		   :height-source 32
		   :width-dest 32 :height-dest 32
		   :transparent-color (encode-rgb 0 255 0)))

;;タイトル画面
(defun render-title-gamen (mes1 mes2)
  (render-background)
  (select-object *hmemdc* *font140*)
  (set-bk-mode *hmemdc* :transparent)
  (set-text-color *hmemdc* (encode-rgb 0 155 255))
  (text-out *hmemdc* (format nil "~a" mes1) 130 10)
  
  (select-object *hogememdc* *objs-img*)
  ;;カーソル表示
  (new-trans-blt 380 (+ 400 (* *cursor* 50)) (* 32 +cursor+) 0 32 32 32 32)
  (select-object *hmemdc* *font40*)
  (text-out *hmemdc* "setteing.txtで名前と接続先アドレスを設定してください" 100 150)
  (text-out *hmemdc* "1行目に名前、2行目にアドレスです" 100 190)
  (text-out *hmemdc* (format nil "名前：~a" *name*) 100 300)
  (text-out *hmemdc* (format nil "接続先アドレス：~a" *addr*) 100 340)
  (set-text-color *hmemdc* (encode-rgb 255 255 255))
  (text-out *hmemdc* (format nil "~a" mes2) 430 400)
  (text-out *hmemdc* (format nil "名前とアドレスを読み込む") 430 450)
  (text-out *hmemdc* (format nil "おわる") 430 500))





;;ゲーム全体描画
(defun render-game (hdc)
  (transparent-blt hdc 0 0 *hmemdc* 0 0
		   :width-dest *change-screen-w* :height-dest *change-screen-h*
		   :width-source (rect-right *c-rect*) :height-source (rect-bottom *c-rect*) 
		   :transparent-color (encode-rgb 0 255 0)))


(defun decode-u32 (arr)
  (+ (ash (aref arr 0) 24)
     (ash (aref arr 1) 16)
     (ash (aref arr 2) 8)
     (ash (aref arr 3) 0)))

(defun read-message (stream)
  (let ((arr (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence arr stream)
    (let* ((len (decode-u32 arr))
           (buf (make-array len :element-type '(unsigned-byte 8))))
      (read-sequence buf stream)
      (let ((str (babel:octets-to-string (gzip-stream:gunzip-sequence buf) :encoding :utf-8)))
        (v:debug :network str)
        (jonathan:parse str :as :alist)))))

;;(defun message-type (message)
;;  (intern (map 'string #'char-upcase (lookup "type" message))))
(defun message-type (message)
  (lookup "type" message))

;;mes表示
(defun do-msg (mes x y font)
  (select-object *hmemdc* font)
  (set-bk-mode *hmemdc* :transparent)
  (set-text-color *hmemdc* (encode-rgb 0 155 255))
  (text-out *hmemdc* mes x y))


(defun looks-like-ip (str)
  (every (lambda (c)
           (case c
             ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.) t)
             (t nil))) str))



(defun make-socket-stream (address port)
  (let ((sock (make-instance 'inet-socket :type :stream :protocol :tcp))
        (addr (if (looks-like-ip address)
                  (make-inet-address address)
                (handler-case
                 (car (host-ent-addresses (get-host-by-name address)))

                 (host-not-found-error
                  (c)
                  (declare (ignore c))
		  (do-msg (format nil "ホスト~aのIPアドレスがわかりませんでした。"
					     address) 70 100 *font40*)
                 ;; (do-msg (format nil "ホスト~aのIPアドレスがわかりませんでした。" address))
                  (return-from make-socket-stream nil))))))

    (if (ignore-errors (socket-connect sock addr port) t)
	(socket-make-stream sock :input t :output t :element-type :default :external-format :utf-8)
      nil)))


;;ゲームを開始する
(defun start-game ()
  (setf *stream* (make-socket-stream *addr* *port*))
  (if *stream*
      (progn
	(loop :for byte :across (babel:string-to-octets (format nil "~A~%" *name*) :encoding :utf-8) :do
	     (write-byte byte *stream*))
	;;(format stream "~a~%" (text name))
	(force-output *stream*)

	(setf *id* (parse-integer (read-line *stream*)))

	(do-msg "受付完了：ゲーム開始待ち中" 100 100 *font40*)

	(setf *game-state* :wait-game-start))
      (progn
	(setf *game-state* :connect-error))))

;;タイトル画面での操作
(defun update-title-and-ending-gamen (hwnd)
  (with-slots (up down z enter) *keystate*
    (incf *input-time*)
    (when (zerop (mod *input-time* 8))
      (cond
	(up
	 (if (= *cursor* 0)
	     (setf *cursor* 2)
	     (decf *cursor*)))
	(down
	 (if (= *cursor* 2)
	     (setf *cursor* 0)
	     (incf *cursor*)))
	((or z enter)
	 (cond
	   ((= *cursor* 0)
	    (start-game))
	   ((= *cursor* 1)
	    (set-name-and-addr))
	   ((= *cursor* 2)
	    (send-message hwnd (const +wm-close+) nil nil))))))))


(defun display-status (status-message)
  (let ((players (lookup "players" (lookup "map" status-message)))
	(num 30))
    (macrolet ((hoge (n)
		 `(incf ,n 40)))
      (render-background)
      (do-msg (format nil "受付完了：ゲーム開始待ち中") 100 num *font40*)
      (do-msg (format nil "参加者待ち(開始まで~a秒)" (lookup "timeout-seconds" status-message))
	100 (hoge num) *font40*)
      (do-msg "参加プレーヤー:" 100 (hoge num) *font40*)
      (dolist (p players)
	(do-msg (format nil  "~a~%" (lookup "name" p))
	  100 (hoge num) *font40*)))))



(defun keystate->command ()
  (with-slots (z c q up down right left) *keystate*
    (cond
      (z  "Z")
      (c  "C")
      (q  "Q")
      (up "UP")
      (down "DOWN")
      (right "RIGHT")
      (left "LEFT")
      (t "STAY"))))

;;プレイ中のループ
(defun wait-game-start ()
  (if (listen *stream*)
      (progn
	(let* ((message (read-message *stream*))
	       (mes (message-type message)))
	  ;;(print (message-type message))
	  ;;(case (message-type message)
	  (cond
	    ((string= mes "status")
	     (display-status message))
	    ((string= mes "playing")
	     (render-map message)

             (when (not (equal *command* (keystate->command)))
               (setf *command* (keystate->command))
               (format *stream* "~a~%" *command*)
               (force-output *stream*))

             )
	    ;;(after-idle #'send-command))
	    ((string= mes "quit")
	     (init-parameter))
	    (t
	     (error (format nil "予期しないメッセージタイプ: ~s"
			    (message-type message)))))))))
      ;;(sleep 0.01)))

;;ゲームループ
(defun main-game-loop (hwnd)
  (cond
    ((eq *game-state* :title)
     (render-title-gamen "モゲアーガの塔" "はじめる")
     (update-title-and-ending-gamen hwnd))
    ((eq *game-state* :connect-error)
     (render-title-gamen "接続失敗" "もう一回")
     (update-title-and-ending-gamen hwnd))
    ((eq *game-state* :wait-game-start)
     (wait-game-start))
    ((eq *game-state* :playing))
    ((eq *game-state* :dead))
    ((eq *game-state* :ending)))
  (invalidate-rect hwnd nil nil))


;;ウィンドウサイズ変更時に画像拡大縮小する
(defun change-screen-size (lp)
  (let* ((change-w (loword lp))
	 (change-h (hiword lp)))
    (setf *change-screen-w* change-w
	  *change-screen-h* change-h)))

;;クライアント領域を*client-w* *client-h*に設定
(defun set-client-size (hwnd)
  (let* ((rc (get-client-rect hwnd))
         (rw (get-window-rect hwnd))
         (new-w (+ *client-w* (- (- (rect-right rw) (rect-left rw))
                               (- (rect-right rc) (rect-left rc)))))
         (new-h (+ *client-h* (- (- (rect-bottom rw) (rect-top rw))
                               (- (rect-bottom rc) (rect-top rc))))))
    (set-window-pos hwnd nil 0 0 new-w new-h '(:no-move :no-zorder))))

;;proc
(defwndproc moge-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (set-brush)
     (set-font)
     (load-images)
     (init-parameter)
     (set-name-and-addr)
     ;;(set-client-size hwnd)
     (setf *c-rect* (get-client-rect hwnd))
     ;;(setf *screen-w* (rect-right *c-rect*)
	;;   *screen-h* (rect-bottom *c-rect*))
     ;;(setf *screen-center-x* (+ (rect-right *c-rect*)
     ;;                         (floor (- (rect-left *c-rect*) (rect-right *c-rect*)) 2)))

     ;;(set-layered-window-attributes hwnd (encode-rgb 255 0 0) 0 (const +lwa-colorkey+))
     (with-dc (hdc hwnd)
       (setf *hmemdc* (create-compatible-dc hdc)
             *hbitmap* (create-compatible-bitmap hdc (rect-right *c-rect*) (rect-bottom *c-rect*))
	     *hogememdc* (create-compatible-dc hdc)
             *hogebitmap* (create-compatible-bitmap hdc (rect-right *c-rect*) (rect-bottom *c-rect*)))
       (select-object *hmemdc* *hbitmap*)
       (select-object *hogememdc* *hogebitmap*)))
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (render-game hdc)))
    ((const +wm-size+)
     (change-screen-size lparam))
     ;;(remake-obj))
    ((const +wm-close+)
     (destroy-window hwnd))
    ;;((const +wm-timer+)
    ;; (invalidate-rect hwnd nil nil))
    ((const +wm-keydown+)
     (moge-keydown wparam))
    ((const +wm-keyup+)
     (moge-keyup wparam))
    ((const +wm-destroy+)
     (delete-dc *hmemdc*)
     (delete-object *hbitmap*)
     (delete-dc *hogememdc*)
     (delete-object *hogebitmap*)
     (delete-brush)
     (delete-images)
     (delete-font)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


;;メイン
(defun moge ()
  (setf *random-state* (make-random-state t))
  (register-class "MOGE" (callback moge-wndproc)
		  :styles (logior-consts +cs-hredraw+ +cs-vredraw+)
                  :cursor (load-cursor :arrow)
                  :background (create-solid-brush (encode-rgb 0 255 0)))
  (let ((hwnd (create-window "MOGE"
                             :window-name "もげぞうの塔クラ"
                             ;;:ex-styles  (logior-consts +WS-EX-LAYERED+ +ws-ex-composited+) ;;透明
                             :styles (logior-consts +ws-overlappedwindow+ +ws-visible+)
                             :x 400 :y 100 :width *screen-w* :height *screen-h*))
        (msg (make-msg)))
    ;;(init-game)
    (show-window hwnd) 
    (update-window hwnd)
    (do ((done nil))
        (done)
      (let ((m (ftw:peek-message msg :remove-msg :remove :error-p nil)))
        (cond
          (m
            ;;(let ((r (get-message msg)))
            (cond
              ((= (msg-message msg) (const +wm-quit+))
               (setf done t))
              (t
               (translate-message msg)
               (dispatch-message msg))))
          (t
            (sleep 0.01)
            (main-game-loop hwnd)))))
    (msg-wparam msg)))
