;;TODO 
;;マルチスレッドの終わらせ方わからん
(defvar *port* 24336)
(defvar +client-read-timeout+ 10)
(defvar +registration-timeout+ 10) ;;一人目の参加から何秒でゲームを開始するか。

;;TODO
(defun create-donjon (g i)
  (let ((d (make-donjon :tate *tate-block-num* :yoko *yoko-block-num* :stage i)))
    (maze d (length (players g)))
    (setf (aref (donjons g) i) d)))


;;ダンジョンを最初に全部作成しておく
(defun make-donjons ()
  (let ((donjons-arr (make-array (1+ *donjons-num*))))
    (loop :for i :from 1 :to *donjons-num*
       :do (let ((d (make-donjon :tate *tate-block-num* :yoko *yoko-block-num* :stage i)))
	     (maze d)
	     (setf (aref donjons-arr i) d)))
    donjons-arr))
	 
       

(defun new-game ()
  (make-instance 'game :donjons (make-donjons)))

;;効果音ならす
(defmethod game-play-sound ((g game) path stage)
  (setf (events g) (append (events g) (list `("se" ,path ,stage)))))

;;時間変換
(defun get-hms (n)
  (multiple-value-bind (h m1) (floor n 3600000)
    (multiple-value-bind (m s1) (floor m1 60000)
      (multiple-value-bind (s ms1) (floor s1 1000)
	(multiple-value-bind (ms) (floor ms1 10)
	  (values h m s ms))))))

;;1以上のランダムな数
(defun randval (n)
  (1+ (random n)))

(defun rand+- (n)
  (let ((a (1+ (random n))))
    (if (= (random 2) 1)
	a
	(- a))))

;;'(:up :down :right :left)
(defun rand-dir (lst new-lst)
  (if (null lst)
      new-lst
      (let ((hoge (nth (random (length lst)) lst)))
	(rand-dir (remove hoge lst) (cons hoge new-lst)))))

;;判定
(defun obj-hit-p (obj1 obj2)
  (let ((obj1-px (+ (x obj1) (w/2 obj1)))
	(obj1-py (+ (y obj1) (h/2 obj1)))
	(obj2-px (+ (x obj2) (w/2 obj2)))
	(obj2-py (+ (y obj2) (h/2 obj2))))
    (and (< (abs (- obj1-px obj2-px)) (+ (w/2 obj1) (w/2 obj2)))
	 (< (abs (- obj1-py obj2-py)) (+ (h/2 obj1) (h/2 obj2))))))


;;ブロックとプレイヤーの当たり判定
(defun block-hit-p (g p)
  (let ((hoge nil))
    (loop for obj in (donjon-blocks (aref (donjons g) (stage p)))
       do (when (and (or (eq (obj-type obj) :hard-block)
			 (eq (obj-type obj) :soft-block))
		     (obj-hit-p p obj))
	    (setf hoge obj)
	    (return)))
    hoge))


;;階層変わったときユニット初期位置
(defun player-init-pos (donjon)
  (nth (random (length (donjon-path donjon))) (donjon-path donjon)))

;;プレイヤーとフロアにあるアイテムの当たり判定
(defun player-hit-item (p g)
  (with-slots (donjons) g
    (let ((donjon (aref donjons (stage p))))
      (loop for obj in (donjon-objects donjon)
	 do
	   (when (obj-hit-p p obj)
	     (case (obj-type obj)
	       (:key
		(game-play-sound g *get-item-wav* (stage p))
		(setf (key? p) t
		      (img obj) +yuka+
		      (obj-type obj) :yuka))
	       (:potion
		(game-play-sound g *get-potion-wav* (stage p))
		(setf (hp p) (maxhp p)
		      (donjon-objects donjon)
		      (remove obj (donjon-objects donjon) :test #'equal)))
	       (:sword
		(with-slots (buki) p
		  (game-play-sound g *get-item-wav* (stage p))
		  (setf (donjon-objects donjon)
			(remove obj (donjon-objects donjon) :test #'equal))
		  (when (> *buki-list-len* (atk buki))
		    (setf (name buki) (nth (atk buki) *buki-list*))
		    (incf (atk buki))))) ;;武器の攻撃力は１づつ上がる))
	       (:hammer
		(game-play-sound g *get-item-wav* (stage p))
		(incf (hammer p))
		(setf (donjon-objects donjon)
		      (remove obj (donjon-objects donjon) :test #'equal)))
	       (:boots
		(game-play-sound g *get-item-wav* (stage p))
		(push obj (item p))
		(setf (ido-spd p) 3 ;;移動速度アップ
		      (boots? p) t
		      (donjon-objects donjon)
		      (remove obj (donjon-objects donjon) :test #'equal)))
	       (:door
		(when (key? p)
		  (game-play-sound g *door-wav* (stage p))
		  (incf (stage p))
		  (let ((pos (player-init-pos (aref donjons (stage p)))))
		    (setf (key? p) nil
			  (obj-type obj) :open-door
			  (img obj) +kaidan+
			  (x p) (* (car pos) *blo-w46*)
			  (y p) (* (cadr pos) *blo-h46*)))))
	       (:open-door
		(game-play-sound g *door-wav* (stage p))
		(incf (stage p)))))))))

;;ダメージ計算
(defmethod damage-calc ((atker player) defender)
  (with-slots (buki) atker
    (let* ((a1 (+ (str atker) (atk buki))))
      (max 1 (floor (* (- a1 (/ (def defender) 2)) (/ (+ 99 (random 55)) 256)))))))

;;ダメージ計算
(defmethod damage-calc ((atker enemy) defender)
  (let* ((a1 (str atker)))
    (max 1 (floor (* (- a1 (/ (def defender) 2)) (/ (+ 99 (random 55)) 256))))))

;;レヴェルアップ時ステータス上昇
(defun status-up (atker)
  (incf (maxhp atker) (1+ (random 3)))
  (incf (str atker) (random 3))
  (incf (def atker) (random 3)))

;;経験値取得
(defun player-get-exp (atker defender g)
  (when (eq 'remote-player (type-of atker))
    (incf (expe atker) (expe defender))
    (loop while (>= (expe atker) (lvup-exp atker))
       do
	 (game-play-sound g *lvup-wav* (stage atker))
	 (status-up atker)
	 (incf (level atker))
	 (setf (expe atker) (- (expe atker) (lvup-exp atker)))
	 (incf (lvup-exp atker) 20))))

;;ダメージ計算して　表示する位置とか設定
(defun set-damage (atker defender g)
  (with-slots (x y obj-type hp atk-spd) defender
    (let* ((dmg-x (+ x 10))
	   (dmg-y (+ y 20))
	   (dmg-num (damage-calc atker defender))
	   (x-dir (if (eq (dir atker) +left+) +left+ +right+))
	   (dmg (make-instance 'dmg-font :x dmg-x :y dmg-y :dmg-num  dmg-num
			       :y-dir +up+ :x-dir x-dir
			       :color (if (eq (type-of defender) 'remote-player)
					  +red+ +white+)
			       :maxy dmg-y :miny (- dmg-y 15))))
      (if (> (hp defender) dmg-num) ;;hpを減らす
	  (decf (hp defender) dmg-num)
	  (setf (hp defender) 0))
      (when (= 0 (hp defender)) ;; hpが0以下になったら死亡
	(setf (dead defender) t)
	(when (eq (type-of atker) 'remote-player)
	  (incf (totaldmg atker) dmg-num)
	  (player-get-exp atker defender g)))
      (push dmg (donjon-dmgs (aref (donjons g) (stage atker)))) ;;ダメージを表示するためのもの
      (when (and (eq obj-type :boss) ;;ボス発狂モード
		 (>= 50 hp))
	(setf (atk-spd defender) 40))
      )))

;;敵と武器の当たり判定
(defun buki-hit-enemy (p g)
  (when (atk-now p)
    (with-slots (buki) p
      (dolist (e (players g))
	(when (and (not (equal p e))
		   (obj-hit-p buki e)
		   (null (dead e)))
	  (setf (atkhit p) t) ;;攻撃があたりました
	  (set-damage p e g)))
      (loop for e in (donjon-enemies (aref (donjons g) (stage p)))
	 do (when (and (obj-hit-p buki e)
		       (null (dead e)))
	      (case (obj-type e)
		((:slime :orc :hydra :dragon :brigand :briball :yote1 :toge :boss)
		 (game-play-sound g *atk-enemy-wav* (stage p))
		 (setf (atkhit p) t) ;;攻撃があたりました
		 (set-damage p e g)))))))) ;;ダメージ処理

;;武器の画像を設定
(defun set-buki-pos (p)
  (with-slots (buki x y dir) p
    (cond
      ((eq dir +down+)
       (setf (x buki) x
	     (y buki) (+ y 20)))
      ((eq dir +up+)
       (setf (x buki) x
	     (y buki) (- y 20)))
      ((eq dir +left+)
       (setf (x buki) (- x 18)
	     (y buki) y))
      ((eq dir +right+)
       (setf (x buki) (+ x 18)
	     (y buki) y)))))

;;方向　キャラの攻撃画像
(defun set-atk-img (p)
  (setf (img p) 0))
	    
;;攻撃画像更新
(defun update-atk-img (p g)
  (incf (atk-c p))
  (when (zerop (mod (atk-c p) (atk-spd p)))
    (incf (img p))
    (when (and (>= 2 (img p))
	       (null (atkhit p)))
      (buki-hit-enemy p g)))
  (when (> (img p) 2)
    (setf (atk-now p) nil
	  (atk-c p) 0
	  (atkhit p) nil
	  (img p) 0)))

(defun update-hammer-img (p)
  (incf (atk-c p))
  (when (zerop (mod (atk-c p) (atk-spd p)))
    (incf (img p)))
  (when (> (img p) 2)
    (setf (hammer-now p) nil
	  (atk-c p) 0
	  (img p) 0)))


;;壁壊す
(defun hammer-hit-kabe (p g)
  (with-slots (buki) p
    (let ((hit? nil)
	  (donjon (aref (donjons g) (stage p))))
      (loop for kabe in (donjon-blocks donjon)
	 do (when (and (obj-hit-p buki kabe)
		       (eq (obj-type kabe) :soft-block))
	      (setf hit? t)
	      (setf (donjon-blocks donjon)
		    (remove kabe (donjon-blocks donjon) :test #'equal))))
	      ;;(setf (obj-type kabe) :yuka
		;;    (img kabe) +yuka+)))
      (when hit?
	(game-play-sound g *atk-block-wav* (stage p))
	(when (>= (hammer p) 1)
	  (decf (hammer p)))))))

;;画像右側めりこみ判定
(defun merikomi-hantei (g p)
  (let ((blo  (block-hit-p g p)))
    (when blo
      (setf (x p) (- (x blo) (w p))))))


;;歩行グラフィック更新
(defun update-ido-anime-img (e)
  (with-slots (walk-c walk-func img) e
    (incf (walk-c e))
    ;;walk-counterが10を超えたら画像更新
    (when (> walk-c 15)
      (cond ;;walk-stateが 0 1 2 1 0 1 2 ってなるようにする
	((= img 0)   (setf walk-func #'+))
	((= img 1))
	((= img 2) (setf walk-func #'-)))
      (setf img (+ (funcall walk-func img 1)))
      (setf walk-c 0))))

;;プレイヤー被ダメージ処理
(defun hit-enemies-player (p g)
  (with-slots (donjons) g
    (loop for e in (donjon-enemies (aref donjons (stage p)))
       do (when (and (obj-hit-p p e)
		     (null (dead e)))
	    (game-play-sound g *damage-wav* (stage p))
	    (set-damage e p g) 
	    (setf (dmg-c p) 50)
	    (case (obj-type e)
	      (:briball
	       (setf (donjon-enemies (aref donjons (stage p)))
		     (remove e (donjon-enemies (aref donjons (stage p))) :test #'equal))))))))
    
;;ランダム方向へ移動 '(:up :down :right :left :stop)
(defun set-rand-dir (g e)
  (loop for d in (rand-dir `(,+up+ ,+down+ ,+right+ ,+left+ +stop+) nil)
     do (cond 
	  ((eq d +stop+)
	   (setf (dir e) +stop+)
	   (return))
	  ((eq d +up+)
	   (decf (y e))
	   (if (block-hit-p g e)
	       (incf (y e))
	       (progn (setf (dir e) +up+)
		      (return))))
	  ((eq d +down+)
	   (incf (y e))
	   (if (block-hit-p g e)
	       (decf (y e))
	       (progn (setf (dir e) +down+)
		      (return))))
	  ((eq d +right+)
	   (incf (x e))
	   (if (block-hit-p g e)
	       (decf (x e))
	       (progn (setf (dir e) +right+)
		      (return))))
	  ((eq d +left+)
	   (decf (x e))
	   (if (block-hit-p g e)
	       (incf (x e))
	       (progn (setf (dir e) +left+)
		      (return)))))))



;;敵の移動
(defun update-enemy-pos (g e)
  (cond
    ((eq (dir e) +up+)
     (decf (y e) (ido-spd e))
     (let ((kabe  (block-hit-p g e)))
       (when kabe
	 (setf (y e) (+ (y kabe) (h kabe)))
	 (set-rand-dir g e))))
    ((eq (dir e) +down+)
     (incf (y e) (ido-spd e))
     (let ((kabe  (block-hit-p g e)))
       (when kabe
	 (setf (y e) (- (y kabe) (h e)))
	 (set-rand-dir g e))))
    ((eq (dir e) +right+)
     (incf (x e) (ido-spd e))
     (let ((kabe  (block-hit-p g e)))
       (when kabe
	 (setf (x e) (- (x kabe) (w e)))
	 (set-rand-dir g e))))
    ((eq (dir e) +left+)
     (decf (x e) (ido-spd e))
     (let ((kabe  (block-hit-p g e)))
       (when kabe
	 (setf (x e) (+ (x kabe) (w kabe)))
	 (set-rand-dir g e))))))

;;dx dy以内のeから見たプレイヤーの方向を返す
(defun enemy-can-atk? (g e dx dy)
  (let ((ret nil))
    (dolist (p (players g))
      (let* ((diffx (- (x e) (x p)))
	     (diffy (- (y e) (y p)))
	     (absx (abs diffx))
	     (absy (abs diffy)))
	(when (and (>= dx absx)
		   (>= dy absy))
	  (cond
	    ((and (>= diffx 0) (>= diffy 0))
	     (if (>= absx absy)
		 (setf ret +left+)
		 (setf ret +up+)))
	    ((and (>= diffx 0) (> 0 diffy))
	     (if (>= absx absy)
		 (setf ret +left+)
		 (setf ret +down+)))
	    ((and (> 0 diffx) (>= diffy 0))
	     (if (>= absx absy)
		 (setf ret +right+)
		 (setf ret +up+)))
	    ((and (> 0 diffx) (> 0 diffy))
	     (if (>= absx absy)
		 (setf ret +right+)
		 (setf ret +down+)))))
	(when ret
	  (return-from enemy-can-atk? ret))))
    ret))

;;プレイヤーにeの攻撃があたる方向 atk-x = 攻撃距離
(defun set-can-atk-dir (g e atk-x atk-y)
  (let ((f-dir (enemy-can-atk? g e atk-x atk-y)))
    (if f-dir
	(setf (dir e) f-dir)
	nil)))

;;スライムの行動
(defun update-slime (g e change-dir-time)
  (incf (dir-c e)) ;;移動カウンター更新
  (update-ido-anime-img e)
  (if (> (dir-c e) change-dir-time)
      (progn (set-rand-dir g e)
	     (setf (dir-c e) 0))
      (update-enemy-pos g e)))

;;オークの攻撃エフェクト追加
(defun add-orc-atk-effect (g e dx dy)
  (let ((atk (make-instance 'enemy :img 0 :obj-type :orc-atk
			    :x (- (x e) dx) :y (- (y e) dy) :stage (stage e)
			    :str (str e) :anime-img +orc-atk+
			    :moto-w (moto-w e) :moto-h (moto-h e)
			    :w (w e) :h (h e) :w/2 (w/2 e) :h/2 (h/2 e))))
    (if (null (block-hit-p g atk)) ;;ブロックにぶつかるなら追加しない
	(push atk (donjon-enemies (aref (donjons g) (stage e)))))))

;;攻撃エフェクト出す方向
(defun check-orc-atk-effect-dir (g e)
  (cond
    ((eq (dir e) +up+) (add-orc-atk-effect g e 0 (h e)))
    ((eq (dir e) +down+) (add-orc-atk-effect g e 0 (- (h e))))
    ((eq (dir e) +left+) (add-orc-atk-effect g e (w e) 0))
    ((eq (dir e) +right+) (add-orc-atk-effect g e (- (w e)) 0))))

;;攻撃エフェクト消えるまで待つ
(defun wait-atk-effect (e wait-time)
  (incf (atk-c e))
  (when (>= (atk-c e) wait-time)
    (setf (atk-now e) nil
	  (atk-c e) 0)))

;;オークの行動
(defun update-orc (g e)
  (cond
    ((atk-now e)
     (wait-atk-effect e *orc-atk-effect-time*))
    (t
     (incf (dir-c e)) ;;移動カウンター更新
     (when (and (= 1 (random 50)) ;;攻撃
		(set-can-atk-dir g e (w e) (h e)))
       (check-orc-atk-effect-dir g e)
       (setf (atk-now e) t))
     (update-ido-anime-img e)
     (if (> (dir-c e) 40)
	 (progn (set-rand-dir g e)
		(setf (dir-c e) 0))
	 (update-enemy-pos g e)))))

;;ヒドラの攻撃エフェクトを敵として追加
(defun add-hydra-atk (g e)
  (let ((atk (make-instance 'enemy :img 0 :obj-type :hydra-atk
			    :anime-img +hydra-atk+ :stage (stage e)
			    :x (- (x e) 32) :y (y e) :str (str e)
			    :moto-w 32 :moto-h 32 :dir (dir e)
			    :w 32 :h 32 :w/2 16 :h/2 16)))
    (setf (centerx atk) (+ (x e) (w/2 e))
	  (centery atk) (+ (y e) (h/2 e)))
    (push atk (donjon-enemies (aref (donjons g) (stage e))))))

;;ヒドラの攻撃更新 ヒドラの周りを一周させる
(defun update-hydra-atk (g e)
  (incf (atk-c e))
  (let* ((radian  (/ (* (deg e) pi) 180))
	 (addx (floor (* (cos radian) 30)))
	 (addy (floor (* (sin radian) 30))))
    ;;(centerx (+ (x e) (w/2 e)))
    ;;(centery (+ (y e) (h/2 e))))
    (setf (x e) (- (+ (centerx e) addx) (w/2 e))
	  (y e) (- (+ (centery e) addy) (h/2 e)))
    (incf (deg e) 10)
    (when (>= (atk-c e) *hydra-atk-effect-time*)
      (setf (donjon-enemies (aref (donjons g) (stage e)))
	    (remove e (donjon-enemies (aref (donjons g) (stage e))) :test #'equal)))))

;;ヒドラの行動
(defun update-hydra (g e)
  (cond
    ((atk-now e)
     (wait-atk-effect e *hydra-atk-effect-time*)) ;;TODO
    (t
     (incf (dir-c e)) ;;移動カウンター更新
     (when (and (= 1 (random 40)) ;;攻撃
		(set-can-atk-dir g e (w e) (h e) ))
       (add-hydra-atk g e)
       (setf (atk-now e) t))
     (update-ido-anime-img e)
     (if (> (dir-c e) 40)
	 (progn (set-rand-dir g e)
		(setf (dir-c e) 0))
	 (update-enemy-pos g e)))))

;;ブリガンドのボール追加
(defun add-bri-ball (g e dx dy)
  (let ((ball (make-instance 'enemy :img 0 :obj-type :briball
			     :anime-img +brigand-ball+ :stage (stage e)
			     :moto-w 32 :moto-h 32 :dir (dir e)
			     :str (str e) :maxhp 1 :hp 1 :def 0
			     :w 16 :h 16 :w/2 8 :h/2 8)))
    (setf (x ball) (- (x e) dx)
	  (y ball) (- (y e) dy))
    (if (null (block-hit-p g ball))
	(push ball (donjon-enemies (aref (donjons g) (stage e)))))))

;;ブリガンドボールの方向
(defun add-bri-ball-dir (g e)
  (cond
    ((eq (dir e) +up+) (add-bri-ball g e 0 20))
    ((eq (dir e) +down+) (add-bri-ball g e 0 -20))
    ((eq (dir e) +left+) (add-bri-ball g e 20 0))
    ((eq (dir e) +right+) (add-bri-ball g e -20 0))))

;;ブリガンドの行動
(defun update-brigand (g e)
  (incf (dir-c e)) ;;移動カウンター更新
  (incf (atk-c e)) ;;攻撃カウンター
  (update-ido-anime-img e)
  (when (and (>= (atk-c e) 90) ;;攻撃
	     (set-can-atk-dir g e 600 600))
    (setf (atk-c e) 0)
    (add-bri-ball-dir g e))
  (if (> (dir-c e) 40)
      (progn (set-rand-dir g e)
	     (setf (dir-c e) 0))
      (update-enemy-pos g e)))

;;火追加 
(defun add-fire (g e fire-n)
  (let ((fire (make-instance 'enemy :img 0 :obj-type :fire
			     :str (str e) :anime-img +dragon-fire+
			     :moto-w *fire-w* :moto-h *fire-h* :stage (stage e)
			     :w *fire-w* :h *fire-h* :w/2 *fire-w/2* :h/2 *fire-h/2*)))
    (cond
      ((eq (dir e) +up+)
       (setf (x fire) (x e)
	     (y fire) (- (y e) (* *fire-h* fire-n))))
      ((eq (dir e) +down+)
       (setf (x fire) (x e)
	     (y fire) (+ (y e) (h e) (- (* *fire-h* fire-n) *fire-h*))))
      ((eq (dir e) +right+)
       (setf (x fire) (+ (x e) (w e) (- (* *fire-w* fire-n) *fire-w*))
	     (y fire) (y e)))
      ((eq (dir e) +left+)
       (setf (x fire) (- (x e) (* *fire-w* fire-n))
	     (y fire) (y e))))
      
    (if (null (block-hit-p g fire)) ;;ブロックにぶつかるなら追加しない
	(push fire (donjon-enemies (aref (donjons g) (stage e))))
	(setf (atk-now e) nil ;;初期化
	      (atk-c e) 0))))

;;火追加ボス 
(defun boss-add-fire (g e fire-n)
  (let ((fire (make-instance 'enemy :img 0 :obj-type :fire
			     :str (str e) :anime-img +dragon-fire+
			     :moto-w *fire-w* :moto-h *fire-h* :stage (stage e)
			     :w *fire-w* :h *fire-h* :w/2 *fire-w/2* :h/2 *fire-h/2*))
	(fire2 (make-instance 'enemy :img 0 :obj-type :fire
			      :str (str e) :anime-img +dragon-fire+
			      :moto-w *fire-w* :moto-h *fire-h* :stage (stage e)
			      :w *fire-w* :h *fire-h* :w/2 *fire-w/2* :h/2 *fire-h/2*)))
    (cond
      ((eq (dir e) +up+)
       (setf (x fire) (x e)
	     (y fire) (- (y e) (* *fire-h* fire-n))
	     (x fire2) (+ (x e) *fire-w*)
	     (y fire2) (- (y e) (* *fire-h* fire-n))))
      ((eq (dir e) +down+)
       (setf (x fire) (x e)
	     (x fire2) (+ (x e) *fire-w*)
	     (y fire) (+ (y e) (h e) (- (* *fire-h* fire-n) *fire-h*))
	     (y fire2) (+ (y e) (h e) (- (* *fire-h* fire-n) *fire-h*))))
      ((eq (dir e) +right+)
       (setf (x fire) (+ (x e) (w e) (- (* *fire-w* fire-n) *fire-w*))
	     (x fire2) (+ (x e) (w e) (- (* *fire-w* fire-n) *fire-w*))
	     (y fire) (y e)
	     (y fire2) (+ (y e) *fire-h*)))
      ((eq (dir e) +left+)
       (setf (x fire) (- (x e) (* *fire-w* fire-n))
	     (x fire2) (- (x e) (* *fire-w* fire-n))
	     (y fire) (y e)
	     (y fire2) (+ (y e) *fire-h*))))
    
    (if (null (block-hit-p g fire)) ;;ブロックにぶつかるなら追加しない
	(progn (push fire (donjon-enemies (aref (donjons g) (stage e))))
	       (push fire2 (donjon-enemies (aref (donjons g) (stage e)))))
	(setf (atk-now e) nil ;;初期化
	      (atk-c e) 0))))

;;火が追加できるか
(defun check-add-fire (g e max-fire fire-time)
  (incf (atk-c e)) ;;火を追加する間隔
  (when (zerop (mod (atk-c e) fire-time))
    (let ((fire-n (floor (atk-c e) fire-time)))
      (case (obj-type e)
	(:dragon (add-fire g e fire-n))
	(:boss (boss-add-fire g e fire-n)))
      (when (= fire-n max-fire)
	(setf (atk-now e) nil
	      (atk-c e) 0)))))

;;ドラゴンの行動
(defun update-dragon (g e)
  (cond
    ((atk-now e)
     (check-add-fire g e 3 30))
    (t
     (incf (dir-c e)) ;;移動カウンター更新
     (when (and (= 1 (random 50)) ;;攻撃
		(set-can-atk-dir g e (* (w e) 3) (* (h e) 3)))
       (setf (atk-now e) t))
        ;;(set-enemy-atk e))
     (update-ido-anime-img e)
     (if (> (dir-c e) 40)
	 (progn (set-rand-dir g e)
		(setf (dir-c e) 0))
	 (update-enemy-pos g e)))))

;;火の更新
(defun update-fire (g e)
  (incf (atk-c e))
  (when (>= (atk-c e) 50) ;;火を消す
    (setf (donjon-enemies (aref (donjons g) (stage e)))
	  (remove e (donjon-enemies (aref (donjons g) (stage e))) :test #'equal))))

;;ぶりボールの更新
(defun update-briball (g e)
  (cond
    ((eq (dir e) +up+) (decf (y e)))
    ((eq (dir e) +down+) (incf (y e)))
    ((eq (dir e) +left+) (decf (x e)))
    ((eq (dir e) +right+) (incf (x e))))
  (when (block-hit-p g e)
    (setf (donjon-enemies (aref (donjons g) (stage e)))
	  (remove e (donjon-enemies (aref (donjons g) (stage e))) :test #'equal))))

(defun update-orc-atk-effect (g e)
  (incf (atk-c e))
  (when (>= (atk-c e) *orc-atk-effect-time*)
    (setf (donjon-enemies (aref (donjons g) (stage e)))
	  (remove e (donjon-enemies (aref (donjons g) (stage e))) :test #'equal))))

;;ボスのとげ攻撃
(defun boss-toge-atk (g e)
  (let ((toge (make-instance 'enemy :img 0 :obj-type :toge :hp 1 :maxhp 1
			     :anime-img +boss-atk1+ :stage (stage e)
			     :str (str e) :x (x e) :y (+ (y e) (floor (h e) 2))
			     :moto-w *fire-w* :moto-h *fire-h* :vx (rand+- 3) :vy (rand+- 3)
			     :w *fire-w* :h *fire-h* :w/2 *fire-w/2* :h/2 *fire-h/2*)))
    (push toge (donjon-enemies (aref (donjons g) (stage e))))
    (setf (atk-now e) nil
	  (atk-c e) 0)))

;;ボスの行動
(defun update-boss (g e)
  (cond
    ((eq (atk-now e) :fire)
     (check-add-fire g e 5 20))
    ((eq (atk-now e) :toge)
     (boss-toge-atk g e))
    (t
     (incf (dir-c e)) ;;移動カウンター更新
     (incf (atk-c e))
     (when (zerop (mod (atk-c e) (atk-spd e)))
       (let ((ran (random 2)))
     	 (cond
     	   ((and (= 0 ran) ;;攻撃
     		 (set-can-atk-dir g e (* (w e) 30) (* (h e) 30)))
     	    (setf (atk-now e) :fire))
     	   ((= ran 1)
     	    (setf (atk-now e) :toge)))
     	 (setf (atk-c e) 0)))
     (update-ido-anime-img e)
     (if (> (dir-c e) 40)
	 (progn (set-rand-dir g e)
		(setf (dir-c e) 0))
	 (update-enemy-pos g e)))))

;;ボスのとげ攻撃の更新
(defun update-toge (g e)
  (incf (x e) (vx e))
  (incf (y e) (vy e))
  (incf (atk-c e))
  (if (>= (atk-c e) 500)
      (setf (donjon-enemies (aref (donjons g) (stage e)))
	    (remove e (donjon-enemies (aref (donjons g) (stage e))) :test #'equal))
      (when (block-hit-p g e)
	(let ((vx0 (vx e))
	      (vy0 (vy e)))
	  (cond
	    ((>= *blo-w46* (x e))
	     (setf (vx e) (- (vx e))))
	    ((>= (+ (x e) (w e)) (- *map-w* *blo-w46*))
	     (setf (vx e) (- (vx e))))
	    ((<= (y e) *blo-h46*)
	     (setf (vy e) (- (vy e))))
	    ((>= (+ (y e) (h e)) (- *map-h* *blo-h46*))
	     (setf (vy e) (- (vy e)))))
	  (decf (x e) vx0)
	  (decf (y e) vy0)))))


;;ダメージフォントの位置更新
(defun update-damage-font (donjon)
  (loop for dmg in (donjon-dmgs donjon)
     do
       (cond
	 ((eq +up+ (y-dir dmg))
	  (if (eq (x-dir dmg) +right+)
	      (incf (x dmg))
	      (decf (x dmg)))
	  (decf (y dmg))
	  (when (= (y dmg) (miny dmg))
	    (setf (y-dir dmg) +down+)))
	 ((eq +down+ (y-dir dmg))
	  (if (eq (x-dir dmg) +right+)
	      (incf (x dmg))
	      (decf (x dmg)))
	  (incf (y dmg))
	  (when (= (y dmg) (maxy dmg))
	    (setf (donjon-dmgs donjon)
		  (remove dmg (donjon-dmgs donjon) :test #'equal)))))))

;;敵の位置更新
(defun update-enemies (g stage-list)
  (with-slots (donjons) g
    (loop for s in stage-list
       do (let ((donjon (aref donjons s)))
	    (loop for e in (donjon-enemies donjon)
	       do (when (null (dead e))
		   (case (obj-type e)
		     (:slime   (update-slime g e 40))
		     (:dragon  (update-dragon g e))
		     (:brigand (update-brigand g e))
		     (:hydra   (update-hydra g e))
		     (:hydra-atk (update-hydra-atk g e))
		     (:fire    (update-fire g e))
		     (:briball (update-briball g e))
		     (:orc     (update-orc g e))
		     (:yote1   (update-slime g e 10))
		     (:toge    (update-toge g e))
		     (:boss    (update-boss g e))
		     (:orc-atk (update-orc-atk-effect g e)))))))))








;;ダメージフォントの位置更新
(defun update-damage-fonts (g stage-list)
  (with-slots (donjons) g
    (loop for s in stage-list
       do (let ((donjon (aref donjons s)))
	    (update-damage-font donjon)))))

;;アイテムの画像
(defun item-img (item)
  (case item
    (:boots +boots+)
    (:potion +potion+)
    (:hammer +hammer+)
    (:sword +sword+)))

;;通常のドロップアイテム
(defun normal-drop-item ()
  (let* ((n (random 100)))
    (cond
      ((>= 5 n 0)   :boots)
      ((>= 25 n 10)  :potion)
      ((>= 45 n 26) :hammer)
      ((>= 51  n 46) :sword)
      (t nil))))

;;靴持ってる時のドロップアイテム
(defun no-boots-drop-item ()
  (let* ((n (random 100)))
    (cond
      ((>= 25 n 10)  :potion)
      ((>= 45 n 26) :hammer)
      ((>= 51  n 46) :sword)
      (t nil))))

;;アイテムの靴を落とす
(defun enemy-drop-item (e g stage)
  (let* ((item (no-boots-drop-item)))
    (when item
      (let ((drop-item (make-instance 'obj :img (item-img item)
				      :x (x e) :y (y e) :w 32 :h 32 :w/2 16 :h/2 16
				      :moto-w 32 :moto-h 32 :obj-type item)))
	(push drop-item (donjon-objects (aref (donjons g) stage)))))))

;;ボスを倒したら
(defun go-ending ()
  (setf (state *p*) :ending
	(endtime *p*) (get-internal-real-time)))

;;死んだ敵の情報を消す
(defun delete-enemies (g stage-list)
  (with-slots (donjons) g
    (loop for s in stage-list
       do (let ((donjon (aref donjons s)))
	    (loop for e in (donjon-enemies donjon)
	       do (when (and (null (dmg e))
			     (dead e))
		    (when (eq (obj-type e) :boss)
		      (setf (clear g) t))
		    (enemy-drop-item e g s)
		    (setf (donjon-enemies donjon)
			  (remove e (donjon-enemies donjon) :test #'equal))))))))


(defun chomp (line)
  (let ((last-char-pos
         (position-if (lambda (c) (and (not (equal c #\Return)) (not (equal c #\Linefeed))))
                      line :from-end t)))
    (if last-char-pos
        (subseq line 0 (1+ last-char-pos))
      "")))

(define-condition handshake-error (error) ())

(defun json-true-false (v)
  (if v t :false))

(defun 1or0 (v)
  (if v 1 0))

(defun make-damage-list (p)
  (let ((hoge 0))
    (addx hoge (x p))
    (addy hoge (y p))
    (addcolor hoge (color p))
    (adddmg-num hoge (dmg-num p))
    `(:|data| ,hoge)))

(defun check-dmg-list (p)
  (if (dmg p)
      (make-damage-list (dmg p))
      0))

(defun make-object-list (p)
  (let ((hoge 0))
    (addx hoge (x p))
    (addy hoge (y p))
    (addw hoge (w p))
    (addh hoge (h p))
    (addimg hoge (img p))
    (addmoto-w hoge (moto-w p))
    (addmoto-h hoge (moto-h p))
    `(:|data| ,hoge)))

(defun make-enemy-list (p)
  (let ((hoge 0))
    (addx hoge (x p))
    (addy hoge (y p))
    (addhp hoge (hp p))
    (addmaxhp hoge (maxhp p))
    (addanime-img hoge (anime-img p))
    (addw hoge (w p))
    (addh hoge (h p))
    (adddir hoge (dir p))
    (adddead hoge (1or0 (dead p)))
    (addimg hoge (img p))
    (addmoto-w hoge (moto-w p))
    (addmoto-h hoge (moto-h p))
    `(:|data| ,hoge)))


;;ダンジョンのデータリスト
(defun donjon-data-list (map)
  `(:|blocks| ,(mapcar #'make-object-list (remove-if
					   (lambda (b)
					     (eq (obj-type b) :hard-block))
					   (donjon-blocks map)))
     :|enemies| ,(mapcar #'make-enemy-list (donjon-enemies map))
     :|item| ,(mapcar #'make-object-list (donjon-objects map))
     :|dmg| ,(mapcar #'make-damage-list (donjon-dmgs map))
     ;;:|yuka| ,(mapcar #'make-object-list (donjon-yuka map))
  ))

;; (player-data name  buki-data donjon-data)
(defun make-player-list (g p)
  (let ((hoge 0))
    (addid hoge (id p)) ;; 3 max7
    (addlevel hoge (level p)) ;; 5 max31
    (addhp hoge (hp p)) ;; 7 max127
    (addmaxhp hoge (maxhp p)) ;; 7 max127
    (addx hoge (x p)) ;; 10 max1023
    (addy hoge (y p)) ;; 10 max1023
    (addexp hoge (expe p)) ;;8 max255
    (addstage hoge (stage p)) ;; 5 max31
    (addlvup-exp hoge (lvup-exp p)) ;;8 max255
    (addstr hoge (str p)) ;; 6 max63
    (adddef hoge (def p)) ;; 6 max63
    (addhammer hoge (hammer p)) ;; 5 max31
    (addw hoge (w p)) ;; 7 max127
    (addh hoge (h p)) ;; 7 max127
    (adddir hoge (dir p)) ;; 3 max7
    (addimg hoge (img p)) ;; 4 max15
    (addready hoge (1or0 (ready? p))) ;; 1
    (addhammer-now hoge (1or0 (hammer-now p))) ;; 1
    (addatk-now hoge (1or0 (atk-now p))) ;; 1
    (adddead hoge (1or0 (dead p))) ;; 1
    (addmoto-w hoge (moto-w p)) ;; 7 max127
    (addmoto-h hoge (moto-h p)) ;; 7 max127
    `(:|data| ,hoge  :|name| ,(name p) ;; todo 35 7*5  
	    :|buki| ,(make-object-list (buki p)) ;; 1
	    :|donjon| ,(donjon-data-list (aref (donjons g) (stage p)))
	    :|events| ,(remove nil (mapcar #'(lambda (x) (if (= (stage p) (third x))
							     x)) (events g))))))

(defun make-players-list (g)
  (loop for p in (players g)
       collect (make-player-list g p)))

;;(type players event)
(defun make-donjon-data (g)
  (append `(:|type| ,+playing+)
          `(:|players| ,(make-players-list g))
	  `(:|events| 0))) ;;,(events g)))) ここがないとバグる



(defun socket-name-string (sock)
  (multiple-value-bind (addr port)
      (socket-name sock)
    (format nil "~a.~a.~a.~a:~a"
            (aref addr 0)
            (aref addr 1)
            (aref addr 2)
            (aref addr 3)
            port)))

(defun make-server-socket ()
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp))
        (addr (make-inet-address "0.0.0.0")))
    (setf (sockopt-reuse-address s) t)
    (setf (non-blocking-mode s) t)
    (socket-bind s addr *port*)
    (socket-listen s 5)
    (v:info :server "~aで接続受け付け開始。" (socket-name-string s))
    s))


(defun socket-peername-string (sock)
  (multiple-value-bind (addr port)
      (socket-peername sock)
    (format nil "~a.~a.~a.~a:~a"
            (aref addr 0)
            (aref addr 1)
            (aref addr 2)
            (aref addr 3)
            port)))

(defun read-line2 (stream)
  (let ((len 0) (product nil))
	  (let ((byte (read-byte stream nil nil)))
	    (loop until (or (null byte) (= byte 10)) do
	      (push byte product)
	      (incf len)
	      (setf byte (read-byte stream nil nil))))
  (babel:octets-to-string
    (make-array len :element-type '(unsigned-byte 8) :initial-contents (nreverse product))
    :encoding :utf-8)))


(defmethod remote-player-receive-name (rp)
  (let ((len 0) (product nil) (name ""))
    (flet ((make-res-string (len product)
             (babel:octets-to-string
              (make-array len :element-type '(unsigned-byte 8) :initial-contents (nreverse product))
              :encoding :utf-8)))
      (handler-case
	  (let ((byte (read-byte (stream1 rp) nil nil)))
	    (loop until (or (null byte) (= byte 10)) do
	      (push byte product)
	      (incf len)
	      (setf byte (read-byte (stream1 rp) nil nil)))
	    ;;(format t "~S~%" product)
	    
	    (setf name (make-res-string len product))
	    (when (equal name "")
	      (v:error :network "AIの名前が空です。")
	      (error 'handshake-error))
	    (setf (name rp) name))
	(end-of-file (c)
	  (v:error :network "~A" c)
	  (error 'handshake-error))))))



(defmethod remote-player-close-stream ((rp player))
  (handler-case
      (if (stream1 rp)
	  (progn
	    (close (stream1 rp))
	    (setf (stream1 rp) nil))
	  (v:info :network "プレーヤー~aの存在しないストリームを閉じようとしました。"
		  (name rp)))
    (sb-int:simple-stream-error
	(c)
      (declare (ignore c))

      (v:error :network "~aとの接続をクローズ時にストリームエラー。" (name rp)))))



;; プレーヤーを参加させる。ここでIDを割り当てる。
;;初期値も決める
(defun game-add-player (g player)
  (let* ((id (if (players g)
		 (loop for i in '(0 1 2 3 4 5)
		    when (null (find i (mapcar #'id (players g))))
		    return i)
		 0))
         (pos (player-init-pos (aref (donjons g) (stage player)))))
      (setf (id player) id
	    (x player) (* (car pos) *blo-w46*)
	    (y player) (* (cadr pos) *blo-h46*)
	    (w player) *p-w* (moto-w player) *p-w*
	    (h player) *p-h* (moto-h player) *p-h*
	    (w/2 player) *p-w/2* (h/2 player) *p-h/2*
	    (stage player) 1 (str player) 5 (def player) 2
	    (ido-spd player) 2 (hammer player) 3
	    (buki player)
	    (make-instance 'buki :name "こん棒" :atk 1 :w *p-w* :h *p-h* :moto-w *p-w* :moto-h *p-h* :w/2 *p-w/2* :h/2 *p-h/2* :img 0)
	    (img player) +down+ (dir player) +down+)
      
      (setf (players g) (append (players g) (list player)))))


(defmethod remote-player-send-id (player)
  (write-sequence (babel:string-to-octets (format nil "~a~%" (id player)) :encoding :utf-8) (stream1 player))
  (finish-output (stream1 player)))

(defun encode-u32 (n)
  (let ((ls nil))
    (push (mod n 256) ls)
    (setf n (truncate n 256))
    (push (mod n 256) ls)
    (setf n (truncate n 256))
    (push (mod n 256) ls)
    (setf n (truncate n 256))
    (push (mod n 256) ls)
    (setf n (truncate n 256))
    ls))

(defun game-kill-player (p)
  (setf (dead p) t))

(defmethod remote-player-send-message ((rp remote-player) data)
  (when (stream1 rp)
    (let* ((diff (diff:diff (lastmsg rp) data))
	   (json (gzip-stream:gzip-sequence
		  (babel:string-to-octets (jonathan:to-json diff) :encoding :utf-8))))
      (setf (lastmsg rp) data)
      (v:debug :network "size ~A bytes" (length json))
      (write-sequence (encode-u32 (length json)) (stream1 rp))
      (write-sequence json (stream1 rp))
      (finish-output (stream1 rp)))))

(defun game-broadcast-message (g data)
  (dolist (rp (players g))
    (handler-case
     (remote-player-send-message rp data)

     (sb-int:simple-stream-error
      (c)
      (declare (ignore c))
      (game-kill-player  rp)
      ;;(remote-player-close-stream rp)
      (v:error :network "~aへのメッセージ送信時にストリームエラー。死亡扱い。" (name rp))))))

(defun backgrounds (g)
  (loop for i from 1 to 10
	collect (let ((blocks (donjon-blocks (aref (donjons g) i)))
		      (yuka (donjon-yuka (aref (donjons g) i))))
		  (list :|blocks| (mapcar #'make-object-list
					  (remove-if (lambda (b) (eq (obj-type b) :soft-block))
						     blocks))
			:|yuka| (mapcar #'make-object-list yuka)))))

(defun add-backgrounds (donjon-data g)
  (append donjon-data
	  `(:|backgrounds| ,(backgrounds g))))

;;リモートプレーヤーにゲーム状態のJSONを送る。
(defun game-broadcast-map (g &key with-backgrounds)
  (let ((data (make-donjon-data g)))
    (if with-backgrounds
	(setf data (add-backgrounds data g)))
    (game-broadcast-message g data)
    ;; イベントリストはブロードキャストごとにクリアする。
    (setf (events g) nil)))

(defun player-rank (rp)
  `(:|name| ,(name rp)
     :|totaldmg| ,(totaldmg rp)))

(defun make-ranking-data (g)
  (loop for rp in (players g)
       collect (player-rank rp)))

(defun game-broadcast-result (g)
  (game-broadcast-message
   g
   `(:|type| ,+result+
      :|ranking| ,(make-ranking-data g))))

(defun game-broadcast-status (g timeout-seconds)
  (game-broadcast-message g `(:|type| ,+status+ :|timeout-seconds| ,timeout-seconds
			       :|map| ,(make-donjon-data g))))


(defun game-broadcast-quit (rp mes)
  (handler-case
      (remote-player-send-message rp `(:|type| ,mes))
    (sb-int:simple-stream-error
	(c)
      (declare (ignore c))

      (v:error :network "~aへのメッセージ送信時にストリームエラー。" (name rp)))))

;; リモートプレーヤーとの接続を切る。ゲーム終了時の後処理。
(defun game-close-connections (g)
  (dolist (rp (players g))
    (remote-player-close-stream rp)))




(defun valid-command? (str)
  (or
   (equal "Z" str)
   (equal "Q" str)
   (equal "C" str)
   (equal "LEFT" str)
   (equal "RIGHT" str)
   (equal "UP" str)
   (equal "DOWN" str)
   (equal "ENTER" str)
   (equal "STAY" str)))

(defun make-error-message (&key reason message)
  `(:|type| "error" :|reason| ,reason :|message| ,message))

;; リモートプレーヤーから届いているコマンドを受け取る。
(defun try-read-remote-commands (g)
  (dolist (rp (players g))
    (when (null (dead rp))
    ;;(bt:with-lock-held (*lock*)
    (when (listen (stream1 rp))
      ;; 読み込めるデータがある。1行全て読み込める
      ;; とは限らないが…。
      (v:debug :game "プレーヤー~aからコマンドの読み込み開始。" (name rp))
      (let ((cmd (chomp (read-line2 (stream1 rp)))))
	(v:debug :game "プレーヤー~aからコマンドの読み込み完了。~a" (name rp) cmd)
	(if (valid-command? cmd)
	    (progn
	      (setf (command rp) cmd))
	    (progn
	      (when (not (dead rp))
		(v:info :game "プレーヤー「~s」は不正なコマンド「~s」により反則負け。" (name rp) cmd)
		(remote-player-send-message rp (make-error-message :reason "protocol-error" :message (format nil "不正なコマンド~s" cmd)))
		(remote-player-close-stream rp)
		(game-kill-player rp)))))))))


;; プレーヤーが動く。全てのプレーヤーのコマンドが入力されてから呼び出
;; す。
(defun game-move-player (p g)
  (when (not (dead p))
    (cond
      ((equal (command p) "Z")
       (set-atk-img p)
       (set-buki-pos p)
       (setf (atk-now p) t)
       (buki-hit-enemy p g))
      ((and (equal (command p) "C")
	    (> (hammer p) 0))
       (set-atk-img p)
       (set-buki-pos p)
       (setf (hammer-now p) t)
       (hammer-hit-kabe p g))
      ((equal (command p) "UP")
       (when (/= (dir p) +up+)
	 (setf (dir p) +up+))
       (update-ido-anime-img p)
       (decf (y p) (ido-spd p))
       (let ((blo  (block-hit-p g p)))
	 (when blo
	   (incf (y p) (ido-spd p))
	   (merikomi-hantei g p))))
      ((equal (command p) "DOWN")
       (when (/= (dir p) +down+)
	 (setf (dir p) +down+))
       (update-ido-anime-img p)
       (incf (y p) (ido-spd p))
       (let ((blo  (block-hit-p g p)))
	 (when blo
	   (decf (y p) (ido-spd p))
	   (merikomi-hantei g p))))
      ((equal (command p) "RIGHT")
       (when (/= (dir p) +right+)
	 (setf (dir p) +right+))
       (update-ido-anime-img p)
       (incf (x p) (ido-spd p))
       (let ((blo  (block-hit-p g p)))
	 (when blo
	   (decf (x p) (ido-spd p))
	   (merikomi-hantei g p))))
      ((equal (command p) "LEFT")
       (when (/= (dir p) +left+)
	 (setf (dir p) +left+))
       (update-ido-anime-img p)
       (decf (x p) (ido-spd p))
       (let ((blo  (block-hit-p g p)))
	 (when blo
	   (incf (x p) (ido-spd p))
	   (merikomi-hantei g p))))
      ((equal (command p) "STAY")))))

;;プレイヤーの色々更新
(defun update-player (p g)
  (when (> (dmg-c p) 0) ;;敵からの攻撃食らう間隔
    (decf (dmg-c p)))
  (when (zerop (dmg-c p)) ;;dmg-cが0の時
    (hit-enemies-player p g) ;;ダメージ処理
    (when (dead p) ;;ゲームオーバー
      (setf (state p) :dead)))
  (when (null (dead p))
    (cond
      ((atk-now p)
       (update-atk-img p g))
      ((hammer-now p)
       (update-hammer-img p))
      (t
       (game-move-player p g)
       (player-hit-item p g)))))

;;プレイヤーの情報更新
(defun update-players (g)
  (dolist (p (players g))
    (update-player p g)))


(defun update-game (g)
  (with-slots (players) g
    (let ((stage-list (remove-duplicates (mapcar #'stage players))))
      (update-players g)
      (update-enemies g stage-list)
      (update-damage-fonts g stage-list)
      (delete-enemies g stage-list))))

(defun game-end? (g)
  (every #'dead (players g)))

(defun game-broadcast-all-quit (g)
  (game-broadcast-message g `(:|type| ,+quit+)))
 
(defclass server ()
  (
   (server-socket :initform (make-server-socket))
   (g :initform (new-game))
   ;; app-func:
   ;;     player-registration プレーヤー登録受け付け中。
   ;;     playing ゲームプレイ中。
   (app-func :initform #'player-registration)
   ;; first-registration-time: 最初の参加者が登録した時刻。
   (first-registration-time :initform nil)
   (frame :initform 0)
   ))

(defmethod player-registration ((s server))
  (with-slots
   (app-func g first-registration-time server-socket)
   s

   ;; プレーヤーの登録処理。

   ;; 非ブロックモードのソケットなのでacceptはブロックしない。
   (let ((client (socket-accept server-socket)))
     (when client
       ;; クライアントからの接続がある。
       (v:info :network "クライアントから接続: ~a" (socket-peername-string client))
       ;; ハンドシェイク。この処理は同期的でブロックし得る。
       (let* ((stream (socket-make-stream client
                                          :input t
                                          :output t
                                          :element-type '(unsigned-byte 8)
                                          :timeout +client-read-timeout+))
              (player (make-instance 'remote-player :stream1 stream :socket1 client)))
         (handler-case
             (remote-player-receive-name player)

           (handshake-error
            (c)
            (declare (ignore c))

            (v:error :network "~aとのハンドシェイクに失敗。" (socket-peername-string client))
            (remote-player-close-stream player)
            (return-from player-registration))

           (sb-sys:io-timeout
            (c)
            (declare (ignore c))

            (v:error :network "~aとのハンドシェイク中にタイムアウト。" (socket-peername-string client))
            (remote-player-close-stream player)
            (return-from player-registration)))

         ;; ハンドシェイクが完了したらゲーム構造体にプレーヤー
         ;; を追加。(id player)がセットされる。
         (game-add-player g player)
	 ;; first-registration-timeがnilの場合は、これが最初の
	 ;; プレーヤーであるので、現在時刻からゲーム開始までの
	 ;; カウントダウンを開始する。
         ;;(when (not first-registration-time)
         ;;  (setf first-registration-time (get-internal-real-time)))
         (v:info :game "プレーヤー~a(ID: ~a)を登録。" (name player) (id player))
         (remote-player-send-id player))))
   ;;準備完了チェック
   ;;(try-read-remote-commands g)
   (dolist (rp (players g))
     (when (equal "ENTER" (command rp))
       (setf (ready? rp) t)))
   (when (and (every #'ready? (players g))
	      (null first-registration-time))
     (setf first-registration-time (get-internal-real-time)))
   
   ;; カウントダウン中ならばstatusメッセージをブロードキャスト
   ;; する。
   ;; (when first-registration-time
   ;;   (let ((timeout (- +registration-timeout+
   ;; 		       (truncate (- (get-internal-real-time)
   ;; 				    first-registration-time)
   ;; 				 internal-time-units-per-second))))
   (when (null (players g))
     (setf first-registration-time nil))
   (when (players g)
     (let ((timeout (if first-registration-time
			(- +registration-timeout+
			   (truncate (- (get-internal-real-time)
					first-registration-time)
				     internal-time-units-per-second))
			999)))
       (game-broadcast-status g timeout)
       ;; メッセージのが送信できなかったらそのプレーヤーの死亡
       ;; フラグが立つので、そのプレーヤーを削除する。
       (setf (players g) (remove-if #'dead (players g)))
       ;; プレーヤーが残っていなかったら、カウントダウンをキャ
       ;; ンセルする。
       (when (null (players g))
	 (setf first-registration-time nil))
     ))

   ;; 最初の参加から+registration-timeout+秒たったか、あるいは4
   ;; 人揃っていたらゲーム開始。
   (when first-registration-time
     (let ((sec (truncate (- (get-internal-real-time) first-registration-time)
			  internal-time-units-per-second)))
       (when (or (>= sec +registration-timeout+)
                 (<= 4 (length (players g))))
         (setf app-func #'playing)
         (setf first-registration-time nil)
         (v:info :game "ゲーム開始。")
	 ;; 最初のmapメッセージでは各階の不変な地形を送る。
         (game-broadcast-map g :with-backgrounds t))))))

(defmethod playing ((s server))
  (with-slots
	(app-func g server-socket) s
    ;;途中追加みたいなのtodo
    (let* ((client (socket-accept server-socket)))
      (when client
	(let* ((stream (socket-make-stream client
                                          :input t
                                          :output t
                                          :element-type '(unsigned-byte 8)
                                          :timeout +client-read-timeout+))
	       (rp (make-instance 'remote-player :stream1 stream :socket1 client)))
	  (remote-player-receive-name rp)
	  (if (> 4 (length (players g)))
	      (progn (game-add-player g rp)
		     (remote-player-send-id rp)
		     (game-broadcast-map g :with-backgrounds t))
	      (progn
		(v:error :game "~aがきたけど帰した" (name rp) )
		(game-broadcast-quit rp +stop+)
		;;(setf (players g) (remove rp (players g) :test #'equal))
		(remote-player-close-stream rp)
		)))))
    (cond
      ((clear g)
       (v:info :game "ゲームクリア")
       (game-broadcast-result g)
       (game-close-connections g)
       (setf g (new-game))
       (setf app-func #'player-registration))
      ((null (players g))
       (v:info :game "プレーヤーが居なくなったためゲーム終了")

       (setf g (new-game))
       (setf app-func #'player-registration))

      ((game-end? g)
       (v:info :game "全員死んだ・・・")
       ;;(game-broadcast-result g)
       (game-broadcast-all-quit g)
       (game-close-connections g)

       (setf g (new-game))
       (setf app-func #'player-registration))

      (t
       ;;(try-read-remote-commands g)
       ;;プレイヤーからの終了コマンド
       (dolist (rp (players g))
	 (cond
	   ((dead rp)
	    (game-broadcast-quit rp +dead+)
	    (remote-player-close-stream rp)
	    (v:error :game "~aが死亡しました。" (name rp))
	    (setf (players g) (remove rp (players g) :test #'equal)))
	   ((and (not (dead rp))
		 (equal (command rp) "Q"))
	    ;;(bt:with-lock-held (*lock*)
	      (game-kill-player  rp)
	      (game-broadcast-quit rp +quit+)
	      (remote-player-close-stream rp)
	      (v:error :game "~aがやめました。" (name rp))
	      (setf (players g) (remove rp (players g) :test #'equal)))))
       ;; ゲーム状態の更新。
       (update-game g)
       (game-broadcast-map g)
       ))))

(defmethod server-do-iter ((s server))
  (with-slots
	(frame app-func) s

    (let ((t0 (get-internal-real-time)))
      (if (zerop (mod frame 1000))
	  (v:debug :server "~A frames elapsed" frame))
      (funcall app-func s)
      (incf frame)
      (let ((d (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
	(if (< d 1/30)
	    (sleep (- 1/30 d))
	    (v:warn :server "処理に時間のかかったフレーム。~dミリ秒" (* 1000 d)))))))

(defun output-kun (s)
  (handler-case
      (loop :do
	 (server-do-iter s))
    (sb-sys:interactive-interrupt (int)
      (declare (ignore int))
      (format t "hoge~%"))))

(defun input-kun (s)
  (with-slots (g) s
    (handler-case
	(loop :do
	   (try-read-remote-commands g))
      (sb-sys:interactive-interrupt (int)
	(declare (ignore int))
	(format t "moge~%")))))

;;さば
(defun server-main ()
  (handler-case
      (progn
	(setf *random-state* (make-random-state t))
	
	(let* ((s (make-instance 'server)) (t1) (t2))
	  (setf t1 (bt:make-thread (lambda ()
				     (loop :do (server-do-iter s)))))
	  (setf t2 (bt:make-thread (lambda ()
				     (with-slots (g) s
				       (loop :do (try-read-remote-commands  g))))))
	  
	  (bt:join-thread t1)
	  (bt:join-thread t2)))
    (sb-sys:interactive-interrupt (int)
      (declare (ignore int))
      (progn
	;; (bt:join-thread t1)
	;; (bt:join-thread t2)
	(format t "moge~%")))))
