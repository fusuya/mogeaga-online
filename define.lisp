
(defmacro my-enum (&rest names)
  `(progn
     ,@(loop for i from 0
             for name in names
        collect `(defparameter ,name ,i))))

;;".\\images\\*.*" ロードした画像の配列を作る
(defun make-imgs-array (img-path)
  (let* ((img-list (mapcar #'namestring (directory img-path)))
         (imgs (make-array (length img-list))))
    (loop for str in img-list
          for i from 0
          do (setf (aref imgs i)
                   (load-image str :type :bitmap
                               :flags '(:load-from-file :create-dib-section))))
    imgs))

(defparameter *lock* (bt:make-lock))
(defparameter *p-img* nil)
(defparameter *p-atk-img* nil)
(defparameter *buki-img* nil)
(defparameter *hammer-img* nil)
(defparameter *monster-anime* nil)
(defparameter *objs-img* nil)
;;ダンジョン階数
(defparameter *donjons-num* 10)

;;プレイヤー画像切り替えよう
(defconstant +down+ 0)
(defconstant +left+ 2)
(defconstant +right+ 3)
(defconstant +up+ 1)
(defconstant +stop+ 4)

;;敵画像切り替えよう
(defconstant +brigand-anime+ 0)
(defconstant +dragon-anime+ 1)
(defconstant +hydra-anime+ 2)
(defconstant +yote-anime+ 3)
(defconstant +orc-anime+ 4)
(defconstant +slime-anime+ 5)
(defconstant +boss-anime+ 11)
(defconstant +hydra-atk+ 6)
(defconstant +brigand-ball+ 7)
(defconstant +dragon-fire+ 8)
(defconstant +orc-atk+ 9)
(defconstant +boss-atk1+ 10)

;;敵の攻撃演出時間
(defparameter *orc-atk-effect-time* 30)
(defparameter *hydra-atk-effect-time* 30)

;;透過用
(defcfun (%set-layered-window-attributes "SetLayeredWindowAttributes" :convention :stdcall)
         :boolean
  (hwnd :pointer)
  (crkey :int32)
  (balpha :uint8)
  (dwflags :uint32))

(defun set-layered-window-attributes (hwnd crkey balpha dwflags)
  (%set-layered-window-attributes hwnd crkey balpha dwflags))

(defparameter *font140* nil)
(defparameter *font90* nil)
(defparameter *font70* nil)
(defparameter *font40* nil)
(defparameter *font30* nil)
(defparameter *font20* nil)
(defparameter *font10* nil)


;;(defparameter *tate* 11) ;;マップサイズ
;;(defparameter *yoko* 11)
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)

(defparameter *map* nil)
(defparameter *ido?* nil)
(defparameter *p* nil)
(defparameter *pt* nil)

(defparameter *battle?* nil)
(defparameter *monster-num* 10)
(defparameter *monster-level* 1) ;;階数によるモンスターのレベル
(defparameter *boss?* 0)
(defparameter *end* 0)
(defparameter *lv-exp* 100)
(defparameter *start-time* 0)
(defparameter *ha2ne2* nil)
(defparameter *urawaza* nil)
(defparameter *images* nil)
(defparameter *anime-monsters-img* nil)

(defparameter *atk-block-wav* "./wav/atk-block.wav")
(defparameter *atk-enemy-wav* "./wav/atk-enemy.wav")
(defparameter *damage-wav* "./wav/damage.wav")
(defparameter *door-wav* "./wav/door.wav")
(defparameter *get-item-wav* "./wav/get-item.wav")
(defparameter *get-potion-wav* "./wav/get-potion.wav")
(defparameter *lvup-wav* "./wav/lvup.wav")



;;基本サイズ 元の画像サイズ
(defparameter *obj-w* 32)
(defparameter *obj-h* 32)


;;元のブロック画像のサイズ
(defparameter *blo-w* 32)
(defparameter *blo-h* 32)
;;表示するブロック画像のサイズ
(defparameter *blo-w46* 42)
(defparameter *blo-h46* 42)

;;炎サイズ
(defparameter *fire-w* 32)
(defparameter *fire-h* 32)
(defparameter *fire-w/2* (floor *fire-w* 2))
(defparameter *fire-h/2* (floor *fire-h* 2))

;;プレイヤーのサイズ
(defparameter *p-w* 24)
(defparameter *p-h* 32)
(defparameter *p-w/2* (floor *p-w* 2))
(defparameter *p-h/2* (floor *p-h* 2))



(defparameter *w/2* (floor *obj-w* 2))
(defparameter *h/2* (floor *obj-h* 2))

;;オブジェクト画像表示サイズ
(defparameter *w-test* 36)
(defparameter *h-test* 36)

;;
(defparameter *tate* 20)
(defparameter *yoko* 30)

(defparameter *yoko-block-num* 21)
(defparameter *tate-block-num* 13)

;;ゲームマップ領域
(defparameter *map-w* (* *yoko-block-num* *blo-w46*))
(defparameter *map-h* (* *tate-block-num* *blo-h46*))
;;プレイヤーのステータス表示用領域サイズ
(defparameter *status-w* 120)
(defparameter *status-h* 160)


(defparameter *screen-w* (+ *map-w* *status-w*))
(defparameter *screen-h* (+ *map-h* *status-h*))

(defparameter *change-screen-w* *screen-w*)
(defparameter *change-screen-h* *screen-h*)

(defparameter *waku-size* 10) ;;ゲームフィールドの周りの枠太さ
(defparameter *c-rect* nil) ;;クライアント領域

(defparameter *start-time* 0)



;;画面領域
(defparameter *client-w* (+ *map-w* 150))
(defparameter *client-h* (* *blo-h46* *tate*))

(defparameter *screen-center-x* nil)

(defparameter *brush* nil)
(defparameter *start* nil)
(defparameter *hmemDC* nil)
(defparameter *hbitmap* nil)


(defparameter *hogememDC* nil)
(defparameter *hogebitmap* nil)

(defparameter *kabe-break* nil)
(defparameter *HPbar-max* 40)

(defparameter *out* nil)

(my-enum +boots+ +door+ +hammer+ +hard-block+ +key+ +potion+ +soft-block+ +yuka+ +sword+ +cursor+ +kaidan+)

(my-enum +white+ +red+  +green+ +blue+ +yellow+ +cyan+ +pink+ +purple+)

(my-enum +status+ +playing+ +result+ +dead+ +stop-entry+ +quit+ +error+)
;;(my-enum +red+ +white+)

(defclass keystate ()
  ((right :accessor right :initform nil :initarg :right)
   (left  :accessor left  :initform nil :initarg :left)
   (down  :accessor down  :initform nil :initarg :down)
   (up    :accessor up    :initform nil :initarg :up)
   (enter :accessor enter :initform nil :initarg :enter)
   (shift :accessor shift :initform nil :initarg :shift)
   (z     :accessor z     :initform nil :initarg :z)
   (x     :accessor x     :initform nil :initarg :x)
   (q     :accessor q     :initform nil :initarg :q)
   (c     :accessor c     :initform nil :initarg :c)))

(defparameter *keystate* (make-instance 'keystate))


;;ドロップアイテムリスト
(defparameter *drop-item*
  '(:boots :atkup :defup))

(defstruct donjon
  (map nil)  ;;マップ
  (tate *tate*)  ;;縦幅
  (yoko *yoko*)  ;;横幅
  (enemies nil)
  (stage 0)
  (dmgs nil) ;;表示するdmgリスト
  (path nil)
  (yuka nil) ;;床
  (blocks nil) ;;ブロック
  (objects nil) ;;鍵とドア
  (drop-item (copy-tree *drop-item*))
  (stop-list nil)) ;;行き止まりリスト


;;ブロックとか
(defclass obj ()
  ((x        :accessor x        :initform 0      :initarg :x)
   (y        :accessor y        :initform 0      :initarg :y)
   (x2       :accessor x2       :initform 0      :initarg :x2)
   (y2       :accessor y2       :initform 0      :initarg :y2)
   (w        :accessor w        :initform 0      :initarg :w)
   (h        :accessor h        :initform 0      :initarg :h)
   (moto-w   :accessor moto-w   :initform 0      :initarg :moto-w)
   (moto-h   :accessor moto-h   :initform 0      :initarg :moto-h)
   (w/2      :accessor w/2      :initform 0      :initarg :w/2)
   (h/2      :accessor h/2      :initform 0      :initarg :h/2)
   (obj-type :accessor obj-type :initform 0      :initarg :obj-type)
   (img      :accessor img      :initform nil    :initarg :img)))

(defclass dmg-font (obj)
  ((dmg-num  :accessor dmg-num   :initform 0     :initarg :dmg-num)
   (miny     :accessor miny      :initform 0     :initarg :miny)
   (maxy     :accessor maxy      :initform 0     :initarg :maxy)
   (y-dir    :accessor y-dir     :initform :up   :initarg :y-dir)
   (x-dir    :accessor x-dir     :initform :left :initarg :x-dir)
   (color    :accessor color     :initform nil   :initarg :color)
   ))

(defclass buki (obj)
  ((atk  :accessor atk       :initform 0   :initarg :atk)
   (name :accessor name      :initform nil :initarg :name)))

;;プレイヤーと敵で共通で使うやつ
(defclass common (obj)
  ((hp        :accessor hp        :initform 30    :initarg :hp)
   (maxhp     :accessor maxhp     :initform 30    :initarg :maxhp)
   (agi       :accessor agi       :initform 30    :initarg :agi)
   (def       :accessor def       :initform 30    :initarg :def)
   (str       :accessor str       :initform 30    :initarg :str)
   (dead      :accessor dead      :initform nil   :initarg :dead)    ;;死亡判定
   (ido-spd   :accessor ido-spd   :initform 2     :initarg :ido-spd) ;;移動速度
   (level     :accessor level     :initform 1     :initarg :level)
   (dmg       :accessor dmg       :initform nil   :initarg :dmg)     ;;ダメージ表示用
   (dmg-c     :accessor dmg-c     :initform 0     :initarg :dmg-c)   ;;ダメージを受ける間隔
   (race      :accessor race      :initform nil   :initarg :race)    ;;種族  0:プレイヤー 1:オーク 2:スライム 3:ヒドラ 4:ブリガンド 5 メテルヨテイチ
   (walk-c    :accessor walk-c    :initform 0     :initarg :walk-c)  ;;歩行アニメカウンター
   (walk-func :accessor walk-func :initform #'+   :initarg :walk-func)
   (dir       :accessor dir       :initform +down+ :initarg :dir)     ;;現在の方向
   (dir-c     :accessor dir-c     :initform 0     :initarg :dir-c)   ;;方向転換用カウンター
   (atk-now   :accessor atk-now   :initform nil   :initarg :atk-now) ;;攻撃中か
   (atk-c     :accessor atk-c     :initform 0     :initarg :atk-c)   ;;攻撃モーション更新用
   (atk-img   :accessor atk-img   :initform 0     :initarg :atk-img) ;;攻撃画像番号 ０～２
   (atk-spd   :accessor atk-spd   :initform 8     :initarg :atk-spd) ;;攻撃速度
   (expe      :accessor expe      :initform 0     :initarg :expe) ;;もらえる経験値orプレイヤーの所持経験値
   (stage      :accessor stage       :initform 1   :initarg :stage)    ;;プレイヤーのいる階層
   ))

;;適用
(defclass enemy (common)
  ((drop         :accessor drop       :initform nil :initarg :drop)    ;;ドロップするアイテム
   (vx           :accessor vx         :initform 2   :initarg :vx)
   (vy           :accessor vy         :initform 2   :initarg :vy)
   (anime-img    :accessor anime-img  :initform 0   :initarg :anime-img)))

(defclass slime (enemy) ())
(defclass orc (enemy) ())
(defclass hydra (enemy) ())
(defclass brigand (enemy) ())
(defclass dragon (enemy) ())
(defclass boss (enemy) ())
(defclass yote1 (enemy) ())
(defclass fire (enemy) ())
(defclass briball (enemy) ())
(defclass orc-atk (enemy) ())
(defclass toge (enemy) ())
(defclass hydra-atk (enemy)
  ((centerx      :accessor centerx    :initform 30  :initarg :centerx)
   (centery      :accessor centery    :initform 30  :initarg :centery)
   (deg          :accessor deg        :initform 10  :initarg :deg)))


;;プレイヤー用
(defclass player (common)
  ((key?       :accessor key?        :initform nil :initarg :key?)     ;;鍵所持
   (lvup-exp   :accessor lvup-exp    :initform 100 :initarg :lvup-exp) ;;次のレベルアップに必要な経験値
   (name       :accessor name        :initform nil :initarg :name)     ;;名前
   (hammer     :accessor hammer      :initform 0   :initarg :hammer)   ;;所持ハンマー
   (buki       :accessor buki        :initform nil :initarg :buki)     ;;武器
   (boots?     :accessor boots?      :initform nil :initarg :boots?) ;;現在の階層
   (hammer-now :accessor hammer-now  :initform nil :initarg :hammer-now) ;;ハンマー中か
   (item       :accessor item        :initform nil :initarg :item)     ;;所持アイテム
   
   (state      :accessor state       :initform :title :initarg :state)
   
   (cursor     :accessor cursor      :initform 0   :initarg :cursor)
   (endtime    :accessor endtime     :initform 0   :initarg :endtime)  ;;クリア時間
   (atkhit     :accessor atkhit      :initform nil :initarg :atkhit)   ;;攻撃モーション中に当たったか
   ))

(defclass remote-player (player)
  ((stream1    :accessor stream1     :initform nil :initarg :stream1)
   (command    :accessor command     :initform nil :initarg :command)
   (ready?     :accessor ready?      :initform nil :initarg :ready?)
   (id         :accessor id          :initform 0   :initarg :id)
   (totaldmg   :accessor totaldmg    :initform 0   :initarg :totaldmg)
   (socket1    :accessor socket1     :initform nil :initarg :socket1)
   (lastmsg    :accessor lastmsg     :initform :empty :initarg :lastmsg)))


(defclass game ()
  ((players    :accessor players     :initform nil :initarg :players)
   (donjons    :accessor donjons     :initform 0   :initarg :donjons)
   (events     :accessor events      :initform nil :initarg :events)
   (clear      :accessor clear       :initform nil :initarg :clear)))

;;----------------------------------------------------

(defmacro addx (hoge  x)
  `(setf ,hoge (logior ,hoge (ash ,x 0))))

(defmacro getx (hoge)
  `(logand (ash ,hoge 0) #b1111111111))

(defmacro addy (hoge y)
  `(setf ,hoge (logior ,hoge (ash ,y 10))))

(defmacro gety (hoge)
  `(logand (ash ,hoge -10) #b1111111111))

;;----------------------------------------------------
;;ダメージ用
(defmacro addcolor (hoge color) ;; 1
  `(setf ,hoge (logior ,hoge (ash ,color 20))))
(defmacro getcolor (hoge)
  `(logand (ash ,hoge -20) #b1))

(defmacro adddmg-num (hoge dmg-num) ;; 7
  `(setf ,hoge (logior ,hoge (ash ,dmg-num 21))))
(defmacro getdmg-num (hoge)
  `(logand (ash ,hoge -21) #b1111111))

;;----------------------------------------------------
;; object and enemy and player 共通
(defmacro addw (hoge w) ;; 7 
  `(setf ,hoge (logior ,hoge (ash ,w 20))))
(defmacro getw (hoge)
  `(logand (ash ,hoge -20) #b1111111))

(defmacro addh (hoge h) ;; 7
  `(setf ,hoge (logior ,hoge (ash ,h 27))))
(defmacro geth (hoge)
  `(logand (ash ,hoge -27) #b1111111))

(defmacro addmoto-w (hoge moto-w) ;; 7 
  `(setf ,hoge (logior ,hoge (ash ,moto-w 34))))
(defmacro getmoto-w (hoge)
  `(logand (ash ,hoge -34) #b1111111))

(defmacro addmoto-h (hoge moto-h) ;; 7
  `(setf ,hoge (logior ,hoge (ash ,moto-h 41))))
(defmacro getmoto-h (hoge)
  `(logand (ash ,hoge -41) #b1111111))

(defmacro addimg (hoge img) ;; 4
  `(setf ,hoge (logior ,hoge (ash ,img 48))))
(defmacro getimg (hoge)
  `(logand (ash ,hoge -48) #b1111))
;;----------------------------------------------------
;; enemy and player 共通
(defmacro addhp (hoge hp) ;; 7 
  `(setf ,hoge (logior ,hoge (ash ,hp 55))))
(defmacro gethp (hoge)
  `(logand (ash ,hoge -55) #b1111111))

(defmacro addmaxhp (hoge maxhp) ;; 7
  `(setf ,hoge (logior ,hoge (ash ,maxhp 62))))
(defmacro getmaxhp (hoge)
  `(logand (ash ,hoge -62) #b1111111))

(defmacro adddir (hoge img) ;; 3
  `(setf ,hoge (logior ,hoge (ash ,img 69))))
(defmacro getdir (hoge)
  `(logand (ash ,hoge -69) #b111))

(defmacro adddead (hoge dead) ;; 1
  `(setf ,hoge (logior ,hoge (ash ,dead 72))))
(defmacro getdead (hoge)
  `(logand (ash ,hoge -72) #b1))

;;----------------------------------------------------
;; enemy
(defmacro addanime-img (hoge anime-img) ;; 4 
  `(setf ,hoge (logior ,hoge (ash ,anime-img 73))))
(defmacro getanime-img (hoge)
  `(logand (ash ,hoge -73) #b1111))
;;----------------------------------------------------
;; player
(defmacro addid (hoge id) ;; 3 
  `(setf ,hoge (logior ,hoge (ash ,id 73))))
(defmacro getid (hoge)
  `(logand (ash ,hoge -73) #b111))

(defmacro addlevel (hoge level) ;; 5 
  `(setf ,hoge (logior ,hoge (ash ,level 76))))
(defmacro getlevel (hoge)
  `(logand (ash ,hoge -76) #b11111))

(defmacro addexp (hoge exp) ;; 8 
  `(setf ,hoge (logior ,hoge (ash ,exp 81))))
(defmacro getexp (hoge)
  `(logand (ash ,hoge -81) #b11111111))

(defmacro addlvup-exp (hoge lvup-exp) ;; 8 
  `(setf ,hoge (logior ,hoge (ash ,lvup-exp 89))))
(defmacro getlvup-exp (hoge)
  `(logand (ash ,hoge -89) #b11111111))

(defmacro addstage (hoge stage) ;; 5 
  `(setf ,hoge (logior ,hoge (ash ,stage 97))))
(defmacro getstage (hoge)
  `(logand (ash ,hoge -97) #b11111))

(defmacro addstr (hoge str) ;; 6
  `(setf ,hoge (logior ,hoge (ash ,str 102))))
(defmacro getstr (hoge)
  `(logand (ash ,hoge -102) #b111111))

(defmacro adddef (hoge def) ;; 6 
  `(setf ,hoge (logior ,hoge (ash ,def 108))))
(defmacro getdef (hoge)
  `(logand (ash ,hoge -108) #b111111))

(defmacro addhammer (hoge hammer) ;; 5 
  `(setf ,hoge (logior ,hoge (ash ,hammer 114))))
(defmacro gethammer (hoge)
  `(logand (ash ,hoge -114) #b11111))

(defmacro addready (hoge ready) ;; 1 
  `(setf ,hoge (logior ,hoge (ash ,ready 119))))
(defmacro getready (hoge)
  `(logand (ash ,hoge -119) #b1))

(defmacro addatk-now (hoge atk-now) ;; 1 
  `(setf ,hoge (logior ,hoge (ash ,atk-now 120))))
(defmacro getatk-now (hoge)
  `(logand (ash ,hoge -120) #b1))

(defmacro addhammer-now (hoge hammer-now) ;; 1 
  `(setf ,hoge (logior ,hoge (ash ,hammer-now 121))))
(defmacro gethammer-now (hoge)
  `(logand (ash ,hoge -121) #b1))


;;----------------------------------------------------

(defun AddDest (move)
  (ash move 0))

(defun AddStart (move)
  (ash move 8))

(defun AddPro (move)
  (ash move 16))

(defun AddMove (move)
  (ash move 17))

(defun AddCap (move)
  (ash move 23))

(defun GetDest (move)
  (logand (ash move 0) #xff))

(defun GetStart (move)
  (logand (ash move -8) #xff))

(defun GetPro (move)
  (logand (ash move -16) #x01))

(defun GetMove (move)
  (logand (ash move -17) #x1f))

(defun GetCap (move)
  (logand (ash move -23) #xff))
