
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

(defparameter *p-img* nil)
(defparameter *p-atk-img* nil)
(defparameter *buki-img* nil)
(defparameter *hammer-img* nil)
(defparameter *monster-anime* nil)
(defparameter *objs-img* nil)

;;プレイヤー画像切り替えよう
(defconstant +down+ 0)
(defconstant +left+ 2)
(defconstant +right+ 3)
(defconstant +up+ 1)

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


;;拡大
(Defparameter *mag-w* 1)
(Defparameter *mag-h* 1)

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

(my-enum +boots+ +door+ +hammer+ +hard-block+ +key+ +potion+ +soft-block+ +yuka+ +sword+ +cursor+ +kaidan+)

(my-enum +purple+ +red+ +green+ +blue+ +yellow+ +cyan+ +pink+ )

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
   (dir       :accessor dir       :initform :down :initarg :dir)     ;;現在の方向
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
  ((centerx      :accessor centerx    :initform 30  :initarg :centerx)
   (centery      :accessor centery    :initform 30  :initarg :centery)
   (drop         :accessor drop       :initform nil :initarg :drop)    ;;ドロップするアイテム
   (vx           :accessor vx         :initform 2   :initarg :vx)
   (vy           :accessor vy         :initform 2   :initarg :vy)
   (deg          :accessor deg        :initform 10  :initarg :deg)
   (anime-img    :accessor anime-img  :initform 0   :initarg :anime-img)))

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
   (id         :accessor id          :initform 0   :initarg :id)
   (socket1    :accessor socket1     :initform nil :initarg :socket1)))


(defclass game ()
  ((players    :accessor players     :initform nil :initarg :players)
   (donjons    :accessor donjons     :initform 0   :initarg :donjons)
   (events     :accessor events      :initform nil :initarg :events)))
