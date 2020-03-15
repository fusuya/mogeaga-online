
(defparameter *game-state* :title)
(defparameter *cursor* 0)
(defparameter *input-time* 0)
(defparameter *stream* nil)
(defparameter *command* nil)
(defparameter *name* nil)
(defparameter *addr* nil) ;;�ڑ���A�h���X
(defparameter *id* 0)
(defparameter *port* 24336)

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

;;���O�ƃA�h���X��ǂݍ���
(defun set-name-and-addr ()
  (with-open-file (in "./setting.txt" :if-does-not-exist nil)
    (if in
	(loop :for line = (read-line in nil)
	     :for i from 0
	   :while line
	   :do (case i
		 (0 (setf *name* (chomp line)))
		 (1 (setf *addr* (chomp line))))))))

;;�u ���V����
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
  (setf *font140* (create-font "MS�S�V�b�N" :height 140)
	*font90* (create-font "MS�S�V�b�N" :height 90)
	*font70* (create-font "MS�S�V�b�N" :height 70)
        *font40* (create-font "MS�S�V�b�N" :height 40)
	*font30* (create-font "MS�S�V�b�N" :height 22 :width 9)
        *font20* (create-font "MS�S�V�b�N" :height 25 :width 12 :weight (const +fw-bold+))
	*font10* (create-font "MS�S�V�b�N" :height 16 :width 8)))

(defun delete-font ()
  (delete-object *font140*)
  (delete-object *font90*)
  (delete-object *font70*)
  (delete-object *font40*)
  (delete-object *font30*)
  (delete-object *font20*)
  (delete-object *font10*)
  )

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
  (flet ((li (path)
             (load-image path
                         :type :bitmap
                         :flags '(:load-from-file :create-dib-section))))
        (setf *objs-img* (li "./img/objs-img2.bmp")
              *p-img* (li "./img/p-ido-anime.bmp")
              *p-atk-img* (li "./img/p-atk-anime.bmp")
              *hammer-img* (li "./img/hammer-anime.bmp")
              *anime-monsters-img* (li "./img/monsters.bmp")
              *buki-img* (li "./img/buki-anime.bmp"))))

;;�L�[�������Ƃ�
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

;;�L�[�������Ƃ�
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
(defun trans-blt (x y x-src y-src w-src h-src w-dest h-dest)
  (transparent-blt *hmemdc* x y *hogememdc* x-src y-src :width-source w-src
		   :height-source h-src
		   :width-dest w-dest :height-dest h-dest
		   :transparent-color (encode-rgb 0 255 0)))

;;p�̎󂯂��_���[�W�\��
(defun render-dmg (p color)
  (let ((dmg (getf p :|dmg|)))
    (when dmg
      (let ((x (getf dmg :|x|))
	    (y (getf dmg :|y|))
	    (num (getf dmg :|dmg-num|)))
	(select-object *hmemdc* *font20*)
      
      (set-bk-mode *hmemdc* :transparent)
      ;;�����
      (set-text-color *hmemdc* (encode-rgb 0 0 0))
      (text-out *hmemdc* (format nil "~d" num) (- x 2) y)
      (text-out *hmemdc* (format nil "~d" num) (+ x 2) y)
      (text-out *hmemdc* (format nil "~d" num) x (- y 2))
      (text-out *hmemdc* (format nil "~d" num) x (+ y 2))
      ;;
      (set-text-color *hmemdc* color)
      (text-out *hmemdc* (format nil "~d" num) x y)))))

;;�A�j���\��
(defun render-enemy (objs)
  (let* ((obj (getf objs :|data|))
	 (x (getx obj))
         (y (gety obj))
	 (w (getw obj))
         (h (geth obj))
         (dead (getdead obj))
	 (img (getimg obj))
	 (anime-num (getanime-img obj))
         ;; �]���f�[�^�ʐߖ�̂��߂�moto-w/h��w/h�Ɠ����������ꍇ�͏ȗ�
         ;; ���Ă���̂ŁAmoto-w/h���Ȃ����w/h�̒l���g���B
	 (moto-w (getmoto-w obj))
         (moto-h (getmoto-h obj)))
    (when (eq dead 0) ;;����łȂ�������\��
      (select-object *hogememdc* *anime-monsters-img*)
      (trans-blt x y (* moto-w img) (* *obj-h* anime-num)
		     moto-w moto-h w h))))

;;�G�\��
(defun render-enemies (enemies)
  (loop for e in enemies
     do (render-enemy e)))

;;���݂̕���
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

;;�U�����̕`��
(defun render-p-atk (obj atk-img)
  (let* ((data (getf obj :|data|))
	 (buki1 (getf obj :|buki|))
	 (buki (getf buki1 :|data|))
	 (x (getx data)) (y (gety data))
         (name (getf obj :|name|))
	 (w (getw data)) (h (geth data))
         (dir (getdir data))
	 (moto-w (getmoto-w data))
         (moto-h (getmoto-h data))
	 (img (getimg data))
	 (xb (getx buki))
         (yb (gety buki)))
    (render-player-name name x y)
    (cond
      ((eq dir +down+)
       (select-object *hogememdc* *p-atk-img*)
       (trans-blt x y  (* w img) (* h dir) moto-w moto-h w h)
       (select-object *hogememdc* atk-img)
       (trans-blt xb yb (* w img) (* h dir) moto-w moto-h w h))
      (t
       (select-object *hogememdc* atk-img)
       (trans-blt xb yb (* w img) (* h dir) moto-w moto-h w h)
       (select-object *hogememdc* *p-atk-img*)
       (trans-blt x y (* w img) (* h dir) moto-w moto-h w h)))))
  
;;�v���C���[�\��
(defun render-player (obj name)
  (let* ((data (getf obj :|data|))
	 (atk-now (getatk-now data))
	 (hammer-now (gethammer-now data)))
    (cond
      ((eq atk-now 1)
       (render-p-atk obj *buki-img*))
      ((eq hammer-now 1)
       (render-p-atk obj *hammer-img*))
      (t
       (let ((x (getx data))
             (y (gety data))
	     (w (getw data))
             (h (geth data))
             (dir (getdir data))
	     (moto-w (getmoto-w data))
             (moto-h (getmoto-h data))
	     (img (getimg data)))
	 ;;(v:debug :game (format nil "~d" x))
	 ;;(v:debug :game (format nil "~d" y))
	 (render-player-name name x y)
	 (select-object *hogememdc* *p-img*)
	 (trans-blt x y (* moto-w img) (* moto-h dir)
			moto-w moto-h w h))))))

;;*objs-img*�̕`��
(defun render-objs-img (objs)
  (let* ((obj (getf objs :|data|))
	 (x (getx obj))
	 (y (gety obj))
	 (w (getw obj))
	 (h (geth obj))
	 (moto-w (getmoto-w obj))
	 (moto-h (getmoto-h obj))
	 (img (getimg obj)))
    (select-object *hogememdc* *objs-img*)
    (trans-blt x y (* moto-w img) 0
		   moto-w moto-h w h)))

;;�v���C���[�̃X�e�[�^�X�\��
(defun render-p-status (p name)
  (let* ((num 10)
	 (level (getlevel p))
         (hp (gethp p))
	 (maxhp (getmaxhp p))
         (str (getstr p))
	 (def (getdef p))
         (exp (getexp p))
	 (lvupexp (getlvup-exp p))
         (hammer (gethammer p)))
    (macrolet ((hoge (n)
		 `(incf ,n 25)))
      (select-object *hmemdc* *font30*)
      (set-text-color *hmemdc* (encode-rgb 255 255 255))
      (set-bk-mode *hmemdc* :transparent)
      (text-out *hmemdc* (format nil "~a" name) (+ *map-w* 10) num)
      (text-out *hmemdc* (format nil "Lv:~2d" level) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "HP:~2d/~2d" hp maxhp) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "�U:~2d" str) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "�h:~2d" def) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "exp") (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "~3d/~3d" exp lvupexp) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "�n���}�[") (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "�c��:~d��" hammer) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "id ~d" (getid p)) (+ *map-w* 10) (hoge num))
      (text-out *hmemdc* (format nil "stage ~d" (getstage p)) (+ *map-w* 10) (hoge num)))))
      ;;;(text-out *hmemdc* (format nil "���Q�A�[�K�̓� ~2,'0d�K" (stage *p*)) 10 (+ *map-h* 10)))))
      ;;(text-out *hmemdc* (format nil "~2,'0d:~2,'0d:~2,'0d:~2,'0d" h m s ms) 200 (+ *map-h* 10))))))

(defun render-other-p-status (p x name)
  (let* ((num (+ *map-h* 10))
	 (level (getlevel p)) (hp (gethp p))
	 (maxhp (getmaxhp p)) (str (getstr p)))
    (macrolet ((hoge (n)
		 `(incf ,n 25)))
      (select-object *hmemdc* *font30*)
      (set-text-color *hmemdc* (encode-rgb 255 255 255))
      (set-bk-mode *hmemdc* :transparent)
      (text-out *hmemdc* (format nil "~a" name) (+ 10 (* x 100)) num)
      (text-out *hmemdc* (format nil "Lv:~2d" level) (+ 10 (* x 100)) (hoge num))
      (text-out *hmemdc* (format nil "HP:~2d/~2d" hp maxhp) (+ 10 (* x 100)) (hoge num))
      (text-out *hmemdc* (format nil "�U:~2d" str) (+ 10 (* x 100)) (hoge num))
      (text-out *hmemdc* (format nil "stage:~2d" (getstage p)) (+ 10 (* x 100)) (hoge num)))))

;;�v���C���[�����`��
(defun render-players (players now-stage)
  (let ((x 0))
    (dolist (player players)
      (let ((p-data (getf player :|data|))
	    (name (getf player :|name|)))
	(if (= *id* (getid p-data))
	    (render-p-status p-data name)
	    (progn
	      (render-other-p-status p-data x name)
	      (incf x)))
	(when (= (getstage p-data) now-stage)
	  (render-player player name))))))



(defun render-objects (objs)
  (loop for obj in objs
        do (render-objs-img obj)))

;;�u���b�N�`��
(defun render-block (blocks)
  (render-objects blocks))

;;���`��
(defun render-yuka (mes)
  (render-objects (getf mes :|yuka|)))

;;���Ƃ��`��
(defun render-item (items)
  (render-objects items))

;;�o�b�N�O���E���h
(defun render-background ()
  (select-object *hmemdc* (get-stock-object :black-brush))
  (rectangle *hmemdc* 0 0 (rect-right *c-rect*) (rect-bottom *c-rect*)));;*change-screen-w* *change-screen-h*))

;;HP�o�[�\��
(defun render-hpbar (e hp maxhp)
  (let* ((len (floor (* (/ hp maxhp) *hpbar-max*)))
	 (x (getx e)) (y (gety e))
	 (hp-w (+ x len)))
    ;;�c��HP
    (select-object *hmemdc* (aref *brush* +green+))
    (rectangle *hmemdc* x  (- y 15) hp-w y)
    ;;������HP
    (select-object *hmemdc* (aref *brush* +red+))
    (rectangle *hmemdc* hp-w (- y 15) (+ hp-w (- *hpbar-max* len)) y)))

;;�_���[�W�\��
(defun render-damage (dmgs)
  (let* ((dmg (getf dmgs :|data|))
	 (color (if (eq +white+ (getcolor dmg))
		    (encode-rgb 255 255 255)
		    (encode-rgb 255 124 0)))
	 (dmg-num (getdmg-num dmg))
	 (x (getx dmg))
	 (y (gety dmg)))
    (select-object *hmemdc* *font20*)
    (set-bk-mode *hmemdc* :transparent)
    ;;�����
    (set-text-color *hmemdc* (encode-rgb 0 0 0))
    (text-out *hmemdc* (format nil "~d" dmg-num) (- x 2) y)
    (text-out *hmemdc* (format nil "~d" dmg-num) (+ x 2) y)
    (text-out *hmemdc* (format nil "~d" dmg-num) x (- y 2))
    (text-out *hmemdc* (format nil "~d" dmg-num) x (+ y 2))
    ;;
    (set-text-color *hmemdc* color)
    (text-out *hmemdc* (format nil "~d" dmg-num) x y)
    ))

;;�S�Ẵ_���[�W�\��
(defun render-all-damage (dmgs)
  (dolist (dmg dmgs)
    (render-damage dmg)))

;;HP�o�[�\��
(defun render-all-hpbar (enemies)
  (dolist (e1 enemies)
    (let* ((e (getf e1 :|data|))
	   (maxhp (getmaxhp e))
	   (hp (gethp e)) (dead (getdead e)))
      (when (and (/= maxhp hp)
		 (eq dead 0))
	(render-hpbar e hp maxhp)))))

(defvar *backgrounds* nil)

;;�}�b�v��\��
(defun render-map (mes)
  (let ((backgrounds (getf mes :|backgrounds|)))
    (if backgrounds
        (setf *backgrounds* (coerce backgrounds 'vector))))

  (let* ((players (getf mes :|players|))
	 (mine (find *id* players :key #'(lambda (x) (getid (getf x :|data|)))))
	 (mine-data (getf mine :|data|))
	 (now-stage (getstage mine-data))
	 (donjon (getf mine :|donjon|))
	 (blocks (getf donjon :|blocks|))
	 (item (getf donjon :|item|))
	 (dmg (getf donjon :|dmg|))
	 (enemies (getf donjon :|enemies|)))
    (render-background)
    (render-objects (getf (aref *backgrounds* (1- now-stage)) :|yuka|))
    (render-objects (getf (aref *backgrounds* (1- now-stage)) :|blocks|)) ; hard-block
    (render-block blocks) ; soft-block
    (render-item item)
    (render-enemies enemies)
    (render-players players now-stage)
    (render-all-damage dmg)
    (render-all-hpbar enemies)
    (render-events (getf mes :|events|))))

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

;;�^�C�g�����
(defun render-title-gamen (mes1 mes2)
  (render-background)
  (select-object *hmemdc* *font140*)
  (set-bk-mode *hmemdc* :transparent)
  (set-text-color *hmemdc* (encode-rgb 0 155 255))
  (text-out *hmemdc* (format nil "~a" mes1) 130 10)
  
  (select-object *hogememdc* *objs-img*)
  ;;�J�[�\���\��
  (trans-blt 380 (+ 400 (* *cursor* 50)) (* 32 +cursor+) 0 32 32 32 32)
  (select-object *hmemdc* *font40*)
  (text-out *hmemdc* "setting.txt�Ŗ��O�Ɛڑ���A�h���X��ݒ肵�Ă�������" 100 150)
  (text-out *hmemdc* "1�s�ڂɖ��O�A2�s�ڂɃA�h���X�ł�" 100 190)
  (text-out *hmemdc* (format nil "���O�F~a" *name*) 100 300)
  (text-out *hmemdc* (format nil "�ڑ���A�h���X�F~a" *addr*) 100 340)
  (set-text-color *hmemdc* (encode-rgb 255 255 255))
  (text-out *hmemdc* (format nil "~a" mes2) 430 400)
  (text-out *hmemdc* (format nil "���O�ƃA�h���X��ǂݍ���") 430 450)
  (text-out *hmemdc* (format nil "�����") 430 500))

;;�Q�[���S�̕`��
(defun render-game (hdc)
  (transparent-blt hdc 0 0 *hmemdc* 0 0
		   :width-dest *change-screen-w* :height-dest *change-screen-h*
		   :width-source (rect-right *c-rect*)
		   :height-source (rect-bottom *c-rect*) 
		   :transparent-color (encode-rgb 0 255 0)))

(defun decode-u32 (arr)
  (+ (ash (aref arr 0) 24)
     (ash (aref arr 1) 16)
     (ash (aref arr 2) 8)
     (ash (aref arr 3) 0)))

(defparameter *lastmsg* :empty)
(defun read-message (stream)
  (let ((arr (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence arr stream)
    (let* ((len (decode-u32 arr))
           (buf (make-array len :element-type '(unsigned-byte 8))))
      (read-sequence buf stream)
      (let ((str (babel:octets-to-string (gzip-stream:gunzip-sequence buf) :encoding :utf-8)))
        ;;(v:debug :network str)
        (let ((diff (jonathan:parse str :as :plist)))
	  (setf *lastmsg* (diff:patch *lastmsg* diff))
	  *lastmsg*)))))

(defun message-type (message)
  (getf message :|type|))

;;mes�\��
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
		  (do-msg (format nil
                                  "�z�X�g~a��IP�A�h���X���킩��܂���ł����B"
                                  address)
                          70 100
                          *font40*)
                  (return-from make-socket-stream nil))))))

    (if (ignore-errors (socket-connect sock addr port) t)
	(socket-make-stream sock :input t :output t :element-type :default :external-format :utf-8)
      nil)))

;;�Q�[�����J�n����
(defun start-game ()
  (setf *stream* (make-socket-stream *addr* *port*))
  (if *stream*
      (progn
	;; (loop :for byte :across (babel:string-to-octets (format nil "~A~%" *name*) :encoding :utf-8) :do
	;;      (write-byte byte *stream*))
	(format *stream* "~A~%" *name*)
	(force-output *stream*)

	(setf *id* (parse-integer (read-line *stream*)))

	;;(do-msg "��t�����F�Q�[���J�n�҂���" 100 100 *font40*)

	(setf *game-state* :wait-game-start))
      (progn
	(setf *game-state* :connect-error))))

;;�^�C�g����ʂł̑���
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

;;���ʉ�ʂł̃L�[����
(defun input-reslut-gamen (hwnd)
  (with-slots (q enter) *keystate*
    (incf *input-time*)
    (when (zerop (mod *input-time* 8))
      (cond
	(enter
	 (setf *game-state* :title))
	(q
	 (send-message hwnd (const +wm-close+) nil nil))))))

(defun display-status (status-message)
  (let ((players (getf (getf status-message :|map|) :|players|))
	(num 30))
    (macrolet ((incf40 (n)
		 `(incf ,n 40)))
      (render-background)
      (do-msg (format nil "��t�����F�Q�[���J�n�҂���") 100 num *font40*)
      (do-msg (format nil "Enter�ŏ�������") 100 (incf40 num) *font40*)
      (do-msg (format nil "�Q���ґ҂�(�J�n�܂�~a�b)" (getf status-message :|timeout-seconds|))
	100 (incf40 num) *font40*)
      (do-msg "�Q���v���[���[:" 100 (incf40 num) *font40*)
      (dolist (p players)
	(let* ((data (getf p :|data|))
	       (ready? (getready data))
	       (msg (if (eq ready? 1) "��������" "������")))
	  (do-msg (format nil  "~a:~a~%" (getf p :|name|) msg)
	    100 (incf40 num) *font40*))))))

;;���ʔ��\
(defun render-result (mes)
  (let* ((ranking (getf mes :|ranking|))
	 (num 30)
	 (l1 (loop for i in ranking
		collect (list (getf i :|name|) (getf i :|totaldmg|))))
	 (l2 (sort l1 #'>= :key #'second)))
    (macrolet ((incf40 (n)
		 `(incf ,n 40)))
      (render-background)
      (do-msg (format nil "�Q�[���N���A�I") 100 num *font40*)
      (do-msg (format nil "�v���x�����L���O") 100 (incf40 num) *font40*)
      (loop for p in l2
	 for i from 1
	 do (do-msg (format nil  "~a:~a�|�C���g~%" (car p) (cadr p))
	      100 (incf40 num) *font40*))
      (do-msg (format nil "Enter:�^�C�g����ʂ�") 100 400 *font40*)
      (do-msg (format nil "q:�Q�[���I��") 100 450 *font40*))))


    

(defun keystate->command ()
  (with-slots (z c q up down right left enter) *keystate*
    (cond
      (z  "Z")
      (c  "C")
      (q  "Q")
      (up "UP")
      (down "DOWN")
      (right "RIGHT")
      (left "LEFT")
      (enter "ENTER")
      (t "STAY"))))

;;�v���C���̃��[�v
(defun wait-game-start ()
  (if (listen *stream*)
      (progn
	(let* ((message (read-message *stream*))
	       (type (message-type message)))
	  (cond
	    ((eq type +status+)
	     (display-status message)
	     (when (not (equal *command* (keystate->command)))
               (setf *command* (keystate->command))
               (format *stream* "~a~%" *command*)
               (force-output *stream*)))
	    ((eq type +playing+)
	     (render-map message)

             (when (not (equal *command* (keystate->command)))
               (setf *command* (keystate->command))
               (format *stream* "~a~%" *command*)
               (force-output *stream*)))
	    ((eq type +result+)
	     (render-result message)
	     (setf *game-state* :result))
	    ((eq type +dead+)
	     (init-parameter)
	     (setf *game-state* :dead))
	    ((eq type +stop-entry+)
	     (init-parameter)
	     (setf *game-state* :stop))
	    ((eq type +quit+)
	     (init-parameter))
	    (t
	     (error (format nil "�\�����Ȃ����b�Z�[�W�^�C�v: ~s"
			    (message-type message)))))))))

;;�Q�[�����[�v
(defun main-game-loop (hwnd)
  (case *game-state*
    (:title
     (render-title-gamen "���Q�A�[�K�̓�" "�͂��߂�")
     (update-title-and-ending-gamen hwnd))
    (:connect-error
     (render-title-gamen "�ڑ����s" "�������")
     (update-title-and-ending-gamen hwnd))
    (:wait-game-start
     (wait-game-start))
    (:result
     (input-reslut-gamen hwnd))
    (:playing)
    (:dead
     (render-title-gamen "�Q�[���I�[�o�[" "�������")
     (update-title-and-ending-gamen hwnd))
    (:stop
     (render-title-gamen "���̐l���v���C��" "�������")
     (update-title-and-ending-gamen hwnd))
    (:ending))
  (invalidate-rect hwnd nil nil))

;;�E�B���h�E�T�C�Y�ύX���ɉ摜�g��k������
(defun change-screen-size (lp)
  ;; (setf *change-screen-w* (rect-right lp)
  ;; 	*change-screen-h* (rect-bottom lp)))
  (let* ((change-w (loword lp))
  	 (change-h (hiword lp)))
    (setf *change-screen-w* change-w
  	  *change-screen-h* change-h)))

;;�N���C�A���g�̈��*client-w* *client-h*�ɐݒ�
(defun set-client-size (hwnd)
  (let* ((rc (get-client-rect hwnd))
         (rw (get-window-rect hwnd))
         (new-w (+ *screen-w* (- (- (rect-right rw) (rect-left rw))
                               (- (rect-right rc) (rect-left rc)))))
         (new-h (+ *screen-h* (- (- (rect-bottom rw) (rect-top rw))
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
     (set-client-size hwnd)
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


;;���C��
(defun moge ()
  (setf *random-state* (make-random-state t))
  (register-class "MOGE" (callback moge-wndproc)
		  :styles (logior-consts +cs-hredraw+ +cs-vredraw+)
                  :cursor (load-cursor :arrow)
                  :background (create-solid-brush (encode-rgb 0 255 0)))
  (let ((hwnd (create-window "MOGE"
                             :window-name "���������̓��N��"
                             :styles (logior-consts +ws-overlappedwindow+ +ws-visible+)
                             :x 400 :y 100 :width *screen-w* :height *screen-h*))
        (msg (make-msg)))
    (show-window hwnd) 
    (update-window hwnd)
    (do ((done nil))
        (done)
        (if (ftw:peek-message msg :remove-msg :remove :error-p nil)
            (cond
             ((= (msg-message msg) (const +wm-quit+))
              (setf done t))
             (t
              (translate-message msg)
              (dispatch-message msg)))
	    (progn
	      (sleep 0.01)
	      (main-game-loop hwnd))))
    (msg-wparam msg)))
