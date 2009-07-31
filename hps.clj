(ns hps
  (:refer-clojure :exclude (+ - * /))
  (:use net.philh.cloggle
	net.philh.gvec
	clojure.contrib.def
	clojure.contrib.pprint
	clojure.contrib.generic.arithmetic)
  (:import
   [java.awt Frame]
   [java.awt.event KeyEvent MouseEvent
    WindowListener WindowAdapter KeyAdapter MouseAdapter MouseMotionAdapter]
   [javax.media.opengl GLAutoDrawable GLCanvas GLEventListener]
   [com.sun.opengl.util Animator]))

(defvar *w-range* 1
  "The w coordinate of a point must be between positive and negative this.")

(def dvorak? (= (nth *command-line-args* 0) "-d"))
(def rot-key? (if dvorak? #{\i \u \e \. \o \,} #{\f \g \d \e \s \w}))
(def restart-key \space)
(def x-key? (if dvorak? #{\i \u} #{\g \f}))
(def y-key? (if dvorak? #{\e \.} #{\d \e}))
(def z-key? (if dvorak? #{\o \,} #{\s \w}))
(def up-key? (if dvorak? #{\, \e \u} #{\w \d \f}))
(def down-key? (if dvorak? #{\o \. \i} #{\s \e \g}))

(def pi Math/PI)
(def pi*2 (* pi 2))
(def pi_2 (/ pi 2))
(def pi_3 (/ pi 3))
(def pi_5 (/ pi 5))
(def pi_10 (/ pi 10))

(defn militime
  "Returns the current time in miliseconds."
  []
  (/ (. java.lang.System nanoTime) 1000000))

(defn init-vars
  "Initialise or re-initialise the various game-related variables."
  []

;; This doesn't really work, because rotation isn't commutative. Two x rotations
;; followed by a y rotation is not the same as a y rotation between two x
;; rotations, but with this model, any combination of those three will become
;; xxy.
;; But it seems to behave adequately.
  (def x-rot (ref 0))
  (def y-rot (ref 0))
  (def z-rot (ref 0))

  (def view-pos (ref (gvec 0 0))) ;opengl uses degrees
  (def mouse-pos (ref nil))

  (defvar stage-lines
    (concat
     (for [theta (range 0 pi pi_5)]
       (for [phi (range 0 pi*2 pi_10)]
	 (gvec-polar 1 theta phi)))
     (for [phi (range 0 pi pi_5)]
       (for [theta (range 0 pi*2 pi_10)]
	 (gvec-polar 1 theta phi))))
    "Seq of seqs. Each one contains the vertices of one line of the stage.")

  (defvar stage-lines-rotated
    (ref stage-lines)
    "stage-lines after applying the stage rotation.")

  (defvar goal-pos (gvec-polar (+ 0.25 (rand 0.6)) (rand pi*2) (rand pi))
    "The location of the centre of the goal.")

  (defvar goal-points
    (map (partial + goal-pos)
	 (concat (for [theta (range 0 pi*2 pi_5)
		       phi (range pi_5 pi pi_5)]
		   (gvec-polar 0.15 theta phi))
		 [(gvec-polar 0.15 0 0)
		  (gvec-polar 0.15 0 pi)])))

  (defvar hsphere-radius 0.1
    "The radius of the hypersphere.")

  (let [r hsphere-radius]
    (defvar hsphere-points
      (concat [(gvec-polar r 0 0 0)] 
	      (for [theta (range pi_10 pi*2 pi_5) ;offset makes it visible
		    phi (range 0 pi pi_5)	  ;from default viewpoint.
		    psi (range pi_5 pi pi_5)]
		(gvec-polar r theta phi psi))
	      [(gvec-polar r 0 0 pi)])
      "A seq of the points in the hypersphere."))

  (defvar hsphere-pos (ref (gvec 0 0 0 hsphere-radius))
    "The current position of the hypersphere.")
  (defvar hsphere-vel (ref (gvec 0 0 0 0))
    "The current velocity of the hypersphere.")
  (defvar hsphere-acc (ref (gvec 0 0 0 0))
    "The current acceleration of the hypersphere.")

  (defvar stage-normal (ref (gvec 0 0 0 1))
    "The direction of \"up\". Rotates with the stage.")

  (defvar game-state (ref nil)
    ":win, :lose or nil as appropriate.")

  (defvar alpha (ref 0.5)
    "The alpha value of coordinates in 4-space.")

  (defvar init-time (ref nil)
    "The time this game was initialised.")
  )

(init-vars)

(defn stage-rotation
  "Rotates a point in 4-space according to orientation of the stage."
  [gv]
  (-> gv
    (plane-rotate :x :w @x-rot)
    (plane-rotate :y :w @y-rot)
    (plane-rotate :z :w @z-rot)))

(defn w-magnitude
  "Converts a w value between ±*w-range* to [0,1]."
  [#^Double w] (/ (Math/abs w) *w-range*))

(defmacro vertex*
  "Macro version of vertex, for speed."
  [x y z]
  `(. opengl-context glVertex3d ~x ~y ~z))
(defmacro color*
  "Macro version of color, for speed."
  [r g b a]
  `(. opengl-context glColor4d ~r ~g ~b ~a))

(defmacro vertex-4 ; macro for speed.
  "Embeds a vertex in 3-space and colors it according to its w component.
Vertices with positive w are red, negative w displays blue. 0 w is white."
  ([gv] `(let [v# ~gv]
	   (vertex-4 (v# :x) (v# :y) (v# :z) (v# :w))))
  ([x y z w]
     `(let [mag# (w-magnitude ~w)
	    -mag# (- 1 mag#)]
	(when (<= mag# 1)
	  (color* (if (pos? ~w) 1 -mag#)
		  -mag#
		  (if (neg? ~w) 1 -mag#)
		  @alpha)
	  (vertex* ~x ~y ~z)))))

(def #^Frame frame (new Frame))
(def #^GLCanvas canvas (new GLCanvas))
(def #^Animator animator (new Animator canvas))

(defn quit-game
  "Exits the game."
  []
  (.dispose frame)
  (.stop animator))

(defn test-conditions
  "Returns :win if the game is won because the hypersphere entered the goal, or
:lose if the game is lost because it went off-stage at the edge."
  []
  (cond (>= (length (- @hsphere-pos
		       (* hsphere-radius @stage-normal)))
	    1)
	  :lose
	(<= (length (- @hsphere-pos goal-pos)) 0.1)
	  :win
	true nil))

(. canvas addGLEventListener
   (proxy [GLEventListener] []
     (init [x])
     (reshape
      [#^GLAutoDrawable drawable x y w h]
      (ctx (.getGL drawable)
	(glPolygonMode GL_BACK GL_LINE)
	(glEnable GL_BLEND)
	;;(glEnable GL_LIGHTING)
	;;(glEnable GL_LIGHT0)
	;;(glEnable GL_COLOR_MATERIAL)
	(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
	(glMatrixMode GL_PROJECTION)
	(glLoadIdentity)
	(glFrustum -1 1 -1 1 1 5)
	;;(glOrtho -1 1 -1 1 1 5)
	(glTranslatef 0 0 -2)
	(glMatrixMode GL_MODELVIEW)
	(glLoadIdentity))) 
     (display
      [#^GLAutoDrawable drawable]
      (ctx (.getGL drawable)
	(dosync
	 (if (not @init-time) (ref-set init-time (militime)))
	 (when (and (not @game-state) (test-conditions))
	   (ref-set game-state (test-conditions))
	   (cl-format true "~A in ~,2F seconds.~%"
		      (if (= @game-state :win) "win" "lose")
		      (double (/ (- (militime) @init-time) 1000))))
	 (when @game-state
	   (ref-set hsphere-acc (gvec 0 0 0 -0.1))
	   (alter alpha - 0.05))
	 (alter hsphere-vel + @hsphere-acc)
	 (alter hsphere-pos + @hsphere-vel))
	(glClear GL_COLOR_BUFFER_BIT)
	(glClear GL_DEPTH_BUFFER_BIT)
	(glLoadIdentity)
	(glRotated (@view-pos :x) 0 1 0)
	(glRotated (@view-pos :y) 1 0 0)
	;;(glLightfv GL_LIGHT0 GL_POSITION [0.8 0.8 0 1] 0)
	(doseq [l @stage-lines-rotated] ;200 points
	  (beg-end GL_LINE_LOOP
	    (doseq [v l]
	      (vertex-4 v))))
	(beg-end GL_POINTS		;202 points
	  (doseq [v hsphere-points]
	    (vertex-4 (+ @hsphere-pos v)))
	  (doseq [v goal-points]
	    (vertex-4 (stage-rotation v))))))))

(.setSize canvas 600 600)
(.add frame canvas)
(.pack frame)

(. frame addWindowListener
   (proxy [WindowAdapter] []
     (windowClosing
      [event]
      (quit-game))))

(defn keypress-listener
  "The function called on keyPressed events. We need separate KeyListeners for
canvas and frame because canvas doesn't register them until the mouse has been
clicked, and frame doesn't register them afterwards."
  [#^KeyEvent event]
  (let [key (.getKeyChar event)]
    (cond
      (= (.getKeyCode event) KeyEvent/VK_ESCAPE) (quit-game)
        (= key restart-key) (init-vars)
      (rot-key? key)
        (do
	  (let [x? (x-key? key)
		y? (y-key? key)
		z? (z-key? key)
		axis (cond x? :x y? :y z? :z)
		dir (if (up-key? key) + -)]
	    (dosync
	     (alter (cond x? x-rot
			  y? y-rot
			  z? z-rot)
		    dir
		    (/ pi 60))
	     (alter hsphere-pos
		    plane-rotate axis :w (dir (/ pi 60)))
	     (alter hsphere-vel
		    plane-rotate axis :w (dir (/ pi 60)))
	     (ref-set stage-normal (stage-rotation (gvec 0 0 0 1)))
	     (ref-set hsphere-acc
		      (* 0.1 (- (* @stage-normal (@stage-normal :w))
				(gvec 0 0 0 1))))
	     (ref-set stage-lines-rotated
		      (map (fn [l]
			     (map stage-rotation l))
			   stage-lines)))))))  )

(. frame addKeyListener
   (proxy [KeyAdapter] []
     (keyPressed [event] (keypress-listener event))))
(. canvas addKeyListener
   (proxy [KeyAdapter] []
     (keyPressed [event] (keypress-listener event))))

(. canvas addMouseMotionListener
   (proxy [MouseMotionAdapter] []
     (mouseMoved
      [#^MouseEvent event]
       (dosync (ref-set mouse-pos (gvec (.getPoint event)))))
     (mouseDragged
      [#^MouseEvent event]
      (dosync (if @mouse-pos ;might be nil if we reset then drag without moving.
		(alter view-pos +
		       (* 0.3 (- (gvec (.getPoint event)) @mouse-pos))))
	      (ref-set mouse-pos (gvec (.getPoint event)))))))

(.show frame)
(.start animator)
