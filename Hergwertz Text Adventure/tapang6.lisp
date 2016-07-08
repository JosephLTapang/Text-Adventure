;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name: Joseph Tapang                            Date: 10 April 16
;;;; Course: ICS313        Assignment: 6  
;;;; File: tapang6.lisp

;;; Hergwerts Golem Galumph
;;;ID RELATED CODE BEGINS
(defconstant +ID+ '("Joseph Tapang")); constant variable ID that contains my name

;;FINAL ID FUNCTION
(defun id (class assignment)"Function that prints out my name, course, and assignment number." 
  (princ "Name: ") (princ (car +ID+))(terpri);Use global constant +ID+ to print name.
  (princ "Course: ICS")(princ class)(terpri);prints out the input class.
  (princ "Assignment #  ")(princ assignment)nil);prints out the input assignment.
;;;ID RELATED CODE ENDS


;;;BEGIN HERGWERTS LOCATION CODE
;;; Source: Land of Lisp textbook. Modified with different locations and content.

;;Global variable that defines nodes that are a combination of area and its description.
(defparameter *nodes* '((entrance (You see the demolished remains of a door and above it a rickety sign labelled Hergwerts Magicians Department. Looking closer
							you see a note from Ferderx about the successful delivery of one golem murderizer.))
                        (foyer (You are in the main hall of hergwerts magicians department. A giant staircase looms 
							before you. The staircase looks as if something heavy was dragged up its steps.))
                        (scroll-library (You are in the remains of the library of scrolls. Most are
							destroyed but there may be some still worth using.))
                        (potions-room (You are in a room that smells like science but looks like a blast zone. The floor is littered with glass and wood shards but you can
						still see a large glyph on the floor. A sign next to it reads..magical combine items glyph..but only really combines three specific items.))
						(mysterious-upstairs-room (You are in a dark room that gives you a peculiar feeling
							of being murderized. That feeling materializes into reality as what you once thought was a column in the middle of the room begins to move. 
							You vaguely remember the demolished door and bowed steps of the staircase. You are jolted from your moment of thought as a gate slams closed behind you. Turning around you realize that where
							the column was now stands a golem. The furnace within its chest crackles with fire and begins to burn brighter as it preps to run its clobber you algorithm.
							If you have a glass-vial full volatile fluid and a teleport-scroll now would be a good time to activate it.))))

;;Global variable that defines the relative location of areas based on where they are next to. 
(defparameter *edges* '((entrance (foyer north door))  
                        (foyer (mysterious-upstairs-room north staircase);mysterious-upstairs-room should have no exit, golem encounter occurs here. 
							(scroll-library west door)
							(potions-room east door)
							(entrance south door))
                        (scroll-library (foyer east door));key item: teleport-scroll
                        (potions-room (foyer west door))));key items: glass-vial, o2-potion, gas-potion, volatile-recipe.

;;Global variable that defines potential objects in the world.
(defparameter *objects* '(digitus-III-scroll teleport-scroll wall-of-text-scroll H2O-potion O2-potion gas-potion glass-vial volatile-recipe golem))

;;Global variable that associates an object to a specific room.
(defparameter *object-locations* '((digitus-III-scroll scroll-library)
                                   (teleport-scroll scroll-library)
                                   (wall-of-text-scroll scroll-library)
                                   (H2O-potion potions-room)
                                   (O2-potion potions-room)
                                   (gas-potion potions-room)
								   (volatile-recipe potions-room)
								   (glass-vial potions-room)
								   (golem mysterious-upstairs-room)))


(defparameter *location* 'entrance);Global variable that defines your start location.
(defparameter *golem* 'golem)
(defparameter *volatile-potion-created* nil);global var that determines if the player created the volatile potion.

(defun describe-location (location nodes)"Function that uses the provided location to determine the preceding location description."
   (cadr (assoc location nodes)))


(defun describe-path (edge)"Function that provides a path description based on the supplied edge."
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))


(defun describe-paths (location edges)"Uses the describe path function to provide information about multiple paths."
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))


(defun objects-at (loc objs obj-loc)"Function that describes what  objects are at a location."
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))


(defun describe-objects (loc objs obj-loc)"Functions that describes the objects at a location."
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))


(defun look ()"Function that allows the user to determine their location, and if there exists related paths and objects." 
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))


(defun walk (direction)"Function that allows the user to walk east, west, upstairs, or downstairs."
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))


(defun pickup (object)"Function that allows user to pick up an object if present."
  (cond ((eq object *golem*) '(You try to pickup the golem but it picks you up instead...to death.))
		((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	  (t '(you cannot get that.))))
	  
;;added after April 18th 
;;non golem room location use
;;golem room location use
(defun use (object)"Function that allows user to use an object if present."
  (cond ((and(eq object 'teleport-scroll)(eq *location* 'mysterious-upstairs-room))'(You read the teleport scroll then suddenly feel extremely hot. The scroll educates you in the cosmos focusing on stars in particular. 
		  It gives you just enough knowledge for you to know that you have been teleported into a sun. You died a fiery educated death.))
		((eq object 'teleport-scroll)'(As the spell fizzles you feel as if the magical energy in this area is absent.))
		((and(eq object 'wall-of-text-scroll)(eq *location* 'mysterious-upstairs-room))'(Lorem ipsum dolor sit amet consectetur adipiscing elit. Maecenas viverra libero sit amet mattis pharetra dui nulla pharetra odio
			  et lobortis leo ante at ex. Nunc molestie bibendum porta. Aenean magna urna condimentum non dignissim at ultrices ut magna. Aliquam nec sapien eget arcu viverra 
			  ornare in vel sapien. Pellentesque posuere libero a nunc consequat eu posuere erat faucibus. Proin imperdiet orci eget consectetur dapibus odio magna tristique 
			  risu convallis lacinia elit leo nec nunc. Aliquam turpis libero viverra id sagittis sed molestie facilisis eros. In ut diam dictum mattis urna vel rhoncus dolor. 
			  Donec sed purus vel urna tempor viverra ut congue purus. Integer semper elit non tellus elementum quis imperdiet lectus feugiat.
			  Nunc consectetur varius tristique. Nunc non sagittis ligula vitae accumsan eros. Fusce mattis lacus arcu et efficitur leo viverra ac. Aliquam leo orci congue vel 
			  sagittis vitae maximus at nisi. Pellentesque mattis ex vel nisi viverra sodales. Phasellus eu fermentum ligula. Maecenas venenatis tincidunt nibh in dapibus. Praesent 
			  in hendrerit libero. Mauris ornare at tellus non mollis. Ut eu enim sem. Nam sit amet dui arcu. Praesent et sapien purus. Nulla non tellus auctor malesuada neque eget
			  consequat leo. Sed augue odio viverra eget nisl eget facilisis ultricies turpis. Proin a pulvinar purus. Duis laoreet orci ut quam lacinia rutrum. Cras urna ex 
			  faucibus in diam at dictum feugiat mi. Ut tempus est et risus rhoncus nec viverra diam rhoncus. Fusce maximus ligula sit amet arcu imperdiet quis accumsan ex aliquam. 
			  Cras ultricies in neque sit amet ornare. Duis convallis purus augue nec fringilla ex maximus eget. Donec at est condimentum auctor libero sed porta mauris. You died by the way.))
		((eq object 'wall-of-text-scroll)'(As the spell fizzles you feel as if the magical energy in this area is absent.))
		((and(eq object 'digitus-III-scroll)(eq *location* 'mysterious-upstairs-room))'(You unfurl the scroll and see an unusual image of the back of a balled fist with the finger in the middle being raised. You don't understand
             what it means but for some reason you feel offended. The offended feeling wears off as the golem power bombs you on the nearest table. You dead.))
		((eq object 'digitus-III-scroll)'(As the spell fizzles you feel as if the magical energy in this area is absent.))
		((and(eq object 'O2-potion)(eq *location* 'mysterious-upstairs-room))'(You pop open the top of the potion and feel moderately refreshed. This is followed by being moderately murdered by the golem. Dead.))
		((eq object 'O2-potion)'(You pop open the top and feel moderately refreshed.))
		((and(eq object 'gas-potion)(eq *location* 'mysterious-upstairs-room))'(You pop open the top releasing a smell straight from an ogres bottom. This is followed by the scent of your own bottom as the golem folds you like a chair.))
		((eq object 'gas-potion)'(Smells like a fart. Terrible.))
		((and(eq object 'H20-potion)(eq *location* 'mysterious-upstairs-room))'(While opening the potion you accidentally spill its contents on your robe. It looks like you wet yourself how embarressing.You dead.))
		((eq object 'H2O-potion)'(Just water how magical.))
		((and (eq object 'glass-vial) (eq *volatile-potion-created* 't)) '(The contents looks like it could blow up an outhouse.))
		((eq object 'glass-vial)'(It looks like it is full to the brim with nothing.))
		((and(eq object 'volatile-recipe)(eq *location* 'mysterious-upstairs-room))'(You read the recipe to the golem. The golem adds your life to the recipe.))
		((eq object 'volatile-recipe)'(It reads if you use this recipe and a glass vial you can mix gas and O2 potion in the vial to make something volatile.))
		((eq object 'golem)'(You try to use the golem but in Hergwerts golem uses you.))
		))
  
;;end addition

(defun inventory ()"Function that returns a list of the objects the user has in their inventory."
  (cons 'items- (objects-at 'body *objects* *object-locations*)))


(defun have (object) "Function to check if an object is a member of your inventory."
    (member object (cdr (inventory))))
;;;END HERGWERTS LOCATION CODE.


;;;BEGIN COMMANDS CODE. Source: Land of Lisp textbook.

(defparameter *allowed-commands* '(look walk pickup inventory use help h ?));Global variable that defines usable commands.

(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
            '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))

;;function that prints out a victory or defeat message. This is dependent on the global var that is or isn't set in the game-action combine.
(game-action activate teleport-scroll glass-vial mysterious-upstairs-room
             (cond ((not *volatile-potion-created*) '(You pat all your pockets but all you find is and empty glass-vial and regret.))
                   ((have 'teleport-scroll) '(At the end of your frantic recital of the teleport-scroll you see a glass vial fling itself
				    out of your pocket and towards the golem. The golem shields its chest but the vial blinks out of existence. Both you and the golem stand around
					in confusion until you notice the vial managed to plant itself into the furnace of the golems chest. At the exact moment you see this there is a 
					bright flash of light followed by the sound of metal and rock twisting and collapsing to the floor. The golem is no more and you have to make 
					a call to Ferderx. The End.))))

;;game-action that allows user to combine specific items. This leads to a win condition if done successfully.
(game-action combine volatile-recipe glass-vial potions-room
             (if (and (not *volatile-potion-created*) (and (have 'O2-potion) (and (have 'glass-vial) (and (have 'volatile-recipe) (have 'gas-potion)))))
                 (progn (setf *volatile-potion-created* 't)(pickup 'volatile-potion)
                        '(The glyph flashes under your feet as the contents of the glass vial begin to bubble and froth. You pop the cover on and slip it in your pocket like any other magician would.))
               '(you hear a voice in your head telling you that these items do not combine.)))
			   		   

  ;;;HELP FUNCTION BEGINS
(defun help ()"Function that returns commands available to the player."
  (princ "Available commands: ")(list *allowed-commands*))

(defun h ()"Function that returns commands available to the player."
  (princ "Available commands: ")(list *allowed-commands*))

(defun ? ()"Function that returns commands available to the player."
  (princ "Available commnds: ")(list  *allowed-commands*))
  ;;;HELP FUNCTION ENDS

  ;;;MACROS BEGIN

;;Macro that adds new object and location to *objects* and *object-locations*.
(defmacro new-object (command obj obj-loc &body body)"Macro that adds a new object to *objects* as well as adding the object and location to *object-locations*."
  `(progn (defun ,command (object object-location)
	    (if (eq object ',obj)
		(eq object-location ',obj-loc)
	    ,@body
	    '(I cant ,command like that.)))
  (pushnew ',obj *objects*)(pushnew ',obj-loc *object-locations*)));pushnews that push object and object location combination to resepective global vars.

;(new-object new-obj whiskey (whiskey living-room));macro test to add a pipe to *objects* global variable.
;(new-object new-obj1 whiskey (whiskey living-room));test to make sure pushnew prevents double additions to the global var.

;;
(new-object new-obj high-carb-burger (high-carb-burger living-room));Adds new object and object location to *objects* and *objects-locations*.

;;*nodes* global var for locations.
(defmacro new-location (command loc-dec &body body)"Macro that adds a new location to *nodes*."
  `(progn (defun ,command (location-description)
            (if (eq location-description ',loc-dec)
            ,@body
            '(I cant ,command like that.)))
  (pushnew ',loc-dec *nodes*)));pushes location description combination into *nodes* global var, pushnew prevents duplicates.

(new-location new-loc (patio (This place reaks of pure evil.)));adds a new location with description.

;;*edges* global var for paths between locations
(defmacro new-path (command path &body body)"Macro that adds a new path to *edges*."
  `(progn (defun ,command (a-path)
            (if (eq a-path ',path)
            ,@body
            '(I cant ,command like that.)))
  (pushnew ',path *edges*)));pushnew should push the new path into the *edges* global var.

(new-path new-p (patio(living-room east door)));new patio location connected to living-room.

  ;;;MACROS END

;;Use this in lisp repl to start the game.
(defun play-hergwerts ()"Function that creates a loop, allowing the user to repeat commands in the game environment."
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (play-hergwerts))))


(defun game-read ()"Function allows the user to type commands without the need for a single quote or parenthesis."
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))


(defun game-eval (sexp)"Function that only allows commands in the global variable *allowed-commands*."
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))


(defun tweak-text (lst caps lit)"Function accomodates spaces, commas, and punctuations such as !"
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))


(defun game-print (lst)"Function that prints human readable output by using tweak-text."
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))
;;;END COMMANDS CODE PART 2.