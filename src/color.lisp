;; -*- Lisp -*-

(defpackage :color
  (:use :cl :alexandria)
  (:export
   #:invalid-color-string
   #:parse))

(in-package :color)

(defparameter *colors* #. (loop with table = (make-hash-table :test #'equalp)
                                for (name value) on '("black"                 "000000"
                                                      "silver"                "c0c0c0"
                                                      "gray"                  "808080"
                                                      "white"                 "ffffff"
                                                      "maroon"                "800000"
                                                      "red"                   "ff0000"
                                                      "purple"                "800080"
                                                      "fuchsia"               "ff00ff"
                                                      "green"                 "008000"
                                                      "lime"                  "00ff00"
                                                      "olive"                 "808000"
                                                      "yellow"                "ffff00"
                                                      "navy"                  "000080"
                                                      "blue"                  "0000ff"
                                                      "teal"                  "008080"
                                                      "aqua"                  "00ffff"
                                                      "orange"                "ffa500"
                                                      "aliceblue"             "f0f8ff"
                                                      "antiquewhite"          "faebd7"
                                                      "aquamarine"            "7fffd4"
                                                      "azure"                 "f0ffff"
                                                      "beige"                 "f5f5dc"
                                                      "bisque"                "ffe4c4"
                                                      "blanchedalmond"        "ffe4c4"
                                                      "blueviolet"            "8a2be2"
                                                      "brown"                 "a52a2a"
                                                      "burlywood"             "deb887"
                                                      "cadetblue"             "5f9ea0"
                                                      "chartreuse"            "7fff00"
                                                      "chocolate"             "d2691e"
                                                      "coral"                 "ff7f50"
                                                      "cornflowerblue"        "6495ed"
                                                      "cornsilk"              "fff8dc"
                                                      "crimson"               "dc143c"
                                                      "darkblue"              "00008b"
                                                      "darkcyan"              "008b8b"
                                                      "darkgoldenrod"         "b8860b"
                                                      "darkgray"              "a9a9a9"
                                                      "darkgreen"             "006400"
                                                      "darkgrey"              "a9a9a9"
                                                      "darkkhaki"             "bdb76b"
                                                      "darkmagenta"           "8b008b"
                                                      "darkolivegreen"        "556b2f"
                                                      "darkorange"            "ff8c00"
                                                      "darkorchid"            "9932cc"
                                                      "darkred"               "8b0000"
                                                      "darksalmon"            "e9967a"
                                                      "darkseagreen"          "8fbc8f"
                                                      "darkslateblue"         "483d8b"
                                                      "darkslategray"         "2f4f4f"
                                                      "darkslategrey"         "2f4f4f"
                                                      "darkturquoise"         "00ced1"
                                                      "darkviolet"            "9400d3"
                                                      "deeppink"              "ff1493"
                                                      "deepskyblue"           "00bfff"
                                                      "dimgray"               "696969"
                                                      "dimgrey"               "696969"
                                                      "dodgerblue"            "1e90ff"
                                                      "firebrick"             "b22222"
                                                      "floralwhite"           "fffaf0"
                                                      "forestgreen"           "228b22"
                                                      "gainsboro"             "dcdcdc"
                                                      "ghostwhite"            "f8f8ff"
                                                      "gold"                  "ffd700"
                                                      "goldenrod"             "daa520"
                                                      "greenyellow"           "adff2f"
                                                      "grey"                  "808080"
                                                      "honeydew"              "f0fff0"
                                                      "hotpink"               "ff69b4"
                                                      "indianred"             "cd5c5c"
                                                      "indigo"                "4b0082"
                                                      "ivory"                 "fffff0"
                                                      "khaki"                 "f0e68c"
                                                      "lavender"              "e6e6fa"
                                                      "lavenderblush"         "fff0f5"
                                                      "lawngreen"             "7cfc00"
                                                      "lemonchiffon"          "fffacd"
                                                      "lightblue"             "add8e6"
                                                      "lightcoral"            "f08080"
                                                      "lightcyan"             "e0ffff"
                                                      "lightgoldenrodyellow"  "fafad2"
                                                      "lightgray"             "d3d3d3"
                                                      "lightgreen"            "90ee90"
                                                      "lightgrey"             "d3d3d3"
                                                      "lightpink"             "ffb6c1"
                                                      "lightsalmon"           "ffa07a"
                                                      "lightseagreen"         "20b2aa"
                                                      "lightskyblue"          "87cefa"
                                                      "lightslategray"        "778899"
                                                      "lightslategrey"        "778899"
                                                      "lightsteelblue"        "b0c4de"
                                                      "lightyellow"           "ffffe0"
                                                      "limegreen"             "32cd32"
                                                      "linen"                 "faf0e6"
                                                      "mediumaquamarine"      "66cdaa"
                                                      "mediumblue"            "0000cd"
                                                      "mediumorchid"          "ba55d3"
                                                      "mediumpurple"          "9370db"
                                                      "mediumseagreen"        "3cb371"
                                                      "mediumslateblue"       "7b68ee"
                                                      "mediumspringgreen"     "00fa9a"
                                                      "mediumturquoise"       "48d1cc"
                                                      "mediumvioletred"       "c71585"
                                                      "midnightblue"          "191970"
                                                      "mintcream"             "f5fffa"
                                                      "mistyrose"             "ffe4e1"
                                                      "moccasin"              "ffe4b5"
                                                      "navajowhite"           "ffdead"
                                                      "oldlace"               "fdf5e6"
                                                      "olivedrab"             "6b8e23"
                                                      "orangered"             "ff4500"
                                                      "orchid"                "da70d6"
                                                      "palegoldenrod"         "eee8aa"
                                                      "palegreen"             "98fb98"
                                                      "paleturquoise"         "afeeee"
                                                      "palevioletred"         "db7093"
                                                      "papayawhip"            "ffefd5"
                                                      "peachpuff"             "ffdab9"
                                                      "peru"                  "cd853f"
                                                      "pink"                  "ffc0cb"
                                                      "plum"                  "dda0dd"
                                                      "powderblue"            "b0e0e6"
                                                      "rosybrown"             "bc8f8f"
                                                      "royalblue"             "4169e1"
                                                      "saddlebrown"           "8b4513"
                                                      "salmon"                "fa8072"
                                                      "sandybrown"            "f4a460"
                                                      "seagreen"              "2e8b57"
                                                      "seashell"              "fff5ee"
                                                      "sienna"                "a0522d"
                                                      "skyblue"               "87ceeb"
                                                      "slateblue"             "6a5acd"
                                                      "slategray"             "708090"
                                                      "slategrey"             "708090"
                                                      "snow"                  "fffafa"
                                                      "springgreen"           "00ff7f"
                                                      "steelblue"             "4682b4"
                                                      "tan"                   "d2b48c"
                                                      "thistle"               "d8bfd8"
                                                      "tomato"                "ff6347"
                                                      "turquoise"             "40e0d0"
                                                      "violet"                "ee82ee"
                                                      "wheat"                 "f5deb3"
                                                      "whitesmoke"            "f5f5f5"
                                                      "yellowgreen"           "9acd32"
                                                      "rebeccapurple"         "663399")
                                by #'cddr
                                do (setf (gethash name table) value)
                                finally (return table)))

(define-condition invalid-color-string (error)
  ((color-string :initarg :color-string :reader color-string))
  (:report (lambda (c stream)
             (format stream "invalid color specification ~A" (color-string c)))))

(defun parse (string)
  (when string
    (let ((hex-value (gethash string *colors* string)))
      (unless (cl-ppcre:scan "(?i)^[0-9a-f]{6}$" hex-value)
        (error 'invalid-color-string
               :color-string string))
      (skippy:rgb-color (parse-integer hex-value :start 0 :end 2 :radix 16)
                        (parse-integer hex-value :start 2 :end 4 :radix 16)
                        (parse-integer hex-value :start 4 :end 6 :radix 16)))))

(hunchentoot:define-easy-handler (color-names :uri "/color-names") (term)
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (s)
    (yason:encode (remove-if-not (if term
                                     (let ((scanner (cl-ppcre:create-scanner term
                                                                             :case-insensitive-mode t)))
                                       (lambda (color-name)
                                         (cl-ppcre:scan scanner color-name)))
                                     (constantly t))
                                 (sort (hash-table-keys *colors*) #'string-lessp)) s)))
