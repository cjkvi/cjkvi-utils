;; 注音→ピンイン変換プログラム

(defvar kdp-zhuyin-consonants
  '(("ㄅ" . "b")
    ("ㄆ" . "p")
    ("ㄇ" . "m")
    ("ㄈ" . "f")
    ("ㄉ" . "d")
    ("ㄊ" . "t")
    ("ㄋ" . "n")
    ("ㄌ" . "l")
    ("ㄍ" . "g")
    ("ㄎ" . "k")
    ("ㄏ" . "h")
    ("ㄐ" . "j")
    ("ㄑ" . "q")
    ("ㄒ" . "x")
    ("ㄓ" . "zh")
    ("ㄔ" . "ch")
    ("ㄕ" . "sh")
    ("ㄖ" . "r")
    ("ㄗ" . "z")
    ("ㄘ" . "c")
    ("ㄙ" . "s")))

(defvar kdp-zhuyin-consonants-regexp
  (regexp-opt (mapcar 'car kdp-zhuyin-consonants)))

(defvar kdp-zhuyin-medials
  '(
    ;; (1) 子音＋介音（＋尾音）
    ;; (2) 介音のみ
    ;; (3) 介音＋尾音
    ("ㄧ" "i" "yi" "y")
    ("ㄨ" "u" "wu" "w")
    ("ㄩ" "ü" "yu" "yu")))

(defvar kdp-zhuyin-medials-regexp
  (regexp-opt (mapcar 'car kdp-zhuyin-medials)))

(defvar kdp-zhuyin-finals
  '(
    ;; (1) （子音＋）尾音
    ;; (2) 介音＋尾音
    ("ㄚ" "a"   "a")
    ("ㄜ" "e"   "e")
    ("ㄛ" "o"   "o")
    ("ㄝ" "ê"   "ê")
    ("ㄞ" "ai"  "ai")
    ("ㄟ" "ei"  "ei")
    ("ㄠ" "ao"  "ao")
    ("ㄡ" "ou"  "ou")
    ("ㄢ" "an"  "an")
    ("ㄣ" "en"  "en")
    ("ㄤ" "ang" "ang")
    ("ㄥ" "eng" "ng") 
    ("ㄦ" "er"  "r")
    ))

(defvar kdp-zhuyin-finals-regexp
  (regexp-opt (mapcar 'car kdp-zhuyin-finals)))

(defvar kdp-zhuyin-syllable-regexp
  (concat "\\(\\(" 
          kdp-zhuyin-medials-regexp "?" kdp-zhuyin-finals-regexp
          "\\)\\|\\("
          kdp-zhuyin-medials-regexp
          "\\)\\)"))

(defvar kdp-zhuyin-voice-regexp
  (concat
   "\\(\\("
   kdp-zhuyin-consonants-regexp kdp-zhuyin-syllable-regexp "?\\)\\|\\("
   kdp-zhuyin-syllable-regexp "\\)\\)"))

(defvar kdp-zhuyin-search-regexp
  (concat "\\(\\(" kdp-zhuyin-voice-regexp "[ˊˇˋ]\\)\\|\\(˙?"
          kdp-zhuyin-voice-regexp "\\)\\)"))

(defvar kdp-zhuyin-regexp
  (concat "^\\(˙\\)?\\(" kdp-zhuyin-consonants-regexp "\\)?\\("
          kdp-zhuyin-medials-regexp "\\)?\\("
          kdp-zhuyin-finals-regexp "\\)?\\([ˊˇˋ]\\)?$"))

(defvar kdp-zhuyin-special
  '(("ㄧㄝ" "ie"   "ye"  )
    ("ㄩㄝ" "üe"   "yue" )
    ("ㄧㄣ" "in"   "yin" )
    ("ㄩㄣ" "ün"   "yun" )
    ("ㄧㄥ" "ing"  "ying")
    ("ㄨㄣ" "un"   "wen" )
    ("ㄨㄥ" "ong"  "weng")
    ("ㄩㄥ" "iong" "yong")))

(defvar kdp-pinyin-voices
  '(("a" "ā" "á" "ǎ" "à")
    ("o" "ō" "ó" "ǒ" "ò")
    ("e" "ē" "é" "ě" "è")
    ("ê" "X" "ế" "X" "ề") ;; 本当は存在しないはず。
    ("i" "ī" "í" "ǐ" "ì")
    ("u" "ū" "ú" "ǔ" "ù")
    ("ü" "ǖ" "ǘ" "ǚ" "ǜ")))

(defun kdp-zhuyin-to-pinyin (zhuyin)
  (if (not (string-match kdp-zhuyin-regexp zhuyin))
      (error "Not proper zhuyin!"))
  (let* ((voice (cond ((equal (match-string 1 zhuyin) "˙") 0)
                      ((equal (match-string 5 zhuyin) "ˊ") 2)
                      ((equal (match-string 5 zhuyin) "ˇ") 3)
                      ((equal (match-string 5 zhuyin) "ˋ") 4)
                      (t 1)))
         (cons   (match-string 2 zhuyin))
         (medial (match-string 3 zhuyin))
         (final (match-string 4 zhuyin))
         (syllable (concat medial final)))
    ;; pinyin-conversion
    ;; consonant
    (if cons (setq cons (cdr (assoc cons kdp-zhuyin-consonants))))
    ;; syllable
    (if (equal syllable "") (setq syllable "i")
      (let ((special (assoc syllable kdp-zhuyin-special)))
        (if special (setq syllable (elt special (if cons 1 2)))
          (setq syllable
                (concat (elt (assoc medial kdp-zhuyin-medials)
                             (if cons 1 (if final 3 2)))
                        (elt (assoc final kdp-zhuyin-finals)
                             (if medial 2 1)))))))
    ;; attach tone algorithm
    ;; If there is an "a", "e", or "o", it will take the tone mark; in the case of "ao", the mark goes on the "a".
    ;; Otherwise, the vowels are "-iu" or "-ui", in which case the second vowel takes the tone mark.
    (concat 
     cons
     (if (string-match "[aeo]" syllable)
         (let ((replace (elt (assoc (match-string 0 syllable) kdp-pinyin-voices)
                             voice)))
           (if replace (replace-match replace t nil syllable)
             (error "Not matching! %s" syllable)))
       (if (string-match "[iu]?\\([[aoeiuüê]\\)" syllable)
           (let ((replace (elt (assoc (match-string 1 syllable) kdp-pinyin-voices)
                               voice)))
             (if replace (replace-match replace t nil syllable 1)
               (error "Not matching! %s syllable")))
         (error "Irregular Tone! %s" syllable))))))

(defun kdp-zhuyin-to-pinyin-region (from to)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward kdp-zhuyin-search-regexp nil t)
        (replace-match 
         (save-match-data (kdp-zhuyin-to-pinyin (match-string 0))))))))

(provide 'kdp-zhuyin)
