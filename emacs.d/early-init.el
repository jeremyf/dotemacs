;; See https://www.reddit.com/r/emacs/comments/ofhket/further_boost_start_up_time_with_a_simple_tweak/
;;
;; I saw a few comments recommending different way to manipulate the
;; gc timing. Actually, I have tried most of them. My take is to
;; moderately increase the threshold. I used to try a large threshold,
;; then what happens is that emacs accumulates a lot of garbage and
;; takes a few seconds to do the GC. This few-second-freeze make me
;; outrage.
;;
;; So after all, I found the default approach is the most robust
;; one. Instead of controlling the GC timing, just let it get things
;; done quickly so that the user won't notice. So in practice, The
;; best advice I read online is to double the GC threshold until you
;; don't feel improvements. My value is (setq gc-cons-threshold
;; 8000000) ;; ~8MB
(setq gc-cons-threshold most-positive-fixnum)