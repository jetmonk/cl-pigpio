

(in-package cl-pigpio)


(defun set-gpio-mode (npin mode &key (pidev *default-pidev*))
  "Set pin NPIN to MODE, one of :INPUT :OUTPUT :ALT0 :ALT1 :ALT2 :ALT3 :ALT4 :ALT5"
  (declare (type (integer 0 53) npin)
	   (type (member :input :output :alt0 :alt1 :alt2 :alt3 :alt4 :alt5) mode))
  (handle-pigpio-error
    (pigpio-gpiosetmode npin
		 (cond ((eql mode :input)  +pi_input+)
		       ((eql mode :output) +pi_output+)
		       ((eql mode :alt1)   +pi_alt1+)
		       ((eql mode :alt2)   +pi_alt2+)
		       ((eql mode :alt3)   +pi_alt3+)
		       ((eql mode :alt4)   +pi_alt4+)
		       ((eql mode :alt5)   +pi_alt5+))
		 :pidev pidev)))

	   
(defun get-gpio-mode (npin &key (pidev *default-pidev*))
  "Get mode of GPIO pin NPIN, returning one of 
  :INPUT :OUTPUT :ALT0 :ALT1 :ALT2 :ALT3 :ALT4 :ALT5"
  (declare (type (integer 0 53) npin))
  (let ((mode
	  (handle-pigpio-error
	    (pigpio-gpiogetmode npin :pidev pidev))))
    (cond ((eql mode +pi_input+)    :input)
	  ((eql mode +pi_output+)   :output) 
	  ((eql mode +pi_alt1+)     :alt1)
	  ((eql mode +pi_alt2+)     :alt2)
	  ((eql mode +pi_alt3+)     :alt3)
	  ((eql mode +pi_alt4+)     :alt4) 
	  ((eql mode +pi_alt5+)     :alt5))))



(defun set-gpio-pullupdown (npin pull-direction &key (pidev *default-pidev*))
  "Set pin NPIN resistor PULL-DIRECTION, one of :PULL-UP :PULL-DOWN :OFF"
  (declare (type (integer 0 53) npin)
	   (type (member :pull-up :pull-down :off) pull-direction))
  (handle-pigpio-error
    (pigpio-gpiosetpullupdown
     npin
     (cond ((eql pull-direction :pull-up)   +pi_pud_up+)
	   ((eql pull-direction :pull-down) +pi_pud_down+)
	   ((eql pull-direction :off)       +pi_pud_off+))
     :pidev pidev)))

;; NOTE:  there is no get-gpio-pullupdown

(defun gpio-read (npin &key (pidev *default-pidev*))
  "Read GPIO level of NPIN as 0 or 1"
  (declare (type (integer 0 53) npin))
  (handle-pigpio-error (pigpio-gpioread npin :pidev pidev)))

(defun gpio-write (npin level &key (pidev *default-pidev*))
  "Set GPIO NPIN to LEVEL 0 or 1"
  (handle-pigpio-error (pigpio-gpiowrite npin level :pidev pidev)))


