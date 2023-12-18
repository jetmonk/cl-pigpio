
(in-package cl-pigpio)


(defun %build-spi-open-flags (spi-num spi-channel 
			      &key
				(spi-mode 0)
				(active-low t)
				(3-wire nil) ;; vs customary 4-wire
				(bytes-before-rx 15) ;; only if not 3-wire
				(lsb-xmit nil) ;; active for aux SPI only
				(lsb-rx   nil) ;; active for aux SPI only
				(word-size 8) ;; in bits
				)
  (flet ((checkparam (val par min max)
	   (when (not (<= min val max))
	     (error "SPI Open PARAM ~A=~A is not in [~A,~A]" par val
		    min max))))
    (checkparam spi-mode "SPI-MODE" 0 3)
    (checkparam bytes-before-rx "BYTES-BEFORE-RX" 0 15)
    (checkparam word-size "WORD-SIZE" 1 15)
    (checkparam spi-num "SPI-NUM" 0 1)
    (checkparam spi-channel "SPI-CHANNEL" 0 (if (zerop spi-num) 2 3)))
    

  
  (let ((ux ;; is CEx is reserved for SPIx?
	  (or (and (= spi-num 0) (member spi-channel '(8 7)))
	      (and (= spi-num 1) (member spi-channel '(18 17 16)))))
	(ce0 (or (= spi-channel 8) (= spi-channel 18)))
	(ce1 (or (= spi-channel 7) (= spi-channel 17)))
	(ce2 (or (= spi-channel 16))))

  
    (logior ;; to produce a 22 bit flag
     ;; spi-mode (1 and 3 do not work on SPI1?)
     spi-mode
     ;; if active low/high, put a 1 bit in
     ;; bits 2,3,4 for CE0,1,2
     (if ce0 (ash (if active-low 0 1) 2) 0)
     (if ce1 (ash (if active-low 0 1) 3) 0)
     (if ce1 (ash (if active-low 0 1) 4) 0)
     ;; is the SPI-CHANNEL pin reserved for SPI? Do all 3 pins
     ;; together.
     (if ce0 (ash (if ux 1 0) 5) 0)
     (if ce1 (ash (if ux 1 0) 6) 0)
     (if ce2 (ash (if ux 1 0) 7) 0)
     ;; is it the main SPI (0), or AUX (1)
     (ash (if (= spi-num 0) 0 1) 8)
     ;; if 3-wire put a 1 in bit 9 - this is an unusual case
     (ash (if 3-wire 1 0) 9)
     ;; number of bytes to write before switching MOSI (write) to MISO
     ;; (read) for case of non-3-wire, in bites 10 to 13; this is rare
     (ash (if (not 3-wire) 0 bytes-before-rx) 10)
     ;; 1 in bit 14 if MOSI is LSB
     (ash (if lsb-xmit 1 0) 14)
     ;; 1 in bit 15 if MISO is LSB
     (ash (if lsb-rx 1 0) 15)
     ;; word size in bits 16 to 21 - default 8 is special 0
     (ash word-size 16))))
		

(defun spi-open (spi-num spi-channel baud
		 &key
		   (spi-mode 0)
		   (active-low t)
		   (3-wire nil)
		   (bytes-before-rx 15) ;; only if not 3-wire
		   (lsb-xmit nil)
		   (lsb-rx   nil)
		   (word-size 8) ;; in bits
		   (pidev *default-pidev*))
  "Open a SPI link on SPI-NUM=0,1 and SPI-CHANNEL=0..2.

ACTIVE-LOW determines whether CS works on active-low.
3-WIRE determines whether this is a normal 3-wire connector.
BYTES-BEFORE-RX matters only if not a 3-wire, and determines
  when to switch from TX to RX.
LSB-XMIT, LSB-RX determine whether data are LSB or not.
  These apply only for SPI-NUM=1 (auxiliary SPI).
WORD-SIZE is the word size in bits, from 1 to 15."
  (let ((flags (%build-spi-open-flags spi-num spi-channel
				      :spi-mode spi-mode
				      :active-low active-low
				      :3-wire 3-wire
				      :bytes-before-rx bytes-before-rx
				      :lsb-xmit lsb-xmit
				      :lsb-rx lsb-rx
				      :word-size word-size)))
    ;;(format t "Flags is  ~22,'0B~%" flags)
    ;;(format t "          1098765432109876543210~%")
    ;;(format t "           2         1         0~%")
    ;;(setf flags 0)
    (handle-pigpio-error
      (pigpio-spiopen spi-channel baud flags :pidev pidev))))

(defun spi-close (handle &key (pidev *default-pidev*))
  "Close a SPI identified by integer HANDLE."
  (handle-pigpio-error
    (pigpio-spiclose handle :pidev pidev)))

(defun spi-read-into-array (handle buf nbytes &key (pidev *default-pidev*))
  "Read NBYTES of data from SPI HANDLE into BUF"
  (declare (type (unsigned-byte 64) handle)
	   (type (simple-array (unsigned-byte 8) (*)) buf)
	   (type (unsigned-byte 10) nbytes))
  (cffi:with-foreign-object (bptr :unsigned-char nbytes)
    (handle-pigpio-error
      (pigpio-spiread handle bptr nbytes :pidev pidev))
    (dotimes (i nbytes)
      (setf (aref buf i) (cffi:mem-aref bptr :unsigned-char i))))
  buf)


(defun spi-read-into-integer (handle nbytes &key (pidev *default-pidev*))
  "Read NBYTES of data from SPI HANDLE into an integer shorter than 64 bits"
  (when (not (<= 1 nbytes 8)) (error "NBYTES=~A, must be in [1,8]" nbytes))
  (cffi:with-foreign-object (bptr :unsigned-char nbytes)
    (handle-pigpio-error
      (pigpio-spiread handle bptr nbytes :pidev pidev))
    (%foreign-to-integer bptr nbytes)))

(defun spi-write-from-array (handle buf nbytes &key (pidev *default-pidev*))
  "Read NBYTES of data from SPI HANDLE into BUF"
  (declare (type (unsigned-byte 64) handle)
	   (type (simple-array (unsigned-byte 8) (*)) buf)
	   (type (unsigned-byte 10) nbytes))
  (cffi:with-foreign-object (bptr :unsigned-char nbytes)
    (dotimes (i nbytes)
      (setf (cffi:mem-aref bptr :unsigned-char i) (aref buf i)))

    (handle-pigpio-error
      (pigpio-spiwrite handle bptr nbytes :pidev pidev))))


(defun spi-write-from-integer (handle value nbytes &key (pidev *default-pidev*))
  "Read NBYTES of data from integer VALUE into SPI HANDLE."
  (declare (type (unsigned-byte 64) handle)
	   (type (unsigned-byte 64) value)
	   (type (integer 1 8) nbytes))
  (cffi:with-foreign-object (bptr :unsigned-char nbytes)
    (%integer-to-foreign value bptr nbytes)
    (handle-pigpio-error
      (pigpio-spiwrite handle bptr nbytes :pidev pidev))))


(defun spi-xfer (handle txbuf rxbuf &key (pidev *default-pidev*))
  "Simultaneously, for SPI HANDLE, write data to TXBUF, and read from RXBUF."
  (declare (type (unsigned-byte 64) handle)
	   (type (simple-array (unsigned-byte 8) (*)) txbuf rxbuf))
  (let ((nbytes (length txbuf)))
    (declare (type (integer 1 4096) nbytes))
    (when (not (= nbytes (length rxbuf)))
      (error "TXBUF and RXBUF have different lengths."))
    (cffi:with-foreign-object (txbptr :unsigned-char nbytes)
      (cffi:with-foreign-object (rxbptr :unsigned-char nbytes)
	(dotimes (i nbytes)
	  (setf (cffi:mem-aref txbptr :unsigned-char i) (aref txbuf i)))
	(handle-pigpio-error
	  (pigpio-spixfer handle  txbptr rxbptr nbytes :pidev pidev))
	(dotimes (i nbytes)
	  (setf (aref rxbuf i) (cffi:mem-aref rxbptr :unsigned-char i)))))
    rxbuf))

(defun spi-xfer-with-integers (handle value nbytes &key (pidev *default-pidev*))
  "Perform an in/out SPI-XFER of NBYTES<=8 with VALUE (<=64 bits) being
the input and an integer being the output.   This is done by placing
and retrieving the integer into a foreign buffer of size NBYTES, MSB
first."
  (declare (type (unsigned-byte 64) handle value)
	   (type (integer 1 8) nbytes))
  (cffi:with-foreign-object (bptr :unsigned-char nbytes)
      (%integer-to-foreign value bptr nbytes)
      (handle-pigpio-error (pigpio-spixfer handle  bptr bptr nbytes :pidev pidev))
      (%foreign-to-integer bptr nbytes)))
  
  


      
    
