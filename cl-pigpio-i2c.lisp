

(in-package cl-pigpio)


;; for a 16 bit word 'word', maybe swap bytes if SWAP-BYTES
;; is true (SWAP-BYTEs is expected to be defined)
(defmacro maybe-swap-bytes (word)
  `(let ((%word ,word))
     (if swap-bytes
	 (logior (ash (logand %word #xff)  8)
		 (ash %word               -8))
	 %word)))

(defun i2c-open (i2c-bus i2c-addr &key (pidev *default-pidev*))
  "Open i2c numbered I2C-BUS (normally 0-1, but more possible with hardware)
with i2caddr (0-127); returns an integer handle.
(pigpio i2cOpen)"
  (declare (type (unsigned-byte 8) i2c-bus)
	   (type (integer 0 127) i2c-addr))
  (handle-pigpio-error
    (pigpio-i2copen i2c-bus i2c-addr 0 :pidev pidev))) ;; flag is always 0

(defun i2c-close (i2c-handle &key (pidev *default-pidev*))
  "Close I2C device described by integer I2C-HANDLE.

(pigpio i2cClose)"
  (declare (type (integer 0) i2c-handle))
  (handle-pigpio-error
    (pigpio-i2cclose i2c-handle :pidev pidev)))


(defun i2c-write-bit (i2c-handle bit &key (pidev *default-pidev*))
  "Write a bit (0/1) to I2C-HANDLE (i2cWriteQuick pigpio function)
(pigpio i2cWriteQuick)"
  (declare (type (integer 0) i2c-handle)
	   (type (unsigned-byte 1) bit))
  (handle-pigpio-error
    (pigpio-i2cwritequick i2c-handle bit :pidev pidev)))

(defun i2c-write-byte (i2c-handle byte &key (pidev *default-pidev*))
  "Write a byte to I2C-HANDLE.
(pigpio i2cWriteByte)"
  (declare (type (integer 0) i2c-handle)
	   (type (unsigned-byte 8) byte))
  (handle-pigpio-error
    (pigpio-i2cwritebyte i2c-handle byte :pidev pidev)))

(defun i2c-read-byte (i2c-handle &key (pidev *default-pidev*))
  "Read a byte from I2C-HANDLE.
(pigpio i2cReadByte)"
  (declare (type (integer 0) i2c-handle))
  (handle-pigpio-error
    (pigpio-i2creadbyte i2c-handle :pidev pidev)))



(defun i2c-write-byte-to-register (i2c-handle register byte &key (pidev *default-pidev*))
  "Write a byte to I2C-HANDLE, at REGISTER (pigpio i2cWriteByteData)
(pigpio i2cWriteByteData)"
  (declare (type (integer 0) i2c-handle)
	   (type (unsigned-byte 8) byte register))
  (handle-pigpio-error
    (pigpio-i2cwritebytedata i2c-handle register byte :pidev pidev)))

(defun i2c-read-byte-from-register (i2c-handle register &key (pidev *default-pidev*))
  "Read a byte from I2C-HANDLE, at REGISTER (pigpio i2cReadByteData)
(pigpio i2cReadByteData)"
  (declare (type (integer 0) i2c-handle)
	   (type (unsigned-byte 8) register))
  (handle-pigpio-error
    (pigpio-i2creadbytedata i2c-handle register :pidev pidev)))



(defun i2c-write-word-to-register (i2c-handle register word
				   &key swap-bytes (pidev *default-pidev*))
  "Write a 16 bit word to I2C-HANDLE, at REGISTER (pigpio i2cWriteWordData).
Word #xAABB is written in order #xBB #xAA unless SWAP-BYTES is T.
(pigpio i2cWriteWordData)"
  (declare (type (integer 0) i2c-handle)
	   (type (unsigned-byte 16) word)
	   (type (unsigned-byte 8) register))
  (handle-pigpio-error
    (pigpio-i2cwriteworddata i2c-handle register (maybe-swap-bytes word) :pidev pidev)))

(defun i2c-read-word-from-register (i2c-handle register
				    &key  swap-bytes (pidev *default-pidev*))
  "Read a 16 bit word from I2C-HANDLE, at REGISTER (pigpio i2cReadWordData)
Word #xAABB is read in order #xBB #xAA unless SWAP-BYTES is T.
(pigpio i2cReadWordData)"
  (declare (type (integer 0) i2c-handle)
	   (type (unsigned-byte 8) register))
  (maybe-swap-bytes
   (handle-pigpio-error
     (pigpio-i2creadworddata i2c-handle register :pidev pidev))))



(defun i2c-read-and-write-word-for-register (i2c-handle register word
					     &key swap-bytes (pidev *default-pidev*))
  "Read a 16 bit word to I2C-HANDLE at REGISTER, and read a word from same.
Word #xAABB is read written in order #xBB #xAA unless SWAP-BYTES is T.
(pigpio i2cProcessCall)"
   (declare (type (integer 0) i2c-handle)
	    (type (unsigned-byte 16) word)
	    (type (unsigned-byte 8) register))
  (maybe-swap-bytes
   (handle-pigpio-error
     (pigpio-i2cprocesscall i2c-handle register
			    (maybe-swap-bytes word)
			    :pidev pidev))))


 



(defun i2c-write-bytes-to-register (i2c-handle register buf 
				    &key (nbytes nil) (pidev *default-pidev*))
  "Write a bock of bytes from :UINT8 array BUF to I2C-HANDLE, at REGISTER.
(pigpio i2cWriteBlockData)"
  (declare (type (integer 0) i2c-handle)
	   (type (unsigned-byte 8) register)
	   (type (simple-array (unsigned-byte 8)) buf)
	   (type (or null (integer 1 32)) nbytes))
  (let* ((nbuf (length buf))
	 (%nbytes (or nbytes nbuf)))
    (declare (type (integer 1 32) %nbytes))
    (when (or (> nbytes nbuf) (> nbytes 32))
      (error "NBYTES=~A (or size of BUF) must be shorter than 32."
	     %nbytes))
    (cffi:with-foreign-object (bptr :uint8 %nbytes)
      (dotimes (i %nbytes)
	(setf (cffi:mem-aref bptr :uint8 i)
	      (aref buf i)))
      (handle-pigpio-error 
	(pigpio-i2cwriteblockdata i2c-handle register bptr %nbytes
				  :pidev pidev)))))


(defun i2c-read-bytes-from-register (i2c-handle register buf
				     &key (nbytes nil) (pidev *default-pidev*))
  "Read a block of bytes from I2C-HANDLE at REGISTER, returning data
in BUF (unsigned-byte array); BUF is the return value (pigpio
i2cReadBlockData)"
  (declare (type (integer 0) i2c-handle)
	   (type (unsigned-byte 8) register)
	   (type (simple-array (unsigned-byte 8)) buf)
	   (type (or null (integer 1 32)) nbytes))
  (let* ((nbuf (length buf))
	 (%nbytes (or nbytes nbuf)))
    (declare (type (integer 1 32) %nbytes))
    (when (or (> nbytes nbuf) (> nbytes 32))
      (error "NBYTES=~A (or size of BUF) must be shorter than 32."
	     %nbytes))
    (cffi:with-foreign-object (bptr :uint8 %nbytes)
      (handle-pigpio-error 
	(pigpio-i2creadblockdata i2c-handle register bptr %nbytes
				 :pidev pidev))
      (dotimes (i %nbytes)
	(setf (aref buf i)
	      (cffi:mem-aref bptr :uint8 i)))
      buf)))



(defun i2c-write-and-read-bytes-for-register (i2c-handle register buf
					      &key (nbytes nil) (pidev *default-pidev*))
  "Write a bytes from unsigned byte array BUF to I2C-HANDLE, at REGISTER.
Then read same number of bytes back into BUF; returns BUF.
(pigpio i2cBlockProcessCall)"
  (declare (type (integer 0) i2c-handle)
	   (type (unsigned-byte 8) register)
	   (type (simple-array (unsigned-byte 8)) buf)
	   (type (or null (integer 1 32)) nbytes))
  (let* ((nbuf (length buf))
	 (%nbytes (or nbytes nbuf)))
    (declare (type (integer 1 32) %nbytes))
    (when (or (> nbytes nbuf) (> nbytes 32))
      (error "NBYTES=~A (or size of BUF) must be shorter than 32."
	     %nbytes))

    (cffi:with-foreign-object (bptr :uint8 %nbytes)
      (dotimes (i %nbytes)
	(setf (cffi:mem-aref bptr :uint8 i)
	      (aref buf i)))
      (handle-pigpio-error 
	(pigpio-i2cblockprocesscall i2c-handle register bptr %nbytes
				    :pidev pidev))
      (dotimes (i %nbytes)
	(setf (aref buf i)
	      (cffi:mem-aref bptr :uint8 i)))
      buf)))




(defun i2c-write-bytes (i2c-handle buf &key (nbytes nil) (pidev *default-pidev*))
  "Write a block of bytes from unsigned-byte array BUF to I2C-HANDLE.
(pigpio i2cWriteDevice)"
  (declare (type (integer 0) i2c-handle)
	   (type (simple-array (unsigned-byte 8)) buf)
	   (type (or null (integer 1 32)) nbytes))
  (let* ((nbuf (length buf))
	 (%nbytes (or nbytes nbuf)))
    (declare (type (integer 1 32) %nbytes))
    (when (or (> nbytes nbuf) (> nbytes 32))
      (error "NBYTES=~A (or size of BUF) must be shorter than 32."
	     %nbytes))
    (cffi:with-foreign-object (bptr :uint8 %nbytes)
      (dotimes (i %nbytes)
	(setf (cffi:mem-aref bptr :uint8 i)
	      (aref buf i)))
      (handle-pigpio-error 
	(pigpio-i2cwritedevice i2c-handle bptr %nbytes :pidev pidev)))))


(defun i2c-read-bytes (i2c-handle buf &key (nbytes nil) (pidev *default-pidev*))
  "Read a block of bytes from I2C-HANDLE, returning data
in BUF (unsigned-byte array); BUF is the return value (pigpio
i2cReadDevice)"
  (declare (type (integer 0) i2c-handle)
	   (type (simple-array (unsigned-byte 8)) buf)
	   (type (or null (integer 1 32)) nbytes))
  (let* ((nbuf (length buf))
	 (%nbytes (or nbytes nbuf)))
    (declare (type (integer 1 32) %nbytes))
    (when (or (> nbytes nbuf) (> nbytes 32))
      (error "NBYTES=~A (or size of BUF) must be shorter than 32."
	     %nbytes))
    (cffi:with-foreign-object (bptr :uint8 %nbytes)
      (handle-pigpio-error 
	(pigpio-i2creaddevice i2c-handle bptr %nbytes :pidev pidev))
      (dotimes (i %nbytes)
	(setf (aref buf i)
	      (cffi:mem-aref bptr :uint8 i)))
      buf)))

(defun i2c-switch-combined (value &key (pidev *default-pidev*))
  "Change 'use combined transactions' mode in i2c-bcm2808 modue. 0 is off, 
non-zero is on.  
(pigpio i2cSwitchCombined)"
  (handle-pigpio-error
    (pigpio-i2cswitchcombined value :pidev pidev)))

(defun i2c-zip (i2c-handle inbuf outbuf &key (pidev *default-pidev*))
  "Executes a sequence of I2C operations on I2C-HANDLE.  Commands are in INBUF
and outputs are placed in OUTBUF (both unsigned-byte 8 arrays).
Returns OUTBUF.   (pigpio i2cZip)

The commands in INBUF are (where P is a one-byte parameter)

End     0              No more commands
Escape	1              Next P is two bytes
On	2              Switch combined flag on
Off	3              Switch combined flag off
Address	4 P            Set I2C address to P
Flags	5 lsb msb      Set I2C flags to lsb + (msb << 8)
Read	6 P            Read P bytes of data
Write	7 P [bytes]    Write P bytes of data
"

  (declare (type (integer 0) i2c-handle)
	   (type (simple-array (unsigned-byte 8)) inbuf outbuf))
  (let* ((n-inbuf (length inbuf))
	 (n-outbuf (length outbuf)))
    (cffi:with-foreign-objects ((bptr-in :uint8 n-inbuf)
				(bptr-out :uint8 n-outbuf))
      
      (dotimes (i n-inbuf)
	(setf (cffi:mem-aref bptr-in :uint8 i)
	      (aref inbuf i)))
      (handle-pigpio-error 
	(pigpio-i2czip i2c-handle bptr-in bptr-out n-inbuf n-outbuf :pidev pidev))
      (dotimes (i n-outbuf)
	(setf (aref outbuf i)
	      (cffi:mem-aref bptr-out :uint8 i)))
      outbuf)))
