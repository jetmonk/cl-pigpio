# CL-PIGPIO

A Common Lisp interface to the Rapsberry Pi [pigpio](https://abyz.me.uk/rpi/pigpio/) library ([pigpio on github](https://github.com/joan2937/pigpio))


## Description

[_pigpio_](https://abyz.me.uk/rpi/pigpio/) is a library to provide a
simplified interface to the Raspberry Pi's hardware devices, like SPI,
I2C, Serial, and GPIO.

_pigpio_ has 1) a direct hardware version (linking with _libpigpio.so_) that
requires the program using it to run as root; and 2) a _pigpiod_
daemon (linked with _libpigpiod_if2.so_) that allows user-land
programs to accesss hardware via network calls to the daemon.

_CL-PIGPIO_ links with both libraries, and allows the user to select
whether the hardware or the daemon form is called.  If the hardware
version is activated but the user's program is not running with root
privileges, an error is generated.

Only one of the two (hardware or daemon) can be run on the same RPi, but
the daemon on other RPis can be accessed remotely.


### Installing

Install in your usual Lisp package tree and run

```
(asdf:load-system "cl-pigpio")
```

or, for just the low level FFI, run

```
(asdf:load-system "pigpio-ffi-universal")
```


## Low level FFI interface

The unified _pigpio/pigpiod_ interface is in the
_pigpio-ffi-universal_ package.


Here, the routines are generally the same as
[documented in the C pigpio
interface](https://abyz.me.uk/rpi/pigpio/cif.html) but with an
additional keyword device specifier.


For example, if the C library has

```
  int i2cOpen(unsigned i2cBus, unsigned i2cAddr, unsigned i2cFlags);
```

then the Lisp version has

```
  (pigpio-ffi-universal:pigpio-i2copen
         i2cbus i2caddr i2cflags
         &key (pidev pigpio-ffi-unversal:*default-pidev*))
```      

The _pidev_ keyword argument can be set as

```
  ;; daemon form, setting *default-pidev* to this device
  (pigpio-ffi-universal:open-pidev/daemon :host "localhost" :port "8888"
                                          :set-default t)
  ;; OR open hardware device, this time not setting the default variable
  (defparameter *my-pidev*
     (pigpio-ffi-universal:open-pidev/hardware :set-default nil))
```

It is necessary to open a device before making any pigpio/pigpiod calls.


## Higher level interface

The _cl-pigpio_ package contains a higher level interface to SPI, I2C, and GPIO.

This interface is not documented in detail, but the exported functions
are visible in _cl-pigpio-package.lisp_, and the functions generally
have good documentation strings.  I2C, SPI, and GPIO have separate
source files.

The purpose of the higher level interface is to translate opaque data
like I2C flags into more easily understable Lisp keywords, and to
perform FFI calls using Lisp buffers rather than using CFFI foreign
pointers.

The high level routines also throw a descriptive Lisp error if the
pigpio library returns an error.

## Glitches and gotchas

* pigpio does not work for RPi5 (yet?) because hardware interface is not documented
* signals are turned off in the hardware version using `gpioCfgGetInternals()` to prevent clashes with any Lisp use of signals.


## Dependencies

Tested with 64 bit SBCL, on Bookworm.

* [_cffi_](https://github.com/cffi/cffi)
* [_pigpio_](https://github.com/joan2937/pigpio), which can be installed as a standard RPi Linux package.



