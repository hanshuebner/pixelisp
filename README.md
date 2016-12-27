# Pixelisp: Raspberry Pi based Game Frame engine

Pixelisp is alternative engine for the
[LEDSEQ Game Frame](http://ledseq.com/product/game-frame/), which is a
16x16 RGB matrix display in a nice, living room compatible housing.

## Features

* Configuration of all aspects of the Game Frame through a Web
  Interface
* Power on/off through IR remote, Web Interface or HTTP POST request
* Live display of current Game Frame content in Web UI
* Upload of animated GIF files with automatic resizing
* Deletion of animations
* Drag&drop configurable animation playlist
* Brightness control
* Animation playback speed control
* Configurable clock style

## Motivation

As delivered, the Game Frame comes with a number of prebuilt
animations on an SD-Card.  In order to change the animations shown,
the frame must be taken from the wall and opened with a screwdriver.
Furthermore, the animations themselves are in a custom format.  To
play contents in animated GIF format, it must first be converted using
a set of command line utilities.  Overall, I found this process too
cumbersome and wished for a solution that allowed me to upload new
animations through Wi-Fi.

I was thus excited when the
[Game Frame Wi-Fi Adapter](https://ledseq.com/product/game-frame-wifi-adapter/)
was released by LEDSEQ, as that promised to do what I wanted: Easy
content updates through Wi-Fi.  However, I found that the adapter was
very limited in its capabilities, as it is based around the
[Particle Photon](https://www.particle.io/products/hardware/photon-wifi-dev-kit),
an IoT development board that relies on cloud services to realise its
full potential and that has only limited local processing
capabilities.

This implementation of a replacement controller for the Game Frame is
based on a
[Raspberry Pi 3 Model B](https://www.raspberrypi.org/products/raspberry-pi-3-model-b/),
which is quite a capable computer compared to the original controllers
used by LEDSEQ.  The price of the Raspberry Pi is higher than the
original controller (Teensy LC, $12) or the Game Frame Wi-Fi Adapter
($29, including the socket adapter).  At around $40, it is still not
outrageously expensive, though, and it is way more powerful.

The Raspberry Pi runs Linux like a desktop machine.  Given that
driving a 16x16 display is not really a demanding task requiring very
high performance, using a Raspberry Pi makes it possible to use a
programming language that supports rapid development.  The
[Raspian Linux distribution](https://www.raspbian.org/) is a popular
Linux distribution for the Raspberry Pi and it comes with drivers for
all of the hardware that is required to control the Game Frame.

## Documentation

More documentation can be found in [the Wiki](https://github.com/hanshuebner/pixelisp/wiki).
