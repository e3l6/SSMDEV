# SSMDev - Solar Spectral Measurement Device
Copyright © 2016 Eric Laursen

An open source device for autonomous sun tracking and measurement of the sun's 
spectral composition from sunrise to sunset, focusing specifically on 
chlorophyll-A and chlorophyll-B peak absorption spectra.

A description of the system function can be found in "description.txt"

I am currently a senior at Portland State University, Portland, OR. This
project is something I'm been thinking about for a while. I took CS 461P Open
Source Development Lab this summer (2016) under Bart Massey. It gave me the
motivation to actually work on the project.

I'm interested in safety critical systems and embedded systems programming,
so I chose to program an STM32F407 Discovery microcontroller using nothing
but the Ada programming language. Thankfully AdaCore has released a very nice
tool chain for ARM boards... GNAT GPL 2016 for ARM ELF format (hosted on
Linux), found at http://libre.adacore.com/

The sun angle calculations have been tested against a few web sites and found
to be reasonably accurate. The files under the directory "demo" are what I am
currently working on, so that I can learn and demonstrate the separate
"ingredients" I need for the whole project to come together (USART, SPI, I2C,
homegrown hookups on individual GPIO pins, etc.).

Portions of this code are available under the "MIT License", others under the
GPL. Please see the file COPYING in this distribution for license terms.
