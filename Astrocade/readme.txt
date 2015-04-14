-- This is a Bally Astrocade port for the MiST board
-- This work is a *derivative* work of the source cited below and from pacedev.net
-- The original source can be downloaded from <http://www.fpgaarcade.com>
-- https://github.com/wsoltys/mist-cores
--
-------------------------------------------------------------------------------
--
-- A simulation model of Bally Astrocade hardware
-- Copyright (c) MikeJ - Oct 2006
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- Redistributions in synthesized form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- Neither the name of the author nor the names of other contributors may
-- be used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- You are responsible for any legal issues arising from your use of this code.
--
-- The latest version of this file can be found at: www.fpgaarcade.com
--
-- Email support@fpgaarcade.com
--

 Revision list

 version 004 spartan3e_hires board build
 version 003 spartan3e board build
 version 002 cpu update
 version 001 initial release

********************************************************************************
     Note :

     Switch0 is used to switch between NTSC and VGA (31K). Up (on) is VGA
     and LED0 will come on.

     Switch1 when up (on) will DISABLE the external flash memory - unplugging 
     the cartridge.

     Switch3 and 2 are a two bit bank select.

     The push button on the rotary control is used for reset.
********************************************************************************

 The following scripts will create a directory called 'build', copy the source
 files, run the sythesizer and Xilinx place and route tools.

 Assuming the Xilinx tools are installed and working, expand the distribution
 zip file (maintaining directory structure).

 Fire up a command prompt and navigate to the directory.

 run :

 Build_roms.bat - this will convert the files in the Roms directory to VHDL
		  files (also in the Roms directory). These may then be used
		  if you wish to simulate the design. Note, the rom binaries
		  provided are blank.

 then either :


 Build_xst.bat - Xilinx build script using Xilinx WebPak
		 (uses bally_xst.ucf constraints file)

 if you add a /xil switch, the script will not run the synthesizer, just the
 place and route tools. You will be left with a .bit file in the Build directory
 you can use to program a chip. Remember to modify the .ucf file for your
 pinout.

 As it is wired up at the moment a 50Mhz clock is required.

 Additional Notes :

   External pull ups of 1k are recommended for the ps/2 inputs.

   ****
   Note, you must use the T80 files in the /source folder, the t80_latest
   is the latest Opencores.org release, but it has some bugs that I have fixed
   and not pushed back yet.
   ****

 Audio out :

   This DAC requires an external RC low-pass filter:

   audio_o 0---XXXXX---+---0 analog audio
		3k3    |
		      === 4n7
		       |
		      GND


 Video Out :

   Video out DAC's. Values here give 0.7 Volt peek video output.

   Use the following resistors for Video DACs :

   video_out(3) 510
   video_out(2) 1k
   video_out(1) 2k2
   video_out(0) 4k7

********************************************************************************
  H O W   T O   P R O G R A M   T H E   F L A S H
********************************************************************************

The Bally FPGA uses the onboard Intel Strata flash to store Bally cartridges.
Each cart is 8K (0x0000 - 0x1FFF)

Switch1 when on (up) DISABLES the cartridge.

Bits 14 and 13 are wired up to switches 3 and 2 on the board so ->

Switch  Flash address mapped into Card space (0x2000 - 0x3FFF in Bally address map)
3   2

off off 0x0000 - 0x1FFF
off on  0x2000 - 0x3FFF
on  off 0x4000 - 0x5FFF
on  on  0x6000 - 0x7FFF

First, you must convert the binary cart file into an MCS file.

I use srec_cat.exe (SRecord) to do the conversion : http://srecord.sourceforge.net/
I just downloaded the Windows executable.

To convert Treasure Cove for example :

srec_cat treasure.bin -binary -offset 0x0000 -o treasure.mcs -intel

where offset is the Flash base address above.

To program the flash you can use the very nice PicoBlaze RS-232 StrataFlash 
programmer by the great Ken Chapman.

http://www.xilinx.com/products/boards/s3estarter/reference_designs.htm

It comes with good instructions, but you basically connect an RS-232 cable from the
Spartan3e starter kit board to your pc, download the PicoBlaze programmer bit file 
using impact, fire up Hyperterminal, E to Erase all then P to program. Send the
MCS file using the Send Text File option in Hyperterminal. When complete, download
the Bally.bit design and play!

The PicoBlaze design from Xilinx uses the female RS-232 connector on the Starter kit
board. I have modified it to use the male connector, then you can use a standard
female-to-female null modem cable with a pin 2-3 swap. This bit file is on the bally
page for download.


 Cheers,

 MikeJ
