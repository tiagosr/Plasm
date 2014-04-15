Plasm - Playful Assembler
=========================

Plasm is a Ruby library/DSL that enables direct generation of assembly code for a number of different processors and architectures, with all the power of Ruby available for helping you generate code for a given target. It is targeted mainly at old 8/16 bit processors and microcontrollers, who aren't offered much help in the way of C/C++ compilers due to limitations in their architectures and such, like the MOS 6502 and Zilog Z80.

## Simple usage example (Z80)

```ruby
require "z80"
project = Assembly.Z80.new
project.asm do
	jp :Start			# label reference
	org 100				# set origin
	
	__ :Start			# label definition
	ld A, [:SomeData] 	# load with a label reference
	out 0x80, A			#
	halt				#

	__ :SomeData		# label definition
	db 0x10				# data declaration
end
project.link.write_out("project.z80.bin")
```

## License (MIT)
 
 Copyright (c) 2014 Tiago Rezende
 
 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.
  
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.