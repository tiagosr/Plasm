#
# Playful Assembler
# (c)2011-2014 Tiago Rezende
#
# Assembler directives
#


require "./memory"
require "./register"
require "./label"
require "./section"
require "./number_addons"


module Assembler


  # The Assembler class
  #
  # Defines the standard functionality of each assembler.
  # Extend this class to support your processor and/or architecture.
  class Assembler

    @@current = nil

    def self.current
      @@current
    end
    
    # push an operation to the linker list and execute it
    # only push the outermost closure in a chain, as it will
    # execute the inner ones in it's time
    def __op op
      if !@__no_pushing
        @__no_pushing = true
        @ops.push op
        op.call
        @__no_pushing = false
      else
        op.call
      end
    end

    private

    def _push_section section
      __op ->{ @section_stack.push section }
    end

    def _pop_section
      __op ->{ @section_stack.pop }
    end

    public

    # instance initialization
    def initialize (name = "")
      @name = name
      @ops = []
      @__no_pushing = false
      @labels = []
      @memory_map = MemoryMap.new
      @rom_bank_map = RomBankMap.new @memory_map
      @sections = [(Section.new "", can_org: true),]
      @section_stack = [@sections[0]]
      @current_rom_bank = nil
      @current_mem_pos = [0, nil]
      @current_label_offset = [0, nil]
      @structs = {}
      @big_endian = false
      @current_phase = :assembling
      @prng = Random.new
    end

    # retrieves the current section from the stack
    def current_section
      @section_stack[-1]
    end

    def assemble &block
      temp = @@current || nil
      @@current = self
      self.instance_eval &block
      link_phase
      @@current = temp
    end
    
    def asm &block
      assemble &block
    end

    # org
    # sets origin address for opcode outputs and label addresses
    def org (address)
      current_section.org address
    end
    
    # orga
    # sets origin address for following label addresses
    def orga (address)
      current_section.orga address
    end
  
    # label
    # stores a new label for the assembly
    def label (name)
      if name.is_a? Symbol
        #__op ->{ self.current_section.tag_label name }
        # won't need to do it twice, will we?
        self.current_section.tag_label name
      else
        raise "label must be a symbol"
      end
    end
    
    # shortcut for label, allowing a syntax more alike the usual
    # assembly code listings
    def __ (name)
      label name
    end
    
    # defines a new section, and scopes the operations
    # in the block to that section
    def section (name, opts = {}, &block)
      sec = Section.new name, @current_mem_pos, opts
      @sections.push sec
      _push_section sec
      self.instance_eval &block
      _pop_section
      sec
    end
  
    
    # defines the memory map for the assembler
    def memory_map (&block)
      @memory_map = MemoryMap.new
      @memory_map.instance_eval &block
    end
  	
    # defines the rom bank map for the project
    def rom_bank_map (&block)
      @rom_bank_map = RomBankMap.new
      @rom_bank_map.instance_eval &block
      @current_rom_bank = @rom_bank_map.default_bank
    end
    
    # Define Byte(s)
    #
    # writes a byte or a sequence of bytes to the current section
    def db(*bytes); __op -> {__db *bytes} end
    def __db (*bytes)
      bytes.each do |b|
        if b.is_a? Array
          b.map!{|i| __db(i)}
        else
          self.current_section.stream_in b
        end
      end
    end
  
    # Define Word(s)
    #
    # writes a word (2 bytes) or a sequence of words to the current section,
    # respecting the endianness of data
    def dw (*words); __op -> {__dw *words} end
    def __dw (*words)
      words.each do |w|
        if w.is_a? Array
          w.map!{|i| __dw(i)}
        else
          if @big_endian
            __db (w.to_i>>8).b, w.b
          else
            __db w.b, (w.to_i>>8).b
          end
        end
      end  
    end
  
    # Define Doubleword(s)
    #
    # writes a double-word (4 bytes) or a sequence of double-words to the current section,
    # respecting the endianness of data

    def dd (*dwords); __op -> {__dd(*dwords)} end
    def __dd (*dwords)
        dwords.each do |w|
          if w.is_a? Array
            w.map!{|i| __dd(i)}
          else
            if @big_endian
              __dw ((w>>16)&0xffff), (w&0xffff)
            else
              __dw (w&0xffff), ((w>>16)&0xffff)
            end
          end
        end
    end
  
    # Define Quadword(s)
    #
    # writes a quad-word (4 bytes) or a sequence of quad-words to the current section,
    # respecting the endianness of data
    def dq (*qwords); __op -> {__dq *qwords} end
    def __dq (*qwords)
      qwords.each do |w|
        if @big_endian
          __dd ((w>>32)&0xffffffff),(w&0xffffffff)
        else
          __dd (w&0xffffffff), ((w>>32)&0xffffffff)
        end
      end
    end
  
    def dsb (count, byte); db Array.new(count, byte) end
    def dsw (count, word); dw Array.new(count, word) end
    def dsd (count, dword); dd Array.new(count, dword) end
    
    def ds (*string)
      if string.is_a? Array
        string.map!{|s| ds s}
      elsif string.is_a? String
        db string.each_byte
      else
        ds string.to_s
      end
    end

    def dzs (*string)
      if string.is_a? Array
        string.map!{|s| dzs s}
      else
        db string.each_byte
        db 0
      end
    end

    def dbcdb (number)
      db (((number.bcd_digit 1) << 4) + (number.bcd_digit 0))
    end

    def dbrnd (count, min, max)
      count.each do
        db (@prng.rand(max - min) + min)
      end
    end

    def dwrnd (count, min, max)
      count.each do
        dw (@prng.rand(max - min) + min)
      end
    end

    def ddrnd (count, min, max)
      count.each do
        dd (@prng.rand(max - min) + min)
      end
    end

    def seed (value)
      @prng.srand(value)
    end

    
    def asc_map (&block)
      @asc_map.instance_eval &block
    end


    def incbin (filename)
      File.open(filename, "rb") do |f|
        db f.each_byte
      end
    end

  
    def struct (label)
      tstruct = AsmStruct.new label
      @structs[label] = tstruct
      tstruct
    end
  
    def empty_fill (byte)
      @rom_bank_map.empty_fill byte
    end
    
    # read from all currently compiled bytes in the current section
    def _rb(addr)
      self.current_section.read_at addr
    end

    # store an opcode with multiple probable outcomes
    def __try_op (*blocks)
      bt = caller
      __op -> {
        last_error = "no error"
        snapshot = @current_section.snapshot_state
        blocks.each do |b|
          begin
            b.call
            #if there's an exception in this block, this return won't be taken
            return
          rescue => e
            last_error = e
          end
        end
        @current_section.set_state snapshot
        # if all blocks failed, raise the last exception raised
        # but first set the backtrace to the calling context 
        # (no need to expose the assembler/linker innards)
        last_error.set_backtrace bt
        raise last_error
      }
    end

    # the link phase allocates spaces, links labels and writes the final stream
    def link_phase
      last_phase = @current_phase
      @current_phase = :linking
      # here goes the magic... whenever I get to it
      @__no_pushing = true
      @ops.each do |op|
        op.call
      end
      @__no_pushing = false
      @current_phase = last_phase
      nil
    end

    def label_emit_dummy_value label
      if @current_phase == :linking
        raise "label %{label} not found"
      else
        return 0
      end
    end

    def define_struct label, &block
      struct = MemStruct.new label
      struct.instance_eval &block
      @structs[label] = struct
    end

    def link

      self
    end
    def write_out filename, **opts
      File.open(filename, "wb") do |f|
        banknum = 0
        bytecount = 0
        debug = opts[:debug] || false
        @rom_bank_map.banks.each do |bank|
          bank_bytecount = 0
          bank.write_out
          puts "writing bank \##{banknum}" if debug
          bank.image.each do |byte|
            f.write_byte byte
            bytecount += 1
            bank_bytecount += 1
          end
          puts "written #{bank_bytecount} bytes for bank \##{banknum}" if debug
          banknum += 1
        end
        puts "wrote #{banknum} banks and #{bytecount} bytes in total" if debug
      end
      self
    end

  end


end


module Kernel
  def with(object, &block)
    object.instance_eval &block
    object
  end
end