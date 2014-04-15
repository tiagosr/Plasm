require "./bank"


module Assembler
  # Memory map object
  #
  # makes available an interface for defining slots for bank switching
  # and rom/ram positions
  class MemoryMap
    def initialize
      @slots = []
      @slot_size = 0xffffffff
    end
    # defines a sized, isolated slot
    def slot (number, address)
      @slots[number] = {:address => address, :size => @slot_size}
    end
    # defines the default slot for unassigned code
    def default_slot (slot_num)
      @default_slot = slot_num
    end
    # defines the size of subsequent slot objects
    def slot_size (size)
      @slot_size = size
    end
    def ___check_consistency
      true # TODO: test if slots fill their spaces correctly #
    end
  end
  
  class RomBankMap
    def initialize (mem_map)
      @mem_map = mem_map
      @empty_fill = 0
      @banks = []
      @current_bank_size = 0
    end

    # defines a size for a bank
    def bank_size size
      @current_bank_size = size
    end
    # defines a number of banks of the same size
    def banks count
      count.each do
        @banks.push Bank.new @current_bank_size
      end
    end
  end
  
  # Struct creator
  #
  # The object interface for defining memory structures
  class MemStruct
    def initialize (name)
      @name = name
  	  @size = 0
  	  @offsets = []
    end
    def byte (label)
      @offsets.push({label: label, offset: @size, size: 1, write: -> l, b {db b.if_within(-128, 255) }})
      @size+=1
    end
    def word (label)
      @offsets.push({label: label, offset: @size, size: 2, write: -> l, w {dw w.if_within(-32768, 65535) }})
      @size+=2
    end
    def dword (label)
      @offsets.push({label: label, offset: @size, size: 4, write: -> l, d { dd d.if_within(-0x80000000, 0xffffffff)}})
      @size+=4
    end
    def qword (label)
      @offsets.push({label: label, offset: @size, size: 8, write: -> l, q { dq q.to_i }})
      @size+=8
    end
    def bytes (label, count)
      @offsets.push({label: label, offset: @size, size: count,   array: true, item_size: 1, write: -> l, a { a.each {|i| db i.if_within(-128,255) } }})
      @size += count
    end
    def words (label, count)
      @offsets.push({label: label, offset: @size, size: count*2, array: true, item_size: 2, write: -> l, a {a.each {|i| dw i.if_within(-32768,65535) } }})
      @size += count*2
    end
    def dwords (label, count)
      @offsets.push({label: label, offset: @size, size: count*4, array: true, item_size: 4, write: -> l, a {a.each {|i| dd i.if_within(-0x80000000,0xffffffff) } }})
      @size += count*4
    end
    def qwords (label, count)
      @offsets.push({label: label, offset: @size, size: count*8, array: true, item_size: 8, write: -> l, a {a.each {|i| dq i } }})
      @size += count*8
    end
    def struct (struct_name, &block)
      mstruct = MemStruct.new(struct_name)
      mstruct.instance_eval &block
      Assembler.current.structs[struct_name] = mstruct
    end
    def instance_of(struct_name, label)
      mstruct = Assembler.current.structs[struct_name]
      @offsets.push({label: label, offset: @size, size: mstruct.size, struct: mstruct, write: -> l, data {
       mstruct._make_rom_instance(Assembler.current.current_section, l, data)
      }})
      @size += mstruct.size
    end
    
    def instances_of(struct_name, label, count)
      mstruct = Assembler.current.structs[struct_name]
      @offsets.push({label: label, offset: @size, size: mstruct.size*count, array: true, struct: mstruct, item_size: mstruct.size, write: ->l,d { }})
      count.times do |c|
        @offsets.push({label: (label.to_s + ".#{c}").to_sym, offset: @size, size: mstruct.size, struct: mstruct, write: -> l,data {
                  mstruct._make_rom_instance(Assembler.current.current_section, l, data[c])
                }})
        @size += mstruct.size
      end
    end

    def size
      @size
    end

    def _make_rom_instance section, name, data = []
      section.tag_label name
      current_offset = section.current_offset
      @offsets.zip(data).each do |field, item|
        fieldname = (name.to_s+"."+field[:name].to_s).to_sym
        if field.key? :array
          section.tag_label fieldname
          
        elsif field.key? :struct
          field[:struct]._make_rom_instance section, fieldname, item
        else
          section.tag_label fieldname
          
        end
      end
    end

    def _make_ram_instance section, name
      section.tag_label name
      current_offset = section.current_offset
      offsets.each do |offset|
        if offset.key? :struct
          section.seek(current_offset+offset[:offset])
          offset[:struct]._make_ram_instance section, (name.to_s+"."+offset[:label].to_s).to_sym
        else
          section.seek (current_offset+offset[:offset])
          section.tag_label (name.to_s + "." + offset[:label].to_s).to_sym
          offset[:size].each do
            section.stream_in nil
          end
        end
      end
    end
  end

end