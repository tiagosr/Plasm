module Assembler
  ######################## Section definition
  class Section
    def initialize (name, opts = {})
      @name = name
      @size = opts[:size] || -1
      @align = opts[:align] || 1
      @origin = opts[:origin] || 0
      @can_org = opts[:can_org] || false
      @type = opts[:type] || :free
      if (opts[:return_org] || false)
        @return_org = here
      else
        @return_org = nil
      end

      @labels = {}
      @data = []
      @org_offset = 0
      @cursor = 0
    end

    # add a byte to the section stream
    def stream_in byte
      if @cursor > @data.size
        (@cursor - @data.size).times {|c| @data.push nil}
      end
      @data[@cursor] = byte
      @cursor += 1
    end

    # builds the assembled byte stream
    def stream_out
      stream = []
      size = @size
      if @size == -1
        size = @data.length
      end
      if @data.length <= size
        stream = Array.new(@data)
        (size - @data.length).times do |i|
          stream.push nil
        end
      else
        raise "section #{@name} exceeds in #{@data.length - size} bytes it's predefined size"
      end
      stream
    end

    def instantiate_struct (label, struct, args)
      Assembler.current.structs[struct]._make_rom_instance self, label, args
    end

    # tags a certain position with a label
    def tag_label(name)
      if @labels.key? name
        raise "repeated label #{name} within section #{@name}"
      else
        @labels[name] = @origin + @cursor + @org_offset
      end
    end

    # reads a byte from the stream 
    def read_at (pos)
      @data[pos]
    end

    def here
      @origin + @cursor + @org_offset
    end

    def org address
      if @can_org
        @cursor = address - @origin
        @org_offset = 0
      else
        raise "cannot set origin inside section #{@name}"
      end
    end

    def orga address
      @org_offset = address - (@origin + @cursor)
    end
  end

  # non-writeable label-only section
  class RamSection < Section
    def initialize (name, parent, opts = {})
      @name = name
      @parent = parent
      @size = opts[:size] || -1
      @align = opts[:align] || 1
      @labels = {}
      @cursor = 0
    end
    def stream_in byte
      if byte != nil
        raise "RAM section #{@name} is not a writeable section"
      else
        @cursor += 1
      end
    end

    def stream_out
      nil
    end

    def tag_label name
      if @labels.key? name
        raise "repeated label #{name} within section #{@name}"
      else
        @labels[name] = @cursor
      end
    end

    def read_at (pos)
      nil
    end

    def instantiate_struct (label, struct, args)
      if args != nil
        Assembler.current.structs[struct]._make_ram_instance self, label
      else
        raise "cannot instantiate a struct in RAM section #{@name} with data"
      end
    end
  end
end