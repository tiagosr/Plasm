module Assembler
  
  class Register
    def initialize (name)
      @name = name
    end
    def ==(other)
      @name == other.name
    end
    def ===(other)
      @name == other.name
    end
  end
  
  class IndexableRegister < Register
    def initialize (name, offset = 0)
  	  @name = name
  	  @offset = offset
    end
    def +(offset)
      self.class.new @name, offset
    end
    def -(offset)
      self.class.new @name, offset
    end
    def offset
      @offset
    end
  end

end