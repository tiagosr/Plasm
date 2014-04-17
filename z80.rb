#
# Ruby Macro Assembler
# (c)2011 Tiago Rezende
#
# Z80 assembler
#
require "./assembler"

module Assembler
class Z80 < Assembler
  B = Register.new(:B)
  C = Register.new(:C)
  D = Register.new(:D)
  E = Register.new(:E)
  H = Register.new(:H)
  L = Register.new(:L)
  A = Register.new(:A)
  BC = Register.new(:BC)
  DE = Register.new(:DE)
  HL = Register.new(:HL)
  SP = Register.new(:SP)
  IX = Register.new(:IX)
  IY = Register.new(:IY)
  IXH = IndexableRegister.new(:IXH)
  IXL = IndexableRegister.new(:IXL)
  IYH = IndexableRegister.new(:IYH)
  IYL = IndexableRegister.new(:IYL)
  R = Register.new(:R)
  I = Register.new(:I)
  IM = Register.new(:IM)
  REGS8 = [B, C, D, E, H, L, A]
  REGS16 = [BC, DE, HL, SP]
  REGSX8 = [IXH, IXL, IYH, IYL]
  REGSX16 = [IX, IY]
  SREGS = [R, I, IM]
  COND_CODES = [:NZ, :Z, :NC, :C, :PO, :PE, :P, :M]
  
  def a; A end
  def b; B end


  
  
  def self.__ld_rr   (offset)
    -> asm, dest, src {asm.db offset}
  end
  
  def self.__ld_rm   (offset)
    -> asm, dest, src {
      addr = src.if_within(0, 65536)
      db offset, addr.lo_byte, addr.hi_byte
    }
  end

  def self.__ld_mr   (offset)
    -> asm, dest, src {
      addr = dest[0].if_within(0, 65536)
      asm.db offset, addr.lo_byte, addr.hi_byte
    }
  end
  
  def self.__ld_rn   (offset)
    -> asm, dest, src { asm.db offset, src.b }
  end
  
  def self.__ld_rixr (offset)
    -> asm, dest, src { asm.db 0xdd, offset }
  end
  
  def self.__ld_riyr (offset)
    -> asm, dest, src { asm.db 0xfd, offset }
  end
  
  def self.__ld_rixm (offset)
    -> asm, dest, src { asm.db 0xdd, offset, src.distance_to(here) }
  end
  
  def self.__ld_riym (offset)
    -> asm, dest, src { asm.db 0xfd, offset, src.distance_to(here) }
  end
  
  def self.__ld_rixmm(offset)
    -> asm, dest, src { asm.db 0xdd, offset, src.lo_byte, src.hi_byte }
  end
  
  def self.__ld_riymm(offset)
    -> asm, dest, src { asm.db 0xfd, offset, src.lo_byte, src.hi_byte }
  end
  
  def self.__ld_mmrix(offset)
    -> asm, dest, src { asm.db 0xdd, offset, dest.lo_byte, dest.hi_byte }
  end
  
  def self.__ld_mmriy(offset)
    -> asm, dest, src { asm.db 0xfd, offset, dest.lo_byte, dest.hi_byte }
  end
  
  def self.__ld_rixn(offset)
    -> asm, dest, src { asm.db 0xdd, offset, src.if_within(-128, 127).b }
  end
  
  def self.__ld_riyn(offset)
    -> asm, dest, src { asm.db 0xfd, offset, src.if_within(-128, 127).b }
  end
  
  def find_arg_type arg
    arg = ADDR if arg.respond_to? :to_i
    arg = [self.find_arg_type(arg[0])] if ((arg.is_a? Array) && (arg.length == 1))
    arg
  end

  def ld dest, src
    tdest = find_arg_type dest
    tsrc = find_arg_type src
    LD_TABLE_DEST_SRC[[tdest, tsrc]].call(self, dest, src)
  end


  ADDR = :address
  LD_TABLE_DEST_SRC = {
    [A, [BC]   ] => __ld_rr(0x0a),
    [A, [DE]   ] => __ld_rr(0x1a),
    [A, B      ] => __ld_rr(0x78),
    [A, C      ] => __ld_rr(0x79),
    [A, D      ] => __ld_rr(0x7a),
    [A, E      ] => __ld_rr(0x7b),
    [A, H      ] => __ld_rr(0x7c),
    [A, L      ] => __ld_rr(0x7d),
    [A, [HL]   ] => __ld_rr(0x7e),
    [A, IXH    ] => __ld_rixr(0x7c),
    [A, IXL    ] => __ld_rixr(0x7d),
    [A, [IX]   ] => __ld_rixm(0x7e),
    [A, IYH    ] => __ld_riyr(0x7c),
    [A, IYL    ] => __ld_riyr(0x7d),
    [A, [IY]   ] => __ld_riym(0x7e),
    [A, A      ] => __ld_rr(0x7f),
    [A, I      ] => -> asm,dest,src { asm.db 0xed, 0x57},
    [A, [ADDR] ] => __ld_rm(0x3a),
    [A, R      ] => -> asm,dest,src { asm.db 0xed, 0x5f},
    [A, ADDR   ] => __ld_rn(0x3e),
    [B, B      ] => __ld_rr(0x40),
    [B, C      ] => __ld_rr(0x41),
    [B, D      ] => __ld_rr(0x42),
    [B, E      ] => __ld_rr(0x43),
    [B, H      ] => __ld_rr(0x44),
    [B, L      ] => __ld_rr(0x45),
    [B, [HL]   ] => __ld_rr(0x46),
    [B, IXH    ] => __ld_rixr(0x44),
    [B, IXL    ] => __ld_rixr(0x45),
    [B, [IX]   ] => __ld_rixm(0x46),
    [B, IYH    ] => __ld_riyr(0x44),
    [B, IYL    ] => __ld_riyr(0x45),
    [B, [IY]   ] => __ld_riym(0x46),
    [B, A      ] => __ld_rr(0x47),
    [B, ADDR   ] => __ld_rn(0x6),
    [C, B      ] => __ld_rr(0x48),
    [C, C      ] => __ld_rr(0x49),
    [C, D      ] => __ld_rr(0x4a),
    [C, E      ] => __ld_rr(0x4b),
    [C, H      ] => __ld_rr(0x4c),
    [C, L      ] => __ld_rr(0x4d),
    [C, [HL]   ] => __ld_rr(0x4e),
    [C, IXH    ] => __ld_rixr(0x4c),
    [C, IXL    ] => __ld_rixr(0x4d),
    [C, [IX]   ] => __ld_rixm(0x4e),
    [C, IYH    ] => __ld_riyr(0x4c),
    [C, IYL    ] => __ld_riyr(0x4d),
    [C, [IY]   ] => __ld_riym(0x4e),
    [C, A      ] => __ld_rr(0x4f),
    [C, ADDR   ] => __ld_rn(0xe),
    [D, B      ] => __ld_rr(0x50),
    [D, C      ] => __ld_rr(0x51),
    [D, D      ] => __ld_rr(0x52),
    [D, E      ] => __ld_rr(0x53),
    [D, H      ] => __ld_rr(0x54),
    [D, L      ] => __ld_rr(0x55),
    [D, [HL]   ] => __ld_rr(0x56),
    [D, IXH    ] => __ld_rixr(0x54),
    [D, IXL    ] => __ld_rixr(0x55),
    [D, [IX]   ] => __ld_rixm(0x56),
    [D, IYH    ] => __ld_riyr(0x54),
    [D, IYL    ] => __ld_riyr(0x55),
    [D, [IY]   ] => __ld_riym(0x56),
    [D, A      ] => __ld_rr(0x57),
    [D, ADDR   ] => __ld_rn(0x16),
    [E, B      ] => __ld_rr(0x58),
    [E, C      ] => __ld_rr(0x59),
    [E, D      ] => __ld_rr(0x5a),
    [E, E      ] => __ld_rr(0x5b),
    [E, H      ] => __ld_rr(0x5c),
    [E, L      ] => __ld_rr(0x5d),
    [E, [HL]   ] => __ld_rr(0x5e),
    [E, IXH    ] => __ld_rixr(0x5c),
    [E, IXL    ] => __ld_rixr(0x5d),
    [E, [IX]   ] => __ld_rixm(0x5e),
    [E, IYH    ] => __ld_riyr(0x5c),
    [E, IYL    ] => __ld_riyr(0x5d),
    [E, [IY]   ] => __ld_riym(0x5e),
    [E, A      ] => __ld_rr(0x5f),
    [E, ADDR   ] => __ld_rn(0x1e),
    [H, B      ] => __ld_rr(0x60),
    [H, C      ] => __ld_rr(0x61),
    [H, D      ] => __ld_rr(0x62),
    [H, E      ] => __ld_rr(0x63),
    [H, H      ] => __ld_rr(0x64),
    [H, L      ] => __ld_rr(0x65),
    [H, [HL]   ] => __ld_rr(0x66),
    [H, A      ] => __ld_rr(0x67),
    [H, ADDR   ] => __ld_rn(0x26),
    [L, B      ] => __ld_rr(0x68),
    [L, C      ] => __ld_rr(0x69),
    [L, D      ] => __ld_rr(0x6a),
    [L, E      ] => __ld_rr(0x6b),
    [L, H      ] => __ld_rr(0x6c),
    [L, L      ] => __ld_rr(0x6d),
    [L, [HL]   ] => __ld_rr(0x6e),
    [L, A      ] => __ld_rr(0x6f),
    [L, ADDR   ] => __ld_rn(0x2e),
    [[BC], A   ] => __ld_rr(0x02),
    [[DE], A   ] => __ld_rr(0x12),
    [[HL], B   ] => __ld_rr(0x70),
    [[HL], C   ] => __ld_rr(0x71),
    [[HL], D   ] => __ld_rr(0x72),
    [[HL], E   ] => __ld_rr(0x73),
    [[HL], H   ] => __ld_rr(0x74),
    [[HL], L   ] => __ld_rr(0x75),
    #[[HL], [HL]] => __ld_rr(0x76), # HALT
    [[HL], A   ] => __ld_rr(0x77),
    [[HL], ADDR] => __ld_rn(0x36),
    [[IX], B   ] => __ld_rixr(0x70),
    [[IX], C   ] => __ld_rixr(0x71),
    [[IX], D   ] => __ld_rixr(0x72),
    [[IX], E   ] => __ld_rixr(0x73),
    [[IX], H   ] => __ld_rixr(0x74),
    [[IX], L   ] => __ld_rixr(0x75),
    [[IX], A   ] => __ld_rixr(0x77),
    [[IX], ADDR] => __ld_rixn(0x36),
    [[IY], B   ] => __ld_riyr(0x70),
    [[IY], C   ] => __ld_riyr(0x71),
    [[IY], D   ] => __ld_riyr(0x72),
    [[IY], E   ] => __ld_riyr(0x73),
    [[IY], H   ] => __ld_riyr(0x74),
    [[IY], L   ] => __ld_riyr(0x75),
    [[IY], A   ] => __ld_riyr(0x77),
    [[IY], ADDR] => __ld_riyn(0x36),
    [[ADDR], A ] => __ld_mr(0x32),
    [[ADDR], HL] => __ld_mr(0x22),
    [[ADDR], IX] => __ld_rixmm(0x22),
    [[ADDR], IY] => __ld_riymm(0x22),
    [BC, ADDR  ] => -> asm, dest, src { asm.db 0x01, src.lo_byte, src.hi_byte },
    [DE, ADDR  ] => -> asm, dest, src { asm.db 0x11, src.lo_byte, src.hi_byte },
    [HL, ADDR  ] => -> asm, dest, src { asm.db 0x21, src.lo_byte, src.hi_byte },
    [HL, [ADDR]] => -> asm, dest, src { asm.db 0x23, src.lo_byte, src.hi_byte },
    [SP, ADDR  ] => -> asm, dest, src { asm.db 0x31, src.lo_byte, src.hi_byte },
    [SP, [ADDR]] => 0,
    [SP, HL    ] => __ld_rr(0xf9),
    [SP, IX    ] => __ld_rixr(0xf9),
    [SP, IY    ] => __ld_riyr(0xf9),
    [I, A      ] => -> asm, dest, src { asm.db 0xed, 0x57 },
    [IX, ADDR  ] => -> asm, dest, src { asm.db 0xdd, 0x21, src.lo_byte, src.hi_byte },
    [IX, [ADDR]] => 0,
    [IY, ADDR  ] => -> asm, dest, src { asm.db 0xfd, 0x21, src.lo_byte, src.hi_byte },
    [IY, [ADDR]] => 0,
    [R, A      ] => -> asm, dest, src { asm.db 0xed, 0x5f }
  }


  
  def self.single_byte **hash
    hash.each do |insn, byte|
      define_method (insn.to_s) do
        # no need to envelope in __op
        db byte
      end
    end
  end

  def self.double_byte **hash
    hash.each do |insn, bytes|
      define_method (insn.to_s) do
        # no need to envelope in __op
        db *bytes
      end
    end
  end

  ############ Single-byte no-argument opcodes ############
  single_byte nop:  0,
    rlca: 7,
    rrca: 0xf,
    rla:  0x17,
    rra:  0x1f,
    daa:  0x27,
    cpl:  0x2f,
    scf:  0x37,
    ccf:  0x3f,
    halt: 0x76,
    ret:  0xc9,
    exx:  0xd9,
    di:   0xf3,
    ei:   0xfb
  
  ############ Double-byte no-argument opcodes ############
  double_byte neg:   [0xed, 0x04],
    retn:  [0xed, 0x05],
    reti:  [0xed, 0x0d],
    rrd:   [0xed, 0x67],
    rld:   [0xed, 0x6f],
    ldi:   [0xed, 0xa0],
    cpi:   [0xed, 0xa1],
    ini:   [0xed, 0xa2],
    outi:  [0xed, 0xa3],
    ldd:   [0xed, 0xa8],
    cpd:   [0xed, 0xa9],
    ind:   [0xed, 0xaa],
    outd:  [0xed, 0xab],
    ldir:  [0xed, 0xb0],
    cpir:  [0xed, 0xb1],
    inir:  [0xed, 0xb2],
    outir: [0xed, 0xb3],
    lddr:  [0xed, 0xb8],
    cpdr:  [0xed, 0xb9],
    indr:  [0xed, 0xba],
    outdr: [0xed, 0xbb]
  
  ############ Single-byte immediate opcodes ##############
  def rst (addr)
    if [0,8,0x10,0x18,0x20,0x28,0x30,0x38].include? addr.to_i
      db 0xc7+addr.to_i
    else
      raise "invalid reset address"
    end
  end
  
  IM_KEY = {0=>0x6,1=>0x16,2=>0x1e}
  def im (mode)
    if IM_KEY.key? mode
      db 0xed, IM_KEY[mode]
    else
      raise "invalid mode"
    end
  end

  ############ Logic/Arithmetic opcodes ###################
  IMM = :immediate
  def find_logic_arg_type arg
    arg = IMM if arg.respond_to? :to_i
    arg = [self.find_arg_type(arg[0])] if ((arg.is_a? Array) && (arg.length == 1))
    arg
  end

  def self.logic_arith_ops table
    table.each do |op, optypes|
      define_method (op.to_s) do |dest, src=nil|
        dest, src = A, dest if src.nil?
        optype = [find_logic_arg_type(dest), find_logic_arg_type(src)]
        if optypes.key? optype
          optypes[optype].call self, dest, src
        else
          raise "invalid arguments for opcode #{op.to_s}"
        end
      end
    end
  end

  def self._lop code
    ->asm, dest, src { asm.db code }
  end

  def self._lopi code
    ->asm, dest, src { asm.db code, src.if_within(-256, 255).b }
  end

  def self._cb code
    ->asm, dest, src { asm.db 0xcb, code }
  end

  logic_arith_ops {
    add:  { [A, B]=>_lop(0x80), [A, C]=>_lop(0x81), [A, D]=>_lop(0x82),    [A, E]=>_lop(0x83),
            [A, H]=>_lop(0x84), [A, L]=>_lop(0x85), [A, [HL]]=>_lop(0x86), [A, A]=>_lop(0x87), [A, IMM]=>_lopi(0xc6), [HL, BC]=>0x09, [HL, DE]=>0x19, [HL, HL]=>0x29, [HL, SP]=>0x39 },
    adc:  { [A, B]=>_lop(0x88), [A, C]=>_lop(0x89), [A, D]=>_lop(0x8A),    [A, E]=>_lop(0x8B),
            [A, H]=>_lop(0x8C), [A, L]=>_lop(0x8D), [A, [HL]]=>_lop(0x8E), [A, A]=>_lop(0x8F), [A, IMM]=>_lopi(0xce) },
    sub:  { [A, B]=>_lop(0x90), [A, C]=>_lop(0x91), [A, D]=>_lop(0x92),    [A, E]=>_lop(0x93),
            [A, H]=>_lop(0x94), [A, L]=>_lop(0x95), [A, [HL]]=>_lop(0x96), [A, A]=>_lop(0x97), [A, IMM]=>_lopi(0xd6) },
    sbc:  { [A, B]=>_lop(0x98), [A, C]=>_lop(0x99), [A, D]=>_lop(0x9A),    [A, E]=>_lop(0x9B),
            [A, H]=>_lop(0x9C), [A, L]=>_lop(0x9D), [A, [HL]]=>_lop(0x9E), [A, A]=>_lop(0x9F), [A, IMM]=>_lopi(0xde) },
    _and: { [A, B]=>_lop(0xa0), [A, C]=>_lop(0xa1), [A, D]=>_lop(0xa2),    [A, E]=>_lop(0xa3),
            [A, H]=>_lop(0xa4), [A, L]=>_lop(0xa5), [A, [HL]]=>_lop(0xa6), [A, A]=>_lop(0xa7), [A, IMM]=>_lopi(0xe6) },
    _xor: { [A, B]=>_lop(0xa8), [A, C]=>_lop(0xa9), [A, D]=>_lop(0xaa),    [A, E]=>_lop(0xab),
            [A, H]=>_lop(0xac), [A, L]=>_lop(0xad), [A, [HL]]=>_lop(0xae), [A, A]=>_lop(0xaf), [A, IMM]=>_lopi(0xee) },
    _or:  { [A, B]=>_lop(0xb0), [A, C]=>_lop(0xb1), [A, D]=>_lop(0xb2),    [A, E]=>_lop(0xb3),
            [A, H]=>_lop(0xb4), [A, L]=>_lop(0xb5), [A, [HL]]=>_lop(0xb6), [A, A]=>_lop(0xb7), [A, IMM]=>_lopi(0xf6) },
    cp:   { [A, B]=>_lop(0xb8), [A, C]=>_lop(0xb9), [A, D]=>_lop(0xba),    [A, E]=>_lop(0xbb),
            [A, H]=>_lop(0xbc), [A, L]=>_lop(0xbd), [A, [HL]]=>_lop(0xbe), [A, A]=>_lop(0xbf), [A, IMM]=>_lopi(0xfe) },
  }
  


  ############ Double-byte immediate opcodes ##############
  def djnz (disp)
    db 0x10, disp.if_within(-128, 127).b
  end
  
  def jr (cc, *vars)
    if COND_CODES.include? cc
      dest = vars[1]
      if dest.is_a?(Symbol)
        addr = dest.relative_to(here).if_within(-128, 127).b
        case cc
        when :NZ then db 0xc3, addr
        when :Z then  db 0xca, addr
        when :NC then db 0xd3, addr
        when :C then  db 0xda, addr
        when :PO then db 0xe3, addr
        when :PE then db 0xea, addr
        when :P then  db 0xf3, addr
        when :M then  db 0xfa, addr
        end
      else
        addr = dest.if_within(0, 65535)
        case cc
        when :NZ then db 0xc3, addr.lo_byte, addr.hi_byte
        when :Z then  db 0xca, addr.lo_byte, addr.hi_byte
        when :NC then db 0xd3, addr.lo_byte, addr.hi_byte
        when :C then  db 0xda, addr.lo_byte, addr.hi_byte
        when :PO then db 0xe3, addr.lo_byte, addr.hi_byte
        when :PE then db 0xea, addr.lo_byte, addr.hi_byte
        when :P then  db 0xf3, addr.lo_byte, addr.hi_byte
        when :M then  db 0xfa, addr.lo_byte, addr.hi_byte
        end
      end
    else
      dest = cc
      if dest.is_a? Fixnum
        db 0x18, dest.if_within(-128,127).b
      else
        db 0x18, dest.relative_to(here).if_within(-128,127).b
      end
    end
  end
  
  def jp (*vars)
    if COND_CODES.include?(vars[0])
      disp = vars[1]
    else
      disp = vars[0]
      if disp.is_a?(Register)
        case disp
        when HL then db 0xe9
        when IX then db 0xdd, 0xe9
        when IY then db 0xfd, 0xe9
        end
      elsif disp.respond_to? :to_i
        db 0xc3, disp.lo_byte, disp.hi_byte
      else
        raise "invalid argument for relative jump"
      end
    end
  end
    
  ############ Single-byte conditional opcodes ############
  def ret (cc = :none)
    case cc
    when :none then db 0xc9
    when :NZ  then db 0xc0
    when :Z   then db 0xc8
    when :NC  then db 0xd0
    when :C   then db 0xd8
    when :PO  then db 0xe0
    when :PE  then db 0xe8
    when :P   then db 0xf0
    when :M   then db 0xf8
    else raise "invalid conditional code"
    end
  end
  
  
  ############ single-byte register parameter opcodes ###########
  def ex (r1, r2)
    args = [r1, r2]
    case args
    when [AF, AF] then db 0x08
    when [[SP], HL] then db 0xe3
    when [DE, HL] then db 0xeb
    else raise "invalid parameters"
    end
  end
  
  def push (reg)
    case reg
    when BC  then db 0xc5
    when DE  then db 0xd5
    when HL  then db 0xe5
    when AF  then db 0xf5
    when IX  then db 0xdd, 0xe5
    when IY  then db 0xfd, 0xe5
    else raise "invalid register pair"
    end
  end
  
  def pop (reg)
    case reg
    when BC  then db 0xc1
    when DE  then db 0xd1
    when HL  then db 0xe1
    when AF  then db 0xf1
    when IX  then db 0xdd, 0xe1
    when IY  then db 0xfd, 0xe1
    else raise "invalid register pair"
    end
  end
  
  def inc (reg)
    case reg
    when BC   then db 0x03
    when DE   then db 0x13
    when HL   then db 0x23
    when SP   then db 0x33
    when B    then db 0x04
    when C    then db 0x0c
    when D    then db 0x14
    when E    then db 0x1c
    when H    then db 0x24
    when L    then db 0x2c
    when [HL] then db 0x34
    when A    then db 0x3c
    when IX   then db 0xdd, 0x23
    when IXH  then db 0xdd, 0x24 # undocumented
    when IXL  then db 0xdd, 0x2d # undocumented
    when IY   then db 0xed, 0x23
    when IYH  then db 0xed, 0x24 # undocumented
    when IYL  then db 0xed, 0x2d # undocumented
    else raise "invalid register"
    end
  end
  
  def dec (reg)
    case reg
    when B    then db 0x05
    when BC   then db 0x0b
    when C    then db 0x0d
    when D    then db 0x15
    when DE   then db 0x1b
    when E    then db 0x1d
    when H    then db 0x25
    when HL   then db 0x2b
    when L    then db 0x2d
    when [HL] then db 0x35
    when SP   then db 0x3b
    when A    then db 0x3d
    when IX   then db 0xdd, 0x2b
    when IXH  then db 0xdd, 0x25 # undocumented
    when IXL  then db 0xdd, 0x2e # undocumented
    when IY   then db 0xed, 0x2b
    when IYH  then db 0xed, 0x25 # undocumented
    when IYL  then db 0xed, 0x2e # undocumented
    else raise "invalid register"
    end
  end
  
  def __bitop(offset,reg1)
    if IX===reg1
      db 0xdd, 0xcb, reg1.offset, 0x06+offset
    elsif IY===reg1
      db 0xdd, 0xcb, reg1.offset, 0x06+offset
    elsif REG8_DISPS.include? reg1
      db 0xcb, REG8_DISPS[reg1]+offset
    else
      raise "invalid register operand"
    end
  end
  
  def rlc (reg); __bitop(0,reg) end
  def rrc (reg); __bitop(8,reg) end
  def rl  (reg); __bitop(0x10, reg) end
  def rr  (reg); __bitop(0x18, reg) end
  def sla (reg); __bitop(0x20, reg) end
  def sra (reg); __bitop(0x28, reg) end
  def sll (reg); __bitop(0x30, reg) end # undocumented
  def srl (reg); __bitop(0x38, reg) end
  
  def bit (idx,reg); __bitop(0x40+(8*idx),reg) end
  def res (idx,reg); __bitop(0x80+(8*idx),reg) end
  def set (idx,reg); __bitop(0xc0+(8*idx),reg) end
  
  def _in (reg,idx=nil)
    if A===reg
      if (idx.is_a? Array)&&(idx.length==1)
        if (idx[0].is_a? Fixnum)&&((0..255)<=>idx[0])
          db 0xdb, idx[0]
        else
          raise "invalid port number"
        end
      end
    elsif [B, C, D, E, H, L].include? reg
      if (idx.is_a? Array)&&(idx.length==1)
        if (idx[0].is_a? Fixnum)&&((0..255)<=>idx[0])
          db 0xed, 0x40+(REG8_DISPS[reg]<<2), idx[0]
        else
          raise "invalid port number"
        end
      end
    elsif (reg.is_a? Array)&&(reg.length==1)
      if (reg[0].is_a? Fixnum)&&((0..255)<=>reg[0])
        db 0xed, 0x70, reg[0]
      else
        raise "invalid port number"
      end
    else
      raise "invalid destination register"
    end
  end
  
  def out (idx, reg)
    if (idx.is_a? Array)&&(idx.length==1)
      idx = idx[0]
    end
    if A===reg
      if (idx.is_a? Fixnum)&&((0..255)<=>idx)
        db 0xd3, idx
      else
        raise "invalid port number"
      end
    elsif [B, C, D, E, H, L].include? reg
      if (idx.is_a? Fixnum)&&((0..255)<=>idx)
        db 0xed, 0x41+(REG8_DISPS[reg]<<2), idx
      else
        raise "invalid port number"
      end
    else
      raise "invalid arguments for opcode"
    end
  end

end

end

