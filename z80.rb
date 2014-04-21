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

  INSTRUCTION_TABLE = [
    ["ADC A,B",      0x88, :none],
    ["ADC A,C",      0x89, :none],
    ["ADC A,D",      0x8A, :none],
    ["ADC A,E",      0x8B, :none],
    ["ADC A,H",      0x8C, :none],
    ["ADC A,L",      0x8D, :none],
    ["ADC A,(HL)",   0x8E, :none],
    ["ADC A,A",      0x8F, :none],
    ["ADC A,IXH",    0x8C, :dd  ],
    ["ADC A,IXL",    0x8D, :dd  ],
    ["ADC A,(IXs)",  0x8E, :dds ],
    ["ADC A,IYH",    0x8C, :fd  ],
    ["ADC A,IYL",    0x8D, :fd  ],
    ["ADC A,(IYs)",  0x8E, :fds ],
    ["ADC A,n",      0xCE, :imm ],
    ["ADC HL,BC",    0x4A, :ed  ],
    ["ADC HL,DE",    0x5A, :ed  ],
    ["ADC HL,HL",    0x6A, :ed  ],
    ["ADC HL,SP",    0x7A, :ed  ],

    ["ADD A,B",      0x80, :none],
    ["ADD A,C",      0x81, :none],
    ["ADD A,D",      0x82, :none],
    ["ADD A,E",      0x83, :none],
    ["ADD A,H",      0x84, :none],
    ["ADD A,L",      0x85, :none],
    ["ADD A,(HL)",   0x86, :none],
    ["ADD A,A",      0x87, :none],
    ["ADD A,IXH",    0x84, :dd  ],
    ["ADD A,IXL",    0x85, :dd  ],
    ["ADD A,(IXs)",  0x86, :dds ],
    ["ADD A,IYH",    0x84, :fd  ],
    ["ADD A,IYL",    0x85, :fd  ],
    ["ADD A,(IYs)",  0x86, :fds ],
    ["ADD A,n",      0xC6, :imm ],
    ["ADD HL,BC",    0x09, :none],
    ["ADD HL,DE",    0x19, :none],
    ["ADD HL,HL",    0x29, :none],
    ["ADD HL,SP",    0x39, :none],
    ["ADD IX,BC",    0x09, :dd  ],
    ["ADD IX,DE",    0x19, :dd  ],
    ["ADD IX,HL",    0x29, :dd  ],
    ["ADD IX,SP",    0x39, :dd  ],
    ["ADD IY,BC",    0x09, :fd  ],
    ["ADD IY,DE",    0x19, :fd  ],
    ["ADD IY,HL",    0x29, :fd  ],
    ["ADD IY,SP",    0x39, :fd  ],

    ["SBC A,B",      0x98, :none],
    ["SBC A,C",      0x99, :none],
    ["SBC A,D",      0x9A, :none],
    ["SBC A,E",      0x9B, :none],
    ["SBC A,H",      0x9C, :none],
    ["SBC A,L",      0x9D, :none],
    ["SBC A,(HL)",   0x9E, :none],
    ["SBC A,A",      0x9F, :none],
    ["SBC A,IXH",    0x9C, :dd  ],
    ["SBC A,IXL",    0x9D, :dd  ],
    ["SBC A,(IXs)",  0x9E, :dds ],
    ["SBC A,IYH",    0x9C, :fd  ],
    ["SBC A,IYL",    0x9D, :fd  ],
    ["SBC A,(IYs)",  0x9E, :fds ],
    ["SBC A,n",      0xDE, :imm ],
    ["SBC HL,BC",    0x42, :ed  ],
    ["SBC HL,DE",    0x52, :ed  ],
    ["SBC HL,HL",    0x62, :ed  ],
    ["SBC HL,SP",    0x72, :ed  ],

    ["SUB B",        0x90, :none],
    ["SUB C",        0x91, :none],
    ["SUB D",        0x92, :none],
    ["SUB E",        0x93, :none],
    ["SUB H",        0x94, :none],
    ["SUB L",        0x95, :none],
    ["SUB (HL)",     0x96, :none],
    ["SUB A",        0x97, :none],
    ["SUB A,IXH",    0x94, :dd  ],
    ["SUB A,IXL",    0x95, :dd  ],
    ["SUB (IXs)",    0x96, :dds ],
    ["SUB IYH",      0x94, :fd  ],
    ["SUB IYL",      0x95, :fd  ],
    ["SUB (IYs)",    0x96, :fds ],
    ["SUB n",        0xD6, :imm ],

    ["AND B",        0xA0, :none],
    ["AND C",        0xA1, :none],
    ["AND D",        0xA2, :none],
    ["AND E",        0xA3, :none],
    ["AND H",        0xA4, :none],
    ["AND L",        0xA5, :none],
    ["AND (HL)",     0xA6, :none],
    ["AND A",        0xA7, :none],
    ["AND IXH",      0xA4, :dd  ],
    ["AND IXL",      0xA5, :dd  ],
    ["AND (IXs)",    0xA6, :dd  ],
    ["AND IYH",      0xA4, :fd  ],
    ["AND IYL",      0xA5, :fd  ],
    ["AND (IYs)",    0xA6, :fd  ],
    ["AND n",        0xE6, :imm ],

    ["OR B",         0xB0, :none],
    ["OR C",         0xB1, :none],
    ["OR D",         0xB2, :none],
    ["OR E",         0xB3, :none],
    ["OR H",         0xB4, :none],
    ["OR L",         0xB5, :none],
    ["OR (HL)",      0xB6, :none],
    ["OR A",         0xB7, :none],
    ["OR IXH",       0xB4, :dd  ],
    ["OR IXL",       0xB5, :dd  ],
    ["OR (IXs)",     0xB6, :dd  ],
    ["OR IYH",       0xB4, :fd  ],
    ["OR IYL",       0xB5, :fd  ],
    ["OR (IYs)",     0xB6, :fd  ],
    ["OR n",         0xF6, :imm ],
    
    ["XOR A,B",      0xA8, :none],
    ["XOR A,C",      0xA9, :none],
    ["XOR A,D",      0xAA, :none],
    ["XOR A,E",      0xAB, :none],
    ["XOR A,H",      0xAC, :none],
    ["XOR A,L",      0xAD, :none],
    ["XOR A,(HL)",   0xAE, :none],
    ["XOR A,A",      0xAF, :none],
    ["XOR A,IXH",    0xAC, :dd  ],
    ["XOR A,IXL",    0xAD, :dd  ],
    ["XOR A,(IXs)",  0xAE, :dds ],
    ["XOR A,IYH",    0xAC, :fd  ],
    ["XOR A,IYL",    0xAD, :fd  ],
    ["XOR A,(IYs)",  0xAE, :fds ],
    ["XOR A,n",      0xEE, :imm ],

    ["BIT *,B",      0x40, :bit ],
    ["BIT *,C",      0x41, :bit ],
    ["BIT *,D",      0x42, :bit ],
    ["BIT *,E",      0x43, :bit ],
    ["BIT *,H",      0x44, :bit ],
    ["BIT *,L",      0x45, :bit ],
    ["BIT *,(HL)",   0x46, :bit ],
    ["BIT *,A",      0x47, :bit ],
    ["BIT *,(IXs)",  0x46, :ddbit],
    ["BIT *,(IYs)",  0x46, :fdbit],
    
    ["CALL C,?",     0xDC, :addr],
    ["CALL M,?",     0xFC, :addr],
    ["CALL NC,?",    0xD4, :addr],
    ["CALL NZ,?",    0xC4, :addr],
    ["CALL P,?",     0xF4, :addr],
    ["CALL PE,?",    0xEC, :addr],
    ["CALL PO,?",    0xE4, :addr],
    ["CALL Z,?",     0xCC, :addr],
    ["CALL ?",       0xCD, :addr],

    ["CCF",          0x3F, :none],
    ["SCF",          0x37, :none],
    
    ["CPD",          0xA9, :ed  ],
    ["CPDR",         0xB9, :ed  ],
    ["CPI",          0xA1, :ed  ],
    ["CPIR",         0xB1, :ed  ],

    ["IND",          0xAA, :ed  ],
    ["INDR",         0xBA, :ed  ],
    ["INI",          0xA2, :ed  ],
    ["INIR",         0xB2, :ed  ],
    ["OTDR",         0xBB, :ed  ],
    ["OTIR",         0xB3, :ed  ],

    ["CP B",         0xB8, :none],
    ["CP C",         0xB9, :none],
    ["CP D",         0xBA, :none],
    ["CP E",         0xBB, :none],
    ["CP H",         0xBC, :none],
    ["CP L",         0xBD, :none],
    ["CP (HL)",      0xBE, :none],
    ["CP A",         0xBF, :none],
    ["CP IXH",       0xBC, :dd  ],
    ["CP IXL",       0xBD, :dd  ],
    ["CP (IXs)",     0xBE, :dds ],
    ["CP IYH",       0xBC, :fd  ],
    ["CP IYL",       0xBD, :fd  ],
    ["CP (HLs)",     0xBE, :fds ],

    ["DEC B",        0x05, :none],
    ["DEC C",        0x0D, :none],
    ["DEC D",        0x15, :none],
    ["DEC E",        0x1D, :none],
    ["DEC H",        0x25, :none],
    ["DEC L",        0x2D, :none],
    ["DEC (HL)",     0x35, :none],
    ["DEC A",        0x3D, :none],
    ["DEC BC",       0x0B, :none],
    ["DEC DE",       0x1B, :none],
    ["DEC HL",       0x2B, :none],
    ["DEC SP",       0x3B, :none],
    ["DEC IX",       0x2B, :dd  ],
    ["DEC IXH",      0x25, :dd  ],
    ["DEC IXL",      0x2D, :dd  ],
    ["DEC (IXs)",    0x35, :dds ],
    ["DEC IY",       0x2B, :fd  ],
    ["DEC IYH",      0x25, :fd  ],
    ["DEC IYL",      0x2D, :fd  ],
    ["DEC (IYs)",    0x35, :fds ],

    ["DI",           0xf3, :none],
    ["EI",           0xfb, :none],

    ["DJNZ @",       0x10, :rel ],

    ["EX AF,AF'",    0x08, :none],
    ["EX DE,HL",     0xEB, :none],
    ["EX (SP),HL",   0xE3, :none],
    ["EX (SP),IX",   0xE3, :dd  ],
    ["EX (SP),IY",   0xE3, :fd  ],
    ["EXX",          0xD9, :none],

    ["HALT",         0x76, :none],

    ["IM 0",         0x46, :ed  ],
    ["IM 1",         0x56, :ed  ],
    ["IM 2",         0x5E, :ed  ],

    ["IN A,(C)",     0x78, :ed  ],
    ["IN A,(n)",     0xDB, :imm ],
    ["IN A,n",       0xDB, :imm ],
    ["IN B,(C)",     0x40, :ed  ],
    ["IN (C)",       0x70, :ed  ],
    ["IN C,(C)",     0x48, :ed  ],
    ["IN D,(C)",     0x50, :ed  ],
    ["IN E,(C)",     0x58, :ed  ],
    ["IN H,(C)",     0x60, :ed  ],
    ["IN L,(C)",     0x68, :ed  ],

    ["OUT (C),B",    0x41, :ed  ],
    ["OUT (C),C",    0x49, :ed  ],
    ["OUT (C),D",    0x51, :ed  ],
    ["OUT (C),E",    0x59, :ed  ],
    ["OUT (C),H",    0x61, :ed  ],
    ["OUT (C),L",    0x69, :ed  ],
    ["OUT (C),0",    0x71, :ed  ],
    ["OUT (C),A",    0x79, :ed  ],
    ["OUT (n),A",    0xD3, :imm ],
    
    ["INC B",        0x04, :none],
    ["INC C",        0x0C, :none],
    ["INC D",        0x14, :none],
    ["INC E",        0x1C, :none],
    ["INC H",        0x24, :none],
    ["INC L",        0x2C, :none],
    ["INC (HL)",     0x34, :none],
    ["INC A",        0x3D, :none],
    ["INC BC",       0x03, :none],
    ["INC DE",       0x13, :none],
    ["INC HL",       0x23, :none],
    ["INC SP",       0x33, :none],
    ["INC IX",       0x23, :dd  ],
    ["INC IXH",      0x24, :dd  ],
    ["INC IXL",      0x2C, :dd  ],
    ["INC (IXs)",    0x34, :dds ],
    ["INC IY",       0x23, :fd  ],
    ["INC IYH",      0x24, :fd  ],
    ["INC IYL",      0x2C, :fd  ],
    ["INC (IYs)",    0x34, :fds ],

    ["JP (HL)",      0xE9, :none],
    ["JP (IX)",      0xE9, :dd  ],
    ["JP (IY)",      0xE9, :fd  ],
    ["JP C,?",       0xDA, :addr],
    ["JP M,?",       0xFA, :addr],
    ["JP NC,?",      0xD2, :addr],
    ["JP NZ,?",      0xC2, :addr],
    ["JP P,?",       0xF2, :addr],
    ["JP PE,?",      0xEA, :addr],
    ["JP PO,?",      0xE2, :addr],
    ["JP Z,?",       0xCA, :addr],
    ["JP ?",         0xC3, :addr],

    ["JR C,@",       0x38, :rel ],
    ["JR NC,@",      0x30, :rel ],
    ["JR NZ,@",      0x20, :rel ],
    ["JR Z,@",       0x28, :rel ],
    ["JR @",         0x18, :rel ],

    ["NEG",          0x44, :ed  ],
    ["NOP",          0x00, :none],

    ["POP BC",       0xc1, :none],
    ["POP DE",       0xd1, :none],
    ["POP HL",       0xe1, :none],
    ["POP AF",       0xf1, :none],
    ["POP IX",       0xe1, :dd  ],
    ["POP IY",       0xe1, :fd  ],

    ["PUSH BC",      0xc5, :none],
    ["PUSH DE",      0xd5, :none],
    ["PUSH HL",      0xe5, :none],
    ["PUSH AF",      0xf5, :none],
    ["PUSH IX",      0xe5, :dd  ],
    ["PUSH IY",      0xe5, :fd  ],

    ["RET NZ",       0xC0, :none],
    ["RET Z",        0xC8, :none],
    ["RET NC",       0xD0, :none],
    ["RET C",        0xD8, :none],
    ["RET PO",       0xE0, :none],
    ["RET PE",       0xE8, :none],
    ["RET P",        0xF0, :none],
    ["RET M",        0xF8, :none],

    ["RETI",         0x4D, :ed  ],
    ["RETN",         0x45, :ed  ],
    ["RET",          0xC9, :none],

    ["RLCA",         0x07, :none],
    ["RLA",          0x17, :none],
    
    ["RLC B",        0x00, :cb  ],
    ["RLC C",        0x01, :cb  ],
    ["RLC D",        0x02, :cb  ],
    ["RLC E",        0x03, :cb  ],
    ["RLC H",        0x04, :cb  ],
    ["RLC L",        0x05, :cb  ],
    ["RLC (HL)",     0x06, :cb  ],
    ["RLC A",        0x07, :cb  ],
    ["RLC (IXs)",    0x06, :ddscb],
    ["RLC (IYs)",    0x06, :fdscb],

    ["RL B",         0x10, :cb  ],
    ["RL C",         0x11, :cb  ],
    ["RL D",         0x12, :cb  ],
    ["RL E",         0x13, :cb  ],
    ["RL H",         0x14, :cb  ],
    ["RL L",         0x15, :cb  ],
    ["RL (HL)",      0x16, :cb  ],
    ["RL A",         0x17, :cb  ],
    ["RL (IXs)",     0x16, :ddscb],
    ["RL (IYs)",     0x16, :fdscb],
    
    ["RRC B",        0x08, :cb  ],
    ["RRC C",        0x09, :cb  ],
    ["RRC D",        0x0A, :cb  ],
    ["RRC E",        0x0B, :cb  ],
    ["RRC H",        0x0C, :cb  ],
    ["RRC L",        0x0D, :cb  ],
    ["RRC (HL)",     0x0E, :cb  ],
    ["RRC A",        0x0F, :cb  ],
    ["RRC (IXs)",    0x0E, :ddscb],
    ["RRC (IYs)",    0x0E, :fdscb],

    ["RR B",         0x18, :cb  ],
    ["RR C",         0x19, :cb  ],
    ["RR D",         0x1A, :cb  ],
    ["RR E",         0x1B, :cb  ],
    ["RR H",         0x1C, :cb  ],
    ["RR L",         0x1D, :cb  ],
    ["RR (HL)",      0x1E, :cb  ],
    ["RR A",         0x1F, :cb  ],
    ["RR (IXs)",     0x1E, :ddscb],
    ["RR (IYs)",     0x1E, :fdscb],
    
    ["RST *",        0xC7, :rst ],
    ["RST",          0xC7, :none],

    ["SET *,B",      0xC0, :set ],
    ["SET *,C",      0xC1, :set ],
    ["SET *,D",      0xC2, :set ],
    ["SET *,E",      0xC3, :set ],
    ["SET *,H",      0xC4, :set ],
    ["SET *,L",      0xC5, :set ],
    ["SET *,(HL)",   0xC6, :set ],
    ["SET *,B",      0xC7, :set ],
    ["SET *,(IXs)",  0xC6, :ixset],
    ["SET *,(IYs)",  0xC6, :iyset],

    ["LDI",          0xA0, :ed  ],
    ["LDD",          0xA8, :ed  ],
    ["LDIR",         0xB0, :ed  ],
    ["LDDR",         0xB8, :ed  ],
    ["LD I,A",       0x47, :ed  ],
    ["LD R,A",       0x4f, :ed  ],
    ["LD A,I",       0x57, :ed  ],
    ["LD A,R",       0x5f, :ed  ],

    ["LD (?),HL",     0x22, :addr],
    ["LD (?),A",      0x32, :addr],
    ["LD (?),BC",     0x43, :eda ],
    ["LD (?),DE",     0x53, :eda ],
    ["LD (?),SP",     0x73, :eda ],
    ["LD (?),IX",     0x22, :dda ],
    ["LD (?),IY",     0x22, :fda ],
    
    ["LD BC,(?)",     0x4B, :eda ],
    ["LD BC,?",       0x01, :addr],
    ["LD DE,(?)",     0x5B, :eda ],
    ["LD DE,?",       0x11, :addr],
    ["LD HL,(?)",     0x2A, :eda ],
    ["LD HL,?",       0x21, :addr],
    ["LD SP,HL",      0xf9, :none],
    ["LD SP,IX",      0xf9, :dd  ],
    ["LD SP,IY",      0xf9, :fd  ],
    ["LD SP,(?)",     0x7b, :eda ],
    ["LD SP,?",       0x31, :addr],

    ["LD (BC),A",     0x02, :none],
    ["LD (DE),A",     0x12, :none],

    ["LD (HL),B",     0x70, :none],
    ["LD (HL),C",     0x71, :none],
    ["LD (HL),D",     0x72, :none],
    ["LD (HL),E",     0x73, :none],
    ["LD (HL),H",     0x74, :none],
    ["LD (HL),L",     0x75, :none],
    ["LD (HL),A",     0x77, :none],

    ["LD (HL),n",     0x36, :imm ],


    ["LD B,B",        0x40, :none],
    ["LD B,C",        0x41, :none],
    ["LD B,D",        0x42, :none],
    ["LD B,E",        0x43, :none],
    ["LD B,H",        0x44, :none],
    ["LD B,L",        0x45, :none],
    ["LD B,(HL)",     0x46, :none],
    ["LD B,A",        0x47, :none],
    ["LD B,IXH",      0x44, :dd  ],
    ["LD B,IXL",      0x45, :dd  ],
    ["LD B,(IXs)",    0x46, :dds ],
    ["LD B,IYH",      0x44, :fd  ],
    ["LD B,IYL",      0x45, :fd  ],
    ["LD B,(IYs)",    0x46, :fds ],
    ["LD B,n",        0x06, :imm ],
    
    ["LD C,B",        0x48, :none],
    ["LD C,C",        0x49, :none],
    ["LD C,D",        0x4A, :none],
    ["LD C,E",        0x4B, :none],
    ["LD C,H",        0x4C, :none],
    ["LD C,L",        0x4D, :none],
    ["LD C,(HL)",     0x4E, :none],
    ["LD C,A",        0x4F, :none],
    ["LD C,IXH",      0x4C, :dd  ],
    ["LD C,IXL",      0x4D, :dd  ],
    ["LD C,(IXs)",    0x4E, :dds ],
    ["LD C,IYH",      0x4C, :fd  ],
    ["LD C,IYL",      0x4D, :fd  ],
    ["LD C,(IYs)",    0x4E, :fds ],
    ["LD C,n",        0x0E, :imm ],

    ["LD D,B",        0x50, :none],
    ["LD D,C",        0x51, :none],
    ["LD D,D",        0x52, :none],
    ["LD D,E",        0x53, :none],
    ["LD D,H",        0x54, :none],
    ["LD D,L",        0x55, :none],
    ["LD D,(HL)",     0x56, :none],
    ["LD D,A",        0x57, :none],
    ["LD D,IXH",      0x54, :dd  ],
    ["LD D,IXL",      0x55, :dd  ],
    ["LD D,(IXs)",    0x56, :dds ],
    ["LD D,IYH",      0x54, :fd  ],
    ["LD D,IYL",      0x55, :fd  ],
    ["LD D,(IYs)",    0x56, :fds ],
    ["LD D,n",        0x16, :imm ],
    
    ["LD E,B",        0x58, :none],
    ["LD E,C",        0x59, :none],
    ["LD E,D",        0x5A, :none],
    ["LD E,E",        0x5B, :none],
    ["LD E,H",        0x5C, :none],
    ["LD E,L",        0x5D, :none],
    ["LD E,(HL)",     0x5E, :none],
    ["LD E,A",        0x5F, :none],
    ["LD E,IXH",      0x5C, :dd  ],
    ["LD E,IXL",      0x5D, :dd  ],
    ["LD E,(IXs)",    0x5E, :dds ],
    ["LD E,IYH",      0x5C, :fd  ],
    ["LD E,IYL",      0x5D, :fd  ],
    ["LD E,(IYs)",    0x5E, :fds ],
    ["LD E,n",        0x1E, :imm ],
    
    ["LD H,B",        0x60, :none],
    ["LD H,C",        0x61, :none],
    ["LD H,D",        0x62, :none],
    ["LD H,E",        0x63, :none],
    ["LD H,H",        0x64, :none],
    ["LD H,L",        0x65, :none],
    ["LD H,(HL)",     0x66, :none],
    ["LD H,A",        0x67, :none],
    ["LD IXH,IXH",    0x64, :dd  ],
    ["LD IXH,IXL",    0x65, :dd  ],
    ["LD H,(IXs)",    0x66, :dds ],
    ["LD IYH,IYH",    0x64, :fd  ],
    ["LD IYH,IYL",    0x65, :fd  ],
    ["LD H,(IYs)",    0x66, :fds ],
    ["LD H,n",        0x26, :imm ],
    
    ["LD L,B",        0x68, :none],
    ["LD L,C",        0x69, :none],
    ["LD L,D",        0x6A, :none],
    ["LD L,E",        0x6B, :none],
    ["LD L,H",        0x6C, :none],
    ["LD L,L",        0x6D, :none],
    ["LD L,(HL)",     0x6E, :none],
    ["LD L,A",        0x6F, :none],
    ["LD IXL,IXH",    0x6C, :dd  ],
    ["LD IXL,IXL",    0x6D, :dd  ],
    ["LD L,(IXs)",    0x6E, :dds ],
    ["LD IYL,IYH",    0x6C, :fd  ],
    ["LD IYL,IYL",    0x6D, :fd  ],
    ["LD L,(IYs)",    0x6E, :fds ],
    ["LD L,n",        0x2E, :imm ],
    
    ["LD (HL),B",     0x70, :none],
    ["LD (HL),C",     0x71, :none],
    ["LD (HL),D",     0x72, :none],
    ["LD (HL),E",     0x73, :none],
    ["LD (HL),H",     0x74, :none],
    ["LD (HL),L",     0x75, :none],
    ["LD (HL),(HL)",  0x76, :none],
    ["LD (HL),A",     0x77, :none],
    ["LD (HL),n",     0x36, :imm ],

    ["LD IXH,B",      0x60, :dd  ],
    ["LD IXH,C",      0x61, :dd  ],
    ["LD IXH,D",      0x62, :dd  ],
    ["LD IXH,E",      0x63, :dd  ],
    ["LD IXH,H",      0x64, :dd  ],
    ["LD IXH,L",      0x65, :dd  ],
    ["LD IXH,(IYs)",  0x66, :dds ],
    ["LD IXH,A",      0x67, :dd  ],

    ["LD IYH,B",      0x60, :fd  ],
    ["LD IYH,C",      0x61, :fd  ],
    ["LD IYH,D",      0x62, :fd  ],
    ["LD IYH,E",      0x63, :fd  ],
    ["LD IYH,H",      0x64, :fd  ],
    ["LD IYH,L",      0x65, :fd  ],
    ["LD IYH,(IYs)",  0x66, :fds ],
    ["LD IYH,A",      0x67, :fd  ],

    ["LD IXL,B",      0x68, :dd  ],
    ["LD IXL,C",      0x69, :dd  ],
    ["LD IXL,D",      0x6A, :dd  ],
    ["LD IXL,E",      0x6B, :dd  ],
    ["LD IXL,H",      0x6C, :dd  ],
    ["LD IXL,L",      0x6D, :dd  ],
    ["LD IXL,(IYs)",  0x6E, :dds ],
    ["LD IXL,A",      0x6F, :dd  ],

    ["LD IYL,B",      0x68, :fd  ],
    ["LD IYL,C",      0x69, :fd  ],
    ["LD IYL,D",      0x6A, :fd  ],
    ["LD IYL,E",      0x6B, :fd  ],
    ["LD IYL,H",      0x6C, :fd  ],
    ["LD IYL,L",      0x6D, :fd  ],
    ["LD IYL,(IYs)",  0x6E, :fds ],
    ["LD IYL,A",      0x6F, :fd  ],

    ["LD (IXs),IXH",  0x74, :dds ],
    ["LD (IXs),IXL",  0x75, :dds ],
    ["LD (IXs),(IXs)",0x76, :dds ],
    ["LD (IXs),IYH",  0x74, :fd  ],
    ["LD (IXs),IYL",  0x75, :fd  ],
    ["LD (IXs),(IYs)",0x76, :fds ],
    
    ["LD (IXs),B",    0x70, :dds ],
    ["LD (IXs),C",    0x71, :dds ],
    ["LD (IXs),D",    0x72, :dds ],
    ["LD (IXs),E",    0x73, :dds ],
    ["LD (IXs),H",    0x74, :dds ],
    ["LD (IXs),L",    0x75, :dds ],
    ["LD (IXs),(HL)", 0x76, :dds ],
    ["LD (IXs),A",    0x77, :dds ],

    ["LD (IYs),B",    0x70, :fds ],
    ["LD (IYs),C",    0x71, :fds ],
    ["LD (IYs),D",    0x72, :fds ],
    ["LD (IYs),E",    0x73, :fds ],
    ["LD (IYs),H",    0x74, :fds ],
    ["LD (IYs),L",    0x75, :fds ],
    ["LD (IYs),(HL)", 0x76, :fds ],
    ["LD (IYs),A",    0x77, :fds ],

    ["LD A,B",        0x78, :none],
    ["LD A,C",        0x79, :none],
    ["LD A,D",        0x7A, :none],
    ["LD A,E",        0x7B, :none],
    ["LD A,H",        0x7C, :none],
    ["LD A,L",        0x7D, :none],
    ["LD A,(HL)",     0x7E, :none],
    ["LD A,A",        0x7F, :none],

    ["LD A,(BC)",     0x0A, :none],
    ["LD A,(DE)",     0x1A, :none],
    ["LD A,(HL)",     0x7E, :none],
    ["LD A,(?)",      0x3A, :addr],
    ["LD A,n",        0x3E, :imm ],

    ["LD A,IXH",      0x7C, :dd  ],
    ["LD A,IXL",      0x7D, :dd  ],
    ["LD A,(IXs)",    0x7E, :dds ],
    ["LD A,IYH",      0x7C, :fd  ],
    ["LD A,IYL",      0x7D, :fd  ],
    ["LD A,(IYs)",    0x7E, :fds ],

    ["LD B,RES *,(IXs)", 0x80, :ldresix],
    ["LD B,RES *,(IYs)", 0x80, :ldresiy],
    ["LD B,RLC (IXs)",   0x00, :ddscb],
    ["LD B,RLC (IYs)",   0x00, :fdscb],
    ["LD B,RRC (IXs)",   0x08, :ddscb],
    ["LD B,RRC (IYs)",   0x08, :fdscb],
    ["LD B,RL (IXs)",    0x10, :ddscb],
    ["LD B,RL (IYs)",    0x10, :fdscb],
    ["LD B,RR (IXs)",    0x18, :ddscb],
    ["LD B,RR (IYs)",    0x18, :fdscb],
    ["LD B,SET *,(IXs)", 0xC0, :ldresix],
    ["LD B,SET *,(IYs)", 0xC0, :ldresiy],
    ["LD B,SLA (IXs)",   0x20, :ddscb],
    ["LD B,SLA (IYs)",   0x20, :fdscb],
    ["LD B,SRA (IXs)",   0x28, :ddscb],
    ["LD B,SRA (IYs)",   0x28, :fdscb],
    ["LD B,SLL (IXs)",   0x30, :ddscb],
    ["LD B,SLL (IYs)",   0x30, :fdscb],
    ["LD B,SRL (IXs)",   0x38, :ddscb],
    ["LD B,SRL (IYs)",   0x38, :fdscb],

    ["LD C,RES *,(IXs)", 0x81, :ldresix],
    ["LD C,RES *,(IYs)", 0x81, :ldresiy],
    ["LD C,RLC (IXs)",   0x01, :ddscb],
    ["LD C,RLC (IYs)",   0x01, :fdscb],
    ["LD C,RRC (IXs)",   0x09, :ddscb],
    ["LD C,RRC (IYs)",   0x09, :fdscb],
    ["LD C,RL (IXs)",    0x11, :ddscb],
    ["LD C,RL (IYs)",    0x11, :fdscb],
    ["LD C,RR (IXs)",    0x19, :ddscb],
    ["LD C,RR (IYs)",    0x19, :fdscb],
    ["LD C,SET *,(IXs)", 0xC1, :ldresix],
    ["LD C,SET *,(IYs)", 0xC1, :ldresiy],
    ["LD C,SLA (IXs)",   0x21, :ddscb],
    ["LD C,SLA (IYs)",   0x21, :fdscb],
    ["LD C,SRA (IXs)",   0x29, :ddscb],
    ["LD C,SRA (IYs)",   0x29, :fdscb],
    ["LD C,SLL (IXs)",   0x31, :ddscb],
    ["LD C,SLL (IYs)",   0x31, :fdscb],
    ["LD C,SRL (IXs)",   0x39, :ddscb],
    ["LD C,SRL (IYs)",   0x39, :fdscb],

    ["LD D,RES *,(IXs)", 0x82, :ldresix],
    ["LD D,RES *,(IYs)", 0x82, :ldresiy],
    ["LD D,RLC (IXs)",   0x02, :ddscb],
    ["LD D,RLC (IYs)",   0x02, :fdscb],
    ["LD D,RRC (IXs)",   0x0A, :ddscb],
    ["LD D,RRC (IYs)",   0x0A, :fdscb],
    ["LD D,RL (IXs)",    0x12, :ddscb],
    ["LD D,RL (IYs)",    0x12, :fdscb],
    ["LD D,RR (IXs)",    0x1A, :ddscb],
    ["LD D,RR (IYs)",    0x1A, :fdscb],
    ["LD D,SET *,(IXs)", 0xC2, :ldresix],
    ["LD D,SET *,(IYs)", 0xC2, :ldresiy],
    ["LD D,SLA (IXs)",   0x22, :ddscb],
    ["LD D,SLA (IYs)",   0x22, :fdscb],
    ["LD D,SRA (IXs)",   0x2A, :ddscb],
    ["LD D,SRA (IYs)",   0x2A, :fdscb],
    ["LD D,SLL (IXs)",   0x32, :ddscb],
    ["LD D,SLL (IYs)",   0x32, :fdscb],
    ["LD D,SRL (IXs)",   0x3A, :ddscb],
    ["LD D,SRL (IYs)",   0x3A, :fdscb],

    ["LD E,RES *,(IXs)", 0x83, :ldresix],
    ["LD E,RES *,(IYs)", 0x83, :ldresiy],
    ["LD E,RLC (IXs)",   0x03, :ddscb],
    ["LD E,RLC (IYs)",   0x03, :fdscb],
    ["LD E,RRC (IXs)",   0x0B, :ddscb],
    ["LD E,RRC (IYs)",   0x0B, :fdscb],
    ["LD E,RL (IXs)",    0x13, :ddscb],
    ["LD E,RL (IYs)",    0x13, :fdscb],
    ["LD E,RR (IXs)",    0x1B, :ddscb],
    ["LD E,RR (IYs)",    0x1B, :fdscb],
    ["LD E,SET *,(IXs)", 0xC3, :ldresix],
    ["LD E,SET *,(IYs)", 0xC3, :ldresiy],
    ["LD E,SLA (IXs)",   0x23, :ddscb],
    ["LD E,SLA (IYs)",   0x23, :fdscb],
    ["LD E,SRA (IXs)",   0x2B, :ddscb],
    ["LD E,SRA (IYs)",   0x2B, :fdscb],
    ["LD E,SLL (IXs)",   0x33, :ddscb],
    ["LD E,SLL (IYs)",   0x33, :fdscb],
    ["LD E,SRL (IXs)",   0x3B, :ddscb],
    ["LD E,SRL (IYs)",   0x3B, :fdscb],

    ["LD H,RES *,(IXs)", 0x84, :ldresix],
    ["LD H,RES *,(IYs)", 0x84, :ldresiy],
    ["LD H,RLC (IXs)",   0x04, :ddscb],
    ["LD H,RLC (IYs)",   0x04, :fdscb],
    ["LD H,RRC (IXs)",   0x0C, :ddscb],
    ["LD H,RRC (IYs)",   0x0C, :fdscb],
    ["LD H,RL (IXs)",    0x14, :ddscb],
    ["LD H,RL (IYs)",    0x14, :fdscb],
    ["LD H,RR (IXs)",    0x1C, :ddscb],
    ["LD H,RR (IYs)",    0x1C, :fdscb],
    ["LD H,SET *,(IXs)", 0xC4, :ldresix],
    ["LD H,SET *,(IYs)", 0xC4, :ldresiy],
    ["LD H,SLA (IXs)",   0x24, :ddscb],
    ["LD H,SLA (IYs)",   0x24, :fdscb],
    ["LD H,SRA (IXs)",   0x2C, :ddscb],
    ["LD H,SRA (IYs)",   0x2C, :fdscb],
    ["LD H,SLL (IXs)",   0x34, :ddscb],
    ["LD H,SLL (IYs)",   0x34, :fdscb],
    ["LD H,SRL (IXs)",   0x3C, :ddscb],
    ["LD H,SRL (IYs)",   0x3C, :fdscb],

    ["LD L,RES *,(IXs)", 0x85, :ldresix],
    ["LD L,RES *,(IYs)", 0x85, :ldresiy],
    ["LD L,RLC (IXs)",   0x05, :ddscb],
    ["LD L,RLC (IYs)",   0x05, :fdscb],
    ["LD L,RRC (IXs)",   0x0D, :ddscb],
    ["LD L,RRC (IYs)",   0x0D, :fdscb],
    ["LD L,RL (IXs)",    0x15, :ddscb],
    ["LD L,RL (IYs)",    0x15, :fdscb],
    ["LD L,RR (IXs)",    0x1D, :ddscb],
    ["LD L,RR (IYs)",    0x1D, :fdscb],
    ["LD L,SET *,(IXs)", 0xC5, :ldresix],
    ["LD L,SET *,(IYs)", 0xC5, :ldresiy],
    ["LD L,SLA (IXs)",   0x25, :ddscb],
    ["LD L,SLA (IYs)",   0x25, :fdscb],
    ["LD L,SRA (IXs)",   0x2D, :ddscb],
    ["LD L,SRA (IYs)",   0x2D, :fdscb],
    ["LD L,SLL (IXs)",   0x35, :ddscb],
    ["LD L,SLL (IYs)",   0x35, :fdscb],
    ["LD L,SRL (IXs)",   0x3D, :ddscb],
    ["LD L,SRL (IYs)",   0x3D, :fdscb],

    ["LD A,RES *,(IXs)", 0x87, :ldresix],
    ["LD A,RES *,(IYs)", 0x87, :ldresiy],
    ["LD A,RLC (IXs)",   0x07, :ddscb],
    ["LD A,RLC (IYs)",   0x07, :fdscb],
    ["LD A,RRC (IXs)",   0x0F, :ddscb],
    ["LD A,RRC (IYs)",   0x0F, :fdscb],
    ["LD A,RL (IXs)",    0x17, :ddscb],
    ["LD A,RL (IYs)",    0x17, :fdscb],
    ["LD A,RR (IXs)",    0x1F, :ddscb],
    ["LD A,RR (IYs)",    0x1F, :fdscb],
    ["LD A,SET *,(IXs)", 0xC7, :ldresix],
    ["LD A,SET *,(IYs)", 0xC7, :ldresiy],
    ["LD A,SLA (IXs)",   0x27, :ddscb],
    ["LD A,SLA (IYs)",   0x27, :fdscb],
    ["LD A,SRA (IXs)",   0x2F, :ddscb],
    ["LD A,SRA (IYs)",   0x2F, :fdscb],
    ["LD A,SLL (IXs)",   0x37, :ddscb],
    ["LD A,SLL (IYs)",   0x37, :fdscb],
    ["LD A,SRL (IXs)",   0x3F, :ddscb],
    ["LD A,SRL (IYs)",   0x3F, :fdscb],


  ]

end

end



end

end

