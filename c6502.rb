#
# 6502 assembler
#
#
#

require "./assembler"

module Assembler

  class C6502 < Assembler
    X = Register.new(:X)
    Y = Register.new(:Y)

    def self.single_byte hash
      hash.each do |insn, byte|
        define_method (insn.to_s) do
          # no need to envelope in __op
          db byte
        end
      end
    end

    def self.two_byte_rel_offset hash
      hash.each do |insn, byte|
        define_method (insn.to_s) do |offset|
          if offset.is_a? Fixnum
            db byte, offset.if_within(-128, 127).b
          else
            __try_op ->{
              db byte, offset.distance_to(here).if_within(-128, 127)
            }
          end
        end
      end
    end

    def self.mode_addressable hash
      hash.each do |insn, modes|
        define_method (insn.to_s) do |arg|
          if (arg.is_a? Hash) and (arg.key? :type)
            if modes.key? arg[:type] and !modes[arg[:type]].nil?
              case arg[:type]
              when :zp
                __try_op ->{ db modes[:zp], arg[:index].if_within(0, 255).b }
              when :zp_x
                __try_op ->{ db modes[:zp_x], arg[:index].if_within(0, 255).b }
              when :zp_y
                __try_op ->{ db modes[:zp_y], arg[:index].if_within(0, 255).b }
              when :ind_x
                __try_op ->{ db modes[:ind_x], arg[:index].if_within(0, 255).b }
              when :ind_y
                __try_op ->{ db modes[:ind_y], arg[:index].if_within(0, 255).b }
              when :abs
                __try_op ->{ db modes[:abs]; dw arg[:address].if_within(0, 65536).w }
              when :ind
                __try_op ->{ db modes[:ind]; dw arg[:address].if_within(0, 65536).w }
              when :rel
                __try_op ->{ db modes[:rel]; dw arg[:offset].if_within(-32768, 32767).w }
              when :abs_x
                __try_op ->{ db modes[:abs_x]; dw arg[:address].if_within(0, 65536).w }
              when :abs_y
                __try_op ->{ db modes[:abs_y]; dw arg[:address].if_within(0, 65536).w }
              else
                raise "invalid type #{arg[:type].to_s} for instruction #{insn.to_s}"
              end
            else
              raise "unknown mode for instruction #{insn.to_s}"
            end
          elsif arg.to_i.is_a? Fixnum
            if modes[:imm] != nil
              __try_op ->{ db modes[:imm], arg.if_within(-256, 255).b }
            else
              raise "instruction #{insn.to_s} doesn't work with immediate values"
            end
          else
            raise "invalid argument for instruction #{insn.to_s}"
          end
        end
      end
    end


    def self.make_argument_type_alias name, args
      define_method name.to_s do *inner_args
        if args.length != inner_args.length
          raise "invalid number of arguments for #{name.to_s}"
        end
        return Hash[args.zip(inner_args)].merge(:type => name)
      end
      name
    end

    
    #################### opcode argument/mode generators

    NON = make_argument_type_alias :none  []
    IMM = make_argument_type_alias :imm   [:immediate]
    ZP  = make_argument_type_alias :zp,    [:index]
    ZPX = make_argument_type_alias :zp_x,  [:index]
    ZPY = make_argument_type_alias :zp_y,  [:index]
    IND = make_argument_type_alias :ind,   [:index]
    INX = make_argument_type_alias :ind_x, [:index]
    INY = make_argument_type_alias :ind_y, [:index]
    REL = make_argument_type_alias :rel,   [:offset]
    ABS = make_argument_type_alias :abs,   [:address]
    ABX = make_argument_type_alias :abs_x, [:address]
    ABY = make_argument_type_alias :abs_y, [:address]

    #################### Single-byte no-argument opcodes
    single_byte {
      php: 0x08,
      clc: 0x16,
      plp: 0x28,
      sec: 0x38,
      rti: 0x40,
      pha: 0x48,
      cli: 0x58,
      rts: 0x60,
      pla: 0x68,
      sei: 0x78,
      dey: 0x88,
      txa: 0x8a,
      tya: 0x98,
      txs: 0x9a,
      tay: 0xa8,
      tax: 0xaa,
      clv: 0xb8,
      tsx: 0xba,
      iny: 0xc8,
      dex: 0xca,
      cld: 0xd8,
      inx: 0xe8,
      nop: 0xea,
      sed: 0xf8,
      phy: 0x5a,
      ply: 0x7a,
      phx: 0xda,
      plx: 0xfa,
      wdm: 0x42,
    }

    #################### Two-byte immediate-argument opcodes
    
    two_byte_rel_offset [
      bpl: 0x10,
      bmi: 0x30,
      bvc: 0x50,
      bvs: 0x70,
      bra: 0x80,
      bcc: 0x90,
      bcs: 0xb0,
      bne: 0xd0,
      beq: 0xf0
    ]

    def bit(num); db 0x89, num.if_within(0, 7) end

    def jsr(addr)
      if addr.is_a? Fixnum
        db 0x20;
        dw addr.if_within(0, 65535).w
      else
        __try_op ->{
          db 0x20;
          dw addr.absolute.if_within(0, 65535)
        }
      end
    end
    #################### mode-addressable opcodes
    

    mode_addressable {
      sta: {           zp: 0x85, zp_x: 0x95,            abs: 0x8d, abs_x: 0x9d, abs_y: 0x99, ind_x: 0x81, ind_y: 0x91},
      lda: {imm: 0xa9, zp: 0xa5, zp_x: 0xb5,            abs: 0xad, abs_x: 0xbd, abs_y: 0xb9, ind_x: 0xa1, ind_y: 0xb1},
      stx: {           zp: 0x86,             zp_y:0x96, abs: 0x87,                                                   },
      ldx: {imm: 0xa2,           zp_x: 0xa6, zp_y:0xb6,            abs_x: 0xae, abs_y: 0xbe,                         },
      sty: {           zp: 0x84, zp_x: 0x94,            abs: 0x8c,                                                   },
      ldy: {imm: 0xa0, zp: 0xa4,             zp_y:0xb4, abs: 0xbc,              abs_y: 0xbe,                         },
      ora: {imm: 0x09, zp: 0x05, zp_x: 0x15,            abs: 0x0D, abs_x: 0x1d, abs_y: 0x19, ind_x: 0x01, ind_y: 0x11},
      _and:{imm: 0x29, zp: 0x25, zp_x: 0x35,            abs: 0x2d, abs_x: 0x3d, abs_y: 0x39, ind_x: 0x21, ind_y: 0x31},
      eor: {imm: 0x49, zp: 0x45, zp_x: 0x55,            abs: 0x4d, abs_x: 0x5d, abs_y: 0x59, ind_x: 0x41, ind_y: 0x51},
      cmp: {imm: 0xc9, zp: 0xc5, zp_x: 0xd5,            abs: 0xcd, abs_x: 0xdd, abs_y: 0xd9, ind_x: 0xc1, ind_y: 0xd1},
      cpx: {imm: 0xe0, zp: 0xe4,                        abs: 0xec,                                                   },
      cpy: {imm: 0xc0, zp: 0xc4,                        abs: 0xcc,                                                   },
      adc: {imm: 0x69, zp: 0x65, zp_x: 0x75,            abs: 0x6d, abs_x: 0x7d, abs_y: 0x79, ind_x: 0x61, ind_y: 0x71},
      sbc: {imm: 0xe9, zp: 0xe5, zp_x: 0xf5,            abs: 0xed, abs_x: 0xfd, abs_y: 0xf9, ind_x: 0xe1, ind_y: 0xf1},
      jmp: {                                            abs: 0x4c, abs_x: 0x7c,                                      },
      inc: {           zp: 0xe6, zp_x: 0xf6,            abs: 0xee, abs_x: 0xfe,                                      },
      dec: {           zp: 0xc6, zp_x: 0xd6,            abs: 0xce, abs_x: 0xde,                                      }
    }



    OPCODE_TABLE_BY_CODE = [
      #00-1f
      {brk: NON}, {ora: INX}, {kil: NON}, {slo: INX}, {nop: ZP }, {ora: ZP }, {asl: ZP }, {slo: ZP },
      {php: NON}, {ora: IMM}, {asl: NON}, {anc: IMM}, {nop: ABS}, {ora: ABS}, {asl: ABS}, {slo: ABS},
      {bpl: REL}, {ora: INY}, {kil: NON}, {slo: INY}, {nop: ZPX}, {ora: ZPX}, {asl: ZPX}, {slo: ZPX},
      {clc: NON}, {ora: ABY}, {nop: NON}, {slo: ABY}, {nop: ABX}, {ora: ABX}, {asl: ABX}, {slo: ABX},
      #20-3f
      {jsr: ABS}, {_and: INX}, {kil: NON}, {rla: INX}, {bit: ZP }, {_and: ZP }, {rol: ZP }, {rla: ZP },
      {plp: NON}, {_and: IMM}, {rol: NON}, {anc: IMM}, {bit: ABS}, {_and: ABS}, {rol: ABS}, {rla: ABS},
      {bmi: REL}, {_and: INY}, {kil: NON}, {rla: INY}, {nop: ZPX}, {_and: ZPX}, {rol: ZPX}, {rla: ZPX},
      {sec: NON}, {_and: ABY}, {nop: NON}, {rla: ABY}, {nop: ABX}, {_and: ABX}, {rol: ABX}, {rla: ABX},
      #40-5f
      {rti: NON}, {eor: INX}, {kil: NON}, {sre: INX}, {nop: ZP }, {eor: ZP }, {lsr: ZP }, {sre: ZP },
      {pha: NON}, {eor: IMM}, {lsr: NON}, {alr: IMM}, {jmp: ABS}, {eor: ABS}, {lsr: ABS}, {sre: ABS},
      {bvc: REL}, {eor: INY}, {kil: NON}, {sre: INY}, {nop: ZPX}, {eor: ZPX}, {lsr: ZPX}, {sre: ZPX},
      {cli: NON}, {eor: ABY}, {nop: NON}, {sre: ABY}, {nop: ABX}, {eor: ABX}, {lsr: ABX}, {sre: ABX},
      #60-7f
      {rts: NON}, {adc: INX}, {kil: NON}, {rra: INX}, {nop: ZP }, {adc: ZP }, {asl: ZP }, {rra: ZP },
      {pla: NON}, {adc: IMM}, {asl: NON}, {arr: IMM}, {jmp: IND}, {adc: ABS}, {asl: ABS}, {rra: ABS},
      {bvs: REL}, {adc: INY}, {kil: NON}, {rra: INY}, {nop: ZPX}, {adc: ZPX}, {asl: ZPX}, {rra: ZPX},
      {sei: NON}, {adc: ABY}, {nop: NON}, {rra: ABY}, {nop: ABX}, {adc: ABX}, {asl: ABX}, {rra: ABX},
      #80-9f
      {nop: IMM}, {sta: INX}, {nop: IMM}, {sax: INX}, {nop: ZP }, {sta: ZP }, {stx: ZP }, {sax: ZP },
      {dei: NON}, {sta: IMM}, {asl: NON}, {xaa: IMM}, {sty: ABS}, {sta: ABS}, {stx: ABS}, {sax: ABS},
      {bcc: REL}, {sta: INY}, {kil: NON}, {ahx: INY}, {nop: ZPX}, {sta: ZPX}, {stx: ZPY}, {sax: ZPX},
      {tya: NON}, {sta: ABY}, {nop: NON}, {tas: ABY}, {shy: ABX}, {sta: ABX}, {shx: ABY}, {ahx: ABX},
      #a0-bf
      {ldy: IMM}, {lda: INX}, {ldx: IMM}, {sax: INX}, {ldy: ZP }, {lda: ZP }, {ldx: ZP }, {lax: ZP },
      {tay: NON}, {lda: IMM}, {lax: NON}, {xaa: IMM}, {ldy: ABS}, {lda: ABS}, {ldx: ABS}, {lax: ABS},
      {bcs: REL}, {lda: INY}, {kil: NON}, {ahx: INY}, {ldy: ZPX}, {lda: ZPX}, {ldx: ZPY}, {lax: ZPX},
      {clv: NON}, {lda: ABY}, {lax: IMM}, {tas: ABY}, {ldy: ABX}, {lda: ABX}, {ldx: ABY}, {lax: ABX},
      #c0-df
      {cpy: IMM}, {cmp: INX}, {nop: IMM}, {dcp: INX}, {cpy: ZP }, {cmp: ZP }, {dec: ZP }, {dcp: ZP },
      {iny: NON}, {cmp: IMM}, {dex: NON}, {axs: IMM}, {cpy: ABS}, {cmp: ABS}, {dec: ABS}, {dcp: ABS},
      {bne: REL}, {cmp: INY}, {kil: NON}, {dcp: INY}, {nop: ZPX}, {cmp: ZPX}, {dec: ZPY}, {dcp: ZPX},
      {cld: NON}, {cmp: ABY}, {nop: NON}, {dcp: ABY}, {nop: ABX}, {cmp: ABX}, {dec: ABY}, {dcp: ABX},
      #e0-ff
      {cpx: IMM}, {sbc: INX}, {nop: IMM}, {isc: INX}, {cpx: ZP }, {sbc: ZP }, {inc: ZP }, {isc: ZP },
      {inx: NON}, {sbc: IMM}, {nop: NON}, {sbc: IMM}, {cpx: ABS}, {sbc: ABS}, {inc: ABS}, {isc: ABS},
      {beq: REL}, {sbc: INY}, {kil: NON}, {isc: INY}, {nop: ZPX}, {sbc: ZPX}, {inc: ZPY}, {isc: ZPX},
      {sed: NON}, {sbc: ABY}, {nop: NON}, {isc: ABY}, {nop: ABX}, {sbc: ABX}, {inc: ABY}, {isc: ABX},
    ]


    def decode_number (string)
      /$[0-9A-F]+\.W/i
      /$[0-9A-F]+\.B/i
      /[0-9]+/i
    end

    ARGUMENT_READERS = {
      "x" => -> asm, str { asm.decode_number(str).to_i.if_within(-128, 255) },
      "?" => -> asm, str { asm.decode_number(str).to_i.if_within(-32768, 65535) }
    }

    OPCODE_TYPES = [
      none: -> asm, bytecode, tokens { asm.db bytecode },
      bkpt: -> asm, bytecode, tokens { asm.breakpoint; asm.db bytecode },
      imm:  -> asm, bytecode, tokens { asm.db bytecode, tokens[0].b }
      bkpi: -> asm, bytecode, tokens { asm.breakpoint; asm.db bytecode, tokens[0].b }
      addr: -> asm, bytecode, tokens { asm.db bytecode, tokens[0].lo_byte, tokens[0].hi_byte },
      rel:  -> asm, bytecode, tokens { asm.db bytecode, tokens[0].relative_to(asm.here).b },
      bbit: -> asm, bytecode, tokens { asm.db bytecode, tokens[0].b, tokens[1].b }
    ]

    # adapted from the WLA-DX 6502 tables
    INSN_TABLE_6502 = [
      [[ "ADC #x",      0x69, :imm ]],
      [[ "ADC (x,X)",   0x61, :imm ]],
      [[ "ADC (x),Y",   0x71, :imm ]],
      [[ "ADC (x)",     0x72, :imm ]],
      [[ "ADC x,X",     0x75, :imm ],
       [ "ADC ?,X",     0x7D, :addr]],
      [[ "ADC ?,Y",     0x79, :addr]],
      [[ "ADC x",       0x65, :imm ],
       [ "ADC ?",       0x6D, :addr]],
      [[ "ADC.B #x",    0x69, :imm ]],
      [[ "ADC.B (x,X)", 0x61, :imm ]],
      [[ "ADC.B (x),Y", 0x71, :imm ]],
      [[ "ADC.B (x)",   0x72, :imm ]],
      [[ "ADC.B x,X",   0x75, :imm ]],
      [[ "ADC.W ?,X",   0x7D, :addr]],
      [[ "ADC.W ?,Y",   0x79, :addr]],
      [[ "ADC.B x",     0x65, :imm ]],
      [[ "ADC.W ?",     0x6D, :addr]],
      [[ "AND #x",      0x29, :imm ]],
      [[ "AND (x,X)",   0x21, :imm ]],
      [[ "AND (x),Y",   0x31, :imm ]],
      [[ "AND (x)",     0x32, :imm ]],
      [[ "AND x,X",     0x35, :imm ],
       [ "AND ?,X",     0x3D, :addr]],
      [[ "AND ?,Y",     0x39, :addr]],
      [[ "AND x",       0x25, :imm ],
       [ "AND ?",       0x2D, :addr]],
      [[ "AND.B #x",    0x29, :imm ]],
      [[ "AND.B (x,X)", 0x21, :imm ]],
      [[ "AND.B (x),Y", 0x31, :imm ]],
      [[ "AND.B (x)",   0x32, :imm ]],
      [[ "AND.B x,X",   0x35, :imm ]],
      [[ "AND.W ?,X",   0x3D, :addr]],
      [[ "AND.W ?,Y",   0x39, :addr]],
      [[ "AND.B x",     0x25, :imm ]],
      [[ "AND.W ?",     0x2D, :addr]],
      [[ "ASL A",       0x0A, :none]],
      [[ "ASL x,X",     0x16, :imm ],
       [ "ASL ?,X",     0x1E, :addr]],
      [[ "ASL x",       0x06, :imm ],
       [ "ASL ?",       0x0E, :addr]],
      [[ "ASL.B x,X",   0x16, :imm ]],
      [[ "ASL.W ?,X",   0x1E, :addr]],
      [[ "ASL.B x",     0x06, :imm ]],
      [[ "ASL.W ?",     0x0E, :addr]],
      [[ "ASL",         0x0A, :none]],
      [[ "BCC x",       0x90, :rel ]],
      [[ "BCS x",       0xB0, :rel ]],
      [[ "BEQ x",       0xF0, :rel ]],
      [[ "BMI x",       0x30, :rel ]],
      [[ "BNE x",       0xD0, :rel ]],
      [[ "BPL x",       0x10, :rel ]],
      [[ "BVC x",       0x50, :rel ]],
      [[ "BVS x",       0x70, :rel ]],
      [[ "BRA x",       0x80, :rel ]],
      [[ "BCC.B x",     0x90, :rel ]],
      [[ "BCS.B x",     0xB0, :rel ]],
      [[ "BEQ.B x",     0xF0, :rel ]],
      [[ "BMI.B x",     0x30, :rel ]],
      [[ "BNE.B x",     0xD0, :rel ]],
      [[ "BPL.B x",     0x10, :rel ]],
      [[ "BVC.B x",     0x50, :rel ]],
      [[ "BVS.B x",     0x70, :rel ]],
      [[ "BRA.B x",     0x80, :rel ]],
      [[ "BIT #x",      0x89, :imm ]],
      [[ "BIT x,X",     0x34, :imm ],
       [ "BIT ?,X",     0x3C, :addr]],
      [[ "BIT x",       0x24, :imm ],
       [ "BIT ?",       0x2C, :addr]],
      [[ "BIT.B x,X",   0x34, :imm ],
       [ "BIT.W ?,X",   0x3C, :addr]],
      [[ "BIT.B x",     0x24, :imm ],
       [ "BIT.W ?",     0x2C, :addr]],
      [[ "BRK x",       0x00, :bkpi]],
      [[ "BRK.B x",     0x00, :bkpi]],
      [[ "BRK",         0x00, :bkpt]],
      [[ "CLC",         0x18, :none]],
      [[ "CLD",         0xD8, :none]],
      [[ "CLI",         0x58, :none]],
      [[ "CLV",         0xB8, :none]],
      [[ "CMP #x",      0xC9, :imm ]],
      [[ "CMP (x,X)",   0xC1, :imm ]],
      [[ "CMP (x),Y",   0xD1, :imm ]],
      [[ "CMP (x)",     0xD2, :imm ]],
      [[ "CMP x,X",     0xD5, :imm ],
       [ "CMP ?,X",     0xDD, :addr]],
      [[ "CMP ?,Y",     0xD9, :addr]],
      [[ "CMP x",       0xC5, :imm ],
       [ "CMP ?",       0xCD, :addr]],
      [[ "CMP.B #x",    0xC9, :imm ]],
      [[ "CMP.B (x,X)", 0xC1, :imm ]],
      [[ "CMP.B (x),Y", 0xD1, :imm ]],
      [[ "CMP.B (x)",   0xD2, :imm ]],
      [[ "CMP.B x,X",   0xD5, :imm ]],
      [[ "CMP.W ?,X",   0xDD, :addr]],
      [[ "CMP.W ?,Y",   0xD9, :addr]],
      [[ "CMP.B x",     0xC5, :imm ]],
      [[ "CMP.W ?",     0xCD, :addr]],
      [[ "CPX #x",      0xE0, :imm ]],
      [[ "CPX x",       0xE4, :imm ],
       [ "CPX ?",       0xEC, :addr]],
      [[ "CPX.B #x",    0xE0, :imm ]],
      [[ "CPX.B x",     0xE4, :imm ]],
      [[ "CPX.W ?",     0xEC, :addr]],
      [[ "CPY #x",      0xC0, :imm ]],
      [[ "CPY x",       0xC4, :imm ],
       [ "CPY ?",       0xCC, :addr]],
      [[ "CPY.B #x",    0xC0, :imm ]],
      [[ "CPY.B x",     0xC4, :imm ]],
      [[ "CPY.W ?",     0xCC, :addr]],
      [[ "DEC x,X",     0xD6, :imm ],
       [ "DEC ?,X",     0xDE, :addr]],
      [[ "DEC x",       0xC6, :imm ],
       [ "DEC ?",       0xCE, :addr]],
      [[ "DEA",         0x3A, :none]],
      [[ "DEX",         0xCA, :none]],
      [[ "DEY",         0x88, :none]],
      [[ "DEC.B x,X",   0xD6, :imm ]],
      [[ "DEC.W ?,X",   0xDE, :addr]],
      [[ "DEC.B x",     0xC6, :imm ]],
      [[ "DEC.W ?",     0xCE, :addr]],
      [[ "EOR #x",      0x49, :imm ]],
      [[ "EOR (x,X)",   0x41, :imm ]],
      [[ "EOR (x),Y",   0x51, :imm ]],
      [[ "EOR (x)",     0x52, :imm ]],
      [[ "EOR x,X",     0x55, :imm ],
       [ "EOR ?,X",     0x5D, :addr]],
      [[ "EOR ?,Y",     0x59, :addr]],
      [[ "EOR x",       0x45, :imm ],
       [ "EOR ?",       0x4D, :addr]],
      [[ "EOR.B #x",    0x49, :imm ]],
      [[ "EOR.B (x,X)", 0x41, :imm ]],
      [[ "EOR.B (x),Y", 0x51, :imm ]],
      [[ "EOR.B (x)",   0x52, :imm ]],
      [[ "EOR.B x,X",   0x55, :imm ]],
      [[ "EOR.W ?,X",   0x5D, :addr]],
      [[ "EOR.W ?,Y",   0x59, :addr]],
      [[ "EOR.B x",     0x45, :imm ]],
      [[ "EOR.W ?",     0x4D, :addr]],
      [[ "INC x,X",     0xF6, :imm ],
       [ "INC ?,X",     0xFE, :addr]],
      [[ "INC x",       0xE6, :imm ],
       [ "INC ?",       0xEE, :addr]],
      [[ "INA",         0x1A, :none]],
      [[ "INX",         0xE8, :none]],
      [[ "INY",         0xC8, :none]],
      [[ "INC.B x,X",   0xF6, :imm ]],
      [[ "INC.W ?,X",   0xFE, :addr]],
      [[ "INC.B x",     0xE6, :imm ]],
      [[ "INC.W ?",     0xEE, :addr]],
      [[ "JMP (?,X)",   0x7C, :addr]],
      [[ "JMP (?)",     0x6C, :addr]],
      [[ "JMP ?",       0x4C, :addr]],
      [[ "JSR ?",       0x20, :addr]],
      [[ "JMP.W (?,X)", 0x7C, :addr]],
      [[ "JMP.W (?)",   0x6C, :addr]],
      [[ "JMP.W ?",     0x4C, :addr]],
      [[ "JSR.W ?",     0x20, :addr]],
      [[ "LDA #x",      0xA9, :imm ]],
      [[ "LDA (x,X)",   0xA1, :imm ]],
      [[ "LDA (x),Y",   0xB1, :imm ]],
      [[ "LDA (x)",     0xB2, :imm ]],
      [[ "LDA x,X",     0xB5, :imm ],
       [ "LDA ?,X",     0xBD, :addr]],
      [[ "LDA ?,Y",     0xB9, :addr]],
      [[ "LDA x",       0xA5, :imm ],
       [ "LDA ?",       0xAD, :addr]],
      [[ "LDA.B #x",    0xA9, :imm ]],
      [[ "LDA.B (x,X)", 0xA1, :imm ]],
      [[ "LDA.B (x),Y", 0xB1, :imm ]],
      [[ "LDA.B (x)",   0xB2, :imm ]],
      [[ "LDA.B x,X",   0xB5, :imm ]],
      [[ "LDA.W ?,X",   0xBD, :addr]],
      [[ "LDA.W ?,Y",   0xB9, :addr]],
      [[ "LDA.B x",     0xA5, :imm ]],
      [[ "LDA.W ?",     0xAD, :addr]],
      [[ "LDX #x",      0xA2, :imm ]],
      [[ "LDX x,Y",     0xB6, :imm ],
       [ "LDX ?,Y",     0xBE, :addr]],
      [[ "LDX x",       0xA6, :imm ],
       [ "LDX ?",       0xAE, :addr]],
      [[ "LDX.B #x",    0xA2, :imm ]],
      [[ "LDX.B x,Y",   0xB6, :imm ]],
      [[ "LDX.W ?,Y",   0xBE, :addr]],
      [[ "LDX.B x",     0xA6, :imm ]],
      [[ "LDX.W ?",     0xAE, :addr]],
      [[ "LDY #x",      0xA0, :imm ]],
      [[ "LDY x,X",     0xB4, :imm ],
       [ "LDY ?,X",     0xBC, :addr]],
      [[ "LDY x",       0xA4, :imm ],
       [ "LDY ?",       0xAC, :addr]],
      [[ "LDY.B #x",    0xA0, :imm ]],
      [[ "LDY.B x,X",   0xB4, :imm ]],
      [[ "LDY.W ?,X",   0xBC, :addr]],
      [[ "LDY.B x",     0xA4, :imm ]],
      [[ "LDY.W ?",     0xAC, :addr]],
      [[ "LSR A",       0x4A, :none]],
      [[ "LSR x,X",     0x56, :imm ],
       [ "LSR ?,X",     0x5E, :addr]],
      [[ "LSR x",       0x46, :imm ],
       [ "LSR ?",       0x4E, :addr]],
      [[ "LSR.B x,X",   0x56, :imm ]],
      [[ "LSR.W ?,X",   0x5E, :addr]],
      [[ "LSR.B x",     0x46, :imm ]],
      [[ "LSR.W ?",     0x4E, :addr]],
      [[ "LSR",         0x4A, :none]],
      [[ "NOP",         0xEA, :none]],
      [[ "ORA #x",      0x09, :imm ]],
      [[ "ORA (x,X)",   0x01, :imm ]],
      [[ "ORA (x),Y",   0x11, :imm ]],
      [[ "ORA (x)",     0x12, :imm ]],
      [[ "ORA x,X",     0x15, :imm ],
       [ "ORA ?,X",     0x1D, :addr]],
      [[ "ORA ?,Y",     0x19, :addr]],
      [[ "ORA x",       0x05, :imm ],
       [ "ORA ?",       0x0D, :addr]],
      [[ "ORA.B #x",    0x09, :imm ]],
      [[ "ORA.B (x,X)", 0x01, :imm ]],
      [[ "ORA.B (x),Y", 0x11, :imm ]],
      [[ "ORA.B (x)",   0x12, :imm ]],
      [[ "ORA.B x,X",   0x15, :imm ]],
      [[ "ORA.W ?,X",   0x1D, :addr]],
      [[ "ORA.W ?,Y",   0x19, :addr]],
      [[ "ORA.B x",     0x05, :imm ]],
      [[ "ORA.W ?",     0x0D, :addr]],
      [[ "PHA",         0x48, :none]],
      [[ "PHP",         0x08, :none]],
      [[ "PHX",         0xDA, :none]],
      [[ "PHY",         0x5A, :none]],
      [[ "PLA",         0x68, :none]],
      [[ "PLP",         0x28, :none]],
      [[ "PLX",         0xFA, :none]],
      [[ "PLY",         0x7A, :none]],
      [[ "ROL A",       0x2A, :none]],
      [[ "ROL x,X",     0x36, :imm ],
       [ "ROL ?,X",     0x3E, :addr]],
      [[ "ROL x",       0x26, :imm ],
       [ "ROL ?",       0x2E, :addr]],
      [[ "ROL.B x,X",   0x36, :imm ]],
      [[ "ROL.W ?,X",   0x3E, :addr]],
      [[ "ROL.B x",     0x26, :imm ]],
      [[ "ROL.W ?",     0x2E, :addr]],
      [[ "ROL",         0x2A, :none]],
      [[ "ROR A",       0x6A, :none]],
      [[ "ROR x,X",     0x76, :imm ],
       [ "ROR ?,X",     0x7E, :addr]],
      [[ "ROR x",       0x66, :imm ],
       [ "ROR ?",       0x6E, :addr]],
      [[ "ROR.B x,X",   0x76, :imm ]],
      [[ "ROR.W ?,X",   0x7E, :addr]],
      [[ "ROR.B x",     0x66, :imm ]],
      [[ "ROR.W ?",     0x6E, :addr]],
      [[ "ROR",         0x6A, :none]],
      [[ "RTI",         0x40, :none]],
      [[ "RTS",         0x60, :none]],
      [[ "SBC #x",      0xE9, :imm ]],
      [[ "SBC (x,X)",   0xE1, :imm ]],
      [[ "SBC (x),Y",   0xF1, :imm ]],
      [[ "SBC (x)",     0xF2, :imm ]],
      [[ "SBC x,X",     0xF5, :imm ],
       [ "SBC ?,X",     0xFD, :addr]],
      [[ "SBC ?,Y",     0xF9, :addr]],
      [[ "SBC x",       0xE5, :imm ],
       [ "SBC ?",       0xED, :addr]],
      [[ "SBC.B #x",    0xE9, :imm ]],
      [[ "SBC.B (x,X)", 0xE1, :imm ]],
      [[ "SBC.B (x),Y", 0xF1, :imm ]],
      [[ "SBC.B (x)",   0xF2, :imm ]],
      [[ "SBC.B x,X",   0xF5, :imm ]],
      [[ "SBC.W ?,X",   0xFD, :addr]],
      [[ "SBC.W ?,Y",   0xF9, :addr]],
      [[ "SBC.B x",     0xE5, :imm ]],
      [[ "SBC.W ?",     0xED, :addr]],
      [[ "SEC",         0x38, :none]],
      [[ "SED",         0xF8, :none]],
      [[ "SEI",         0x78, :none]],
      [[ "STA (x,X)",   0x81, :imm ]],
      [[ "STA (x),Y",   0x91, :imm ]],
      [[ "STA (x)",     0x92, :imm ]],
      [[ "STA x,X",     0x95, :imm ],
       [ "STA ?,X",     0x9D, :addr]],
      [[ "STA ?,Y",     0x99, :addr]],
      [[ "STA x",       0x85, :imm ],
       [ "STA ?",       0x8D, :addr]],
      [[ "STA.B (x,X)", 0x81, :imm ]],
      [[ "STA.B (x),Y", 0x91, :imm ]],
      [[ "STA.B (x)",   0x92, :imm ]],
      [[ "STA.B x,X",   0x95, :imm ]],
      [[ "STA.W ?,X",   0x9D, :addr]],
      [[ "STA.W ?,Y",   0x99, :addr]],
      [[ "STA.B x",     0x85, :imm ]],
      [[ "STA.W ?",     0x8D, :addr]],
      [[ "STX x,Y",     0x96, :imm ]],
      [[ "STX x",       0x86, :imm ],
       [ "STX ?",       0x8E, :addr]],
      [[ "STX.B x,Y",   0x96, :imm ]],
      [[ "STX.B x",     0x86, :imm ]],
      [[ "STX.W ?",     0x8E, :addr]],
      [[ "STY x,X",     0x94, :imm ]],
      [[ "STY x",       0x84, :imm ],
       [ "STY ?",       0x8C, :addr]],
      [[ "STY.B x,X",   0x94, :imm ]],
      [[ "STY.B x",     0x84, :imm ]],
      [[ "STY.W ?",     0x8C, :addr]],
      [[ "STZ x,X",     0x74, :imm ],
       [ "STZ ?,X",     0x9E, :addr]],
      [[ "STZ x",       0x64, :imm ],
       [ "STZ ?",       0x9C, :addr]],
      [[ "STZ.B x,X",   0x74, :imm ]],
      [[ "STZ.W x,X",   0x94, :addr]],
      [[ "STZ.B x",     0x64, :imm ]],
      [[ "STZ.W ?",     0x9C, :addr]],
      [[ "TAX",         0xAA, :none]],
      [[ "TAY",         0xA8, :none]],
      [[ "TSX",         0xBA, :none]],
      [[ "TXA",         0x8A, :none]],
      [[ "TXS",         0x9A, :none]],
      [[ "TYA",         0x98, :none]],
      [[ "TRB x",       0x14, :imm ],
       [ "TRB ?",       0x1C, :addr]],
      [[ "TRB.B x",     0x14, :imm ],
       [ "TRB.W ?",     0x1C, :addr]],

      [[ "TSB x",       0x04, :imm ],
       [ "TSB ?",       0x0C, :addr]],
      [[ "TSB.B x",     0x04, :imm ],
       [ "TSB.W ?",     0x0C, :addr]]
    ]

    OPCODE_TABLE_65C02 = OPCODE_TABLE_6502.insert(0,
        [[ "BBR0 x,x", 0x0F, :bbit]],
        [[ "BBR1 x,x", 0x1F, :bbit]],
        [[ "BBR2 x,x", 0x2F, :bbit]],
        [[ "BBR3 x,x", 0x3F, :bbit]],
        [[ "BBR4 x,x", 0x4F, :bbit]],
        [[ "BBR5 x,x", 0x5F, :bbit]],
        [[ "BBR6 x,x", 0x6F, :bbit]],
        [[ "BBR7 x,x", 0x7F, :bbit]],
        [[ "BBS0 x,x", 0x8F, :bbit]],
        [[ "BBS1 x,x", 0x9F, :bbit]],
        [[ "BBS2 x,x", 0xAF, :bbit]],
        [[ "BBS3 x,x", 0xBF, :bbit]],
        [[ "BBS4 x,x", 0xCF, :bbit]],
        [[ "BBS5 x,x", 0xDF, :bbit]],
        [[ "BBS6 x,x", 0xEF, :bbit]],
        [[ "BBS7 x,x", 0xFF, :bbit]],
        [[ "RMB0 x",   0x07, :imm ]],
        [[ "RMB1 x",   0x17, :imm ]],
        [[ "RMB2 x",   0x27, :imm ]],
        [[ "RMB3 x",   0x37, :imm ]],
        [[ "RMB4 x",   0x47, :imm ]],
        [[ "RMB5 x",   0x57, :imm ]],
        [[ "RMB6 x",   0x67, :imm ]],
        [[ "RMB7 x",   0x77, :imm ]],
        [[ "SMB0 x",   0x87, :imm ]],
        [[ "SMB1 x",   0x97, :imm ]],
        [[ "SMB2 x",   0xA7, :imm ]],
        [[ "SMB3 x",   0xB7, :imm ]],
        [[ "SMB4 x",   0xC7, :imm ]],
        [[ "SMB5 x",   0xD7, :imm ]],
        [[ "SMB6 x",   0xE7, :imm ]],
        [[ "SMB7 x",   0xF7, :imm ]]
      )
  end
end