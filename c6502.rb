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
    end

    
    #################### opcode argument/mode generators

    make_argument_type_alias :zp, [:index]
    make_argument_type_alias :zp_x, [:index]
    make_argument_type_alias :zp_y, [:index]
    make_argument_type_alias :ind, [:index]
    make_argument_type_alias :ind_x, [:index]
    make_argument_type_alias :ind_y, [:index]
    make_argument_type_alias :rel, [:offset]
    make_argument_type_alias :abs, [:address]
    make_argument_type_alias :abs_x, [:address]
    make_argument_type_alias :abs_y, [:address]

    #################### Single-byte no-argument opcodes
    single_byte {
      :php => 0x08,
      :clc => 0x16,
      :plp => 0x28,
      :sec => 0x38,
      :rti => 0x40,
      :pha => 0x48,
      :cli => 0x58,
      :rts => 0x60,
      :pla => 0x68,
      :sei => 0x78,
      :dey => 0x88,
      :txa => 0x8a,
      :tya => 0x98,
      :txs => 0x9a,
      :tay => 0xa8,
      :tax => 0xaa,
      :clv => 0xb8,
      :tsx => 0xba,
      :iny => 0xc8,
      :dex => 0xca,
      :cld => 0xd8,
      :inx => 0xe8,
      :nop => 0xea,
      :sed => 0xf8,
      :phy => 0x5a,
      :ply => 0x7a,
      :phx => 0xda,
      :plx => 0xfa,
      :wdm => 0x42,
    }

    #################### Two-byte immediate-argument opcodes
    
    two_byte_rel_offset [
      :bpl => 0x10,
      :bmi => 0x30,
      :bvc => 0x50,
      :bvs => 0x70,
      :bra => 0x80,
      :bcc => 0x90,
      :bcs => 0xb0,
      :bne => 0xd0,
      :beq => 0xf0
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
      :sta => {              :zp => 0x85, :zp_x => 0x95,                :abs => 0x8d, :abs_x => 0x9d, :abs_y => 0x99, :ind_x => 0x81, :ind_y => 0x91},
      :lda => {:imm => 0xa9, :zp => 0xa5, :zp_x => 0xb5,                :abs => 0xad, :abs_x => 0xbd, :abs_y => 0xb9, :ind_x => 0xa1, :ind_y => 0xb1},
      :stx => {              :zp => 0x86,                :zp_y => 0x96, :abs => 0x87,                                                               },
      :ldx => {:imm => 0xa2,              :zp_x => 0xa6, :zp_y => 0xb6,               :abs_x => 0xae, :abs_y => 0xbe,                               },
      :sty => {              :zp => 0x84, :zp_x => 0x94,                :abs => 0x8c,                                                               },
      :ldy => {:imm => 0xa0, :zp => 0xa4,                :zp_y => 0xb4, :abs => 0xbc,                 :abs_y => 0xbe,                               },
      :ora => {:imm => 0x09, :zp => 0x05, :zp_x => 0x15,                :abs => 0x0D, :abs_x => 0x1d, :abs_y => 0x19, :ind_x => 0x01, :ind_y => 0x11},
      :and => {:imm => 0x29, :zp => 0x25, :zp_x => 0x35,                :abs => 0x2d, :abs_x => 0x3d, :abs_y => 0x39, :ind_x => 0x21, :ind_y => 0x31},
      :eor => {:imm => 0x49, :zp => 0x45, :zp_x => 0x55,                :abs => 0x4d, :abs_x => 0x5d, :abs_y => 0x59, :ind_x => 0x41, :ind_y => 0x51},
      :and => {:imm => 0x29, :zp => 0x25, :zp_x => 0x35,                :abs => 0x2d, :abs_x => 0x3d, :abs_y => 0x39, :ind_x => 0x21, :ind_y => 0x31},
      :cmp => {:imm => 0xc9, :zp => 0xc5, :zp_x => 0xd5,                :abs => 0xcd, :abs_x => 0xdd, :abs_y => 0xd9, :ind_x => 0xc1, :ind_y => 0xd1},
      :cpx => {:imm => 0xe0, :zp => 0xa6,                                             :abs_x => 0xae,                                               },
      :cpy => {:imm => 0xc0, :zp => 0xc4,                                             :abs_x => 0xce,                                               },
      :adc => {:imm => 0x69, :zp => 0x65, :zp_x => 0x75,                :abs => 0x6d, :abs_x => 0x7d, :abs_y => 0x79, :ind_x => 0x61, :ind_y => 0x71},
      :sbc => {:imm => 0xe9, :zp => 0xe5, :zp_x => 0xf5,                :abs => 0xed, :abs_x => 0xfd, :abs_y => 0xf9, :ind_x => 0xe1, :ind_y => 0xf1},
      :jmp => {                                                         :abs => 0x4c, :abs_x => 0x7c,                                               },
      :inc => {              :zp => 0xe6, :zp_x => 0xf6,                :abs => 0xee, :abs_x => 0xfe,                                               },
      :dec => {              :zp => 0xc6, :zp_x => 0xd6,                :abs => 0xce, :abs_x => 0xde,                                               }
    }
  end
end