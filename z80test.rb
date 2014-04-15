require "rubygems"
require "spec"

require "./z80"
include Assembler

describe Z80, "assembly tests" do
  
  before(:each) do
    @asm = Z80.new
  end
  
  it "should generate bytecodes and orgs correctly" do
    with(@asm){
        nop
        rlca
        org 100
        rst 0x38
      }._rb(0).should == 0
    @asm._rb(1).should == 7
    @asm._rb(100).should == 0xff
  end
  
  it "should generate opcode variations correctly" do
    with(@asm){
      ld a, 0
      ld a, b
      #ld a, [0]
      ld [0], a
    }
    @asm._rb(0).should == 0x3e
    @asm._rb(1).should == 0
  end
  
end