require "rubygems"
require "spec"

require "./assembler"

describe Assembler::Assembler, "assembly tests" do
  
  before(:each) do
    @asm = Assembler::Assembler.new
  end
  
  it "should generate bytecodes and orgs correctly" do
    @asm.asm {
        db 0
        db 1
        dw 0x2345
        org 100
        db 2
      }
    @asm._rb(0).should == 0
    @asm._rb(1).should == 1
    @asm._rb(2).should == 0x23
    @asm._rb(3).should == 0x45
    @asm._rb(100).should == 2
  end
  
  it "should generate labels correctly" do
    @asm.asm {
      __ :start
      db :end
      db :start
      db :start + 1
      org 10
      __ :end
    }
    @asm._rb(0).should == 10
    @asm._rb(1).should == 0
    @asm._rb(2).should == 1
  end
  
end