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
    expect(@asm._rb(0)).to eq(0)
    expect(@asm._rb(1)).to eq(1)
    expect(@asm._rb(2)).to eq(0x23)
    expect(@asm._rb(3)).to eq(0x45)
    expect(@asm._rb(100)).to eq(2)
  end
  
  it "should generate labels correctly" do
    @asm.asm {
label :start
      db :end
      db :start
      db :start + 1
      org 10
label :end
    }
    expect(@asm._rb(0)).to eq(10)
    expect(@asm._rb(1)).to eq(0)
    expect(@asm._rb(2)).to eq(1)
  end
  
end