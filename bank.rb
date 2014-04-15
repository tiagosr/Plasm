#
# Ruby Macro Assembler
# (c)2011 Tiago Rezende
#
# Bank operations
#

require "./section"

module Assembler
	class Bank
		def initialize size, opts = {}
			@size = size
			@image = nil
			@sections = []
			@ranges = []
			@free_sections = []
			@free_ranges = [[0, size]]
		end

		def add_section section
			if section.force
				@sections.push section
			else
				@free_sections.push section
			end
		end

		def write_out
			@image = Array.new(@size)
			@sections.each do |section|
				stream = section.stream_out
				@image.slice!(section.start, stream.length)
				@image.insert(section.start, *stream)
				@ranges.insert([section.start, stream.length])
			end
		end
	end

	class BankSection < Section
		def initialize bank, origin = 0
			@bank = bank
			@cursor = 0
			@origin = origin
			@labels = {}
		end
		
		def set_origin origin
			@origin = origin
		end

		def seek point
			@cursor = point - @origin
		end

		def stream byte
			@bank.image[cursor] = byte
		end
	end
end