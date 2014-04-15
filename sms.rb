#
#
#
#
#

require "./arch"
require "./z80"

class SMS < Arch
	include Z80


	SMS_REGION_JAPAN = 3
	SMS_REGION_EXPORT = 4
	GG_REGION_JAPAN = 5
	GG_REGION_EXPORT = 6
	GG_REGION_INTERNATIONAL = 7

	def initialize
		super.initialize

		memory_map do
			default_slot 0
			slot_size 0x7ff0
			slot 0, 0x0000
			slot_size 0x0010
			slot 1, 0x7ff0
			slot_size 0x4000
			slot 2, 0x8000
			slot_size 0x2000
			slot 3, 0xc000
		end

		rom_bank_map do
			banks_total 32
			bank_size 0x7ff0
			banks 1
			bank_size 0x0010
			banks 1
			bank_size 0x4000
			banks 30
		end
	end

	def sms_header product_code, version, region = SMS_REGION_EXPORT
		in_slot 1 do
	    	org 0x7ff0
	    	ds "TMR SEGA  "
	    	#__ :sms_header_checksum_place
	    	db 0, 0 # checksum place
	    	db [(product_code.bcd_digit 1)<<4+(product_code.bcd_digit 0),
	    		(product_code.bcd_digit 3)<<4+(product_code.bcd_digit 2)]
	    	db ((product_code/10000).floor&0xf)<<4 + (version & 0xf)
	    	db ((region & 0xf)<<4)
    	end
		sms_checksum
    	self
	end
	def sms_checksum
		link_time do
			in_slot 1 do
				org 0x7ffa
			end
		end
		self
	end

	def sdsc_header version, date, author, name, description
		__ :sdsc_author
		dzs author
		__ :sdsc_name
		dzs name
		__ :sdsc_description
		dzs description
		org 0x7fe0
		ds "SMSC"
		dbcdb version.floor
		dbcdb (version*100)%100
		dw :author
		dw :name
		dw :description
		self
	end


end