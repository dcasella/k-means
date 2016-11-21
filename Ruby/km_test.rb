#!/usr/bin/ruby
#encoding utf-8

require_relative 'km'

if ARGV.length > 1
	file_obj = File.new(ARGV[0], "r")
	observations = []
	k = ARGV[1].to_i
	
	file_obj.each_line do |line|
		line.chomp!
		observations[file_obj.lineno - 1] = line.split(" ").map(&:to_f)
	end
	
	print km(observations, k), "\n"
	
	file_obj.close
end
