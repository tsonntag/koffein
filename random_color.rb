# coding: utf-8

class Color
  class << self

    @@colors = %w( red green black magenta darkorange gold )
    @@farben = %w( rot grün gelb blau schwarz weiß lila )

    @@ncolors = @@colors.size
    @@nfarben = @@farben.size

    def list n
      lines = (1..n).map do
        rcolor = @@colors[Random.rand(@@ncolors)]
        rfarbe = @@farben[Random.rand(@@nfarben)]
        "  ( \"#{rfarbe}\", \"#{rcolor}\" )"
      end
      [ "[" ] + lines + [ "]"]
    end

    def lists n, nlist
      lists = (1..nlist).map do
        list n
      end

      [ "[" ] + lists + [ "]"]
    end
  end
end

n = ARGV[0].to_i
nlist = ARGV[1].to_i

puts Color.lists(n, nlist)
