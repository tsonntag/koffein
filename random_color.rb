# coding: utf-8

class Arr
  def initialize els, indent = ""
    @els = els
    @indent = indent
  end

  def elmed 
    if @els.is_a? String
#     @indent + "" + @els
      @els
    else
      ar = @els.map do |el|
        Arr.new(el,"  ").elmed()
      end.join( "\n" + @indent + ", " )
      "[" + 
        "\n" +
        @indent + "  " + ar + "\n" +
        @indent + "]"
    end
  end

=begin
  def elmed
    lines = @els.each_with_index.map do |el,i| 
       if el.is_a? String
      #   @indent + (i == 0 ? "  " : ", ") + el 
         el
       else
          Arr.new(el, "  ").elmed
      end
    end
    lines = lines.each_with_index.map do |el,i| 
      if el.is_a? String
         @indent + (i == 0 ? "  " : ", ") + el 
      else
        el
      end
    end
    [@indent + "["] +  lines + [@indent + "]"]
  end
=end

end
 
class ColorList
  def initialize(nitem)
    @nitem = nitem
  end

  @@colors = %w( red green black magenta darkorange gold )
  @@farben = %w( rot grün gelb blau schwarz weiß lila )

  @@ncolors = @@colors.size
  @@nfarben = @@farben.size

  def list 
    (1..@nitem).map do |i|
      rcolor = @@colors[Random.rand(@@ncolors)]
      rfarbe = @@farben[Random.rand(@@nfarben)]
        "( \"#{rfarbe}\", \"#{rcolor}\" )"
    end
  end

  def self.list nitem
    ColorList.new(nitem).list
  end
end

nitem = ARGV[0].to_i
nlist = ARGV[1].to_i

#puts Arr.elmed(ColorList.list(nitem))

#puts "==="
#lists = (1..nlist).map do |l|
#  x = ColorList.list(nitem)
#  y = Arr.elmed(x, "")
#  puts "Y"
#  puts y.class
#  puts y
#  y
#end
#
#puts "XXXX"
#puts Arr.new(lists).elmed("  ")
#
#puts Arr.new(%w(A B)).elmed
puts Arr.new([%w(A B), %w(C D)]).elmed


