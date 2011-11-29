class Polynomial
  include Mongoid::Document
  
  embeds_many :parameters
  field :name,        type: String
  field :sid,         type: String
  field :definition,  type: String
  field :maple,       type: String
  field :type,        type: String
  
  belongs_to :category  

  # MAPLE_PATH = 'usr/local/maple12/bin'
  MAPLE_PATH = '/Library/Frameworks/Maple.framework/Versions/15/bin'
  
  def compute(parameters = nil, equation_type, factor)
    # mixin parameters
    if parameters.present?
      subs = []
      self.parameters.each do |param|
        if parameters.has_key?(param.name)
          subs << "#{param.name} = #{parameters[param.name]}"
        end
      end
      subs_command = "term := subs(#{subs.join(', ')}, term):\n" if subs.present?
    end
    
    # put input in file
    input = "term := #{self.maple}:\n"
    input += subs_command if subs_command.present?

    # choose appropriate commands
    case [category.sid, type, equation_type[:receq] ? 'receq' : 'diffeq']
      when ['polynomials', 'continuous', 'diffeq']   then input += "sumdiffeq(term, k, y(x));"
      when ['polynomials', 'continuous', 'receq']    then input += "sumrecursion(term, k, S(n));"
      when ['polynomials', 'discrete', 'diffeq']     then input += "sumrecursion(term, k, y(x));"
      when ['polynomials', 'discrete', 'receq']      then input += "sumrecursion(term, k, S(n));"
      when ['qpolynomials', 'continuous', 'diffeq']  then input += "qsumdiffeq(term, q, k, y(x));"
      when ['qpolynomials', 'continuous', 'receq']   then input += "qsumrecursion(term, q, k, S(n));"
      when ['qpolynomials', 'discrete', 'diffeq']    then input += "term := subs(q^(-x) = x, term):\n qsumdiffeq(term, q, k, y(x));"
      when ['qpolynomials', 'discrete', 'receq']     then input += "term := subs(q^(-x) = x, term):\n qsumrecursion(term, q, k, S(n));"
    end
    
    stamp = Time.now.to_i.to_s
    filename = 'tmp/computation' + stamp + (5*rand(9)).to_s + '.txt'
    file = File.new(filename, 'w')
    file.write input
    file.close
    
    # compute
    options = (category.sid == 'polynomials') ? ' -qi lib/maple/hsum.mpl' : ' -qi lib/maple/qsum.mpl'
    options += ' -c"interface(prettyprint=false)"'
    output = `#{MAPLE_PATH + '/maple' + options + ' < ' + filename}`.strip
    
    # write raw output in input
    input += "\n\n> " + output.gsub(/\n/,"")
    
    # prepend package import
    package = (category.sid == 'polynomials') ? "read \"hsum.mpl\":\n" : "read \"qsum.mpl\":\n"
    input = package + input   
    
    # latex conversion
    file = File.open(filename, 'w')
    file.truncate(0)
    file.write "latex(#{output});"
    file.close

    output = `#{MAPLE_PATH + '/maple' + options + ' < ' + filename}`
        
    # delete file
    # File.delete(filename)
    
    #
    # substitutions (order of substitutions is relevant!!!)
    #
    regexp = /(\{\\it\s*qdiff\}\s*\\left\()([^\,]*)(,)(\s*[x\s*,]*)(,)(\s*)(\S*)(\s*\\right\))/
    output = output.scan(regexp).uniq.inject(output) do |s, r| 
      xcount = r[3].gsub(/\s/,"").split(',').count
      if type == 'continuous'
        # q-derivatives: qdiff -> D_q
        expr = "D_#{r[6]} #{r[1]}"
        expr = "D_#{r[6]}^#{xcount} #{r[1]}" if xcount > 1
      elsif type == 'discrete'
        # q-differences: qdiff -> y(x + ...)  
        expr = "y\\left( x \\right)"
        expr = "y\\left( x + #{xcount} \\right)" if xcount > 0
      end
      s.gsub(r.join, expr) 
    end 
    
    # differences: y(x + ...) -> self.operator
    regexp = /(y\s*\\left*\(\s*)(x\s*\+*\s*\d*)(\s*\\right*\))/
    output = output.scan(regexp).uniq.inject(output) do |s, r| s.gsub(r.join, self.operator.gsub('x', "{#{r[1]}}")) end
    
    # shifts: S(n +- ...) -> self.operator
    regexp = /(S\s*\\left*\(\s*)(n\s*\+*\s*\d*)(\s*\\right*\))/
    output = output.scan(regexp).uniq.inject(output) do |s, r| s.gsub(r.join, self.operator.gsub('n', "{#{r[1]}}")) end
    regexp = /(S\s*\\left*\(\s*)(n\s*\-*\s*\d*)(\s*\\right*\))/
    output = output.scan(regexp).uniq.inject(output) do |s, r| s.gsub(r.join, self.operator.gsub('n', "{#{r[1]}}")) end
    
    # functions: y(x) -> self.operator
    output.gsub!(/y\s*\\left*\(\s*x\s*\\right*\)/, self.operator)
      
    return [input, output]
  end
  
  def operator
    /^([^=]*)=.*$/.match(self.definition)[1]
  end
end