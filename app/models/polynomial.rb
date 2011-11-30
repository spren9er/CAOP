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
    op = operator[0]
    
    # mixin parameters
    if parameters.present?
      # subs = []
      (self.parameters.map(&:name) + ['n','x']).inject([]) do |r, subs|
        subs << "#{fixparam} = #{varparam}" if fixparam != varparam
      end
      # (self.parameters.map(&:name) + ['n','x']).each do |param_name|
      #   fixparam = param_name
      #   varparam = parameters[param_name]
      #   subs << "#{fixparam} = #{varparam}" if fixparam != varparam
      # end
      subs_command = "term := subs(#{subs.join(', ')}, term):\n" if subs.present?
    end
    
    # put input in file
    input = "term := #{self.maple}:\n"
    input += subs_command if subs_command.present?

    # choose appropriate commands
    case [category.sid, type, equation_type[:receq] ? 'receq' : 'diffeq']
      when ['polynomials', 'continuous', 'diffeq']   then input += "sumdiffeq(term, k, #{op}(x));"
      when ['polynomials', 'continuous', 'receq']    then input += "sumrecursion(term, k, #{op}(n));"
      when ['polynomials', 'discrete', 'diffeq']     then input += "sumrecursion(term, k, #{op}(x));"
      when ['polynomials', 'discrete', 'receq']      then input += "sumrecursion(term, k, #{op}(n));"
      when ['qpolynomials', 'continuous', 'diffeq']  then input += "qsumdiffeq(term, q, k, #{op}(x));"
      when ['qpolynomials', 'continuous', 'receq']   then input += "qsumrecursion(term, q, k, #{op}(n));"
      when ['qpolynomials', 'discrete', 'diffeq']    then input += "term := subs(q^(-x) = x, term):\nDE := qsumdiffeq(term, q, k, #{op}(x)):\nsubs(x = q^(-x), DE);"
      when ['qpolynomials', 'discrete', 'receq']     then input += "term := subs(q^(-x) = x, term):\nRE := qsumrecursion(term, q, k, #{op}(n)):\nsubs(x = q^(-x), RE);"
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
        
    # prepend package import
    package = (category.sid == 'polynomials') ? "> read \"hsum.mpl\":\n" : "> read \"qsum.mpl\":\n"
    input = package + input   
    input.gsub!(/\n/,"\n> ")
    
    # write raw output in input
    input += "\n\n" + output.gsub(/\n/,"")
    
    # latex conversion preparation
    file = File.open(filename, 'w')
    file.truncate(0)
    file.write "latex(#{output});"
    file.close

    # latex conversion
    output = `#{MAPLE_PATH + '/maple' + options + ' < ' + filename}`
        
    # delete file
    File.delete(filename)
    
    #
    # substitutions (order of substitutions is relevant!!!)
    #
    regexp = Regexp.new("(\\left\s*\()*(\s*\{\\it\s*Dq\}_\{\{)(x[,x]*)(\}\})(\s*\\right\s*\))*(\s*\\left\s*\(\s*)(#{op}\s*\\left\(\s*x\s*\\right\s*\))(\s*\\right\s*\))")
    output = output.scan(regexp).uniq.inject(output) do |s, r| 
      xcount = r[2].gsub(/\s/,"").split(',').count
      if type == 'continuous'
        # q-derivatives: qdiff -> D_q
        expr = "D_q #{r[6]}"
        expr = "D_q^#{xcount} #{r[6]}" if xcount > 1
      elsif type == 'discrete'
        # q-differences: qdiff -> y(x + ...)  
        expr = "#{op}\\left( x \\right)"
        expr = "#{op}\\left( x + #{xcount} \\right)" if xcount > 0
      end
      s.gsub(r.join, expr) 
    end 
    
    # differences: y(x + ...) -> self.operator
    regexp = Regexp.new("(#{op}\s*\\left*\(\s*)(x\s*\+*\s*\d*)(\s*\\right*\))")
    output = output.scan(regexp).uniq.inject(output) do |s, r| s.gsub(r.join, self.operator.gsub('x', "{#{r[1]}}")) end
    
    # shifts: y(n +- ...) -> self.operator
    regexp = Regexp.new("(#{op}\s*\\left*\(\s*)(n\s*\+*\s*\d*)(\s*\\right*\))")
    output = output.scan(regexp).uniq.inject(output) do |s, r| s.gsub(r.join, self.operator.gsub('n', "{#{r[1]}}")) end
    regexp = Regexp.new("(#{op}\s*\\left*\(\s*)(n\s*\-*\s*\d*)(\s*\\right*\))")
    output = output.scan(regexp).uniq.inject(output) do |s, r| s.gsub(r.join, self.operator.gsub('n', "{#{r[1]}}")) end
    
    # functions: y(x) -> self.operator
    regexp = Regexp.new("#{op}\s*\\left*\(\s*x\s*\\right*\)")
    output.gsub!(regexp, self.operator)
      
    return [input, output]
  end
  
  def operator
    /^([^=]*)=.*$/.match(self.definition)[1]
  end
end