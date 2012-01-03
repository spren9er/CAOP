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
  
  def compute(param, equation_type)
    op = operator[0]
    qcase = category.sid == 'qpolynomials'
    
    # mixin parameters
    if param.present?
      param_names = self.parameters.map(&:name) + ['n','x']
      param_names << 'q' if qcase
      subs = param_names.inject([]) do |set, param_name|
        fixparam = param_name
        varparam = param[param_name]        
        set << ["#{fixparam} = #{varparam}"] if fixparam != varparam
        set
      end
      subs_command = "term := subs(#{subs.join(', ')}, term):\n" if subs.present?
    end
    
    # special values
    n = param['n']
    x = param['x']
    q = param['q'] if qcase
    factor = param['factor']
    factor = "(#{factor})" if factor =~ /\+|\-/
    
    # put input in file
    input = factor.present? ? "term := #{factor}*#{self.maple}:\n" : "term := #{self.maple}:\n"
    input += subs_command if subs_command.present?

    # choose appropriate commands
    case [category.sid, type, equation_type[:receq] ? 'receq' : 'diffeq']
      when ['polynomials',  'continuous', 'diffeq'] then input += "sumdiffeq(term, k, #{op}(#{x}));"
      when ['polynomials',  'continuous', 'receq']  then input += "sumrecursion(term, k, #{op}(#{n}));"
      when ['polynomials',  'discrete',   'diffeq'] then input += "sumrecursion(term, k, #{op}(#{x}));"
      when ['polynomials',  'discrete',   'receq']  then input += "sumrecursion(term, k, #{op}(#{n}));"
      when ['qpolynomials', 'continuous', 'diffeq'] then input += "qsumdiffeq(term, #{q}, k, #{op}(#{x}));"
      when ['qpolynomials', 'continuous', 'receq']  then input += "qsumrecursion(term, #{q}, k, #{op}(#{n}), recursion = up);"
      when ['qpolynomials', 'discrete',   'diffeq'] then input += "term := subs(#{q}^(-#{x}) = #{x}, term):\nDE := qsumdiffeq(term, #{q}, k, #{op}(#{x})):\nRE := qdiffeqtorecursion(DE, #{q}):\nRE := qshiftrecursion(RE, #{q}):\nsubs(#{x} = #{q}^(-#{x}), RE);"
      when ['qpolynomials', 'discrete',   'receq']  then input += "term := subs(#{q}^(-#{x}) = #{x}, term):\nRE := qsumrecursion(term, #{q}, k, #{op}(#{n}), recursion = up):\nsubs(#{x} = #{q}^(-#{x}), RE);"
    end
    
    stamp = Time.now.to_i.to_s
    filename = 'tmp/computation' + stamp + (5*rand(9)).to_s + '.txt'
    file = File.new(filename, 'w')
    file.write input
    file.close
    
    # compute
    options = qcase ? ' -qi lib/maple/qsum15.mpl' : ' -qi lib/maple/hsum15.mpl'
    options += ' -c"interface(prettyprint=false)"'
    output = `#{MAPLE_PATH + '/maple' + options + ' < ' + filename}`    
        
    # prepend package import
    package = qcase ? "> read \"qsum.mpl\":\n" : "> read \"hsum.mpl\":\n"
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
    # output.gsub!(/\n|\s/, '').gsub!(/\\it/, '\it ')
    # delete file
    File.delete(filename)
    
    raise "ERROR" if output =~ /[e|E]{1}rror/

    # substitutions (order of substitutions is relevant!!!)
    #
    # regexp = Regexp.new("(\\left\()*(\{\\it Dq\}_\{\{)(#{x}[,#{x}]*)(\}\})(\\right\))*(\\left\()(#{op}\\left\(#{x}\\right\))(\\right\))")
    # output = output.scan(regexp).uniq.inject(output) do |s, r| 
    #   xcount = r[2].gsub(/\s/,"").split(',').count
    #   if type == 'continuous'
    #     # q-derivatives: qdiff -> Dq
    #     expr = "D_q #{r[6]}"
    #     expr = "D_q^#{xcount} #{r[6]}" if xcount > 1
    #   elsif type == 'discrete'
    #     # q-differences: qdiff -> y(x + ...)  
    #     expr = "#{op}\\left( #{x} \\right)"
    #     expr = "#{op}\\left( #{x} + #{xcount} \\right)" if xcount > 0
    #   end
    #   s.gsub(r.join, expr) 
    # end 
    # 
    # # q-shifts: invSq -> y(x + ...)
    # regexp = Regexp.new("(\\left\()*(\{\\it invSq\}_\{\{)(#{x}[,#{x}]*)(\}\})(\\right\))*(\\left\()(#{op}\\left\(#{x}\\right\))(\\right\))")
    # output = output.scan(regexp).uniq.inject(output) do |s, r| 
    #   xcount = r[2].gsub(/\s/,'').split(',').count
    #   s.gsub(r.join, expr) 
    # end
    # 
    # # self.operator with substituted values
    # operatr = self.operator.gsub('n', n).gsub('x', x)
    # operatr.gsub!('q', q) if qcase
    # 
    # # prefactor
    # prefactor = factor.present? ? factor + ' \cdot ' : ''
    # 
    # # differences: y(x + ...) -> self.operator
    # regexp = Regexp.new("(#{op}\\\\left*\\()(#{x}\\+\\d*)(\\\\right*\\))")
    # output = output.scan(regexp).uniq.inject(output) do |s, r| s.gsub(r.join, prefactor + operatr.gsub(x, "{#{r[1]}}")) end
    #   
    # # shifts: y(n +- ...) -> self.operator
    # regexp = Regexp.new("(#{op}\\\\left*\\()(#{n}\\+\\d*)(\\\\right*\\))")
    # output = output.scan(regexp).uniq.inject(output) do |s, r| s.gsub(r.join, prefactor + operatr.gsub(n, "{#{r[1]}}")) end
    # regexp = Regexp.new("(#{op}\\\\left*\\()(#{n}\\-\\d*)(\\\\right*\\))")
    # output = output.scan(regexp).uniq.inject(output) do |s, r| s.gsub(r.join, prefactor + operatr.gsub(n, "{#{r[1]}}")) end
    # 
    # # functions: y(x|n) -> self.operator
    # regexp = Regexp.new("#{op}\\\\left*\\([#{n}|#{x}]\\\\right*\\)")
    # output.gsub!(regexp, prefactor + operatr)
          
    return [input, output]
  end
  
  def hyper_check(param, equation_type)
    return true if param['factor'].blank?
    
    qcase = category.sid == 'qpolynomials'    
    n = param['n']
    x = param['x']
    q = param['q']

    input = "term := #{param['factor']}:\n"
    
    # choose appropriate commands
    case [category.sid, type, equation_type[:receq] ? 'receq' : 'diffeq']
      when ['polynomials',  'continuous', 'diffeq'] then input += "type(simpcomb(diff(term, #{x})/term), ratpoly);"
      when ['polynomials',  'continuous', 'receq']  then input += "type(ratio(term,#{n}), ratpoly);"
      when ['polynomials',  'discrete',   'diffeq'] then input += ""
      when ['polynomials',  'discrete',   'receq']  then input += ""
      when ['qpolynomials', 'continuous', 'diffeq'] then input += "type(qsimpcomb(qdiff(term, #{x}, #{q})/term), ratpoly);"
      when ['qpolynomials', 'continuous', 'receq']  then input += "type(qratio(term,#{n}), ratpoly);"
      when ['qpolynomials', 'discrete',   'diffeq'] then input += ""
      when ['qpolynomials', 'discrete',   'receq']  then input += ""    
    end 
        
    stamp = Time.now.to_i.to_s
    filename = 'tmp/computation' + stamp + (5*rand(9)).to_s + '.txt'
    file = File.new(filename, 'w')
    file.write input
    file.close
    
    options = qcase ? ' -qi lib/maple/qsum15.mpl' : ' -qi lib/maple/hsum15.mpl'
    options += ' -c"interface(prettyprint=false)"'
    output = `#{MAPLE_PATH + '/maple' + options + ' < ' + filename}`.strip
    
    # delete file
    File.delete(filename)
    
    return output == 'true'
  end
  
  def operator
    /^([^=]*)=.*$/.match(self.definition)[1]
  end
end