class Polynomial
  include Mongoid::Document
  
  embeds_many :parameters
  field :name,        type: String
  field :sid,         type: String
  field :definition,  type: String
  field :maple,       type: String
  field :type,        type: String
  field :x_min,       type: Float
  field :x_max,       type: Float
  field :position,    type: Integer
  
  belongs_to :category  

  # MAPLE_PATH = '/usr/local/maple15/bin'
  MAPLE_PATH = '/Library/Frameworks/Maple.framework/Versions/15/bin'
  
  def compute(param, equation_type)
    op = operator[0]
    qcase = category.sid == 'qpolynomials'
    factor = param['factor']

    # set up factor and put input in file
    if factor.present? and factor != '1'
      op = 'P'
      factor = "(#{factor})" if factor =~ /\+|\-/
      factor_latex = "P_{n}(x) = #{Polynomial.latex(factor)} \\cdot #{operator}" 
      input = "term := #{factor}*#{self.maple}:\n"
    else
      input = "term := #{self.maple}:\n"
    end
    
    # mixin parameters
    if param.present?
      param_names = self.parameters.map(&:name) + ['n','x']
      param_names << 'q' if qcase
      subs = param_names.inject([]) do |set, param_name|
        fixparam = param_name
        varparam = param[param_name] 
        if factor_latex    
          if varparam =~ /[\\a-zA-Z]+/
            factor_latex.gsub!('\\' + fixparam, '\\' + varparam) if fixparam.length > 1 and varparam.length > 1 
            factor_latex.gsub!('\\' + fixparam,  varparam) if fixparam.length > 1 and varparam.length == 1
            factor_latex.gsub!(fixparam, '\\' + varparam) if fixparam.length == 1 and varparam.length > 1
            factor_latex.gsub!(fixparam, varparam) if fixparam.length == 1 and varparam.length == 1
          else 
            factor_latex.gsub!('\\' + fixparam,  varparam) if fixparam.length > 1
            factor_latex.gsub!(fixparam,  varparam) if fixparam.length == 1 
          end
        end
        set << ["#{fixparam} = #{varparam}"] if fixparam != varparam
        set
      end
      subs_command = "term := subs(#{subs.join(', ')}, term):\n" if subs.present?
    end
    input += subs_command if subs_command.present?
    
    # special values
    reg = %r{[a-zA-Z]+}
    n = param['n']
    arg_n = reg.match(n).try('[]', 0)
    x = param['x']
    arg_x = reg.match(x).try('[]', 0)
    q = param['q'] if qcase
    
    # special polynomials
    if self.special? and equation_type[:diffeq]
      input += "term := subs(#{x} = I*y, term):\n"
      input += "DE := sumrecursion(term, k, #{op}(y)):\n"
      input += "subs(y = #{x}/I, DE);" 
    elsif self.qspecial? and equation_type[:diffeq]
      # do nothing
    else
      # choose appropriate commands
      case [category.sid, type, equation_type[:receq] ? 'receq' : 'diffeq']
        when ['polynomials',  'continuous', 'diffeq'] then input += "sumdiffeq(term, k, #{op}(#{arg_x}));"
        when ['polynomials',  'continuous', 'receq']  then input += "sumrecursion(term, k, #{op}(#{arg_n}));"
        when ['polynomials',  'discrete',   'diffeq'] then input += "sumrecursion(term, k, #{op}(#{arg_x}));"
        when ['polynomials',  'discrete',   'receq']  then input += "sumrecursion(term, k, #{op}(#{arg_n}));"
        when ['qpolynomials', 'continuous', 'diffeq'] then input += "qsumdiffeq(term, #{q}, k, #{op}(#{arg_x}));"
        when ['qpolynomials', 'continuous', 'receq']  then input += "qsumrecursion(term, #{q}, k, #{op}(#{arg_n}), recursion = up);"
        when ['qpolynomials', 'discrete',   'diffeq'] then input += "term := subs(#{q}^(-#{x}) = #{arg_x}, term):\nDE := qsumdiffeq(term, #{q}, k, #{op}(#{arg_x})):\nRE := qdiffeqtorecursion(DE, #{q}):\nRE := qshiftrecursion(RE, #{q}):\nqrecursiontodiffeq(RE, #{q});"
        when ['qpolynomials', 'discrete',   'receq']  then input += "qsumrecursion(term, #{q}, k, #{op}(#{arg_n}), recursion = up);"
      end
    end
      
    stamp = Time.now.to_i.to_s
    filename = 'tmp/computation' + stamp + rand(9999999999999999).to_s + '.txt'
    file = File.new(filename, 'w')
    file.write input
    file.close
        
    # compute
    options = qcase ? ' -qi lib/maple/qsum15.mpl' : ' -qi lib/maple/hsum15.mpl'
    options += ' -c"interface(prettyprint=false)"'
    output = `#{MAPLE_PATH + '/maple' + options + ' < ' + filename}`    
        
    # delete file
    File.delete(filename)
    
    # prepend package import
    package = qcase ? "> read \"qsum.mpl\":\n" : "> read \"hsum.mpl\":\n"
    input = package + input   
    input.gsub!(/\n/,"\n> ")
    
    # write raw output in input
    input += "\n\n" + output.gsub(/\n/,"")
    
    # latex conversion
    output = Polynomial.latex(output);
    
    # TODO: \left( \delta \right) - substitution due to maple bug
          
    return {:input => input, :output => output, :factor => factor_latex}
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
      when ['polynomials',  'discrete',   'diffeq'] then input += "type(ratio(term,#{x}), ratpoly);"
      when ['polynomials',  'discrete',   'receq']  then input += "type(ratio(term,#{n}), ratpoly);"
      when ['qpolynomials', 'continuous', 'diffeq'] then input += "type(qsimpcomb(qdiff(term, #{x}, #{q})/term), ratpoly);"
      when ['qpolynomials', 'continuous', 'receq']  then input += "type(qratio(term,#{n}), ratpoly(anything, #{q}^anything));"
      when ['qpolynomials', 'discrete',   'diffeq'] then input += "type(qratio(term,#{x}), ratpoly(anything, #{q}^anything));"
      when ['qpolynomials', 'discrete',   'receq']  then input += "type(qratio(term,#{n}), ratpoly(anything, #{q}^anything));"    
    end 
        
    stamp = Time.now.to_i.to_s
    filename = 'tmp/computation' + stamp + rand(9999999999999999).to_s + '.txt'
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
  
  def q?
    category.sid == 'qpolynomials'
  end
  
  def qspecial?
    %w{askey_wilson continuous_dual_qhahn continuous_qhahn al_salam_chihara qmeixner_pollaczek continuous_qjacobi continuous_big_qhermite continuous_qlaguerre continuous_qhermite}.include?(sid)
  end
  
  def special?
      %w{wilson continuous_dual_hahn continuous_hahn meixner_pollaczek}.include?(sid)
  end
  
  def plot_points(param, factor=nil)
    numpoints = 150

    # initialization
    input = "Digits:= 50: M := 6: x0 := #{self.x_min}: x1 := #{self.x_max}:\n"
    input += factor.present? ? "term := (#{factor})*#{self.maple}:\n" : "term := #{self.maple}:\n"

    # mixin parameters
    subs = param.inject([]) do |set, p|
      set << ["#{p[0]} = #{p[1].to_f}"] 
      set
    end if param.present?
    subs_command = "term := subs(#{subs.join(', ')}, term):\n" if subs.present?
    input += subs_command if subs_command.present?    
    input += "[seq(op(1,op(1,plot(Sum(subs(n=j,term),k=0..j),x=x0..x1,numpoints=#{numpoints}))), j=1..M)];"

    points = Polynomial.compute(input)
    points = points.gsub("HFloat(undefined)", '0').gsub("\n",'').gsub('\\','')
    points
  end
  
  def self.latex(term)
    Polynomial.compute "latex(#{term});"
  end
  
  def self.compute(input)
    stamp = Time.now.to_i.to_s
    filename = 'tmp/computation' + stamp + rand(9999999999999999).to_s + '.txt'
    file = File.new(filename, 'w')
    file.write input
    file.close
    
    options = ' -q '
    options += ' -c"interface(prettyprint=0, labeling=false, display_zero_complex_part=false)"'
    output = `#{MAPLE_PATH + '/maple' + options + ' < ' + filename}`.strip
    
    # delete file
    File.delete(filename)
    
    return output
  end
end
