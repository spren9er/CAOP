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
    subs_operator = operator
    qcase = category.sid == 'qpolynomials'
    
    # mixin parameters
    if param.present?
      param_names = self.parameters.map(&:name) + ['n','x']
      param_names << 'q' if qcase
      subs = param_names.inject([]) do |set, param_name|
        fixparam = param_name
        varparam = param[param_name]        
        if varparam =~ /[\\a-zA-Z]+/
          subs_operator.gsub!('\\' + fixparam, '\\' + varparam) if fixparam.length > 1 and varparam.length > 1 
          subs_operator.gsub!('\\' + fixparam,  varparam) if fixparam.length > 1 and varparam.length == 1
          subs_operator.gsub!(fixparam, '\\' + varparam) if fixparam.length == 1 and varparam.length > 1
          subs_operator.gsub!(fixparam, varparam) if fixparam.length == 1 and varparam.length == 1
        else 
          subs_operator.gsub!('\\' + fixparam,  varparam) if fixparam.length > 1
          subs_operator.gsub!(fixparam,  varparam) if fixparam.length == 1 
        end
        set << ["#{fixparam} = #{varparam}"] if fixparam != varparam
        set
      end
      subs_command = "term := subs(#{subs.join(', ')}, term):\n" if subs.present?
    end
    
    # special values
    n = param['n']
    x = param['x']
    q = param['q'] if qcase

    # put input in file and set up factor
    factor = param['factor']
    if factor.present? and factor != '1'
      op = 'P'
      factor = "(#{factor})" if factor =~ /\+|\-/
      factor_latex = "P_#{n}(#{x}) = "
      input = "term := #{factor}*#{self.maple}:\n"
    else
      input = "term := #{self.maple}:\n"
    end
    input += subs_command if subs_command.present?
    
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
        when ['polynomials',  'continuous', 'diffeq'] then input += "sumdiffeq(term, k, #{op}(#{x}));"
        when ['polynomials',  'continuous', 'receq']  then input += "sumrecursion(term, k, #{op}(#{n}));"
        when ['polynomials',  'discrete',   'diffeq'] then input += "sumrecursion(term, k, #{op}(#{x}));"
        when ['polynomials',  'discrete',   'receq']  then input += "sumrecursion(term, k, #{op}(#{n}));"
        when ['qpolynomials', 'continuous', 'diffeq'] then input += "qsumdiffeq(term, #{q}, k, #{op}(#{x}));"
        when ['qpolynomials', 'continuous', 'receq']  then input += "qsumrecursion(term, #{q}, k, #{op}(#{n}), recursion = up);"
        when ['qpolynomials', 'discrete',   'diffeq'] then input += "term := subs(#{q}^(-#{x}) = #{x}, term):\nDE := qsumdiffeq(term, #{q}, k, #{op}(#{x})):\nRE := qdiffeqtorecursion(DE, #{q}):\nRE := qshiftrecursion(RE, #{q}):\nqrecursiontodiffeq(RE, #{q});"
        when ['qpolynomials', 'discrete',   'receq']  then input += "qsumrecursion(term, #{q}, k, #{op}(#{n}), recursion = up);"
      end
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
    factor_latex += Polynomial.latex(factor) + ' \cdot ' + subs_operator if factor.present? and factor != '1' 
    
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
  
  def q?
    category.sid == 'qpolynomials'
  end
  
  def qspecial?
    %w{askey_wilson continuous_dual_qhahn continuous_qhahn al_salam_chihara qmeixner_pollaczek continuous_qjacobi continuous_big_qhermite continuous_qlaguerre continuous_qhermite}.include?(sid)
  end
  
  def special?
      %w{wilson continuous_dual_hahn continuous_hahn meixner_pollaczek}.include?(sid)
  end
  
  def self.latex(term)
    input = "latex(#{term});"
    
    stamp = Time.now.to_i.to_s
    filename = 'tmp/computation' + stamp + (5*rand(9)).to_s + '.txt'
    file = File.new(filename, 'w')
    file.write input
    file.close
    
    options = ' -q '
    output = `#{MAPLE_PATH + '/maple' + options + ' < ' + filename}`.strip
    
    # delete file
    File.delete(filename)
    
    return output
  end
end