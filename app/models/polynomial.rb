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
      when ['polynomials', 'continuous', 'diffeq']   then input += "latex(sumdiffeq(term, k, y(x)));"
      when ['polynomials', 'continuous', 'receq']    then input += "latex(sumrecursion(term, k, S(n)));"
      when ['polynomials', 'discrete', 'diffeq']     then input += "latex(sumrecursion(term, k, y(x)));"
      when ['polynomials', 'discrete', 'receq']      then input += "latex(sumrecursion(term, k, S(n)));"
      when ['qpolynomials', 'continuous', 'diffeq']  then input += "latex(qsumdiffeq(term, q, k, y(x)));"
      when ['qpolynomials', 'continuous', 'receq']   then input += "latex(qsumrecursion(term, q, k, S(n)));"
      when ['qpolynomials', 'discrete', 'diffeq']    then input += "latex(qsumdiffeq(term, q, k, y(x)));"
      when ['qpolynomials', 'discrete', 'receq']     then input += "latex(qsumrecursion(term, q, k, S(n)));"
    end
    
    stamp = Time.now.to_i.to_s
    filename = 'tmp/computation' + stamp + (5*rand(9)).to_s + '.txt'
    file = File.new(filename, 'w')
    file.write input
    file.close
    
    # compute
    options = (category.sid == 'polynomials') ? ' -qi lib/maple/hsum.mpl' : ' -qi lib/maple/qsum.mpl'
    output = `#{MAPLE_PATH + '/maple' + options + ' < ' + filename}`
    
    # delete file
    File.delete(filename)
    
    #
    # substitutions
    #
    
    # functions
    output.gsub!(/y\s*\\left*\(\s*x\s*\\right*\)/, self.operator)
    
    # differences
    regexp = /(y\s*\\left*\(\s*)(x\s*\+*\s*\d*)(\s*\\right*\))/
    output = output.scan(regexp).uniq.inject(output) do |s, r| s.gsub(r.join, self.operator.gsub('x', "{#{r[1]}}")) end
    
    # shifts
    regexp = /(S\s*\\left*\(\s*)(n\s*\+*\s*\d*)(\s*\\right*\))/
    output = output.scan(regexp).uniq.inject(output) do |s, r| s.gsub(r.join, self.operator.gsub('n', "{#{r[1]}}")) end
    regexp = /(S\s*\\left*\(\s*)(n\s*\-*\s*\d*)(\s*\\right*\))/
    output = output.scan(regexp).uniq.inject(output) do |s, r| s.gsub(r.join, self.operator.gsub('n', "{#{r[1]}}")) end
      
    # q-derivatives  
    regexp = /(\{\\it\sqdiff\}\s*\\left\()([^\,]*)(,)(\s*[x\s*,]*)(,)(\s*)(\S*)(\s*\\right\))/
    output = output.scan(regexp).uniq.inject(output) do |s, r| 
      exponent = r[3].gsub(/\s/,"").split(',').count
      expr = "D_#{r[6]} #{r[1]}"
      expr = "D_#{r[6]}^#{exponent} #{r[1]}" if exponent > 1
      s.gsub(r.join, expr) 
    end
      
    return output
  end
  
  def operator
    /^([^=]*)=.*$/.match(self.definition)[1]
  end
end