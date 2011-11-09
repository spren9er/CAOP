class Polynomial
  include Mongoid::Document
  
  embeds_many :parameters
  field :name,        type: String
  field :sid,         type: String
  field :definition,  type: String
  field :maple,       type: String
  
  belongs_to :category  

  # MAPLE_PATH = 'usr/local/maple12/bin'
  MAPLE_PATH = '/Library/Frameworks/Maple.framework/Versions/15/bin'
  
  def compute(parameters = nil, type)
    # mixin parameters
    if parameters.present?
      subs = []
      self.parameters.each do |param|
        if parameters.has_key?(param.name)
          subs << "#{param.name} = #{parameters[param.name]}"
        end
      end
      subs_command = "term := subs(#{subs.join(', ')}, term):\n"
    end
    
    # put input in file
    input = "term := #{self.maple}:\n"
    # input += subs_command if subs_command.present?
    
    if type[:receq]
      input += "latex(sumrecursion(term, k, S(n)));"
    elsif type [:diffeq]
      input += "latex(sumdiffeq(term, k, y(x)));"
    end
    
    stamp = Time.now.to_i.to_s
    filename = 'tmp/computation' + stamp + (5*rand(9)).to_s + '.txt'
    file = File.new(filename, 'w')
    file.write input
    file.close
    
    # compute
    options = ' -qi lib/hsum13.mpl'
    output = `#{MAPLE_PATH + '/maple' + options + ' < ' + filename}`
    
    # delete file
    File.delete(filename)
    
    # substitutions
    output.gsub!(/y\s*\\left*\(\s*x\s*\\right*\)/, self.operator)
    regexp = /(S\s*\\left*\(\s*)(n\s*\+*\s*\d*)(\s*\\right*\))/
    output = output.scan(regexp).uniq.inject(output) do |s, r| s.gsub(r.join, self.operator.gsub('n', "{#{r[1]}}")) end
      
    return output
  end
  
  def operator
    /^([^=]*)=.*$/.match(self.definition)[1]
  end
end