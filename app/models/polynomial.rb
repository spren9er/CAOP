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
  
  def compute(parameters = nil)
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
    input += "latex(sumrecursion(term, k, S(n)));"
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
    
    return output
    # Stalker.enqueue("polynomial.maple_computation", :filename => filename)
  end
end