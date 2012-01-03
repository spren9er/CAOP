class Parameter
  include Mongoid::Document
  embedded_in :polynomial
  field :name,   type: String
  field :lower_bound, type: String
  field :upper_bound, type: String
  
  def boundaries
    case upper_bound
      when 'pi'
        return "#{lower_bound} < #{latex_name} < \\pi"
      when 'infinity'
        return "#{latex_name} > #{lower_bound}"
      else
        return "#{lower_bound} < #{latex_name} < #{upper_bound}"
    end
  end
  
  def latex_name
    name.length > 1 ? "\\#{name}" : name
  end
end