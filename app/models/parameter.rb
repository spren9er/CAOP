class Parameter
  include Mongoid::Document
  embedded_in :polynomial
  field :name,   type: String
  field :lower_bound, type: Float
  field :upper_bound, type: Float
  
  def boundaries
    threshold = 1000000
    return "" if lower_bound < -threshold and upper_bound > threshold
    return "#{name} > #{lower_bound.to_i}" if upper_bound > threshold
    return "#{name} < #{upper_bound.to_i}" if lower_bound < -threshold
    return "#{lower_bound.to_i} < #{name} < #{upper_bound.to_i}"
  end
end