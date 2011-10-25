class Parameter
  include Mongoid::Document
  embedded_in :polynomial
  field :name,   type: String
  field :lower_bound, type: Float
  field :upper_bound, type: Float
end