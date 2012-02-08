class Stat
  include Mongoid::Document
  
  field :polynomial_id,       type: String
  field :polynomial_sid,      type: String
  field :ip_address,          type: String
  field :category,            type: String
  field :created_at,          type: Time
  
  attr_accessor :count
  
end