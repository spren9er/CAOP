class PolynomialsController < ApplicationController

  before_filter :set_menu_item

  def index
  end
  
  private
  
  def set_menu_item
    @menu_item = 'polynomials'
  end
  
end