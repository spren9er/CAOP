class PolynomialsController < ApplicationController

  before_filter :set_menu_item

  def index
    nonq = Category.where(sid: 'polynomials').first
    @polynomials = nonq.polynomials
  end
  
  def show
    nonq = Category.where(sid: 'polynomials').first
    @polynomials = nonq.polynomials
    @polynomial = Polynomial.where(sid: params[:id]).first
    
    respond_to do |format|
      format.html
      format.js 
    end
  end
  
  def compute
    @polynomial = Polynomial.where(sid: params[:id]).first
    render :text => @polynomial.compute({'a' => 1, 'b' => 2})  
  end
  
  private
  
  def set_menu_item
    @menu_item = 'polynomials'
  end
  
end