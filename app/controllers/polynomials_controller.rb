class PolynomialsController < ApplicationController

  before_filter :set_menu_item
  before_filter :stats_log, :only => :show
  
  def index
    nonq = Category.where(sid: 'polynomials').first
    @polynomials = nonq.polynomials
  end
  
  def show
    nonq = Category.where(sid: 'polynomials').first
    @polynomials = nonq.polynomials
    @polynomial = Polynomial.where(sid: params[:id]).first
    @polynomial_title = @polynomial.name
    
    @definition = @polynomial.definition.split(' = ')
    @plot_params = @polynomial.parameters.inject({}) {|h, p| h[p.name.to_sym] = p.default; h}

    @diffeq = (@polynomial.type == 'continuous') ?  t('differential_equation') : t('difference_equation')
    @receq = t('recurrence_equation')
    
    respond_to do |format|
      format.html 
      format.js 
    end
  end
  
  def compute
    @polynomial = Polynomial.where(sid: params[:id]).first
    if @polynomial.hyper_check(params[:parameters], params[:type])
      result = @polynomial.compute(params[:parameters], params[:type])

      if result[:output].blank?
        render :text => 'maple_connection', :status => 500 
        return
      end
            
      if result[:output] =~ /[e|E]{1}rror/
        render :text => 'maple_failure', :status => 500 
        return
      end
                  
      render :json => result.to_json
    else
      render :text => 'invalid_parameters', :status => 500
    end
  end

  def plot
    nonq = Category.where(sid: 'polynomials').first
    @polynomials = nonq.polynomials
    @polynomial = Polynomial.where(sid: params[:id]).first
    
    @definition = @polynomial.definition.split(' = ')
    @plot_params = params[:parameters]
    @plot_factor = params[:factor] || 1
        
    respond_to do |format|
      format.html 
      format.js
    end
  end
    
  private
  
  def set_menu_item
    @menu_item = 'polynomials'
  end
  
end