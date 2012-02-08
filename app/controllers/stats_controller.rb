class StatsController < ApplicationController

  before_filter :check_authorized
  
  def index
    @menu_item = 'stats'
    polynomials_stats
    qpolynomials_stats
    latest_stats = Stat.order_by('created_at').reverse
    n = 5
    @latest_stats = []
    used_ip_addresses = []
    latest_stats.each do |s|
      if used_ip_addresses.include?(s.ip_address) 
        @latest_stats[-1].count += 1 if @latest_stats.present?
        next
      end
      s.count = 1
      used_ip_addresses << s.ip_address
      @latest_stats << s 
      break if @latest_stats.count > n-1
    end
  end

  private
  
  ['polynomials_stats', 'qpolynomials_stats'].each do |method_name|
    define_method method_name do
      stats = Stat.where(category: method_name.gsub('_stats',''))
      aggregated_stats = stats.inject({}) do |h, stat|
        h[stat.polynomial_id] ||= 0
        h[stat.polynomial_id] += 1
        h
      end
      instance_variable_set "@#{method_name}", aggregated_stats
    end 
  end
  
  def check_authorized
    render :nothing => true, :status => 500 unless session[:authorized]
    return
  end
  
end