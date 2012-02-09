CAOP::Application.routes.draw do
  match 'admin' => 'application#login'
  match 'logout' => 'application#logout'

  resources :polynomials do
    member do 
      get 'compute'
      get 'plot'
    end
  end

  resources :qpolynomials do
    get 'compute', :on => :member
  end
  
  resources :stats, :only => :index
    
  mathjax 'mathjax'
  
  mount Stylr::Engine => "/stylr", :as => "stylr"
  
  root :to => 'application#index'
end