CAOP::Application.routes.draw do
  match 'admin' => 'application#login'
  match 'logout' => 'application#logout'

  resources :polynomials do
    get 'compute', :on => :member
  
  end
  resources :qpolynomials do
    get 'compute', :on => :member
  end
    
  mathjax 'mathjax'
  
  # mount Stylr::Engine => "/stylr", :as => "stylr"
  
  root :to => 'application#index'
end