source 'http://rubygems.org'

gem 'rails', '~> 3.1.3'
gem 'mongoid', '~> 2.3'
gem 'bson_ext', '~> 1.3'

if File.exist?(fname = File.join(File.dirname(__FILE__), 'tmp', 'local_contentr_path.txt'))
  gem 'contentr', path: File.new(fname).read.strip
else
  gem 'contentr', git: 'git://github.com/metaminded/contentr.git'
end
gem 'stylr', :git => 'git@github.com:provideal/stylr.git'

gem 'simple_form'
gem 'mathjax-rails'


# Gems used only for assets and not required
# in production environments by default.
group :assets do
  gem 'sass-rails', "  ~> 3.1.0"
  gem 'coffee-rails', "~> 3.1.0"
  gem 'uglifier'
end

gem 'jquery-rails'

# Use unicorn as the web server
# gem 'unicorn'

# Deploy with Capistrano
# gem 'capistrano'

# To use debugger
# gem 'ruby-debug19', :require => 'ruby-debug'

group :test do
  # Pretty printed test output
  gem 'turn', :require => false
end
