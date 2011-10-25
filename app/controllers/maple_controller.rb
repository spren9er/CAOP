class MapleController < ActionController::Base
  
  # MAPLE_PATH = 'usr/local/maple12/bin'
  # MAPLE_PATH = 'ssh sprenger@pool-serv1 "/usr/local/maple12/bin/maple -q -s < /tmp/test.txt"'
  MAPLE_PATH = '/Library/Frameworks/Maple.framework/Versions/15/bin'
  
  def compute
    # put input in file
    input = params[:input]
    stamp = Time.now.to_i.to_s
    filename = 'tmp/computation' + (5*rand(9)).to_s + stamp.to_s + '.txt'
    file = File.new(filename, 'w')
    file.puts input
    
    # compute
    flags = ' -q -s '
    output = system(MAPLE_PATH + '/maple' + flags + ' < ' + filename)
    render :text => output
  end
end