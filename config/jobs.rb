require File.expand_path("../environment", __FILE__)

# MAPLE_PATH = 'usr/local/maple12/bin'
MAPLE_PATH = '/Library/Frameworks/Maple.framework/Versions/15/bin'

job "polynomial.maple_computation" do |args|  
    filename = args['filename']
    # compute
    options = ' -qi lib/hsum13.mpl'
    output = `#{MAPLE_PATH + '/maple' + options + ' < ' + filename}`
    
    # delete file
    File.delete(filename)
end