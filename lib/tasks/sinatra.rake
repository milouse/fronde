namespace :sinatra do
  desc 'Stop the underlaying sinatra application'
  task :stop do
    unless File.exist? 'tmp/pids/neruda.pid'
      STDERR.puts 'No pid file found'
      exit 1
    end
    pid = IO.read('tmp/pids/neruda.pid').strip.to_i
    Process.kill('TERM', pid)
    File.unlink 'tmp/pids/neruda.pid'
  end

  desc 'Start the underlaying sinatra application'
  task :start do
    loc_env = ENV['APP_ENV'] || 'development'
    cmd = ['rackup', "-E #{loc_env}", '-P', 'tmp/pids/neruda.pid']
    cmd << '-D' if loc_env == 'production'
    exec cmd.join(' ')
  end
end
