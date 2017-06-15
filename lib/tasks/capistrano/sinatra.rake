# frozen_string_literal: true

namespace :sinatra do
  namespace :restart do
    desc 'Restart the remote neruda application'
    task :remote do
      on roles(:app) do
        within release_path do
          if test("[ -e '#{release_path}/tmp/pids/sinatra.pid' ]")
            execute :pkill, '-F', 'tmp/pids/sinatra.pid'
          end
          execute :bundle, :exec, :rackup, '-E', fetch(:app_env),
                  '-P', 'tmp/pids/sinatra.pid', '-D'
        end
      end
    end
  end
end
