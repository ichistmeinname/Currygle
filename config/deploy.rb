# ----------------------------------------------------------------------------
# Initialization
# ----------------------------------------------------------------------------

# add path to cabal/cabal-dev binary
set :default_environment, { 'PATH' => "~/.ghc-config/ghc/bin:~/.cabal/i386/bin:$PATH" }

# ----------------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------------
set :application, "Curr(y)gle API search"

# Version control information
set :repository,  "ssh://git@git-ps.informatik.uni-kiel.de:55055/theses/2012-sad-ba.git"
set :scm       , :git
set :deploy_via, :remote_cache
set :branch    , "master"

# Deployment configuration
ssh_options[:forward_agent] = true  # use ssh keys
set :use_sudo, false                # no sudo!

set :domain   , "lynch.informatik.uni-kiel.de"
set :port     , 55055
set :deploy_to, "/srv/www/apps/currygle"

role :web, domain                   # HTTP server: Apache2
role :app, domain                   # Web server: Snap internal

# Overwrite shared children
set :shared_children, %w(CurrySearch/log CurrySearch/index CurrySearch/cabal-dev)

set :app_dir         , "CurrySearch"
set :current_app_path, File.join(current_path, app_dir)

# ----------------------------------------------------------------------------
# Deploy tasks
# ----------------------------------------------------------------------------

set :config_dir          , "conf"
set :config_file         , "prod.conf"
set :shared_config_path  , File.join(shared_path       , config_dir )
set :shared_config_file  , File.join(shared_config_path, config_file)

set :pid_file, "server.pid"
set :pid_path, File.join(shared_path, pid_file)

namespace :deploy do
  task :start do
    run "cd #{current_app_path} && make start"
  end

  task :stop do
    run "cd #{current_app_path} && make stop"
  end

  task :restart do
    run "cd #{current_app_path} && make restart"
  end

  task :status do
    run "cd #{current_app_path} && make status"
  end


  desc <<-DESC
  This task will make the release group-writable (if the :group_writable \
  variable is set to true, which is the default). It will then set up \
  symlinks to the shared directory for the log, system, and tmp/pids \
  directories, and will lastly touch all assets in public/images, \
  public/stylesheets, and public/javascripts so that the times are \
  consistent (so that asset timestamping works). This touch process \
  is only carried out if the :normalize_asset_timestamps variable is \
  set to true, which is the default The asset directories can be overridden \
  using the :public_children variable.
  DESC
  task :finalize_update, :except => { :no_release => true } do
    run "chmod -R g+w #{latest_release}" if fetch(:group_writable, true)

    # mkdir -p is making sure that the directories are there for some SCM's that don't
    # save empty folders
    shared_children.map do |d|
      if (d.rindex('/')) then
        run "rm -rf #{latest_release}/#{d} && mkdir -p #{latest_release}/#{d.slice(0..(d.rindex('/')))}"
      else
        run "rm -rf #{latest_release}/#{d}"
      end
      run "ln -s #{shared_path}/#{d.split('/').last} #{latest_release}/#{d}"
    end
  end

end

# ----------------------------------------------------------------------------
# Configuration tasks
# ----------------------------------------------------------------------------

namespace :pidfile do

  desc "Setup pidfile"
  task :setup do
    run "touch #{pid_path}"
  end

  desc "Link the PID file"
  task :create_symlink do
    cur_config_path = File.join(current_app_path, pid_file)
    run "rm -f   #{cur_config_path}"
    run "ln -nsf #{pid_path} #{cur_config_path}"
  end

end

# ----------------------------------------------------------------------------
# Indexer tasks
# ----------------------------------------------------------------------------

namespace :index do

  desc "Create new index"
  task :create do
    run "cd #{current_app_path} && make index"
  end

  desc "Update index"
  task :update do
    run "cd #{current_app_path} && make update-index"
  end

end

# ----------------------------------------------------------------------------
# Cabal tasks
# ----------------------------------------------------------------------------

depend :remote, :command, "ghc"
depend :remote, :command, "cabal"
depend :remote, :command, "cabal-dev"

namespace :cabal do

  task :default do
    install
  end

  task :update do
    cabal "update"
  end

  task :clean do
    cabal "clean"
  end

  task :configure do
    cabal "configure"
  end

  task :build do
    cabal "build"
  end

  task :install do
    cabal "install"
  end

end

def cabal(command)
  run "cd #{current_app_path} && cabal-dev #{command}"
end

# ----------------------------------------------------------------------------
# Task integration
# ----------------------------------------------------------------------------

after "deploy:setup"         , "pidfile:setup"
after "deploy:create_symlink", "pidfile:create_symlink"
after "deploy:update"        , "cabal"
