# frozen_string_literal: true

require 'fileutils'
require 'simplecov'

require 'liquid'

SimpleCov.start do
  # Activate new covering precision
  enable_coverage :branch

  # Remove dev only tools from coverage check
  add_filter ['lib/tasks/sync.rake']
  # Sort coverage results into usefull groups
  add_group 'Core libs', ['lib/fronde', 'lib/ext']
  add_group 'Rake tasks', 'lib/tasks'
  add_group 'Specs', 'spec'
end

ENV['LANG'] = 'en'
ENV['USER'] = 'alice'

require_relative '../lib/fronde/config'
require_relative '../lib/fronde/version'
require_relative '../lib/fronde/org'

def init_testing_environment
  FileUtils.mkdir_p 'tmp/website_testing'
  Dir.chdir 'tmp/website_testing'
  # Full path is now spec/tmp/website_testing
  # Thus we have to rewind 3 level to find fronde src directory
  rakefile = <<~RAKE
    # frozen_string_literal: true

    $LOAD_PATH.unshift('../../../lib')

    Dir.glob('../../../lib/tasks/*.rake').each { |r| import r }

    task default: 'site:build'
  RAKE
  File.write('Rakefile', rakefile)
end

def clean_testing_environment
  safe_files = %w[lib var Rakefile].freeze
  Dir['*'].each do |file|
    next if safe_files.include? file

    if File.directory? file
      FileUtils.rm_r file
      next
    end
    File.unlink file
  end
  FileUtils.rm_r 'var/tmp/timestamps', force: true # might be absent
end

def copy_org_tarball_to_fake_tmp
  tarball = File.expand_path 'tmp/org.tar.gz', __dir__
  FileUtils.mkdir_p 'var/tmp'
  FileUtils.cp tarball, 'var/tmp'
end

def copy_org_lisp_files_to_fake_tmp
  org_version_cookie = File.expand_path 'tmp/last_org_version', __dir__
  org_version = File.read org_version_cookie
  installed_files = File.expand_path "tmp/org-#{org_version}", __dir__
  FileUtils.mkdir_p 'lib'
  FileUtils.cp_r installed_files, 'lib/'
end

def init_fake_org_install
  tmp_dir = File.expand_path 'tmp', __dir__
  # It should be sufficient to have the tarball downloaded to assume it
  # has also already compiled Org.
  return if File.exist? "#{tmp_dir}/org.tar.gz"

  last_version = Fronde::Org.download tmp_dir
  Fronde::Org.compile(
    "#{tmp_dir}/org.tar.gz", last_version,
    "#{tmp_dir}/org-#{last_version}"
  )
end

def proof_content(filename)
  proof = File.expand_path "data/#{filename}", __dir__
  template = Liquid::Template.parse(File.read(proof))
  template.render(
    'test_dir' => Dir.pwd,
    'base_dir' => File.expand_path('../', __dir__),
    'version' => Fronde::VERSION,
    'org_version' => Fronde::Org.current_version
  )
end

def rake(verbose: false)
  rake = Rake.application
  rake.raw_load_rakefile
  Rake.verbose(verbose)
  rake.options.build_all = true
  rake.tasks.each(&:reenable)
  rake
end

def tear_down(path)
  Dir.chdir __dir__
  FileUtils.rm_r path, force: true
end

RSpec.configure do |config|
  config.before(:suite) do
    Dir.chdir __dir__
    FileUtils.mkdir 'tmp'
  end

  config.after(:suite) do
    Dir.chdir __dir__
    FileUtils.rm_r %w[tmp var], force: true
  end
end
