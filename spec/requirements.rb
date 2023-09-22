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

# The following requires other components automatically
require 'fronde/utils'
require 'fronde/org_file'

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

def copy_org_tarball_to_fake_tmp
  tarball = File.expand_path('tmp/org.tar.gz', __dir__)
  FileUtils.mkdir_p 'var/tmp'
  FileUtils.cp tarball, 'var/tmp'
end

def proof_content(filename)
  proof = File.expand_path("data/#{filename}", __dir__)
  template = Liquid::Template.parse(File.read(proof))
  template.render(
    'test_dir' => Dir.pwd,
    'base_dir' => File.expand_path('../', __dir__),
    'version' => Fronde::VERSION,
    'org_version' => Fronde::CONFIG.org_last_version
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
    Fronde::Utils.download_org('tmp')
  end

  config.after(:suite) do
    Dir.chdir __dir__
    FileUtils.rm_r %w[tmp var], force: true
  end
end
