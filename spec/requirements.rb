# frozen_string_literal: true

require 'fileutils'
require 'simplecov'

require 'r18n-core'
R18n.default_places = File.expand_path('../locales', __dir__)
R18n.set 'en'
R18n::Filters.on(:named_variables)

SimpleCov.start do
  # Activate new covering precision
  enable_coverage :branch

  # Remove dev only tools from coverage check
  add_filter ['lib/tasks/sync.rake']
  # Sort coverage results into usefull groups
  add_group 'Core libs', 'lib/fronde'
  add_group 'Rake tasks', 'lib/tasks'
  add_group 'Specs', 'spec'
end

# The following requires other components automatically
require 'fronde/org_file'
require 'fronde/utils'

ENV['LANG'] = 'en'
ENV['USER'] = 'alice'

# rubocop:disable Metrics/MethodLength
def init_testing_website
  FileUtils.mkdir_p 'tmp/website_testing'
  Dir.chdir 'tmp/website_testing'
  # Full path is now spec/tmp/website_testing
  # Thus we have to rewind 3 level to find fronde src directory
  rakefile = <<~RAKE
    # frozen_string_literal: true

    require 'r18n-core'
    R18n.default_places = '../../../locales'
    R18n.set 'en'
    R18n::Filters.on(:named_variables)

    $LOAD_PATH.unshift('../../../lib')

    Dir.glob('../../../lib/tasks/*.rake').each { |r| import r }

    task default: 'site:build'
  RAKE
  File.write('Rakefile', rakefile)
  config = <<~CONF
    ---
    author: Tata
    title: This is a website about test
    org-html:
      html-postamble: '<footer>Published by Fronde.</footer>'
    sources:
    - name: org
      path: src
      target: .
      recursive: false
      exclude: tata\\.org
    - path: src/news
      is_blog: true
    - path: 'titi/test'
      recursive: nil
      exclude: ugly\\.org
    - 'titi/tutu/tata'
    templates:
    - type: before
      selector: title
      content: |
        <meta property="fronde-test" content="test"/>
    preview:
      routes:
        /test: public_html/index.html
  CONF
  File.write('config.yml', config)
end
# rubocop:enable Metrics/MethodLength

def copy_org_tarball_to_fake_tmp
  tarball = File.expand_path('tmp/org.tar.gz', __dir__)
  FileUtils.mkdir_p 'var/tmp'
  FileUtils.cp tarball, 'var/tmp'
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
    FileUtils.rm_r ['tmp', 'var'], force: true
  end
end
