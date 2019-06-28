# frozen_string_literal: true

require 'rake'
require 'fileutils'

describe 'With working org files' do
  before(:all) do
    init_testing_website
    @rake = install_testing_rake
    o = Neruda::OrgFile.new('src/index.org', 'title' => 'My website')
    o.write
  end

  before(:each) do
    @rake.options.build_all = true
    @rake.tasks.each(&:reenable)
  end

  after(:all) do
    Dir.chdir File.expand_path('../', __dir__)
    FileUtils.rm_r 'spec/data/website_testing', force: true
  end

  it 'should build something' do
    @rake.invoke_task('site:build')
    expect(File.exist?('public_html/index.html')).to be(true)
  end
end
