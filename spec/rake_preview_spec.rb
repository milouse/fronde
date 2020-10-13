# frozen_string_literal: true

require 'rake'
require 'open-uri'

context 'when trying preview mode' do
  let(:now_str) { DateTime.now.strftime('%A %-d of %B, %Y at %R') }

  around do |test|
    init_testing_website
    copy_org_tarball_to_fake_tmp
    Neruda::Config.send(:load_settings)
    rake.invoke_task('org:install')

    Neruda::OrgFile.new(
      'src/index.org',
      title: 'My website',
      content: 'Nice content.'
    ).write

    rake.invoke_task('site:build')

    webrick_app = Thread.new do
      rake.invoke_task('site:preview')
    end
    sleep 1 # Necessary to let webrick start

    # Actually run test case
    test.run

    # Cleanup things
    webrick_app.exit # Be sure to kill test server
    webrick_app.join # Be patient before quitting example
  end

  after do
    Dir.chdir File.expand_path('..', __dir__)
    FileUtils.rm_r 'tmp/website_testing', force: true
  end

  it 'is viewable with preview', rake: true do
    home_page = URI('http://localhost:5000/index.html').open.read
    proof = File.expand_path('data/index_proof.html', __dir__)
    proof_content = IO.read(proof).gsub(/__PUB_DATE__/, now_str)
    expect(home_page).to eq(proof_content)
  end

  it 'serves index', rake: true do
    home_page = URI('http://localhost:5000/').open.read
    proof = File.expand_path('data/index_proof.html', __dir__)
    proof_content = IO.read(proof).gsub(/__PUB_DATE__/, now_str)
    expect(home_page).to eq(proof_content)
  end

  it 'is viewable with routes testing', rake: true do
    home_page = URI('http://localhost:5000/test').open.read
    proof = File.expand_path('data/index_proof.html', __dir__)
    proof_content = IO.read(proof).gsub(/__PUB_DATE__/, now_str)
    expect(home_page).to eq(proof_content)
  end

  it 'sends an error if a page is not found', rake: true do
    expect { URI('http://localhost:5000/not_found.html').open.read }.to(
      raise_error(/404 Not Found/)
    )
  end
end
