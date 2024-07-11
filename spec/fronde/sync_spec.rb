# frozen_string_literal: true

require_relative '../../lib/fronde/sync'

describe Fronde::Sync do
  before do
    FileUtils.mkdir_p 'tmp/no_config'
    Dir.chdir 'tmp/no_config'
    Fronde::CONFIG.reset
  end

  after do
    tear_down 'tmp/no_config'
  end

  it 'raises an error when no remote is found' do
    expect { described_class.extract_method_and_remote('html') }.to(
      raise_error(
        Fronde::Sync::Error, 'No html remote path set'
      )
    )
  end

  it 'detects default rsync remote' do
    Fronde::CONFIG.load_test(
      'html_remote' => 'test@example.com:/var/www/site/'
    )
    expect(described_class.extract_method_and_remote('html')).to(
      eq(%w[rsync test@example.com:/var/www/site/])
    )
  end

  it 'defaults to rsync' do
    Fronde::CONFIG.load_test(
      'html_remote' => 'test:test@example.com'
    )
    expect(described_class.extract_method_and_remote('html')).to(
      eq(%w[rsync test:test@example.com])
    )
  end

  it 'detects rsync remote' do
    Fronde::CONFIG.load_test(
      'html_remote' => 'rsync:test@example.com:/var/www/site/'
    )
    expect(described_class.extract_method_and_remote('html')).to(
      eq(%w[rsync test@example.com:/var/www/site/])
    )
  end

  it 'detects neocities remote' do
    Fronde::CONFIG.load_test('html_remote' => 'neocities:test')
    expect(described_class.extract_method_and_remote('html')).to(
      eq(%w[neocities test])
    )
  end

  it 'detects neocities with server remote' do
    Fronde::CONFIG.load_test('html_remote' => 'neocities:test@example.org')
    expect(described_class.extract_method_and_remote('html')).to(
      eq(%w[neocities test@example.org])
    )
  end

  it 'pulls data from remote' do
    allow(Kernel).to receive(:system) { |*args| args.join(' ') }
    Fronde::CONFIG.load_test(
      'html_remote' => 'test:test@example.com',
      'rsync' => ['/usr/local/bin/custom']
    )
    pull_thread = described_class.pull_or_push(:pull, 'html')
    expect(pull_thread.value).to eq \
      '/usr/local/bin/custom test:test@example.com public_html/'
  end

  it 'pushes data to remote' do
    allow(Kernel).to receive(:system) { |*args| args.join(' ') }
    Fronde::CONFIG.load_test(
      'html_remote' => 'test:test@example.com'
    )
    push_thread = described_class.pull_or_push(:push, 'html')
    expect(push_thread.value).to eq \
      'rsync -qrlt --delete public_html/ test:test@example.com'
  end

  it 'tests a push to remote verbosely' do
    allow(Kernel).to receive(:system) { |*args| args.join(' ') }
    Fronde::CONFIG.load_test(
      'html_remote' => 'test:test@example.com'
    )
    push_thread = described_class.pull_or_push(
      :push, 'html', test: true, verbose: true
    )
    expect(push_thread.value).to eq \
      'rsync -nvrlt --delete public_html/ test:test@example.com'
  end
end
