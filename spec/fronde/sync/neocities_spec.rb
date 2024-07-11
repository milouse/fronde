# frozen_string_literal: true

require 'net/http/status'

require_relative '../../../lib/fronde/sync/neocities'

def write_fake_credentials(data = {})
  File.write '.credentials', data.to_yaml
end

def build_fake_http_response(data = {}, code = 200)
  klass = Net::HTTPResponse::CODE_TO_OBJ[code.to_s]
  status = Net::HTTP::STATUS_CODES[code]
  response = klass.new('1.1', code, status)
  response.body = data.to_json
  response.instance_variable_set(:@read, true)
  response
end

def generate_public_dir_content
  FileUtils.mkdir_p 'test/pictures'
  FileUtils.touch(['test/index.html', 'test/pictures/cat.jpg'])
end

class FakeHttp
  attr_accessor :use_ssl, :verify_mode
  attr_writer :request, :started

  def started?
    @started
  end

  def start
    true
  end

  def finish
    true
  end

  def get(uri)
    response = build_fake_http_response
    if uri == '/'
      response.body = '>>/<<'
    else
      response.body = 'Hello World!'
    end
    response
  end

  def request(request)
    if request.path == '/api/list'
      fake_remote_list = [
        { 'path' => 'index.html',
          'is_directory' => false,
          'updated_at' => '2022-04-12 12:42:42 UTC',
          'size' => 0,
          'sha1_hash' => 'da39a3ee5e6b4b0d3255bfef95601890afd80709' },
        { 'path' => 'pictures',
          'is_directory' => true,
          'updated_at' => '2022-04-12 12:42:42 UTC' },
        { 'path' => 'pictures/cat.jpg',
          'is_directory' => false,
          'updated_at' => '2022-04-12 12:42:42 UTC',
          'size' => 0,
          'sha1_hash' => 'da39a3ee5e6b4b0d3255bfef95601890afd80709' },
        { 'path' => 'pictures/bird.jpg',
          'is_directory' => false,
          'updated_at' => '2022-04-12 12:42:42 UTC',
          'size' => 0,
          'sha1_hash' => 'da39a3ee5e6b4b0d3255bfef95601890afd80709' }
      ]
      return build_fake_http_response(files: fake_remote_list)
    end
    @request
  end
end

describe Fronde::Sync::Neocities do
  let(:fake_http) { FakeHttp.new }

  before do
    allow(Net::HTTP).to receive(:new).and_return(fake_http)

    FileUtils.mkdir_p 'tmp/neocities'
    Dir.chdir 'tmp/neocities'
    Fronde::CONFIG.reset
    write_fake_credentials('test_neocities_pass' => 'super secret')
  end

  after do
    FileUtils.rm_f '.credentials'

    tear_down 'tmp/neocities'
  end

  it 'fails when no credentials file is available' do
    FileUtils.rm_f '.credentials' # Remove it to fails the test
    expect { described_class.new('test@neocities.org', './test') }.to \
      raise_error(Errno::ENOENT)
  end

  it 'fails when no password is available in credentials file' do
    write_fake_credentials # Generate an empty credentials file
    expect { described_class.new('test@neocities.org', './test') }.to \
      raise_error(KeyError)
  end

  it 'works with a password in credentials file' do
    # Use default credentials test generated in before block
    expect { described_class.new('test@neocities.org', './test') }.not_to \
      raise_error
  end

  it 'generates an HTTP Auth string' do
    neocities = described_class.new('test@example.org', './test')
    expect(neocities.instance_variable_get(:@authorization)).to eq \
      'Basic dGVzdDpzdXBlciBzZWNyZXQ='
  end

  it 'connects to neocities', :aggregate_failures do
    fake_http.started = true
    fake_http.request = build_fake_http_response(info: { sitename: 'youpi' })

    neocities = described_class.new('test@example.org', './test')
    expect(neocities.info['sitename']).to eq 'youpi'
    expect(neocities.finish).to be true
  end

  it 'can fail to login to neocities' do
    fake_http.started = false
    fake_http.request = build_fake_http_response({ result: 'error' }, 403)

    neocities = described_class.new('test@example.org', './test')
    expect { neocities.info['sitename'] }.to \
      raise_error(RuntimeError, '{"result"=>"error"}')
  end

  it 'connects to neocities in a block' do
    fake_http.request = build_fake_http_response(info: { sitename: 'youpi' })

    info = nil
    described_class.new('test@example.org', './test') do |neocities|
      info = neocities.info
    end
    expect(info['sitename']).to eq 'youpi'
  end

  it 'lists local files', :aggregate_failures do
    generate_public_dir_content
    neocities = described_class.new('test@example.org', './test')
    file_list = neocities.local_list
    expect(file_list.map { _1['path'] }).to \
      eq(['index.html', 'pictures', 'pictures/cat.jpg'])
    expect(file_list.map { _1['is_directory'] }).to \
      eq([false, true, false])
  end

  it 'lists remote files', :aggregate_failures do
    neocities = described_class.new('test@example.org', './test')
    file_list = neocities.remote_list
    expect(file_list.map { _1['path'] }).to \
      eq(['index.html', 'pictures', 'pictures/cat.jpg', 'pictures/bird.jpg'])
    expect(file_list.map { _1['is_directory'] }).to \
      eq([false, true, false, false])
  end

  it 'computes orphans', :aggregate_failures do
    generate_public_dir_content
    neocities = described_class.new('test@example.org', './test')
    # pictures/bird.jpg is only on remote and should appear as orphan
    # from local point of view.
    file_list = neocities.send(
      :select_orphans, neocities.local_list, neocities.remote_list
    ).to_a
    expect(file_list).to eq(['pictures/bird.jpg'])
    # pictures/bird.jpg is only on remote and should not appear as orphan
    # from remote point of view as it is expected to be transferred in
    # the next pull operation.
    file_list = neocities.send(
      :select_orphans, neocities.remote_list, neocities.local_list
    ).to_a
    expect(file_list).to eq([])
  end

  it 'saves a new file with content', :aggregate_failures do
    generate_public_dir_content
    neocities = described_class.new('test@example.org', './test')
    expect(neocities.local_list.length).to eq 3
    neocities.send(
      :save_file, 'test/test.html', 'Hello World!',
      Time.new(2022, 4, 12, 12, 42, 42)
    )
    expect(File.exist?('test/test.html')).to be true
    expect(File.read('test/test.html')).to eq 'Hello World!'
    expect(neocities.local_list.length).to eq 4
  end

  it 'downloads a file' do
    neocities = described_class.new('test@example.org', './test')
    content = neocities.send(
      :fetch_file_content, fake_http, '/hello.html',
      Digest::SHA1.hexdigest('Hello World!')
    )
    expect(content).to eq 'Hello World!'
  end

  it 'downloads / instead of /index.html' do
    neocities = described_class.new('test@example.org', './test')
    content = neocities.send(
      :fetch_file_content, fake_http, '/index.html',
      Digest::SHA1.hexdigest('>>/<<')
    )
    expect(content).to eq '>>/<<'
  end

  it 'fails to download a file if checksum differ', :aggregate_failures do
    neocities = described_class.new('test@example.org', './test')
    content = neocities.send(
      :fetch_file_content, fake_http, '/hello.html',
      Digest::SHA1.hexdigest('Something else')
    )
    expect(content).to be_nil
    expect do
      neocities.send(
        :fetch_file_content, fake_http, '/hello.html',
        Digest::SHA1.hexdigest('Something else')
      )
      # neocities removes the .html suffix
    end.to output("SHA1 hash differ for /hello\n").to_stderr
  end
end
