# frozen_string_literal: true

SAMPLE_CORRECT_ANSWER = <<~BODY
  <h2 class="package">org <a class="badge" href="org.xml"><img src="/images/rss.svg" alt="Atom Feed"></a></h2><dl><dt>Description</dt><dd>Outline-based notes management and organizer</dd>
  <dt>Latest</dt> <dd><a href="org-9.8.5.tar">org-9.8.5.tar</a> (<a href="org-9.8.5.tar.sig">.sig</a>), 2026-May-30, 9.98&nbsp;MiB</dd>
  <dt>Website</dt> <dd><a href="https://orgmode.org">https://orgmode.org</a></dd>
BODY

SAMPLE_BAD_ANSWER_1 = <<~BODY
  <h2 class="package">org <a class="badge" href="org.xml"><img src="/images/rss.svg" alt="Atom Feed"></a></h2><dl><dt>Description</dt><dd>Outline-based notes management and organizer</dd>
  <dt>Latest</dt> <dd><a href="another_page.html">Something change in format</a></dd>
  <dt>Website</dt> <dd><a href="https://orgmode.org">https://orgmode.org</a></dd>
BODY

SAMPLE_BAD_ANSWER_2 = <<~BODY
  Something goes wrong.
BODY

# class FakeHttp
#   class << self
#     def request(_); end
#   end
# end

class FakeResponse
  attr_reader :body

  def initialize(response)
    @body = response
  end
end

describe Fronde::Org do
  it 'fetches the correct version number' do
    allow(described_class).to(
      receive(:http_get_client)
        .and_return(FakeResponse.new(SAMPLE_CORRECT_ANSWER))
    )
    expect(described_class.fetch_version_number).to eq '9.8.5'
  end

  it 'handles bad version number' do
    allow(described_class).to(
      receive(:http_get_client)
        .and_return(FakeResponse.new(SAMPLE_BAD_ANSWER_1))
    )
    expect(described_class.fetch_version_number).to be_nil
  end

  it 'handles bad response' do
    allow(described_class).to(
      receive(:http_get_client)
        .and_return(FakeResponse.new(SAMPLE_BAD_ANSWER_2))
    )
    expect(described_class.fetch_version_number).to be_nil
  end

  it 'raises an error in download if no version is found',
     :aggregate_failures do
    allow(described_class).to(
      receive(:http_get_client)
        .and_return(FakeResponse.new(SAMPLE_BAD_ANSWER_1))
    )
    expect { described_class.download }.to raise_error RuntimeError
    expect { described_class.download }.to(
      raise_error('No remote Org version found')
    )
  end
end
