# frozen_string_literal: true

# rubocop:disable Style/FormatStringToken
describe 'With working org files' do
  it 'should parse without date', core: true do
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.title).to eq('My sweet article')
    expect(o.date).to be_nil
    expect(o.timekey).to eq('00000000000000')
    expect(o.format('%i - (%t)')).to eq(' - (My sweet article)')
  end

  it 'should parse with a partial date', core: true do
    o = Neruda::OrgFile.new('spec/data/test2.org')
    expect(o.title).to eq('My second article')
    expect(o.date).to eq(DateTime.strptime('2019-06-11 00:00:00', '%Y-%m-%d %H:%M:%S'))
    expect(o.timekey).to eq('20190611000000')
    expect(o.datestring(:short)).to eq('2019-06-11')
    expect(o.format('%i - (%t)')).to eq('2019-06-11 - (My second article)')
    expect(o.lang).to eq('es')
  end

  it 'should parse with a complete date', core: true do
    o = Neruda::OrgFile.new('spec/data/test3.org')
    expect(o.title).to eq('My third article')
    expect(o.date).to eq(DateTime.strptime('2019-06-11 23:42:10', '%Y-%m-%d %H:%M:%S'))
    expect(o.timekey).to eq('20190611234210')
    expect(o.datestring(:short)).to eq('2019-06-11')
    expect(o.format('%i - (%t)')).to eq('2019-06-11 - (My third article)')
  end
end
# rubocop:enable Style/FormatStringToken

describe 'With various titles' do
  it 'should transliterate them into slugs', core: true do
    expect(Neruda::OrgFile.slug('toto')).to eq('toto')
    expect(Neruda::OrgFile.slug('TotO')).to eq('toto')
    expect(Neruda::OrgFile.slug('Tôto')).to eq('toto')
    expect(Neruda::OrgFile.slug('Tôto tata')).to eq('toto-tata')
    expect(Neruda::OrgFile.slug('ÀùïỸç/+*= trulu°`')).to eq('auiyc-trulu')
  end
end

describe 'Without a working file' do
  after(:each) do
    FileUtils.rm 'spec/data/__test__.org', force: true
  end

  it 'should raise if file_name is nil', core: true do
    expect { Neruda::OrgFile.new(nil) }.to raise_error(ArgumentError)
    expect { Neruda::OrgFile.new('') }.to raise_error(ArgumentError)
  end

  it 'should return a new org file structure', core: true do
    now = DateTime.now
    o = Neruda::OrgFile.new('spec/data/__test__.org', title: 'test')
    expect(o.title).to eq('test')
    o_date_str = o.date.strftime('%Y-%m-%d %H:%M')
    expect(o_date_str).to eq(now.strftime('%Y-%m-%d %H:%M'))
    date = o.date.strftime('%Y-%m-%d %a. %H:%M:%S')
    expect(o.author).not_to be_nil
    expect(o.author).to be_an_instance_of(String)
    o.write
    empty_content = <<~CONTENT
      #+title: test
      #+date: <#{date}>
      #+author: #{o.author}
      #+language: en

    CONTENT
    expect(File.exist?('spec/data/__test__.org')).to be(true)
    expect(IO.read('spec/data/__test__.org')).to eq(empty_content)
  end
end

describe 'With configuration' do
  after(:each) do
    # Reset config
    Neruda::Config.load_test({})
  end

  it 'should respect author name', core: true do
    Neruda::Config.load_test('author' => 'Test')
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.author).to eq('Test')
    o = Neruda::OrgFile.new('spec/data/test2.org')
    expect(o.author).to eq('Titi')
    o = Neruda::OrgFile.new('spec/data/test3.org')
    expect(o.author).to eq('Test')
  end

  it 'should compute the right html_file path for existing sources', core: true do
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.html_file).to eq('/data/test1.html')
    Neruda::Config.load_test('domain' => 'http://perdu.com')
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.html_file).to eq('/data/test1.html')
    o = Neruda::OrgFile.new('spec/data/content.org')
    expect(o.html_file).to eq('/data/content.html')
    # The following are weird tests. We begin to test theoric stuff here
    Neruda::Config.load_test('domain' => 'http://perdu.com',
                             'blog_path' => 'data')
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.html_file).to eq('/data/test1.html')
    o = Neruda::OrgFile.new('spec/data/content.org')
    expect(o.html_file).to eq('/data/content.html')
  end

  it 'should compute the right url for existing sources', core: true do
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.url).to eq('/data/test1.html')
    Neruda::Config.load_test('domain' => 'http://perdu.com')
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.url).to eq('http://perdu.com/data/test1.html')
    o = Neruda::OrgFile.new('spec/data/content.org')
    expect(o.url).to eq('http://perdu.com/data/content.html')
    # The following are weird tests. We begin to test theoric stuff here
    Neruda::Config.load_test('domain' => 'http://perdu.com',
                             'blog_path' => 'data')
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.url).to eq('http://perdu.com/data/test1.html')
    o = Neruda::OrgFile.new('spec/data/content.org')
    expect(o.url).to eq('http://perdu.com/data/content.html')
  end

  it 'should compute the right html_file path for theoritical sources', core: true do
    expect(Neruda::OrgFile.target_for_source('src/test.org')).to \
      eq('public_html/test.html')
    expect(Neruda::OrgFile.target_for_source('src/blog/test.org')).to \
      eq('public_html/blog/test.html')
    expect(Neruda::OrgFile.target_for_source('src/blog/toto/tata.org')).to \
      eq('public_html/blog/toto/tata.html')
    expect(Neruda::OrgFile.target_for_source('src/blog/toto/content.org')).to \
      eq('public_html/blog/toto/content.html')
    expect(Neruda::OrgFile.target_for_source('~/tata/tutu/content.org')).to \
      eq('public_html/tutu/content.html')
    expect(Neruda::OrgFile.target_for_source('~/tata/blog/content.org')).to \
      eq('public_html/blog/content.html')
  end

  it 'should compute the right source path for theoritical targets', core: true do
    expect(Neruda::OrgFile.source_for_target('public_html/test.html')).to \
      eq('src/test.org')
    expect(Neruda::OrgFile.source_for_target('public_html/blog/test.html')).to \
      eq('src/blog/test.org')
    lp = 'public_html/blog/toto/tata.html'
    expect(Neruda::OrgFile.source_for_target(lp)).to \
      eq('src/blog/toto/tata.org')
    lp = 'public_html/blog/toto/content.html'
    expect(Neruda::OrgFile.source_for_target(lp)).to \
      eq('src/blog/toto/content.org')
  end
end
