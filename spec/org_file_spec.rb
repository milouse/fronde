# coding: utf-8
# frozen_string_literal: true

# rubocop:disable Style/FormatStringToken, Metric/LineLength
describe 'With working org files' do
  it 'should parse without date' do
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.title).to eq('My sweet article')
    expect(o.date).to be_nil
    expect(o.timekey).to eq('00000000000000')
    expect(o.format('%i - (%t)')).to eq(' - (My sweet article)')
  end

  it 'should parse with a partial date' do
    o = Neruda::OrgFile.new('spec/data/test2.org')
    expect(o.title).to eq('My second article')
    expect(o.date).to eq(DateTime.strptime('2019-06-11 00:00:00', '%Y-%m-%d %H:%M:%S'))
    expect(o.timekey).to eq('20190611000000')
    expect(o.format('%i - (%t)')).to eq('2019-06-11 - (My second article)')
  end

  it 'should parse with a complete date' do
    o = Neruda::OrgFile.new('spec/data/test3.org')
    expect(o.title).to eq('My third article')
    expect(o.date).to eq(DateTime.strptime('2019-06-11 23:42:10', '%Y-%m-%d %H:%M:%S'))
    expect(o.timekey).to eq('20190611234210')
    expect(o.format('%i - (%t)')).to eq('2019-06-11 - (My third article)')
  end
end
# rubocop:enable Style/FormatStringToken, Metric/LineLength

describe 'With various titles' do
  it 'should transliterate them into slugs' do
    expect(Neruda::OrgFile.slug('toto')).to eq('toto')
    expect(Neruda::OrgFile.slug('TotO')).to eq('toto')
    expect(Neruda::OrgFile.slug('Tôto')).to eq('toto')
    expect(Neruda::OrgFile.slug('Tôto tata')).to eq('toto-tata')
    expect(Neruda::OrgFile.slug('ÀùïỸç/+*= trulu°`')).to eq('auiyc-trulu')
  end
end

describe 'With configuration' do
  after(:each) do
    # Reset config
    Neruda::Config.load_test({})
  end

  it 'should respect author name' do
    Neruda::Config.load_test('author' => 'Test')
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.author).to eq('Test')
    o = Neruda::OrgFile.new('spec/data/test2.org')
    expect(o.author).to eq('Titi')
    o = Neruda::OrgFile.new('spec/data/test3.org')
    expect(o.author).to eq('Test')
  end

  it 'should compute the right html_file path for existing sources' do
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

  it 'should compute the right url for existing sources' do
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

  it 'should compute the right html_file path for theoritical sources' do
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
end
