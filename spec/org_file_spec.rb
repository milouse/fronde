# frozen_string_literal: true

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

  it 'should respect author name' do
    Neruda::Config.load_test('author' => 'Test')
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.author).to eq('Test')
    o = Neruda::OrgFile.new('spec/data/test2.org')
    expect(o.author).to eq('Titi')
    o = Neruda::OrgFile.new('spec/data/test3.org')
    expect(o.author).to eq('Test')
  end
end
