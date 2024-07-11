# frozen_string_literal: true

require_relative '../../../lib/fronde/cli/throbber'

describe Fronde::CLI::Throbber do
  let(:thread) { Thread.new { next } }

  before { Fronde::CONFIG.reset }

  it 'selects the right throbber', :aggregate_failures do
    throbber = described_class.new(thread, 'Test')
    expect(throbber.instance_variable_get(:@frames)).to(
      eq(['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
          '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂'])
    )

    Fronde::CONFIG.load_test('throbber' => 'default')
    throbber = described_class.new(thread, 'Test')
    expect(throbber.instance_variable_get(:@frames)).to(
      eq(['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
          '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂'])
    )

    Fronde::CONFIG.load_test('throbber' => 'wrong')
    throbber = described_class.new(thread, 'Test')
    expect(throbber.instance_variable_get(:@frames)).to(
      eq(['⠁ ⠂ ⠄ ⡀ ⠄ ⠂ ⠁', '⠂ ⠁ ⠂ ⠄ ⡀ ⠄ ⠂', '⠄ ⠂ ⠁ ⠂ ⠄ ⡀ ⠄',
          '⡀ ⠄ ⠂ ⠁ ⠂ ⠄ ⡀', '⠄ ⡀ ⠄ ⠂ ⠁ ⠂ ⠄', '⠂ ⠄ ⡀ ⠄ ⠂ ⠁ ⠂'])
    )

    Fronde::CONFIG.load_test('throbber' => 'basic')
    throbber = described_class.new(thread, 'Test')
    expect(throbber.instance_variable_get(:@frames)).to eq('-\|/')
  end

  it 'always compute a terminal size' do
    tests = [[true, 79], [false, 0], [nil, 0]]
    throbber = described_class.new(thread, 'Test')

    allow(throbber).to receive(:`).and_return('24 80')
    tests.each do |data|
      allow(throbber).to receive(:system).with('test -t 0').and_return(data[0])
      expect(throbber.send(:terminal_width)).to eq data[1]
    end
  end
end
