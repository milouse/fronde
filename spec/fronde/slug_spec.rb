# frozen_string_literal: true

describe Fronde::Slug do
  context 'with various titles' do
    it 'transliterates them into slugs', core: true do
      expect(described_class.slug('toto')).to eq('toto')
      expect(described_class.slug('TotO')).to eq('toto')
      expect(described_class.slug('Tôto')).to eq('toto')
      expect(described_class.slug('Tôto tata')).to eq('toto-tata')
      expect(described_class.slug('ÀùéïỸç/+*= truñlu°`')).to eq('aueiyc-trunlu')
    end
  end
end
