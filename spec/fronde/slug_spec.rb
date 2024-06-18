# frozen_string_literal: true

require_relative '../../lib/fronde/slug'

describe Fronde::Slug do
  context 'with various titles' do
    it 'transliterates them into slugs' do
      {
        'toto' => 'toto',
        'TotO' => 'toto',
        'Tôto' => 'toto',
        'Tôto tata' => 'toto-tata',
        'ÀùéïỸç/+*= truñlu°`' => 'aueiyc-trunlu'
      }.each do |title, slug|
        expect(described_class.slug(title)).to eq slug
      end
    end
  end
end
