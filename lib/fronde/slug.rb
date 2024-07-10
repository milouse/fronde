# frozen_string_literal: true

module Fronde
  # Contains method to generate URL compatible strings
  module Slug
    class << self
      def slug(title)
        title.downcase
             .encode('ascii', fallback: ->(k) { translit(k) })
             .encode('utf-8') # Convert back to utf-8 string
             .gsub(/[^\w-]/, '-')
             .squeeze('-')
             .delete_suffix('-')
      end

      # rubocop:disable Metrics/CyclomaticComplexity
      # rubocop:disable Metrics/MethodLength
      def translit(char)
        case char
        when 'á', 'à', 'â', 'ä', 'ǎ', 'ã', 'å'
          'a'
        when 'é', 'è', 'ê', 'ë', 'ě', 'ẽ', '€'
          'e'
        when 'í', 'ì', 'î', 'ï', 'ǐ', 'ĩ'
          'i'
        when 'ó', 'ò', 'ô', 'ö', 'ǒ', 'õ', 'ø'
          'o'
        when 'ú', 'ù', 'û', 'ü', 'ǔ', 'ũ'
          'u'
        when 'ý', 'ỳ', 'ŷ', 'ÿ', 'ỹ'
          'y'
        when 'ç', '©', '🄯'
          'c'
        when 'ñ'
          'n'
        when 'ß'
          'ss'
        when 'œ'
          'oe'
        when 'æ'
          'ae'
        when '®'
          'r'
        when '™'
          'tm'
        else
          '-'
        end
      end
      # rubocop:enable Metrics/CyclomaticComplexity
      # rubocop:enable Metrics/MethodLength
    end
  end
end
