# frozen_string_literal: true

module Fronde
  # Contains method to generate URL compatible strings
  module Slug
    class << self
      def slug(title)
        title.downcase.tr(' ', '-')
             .encode('ascii', fallback: ->(k) { translit(k) })
             .gsub(/[^\w-]/, '').delete_suffix('-')
      end

      def translit(char)
        return 'a' if %w[á à â ä ǎ ã å].include?(char)
        return 'ae' if char == 'æ'
        return 'e' if %w[é è ê ë ě ẽ €].include?(char)
        return 'i' if %w[í ì î ï ǐ ĩ].include?(char)
        return 'o' if %w[ó ò ô ö ǒ õ ø].include?(char)
        return 'oe' if char == 'œ'
        return 'u' if %w[ú ù û ü ǔ ũ].include?(char)
        return 'y' if %w[ý ỳ ŷ ÿ ỹ].include?(char)
        return 'c' if %w[ç © 🄯].include?(char)
        return 'n' if char == 'ñ'
        return 'ss' if char == 'ß'
        return 'r' if char == '®'
        return 'tm' if char == '™'

        '-'
      end
    end
  end
end
