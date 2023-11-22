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
        return 'e' if %w[é è ê ë ě ẽ].include?(char)
        return 'i' if %w[í ì î ï ǐ ĩ].include?(char)
        return 'o' if %w[ó ò ô ö ǒ õ].include?(char)
        return 'u' if %w[ú ù û ü ǔ ũ].include?(char)
        return 'y' if %w[ý ỳ ŷ ÿ ỹ].include?(char)
        return 'c' if char == 'ç'
        return 'n' if char == 'ñ'

        '-'
      end
    end
  end
end
