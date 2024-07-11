# frozen_string_literal: true

require_relative 'r18n'
using R18nPatch

# Monkey patch to add some helpers
module TimePatch
  refine Time do
    # Returns the current Time instance as a localized short string.
    #
    # @return [String] the localized Time string representation
    def l18n_short_date_string
      R18n.l to_date
    end

    # Format the current Time as a HTML `time` tag showing a short date.
    #
    # @return [String] the HTML `time` tag
    def l18n_short_date_html
      "<time datetime=\"#{xmlschema}\">#{l18n_short_date_string}</time>"
    end

    # Returns the current Time instance as a localized long string.
    #
    # @param with_year [Boolean] whether or not the string must contain
    #   the year
    # @return [String] the localized Time string representation
    def l18n_long_date_string(with_year: true)
      R18n.t.long_date_string(self, with_year: with_year)
    end

    # Format the current Time as a HTML `time` tag showing a long date.
    #
    # @return [String] the HTML `time` tag
    def l18n_long_date_html
      "<time datetime=\"#{xmlschema}\">#{l18n_long_date_string}</time>"
    end
  end
end
