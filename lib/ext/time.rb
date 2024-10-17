# frozen_string_literal: true

# Monkey patch to add some helpers
module TimePatch
  refine Time do
    # Returns the current Time instance as a localized short string.
    #
    # @return [String] the localized Time string representation
    def l18n_short_date_string
      I18n.l to_date
    end

    # Format the current Time as a HTML `time` tag showing a short date.
    #
    # @return [String] the HTML `time` tag
    def l18n_short_date_html
      "<time datetime=\"#{xmlschema}\">#{l18n_short_date_string}</time>"
    end

    # Returns the current Time instance as a localized long string.
    #
    # @return [String] the localized Time string representation
    def l18n_long_date_string
      I18n.l self, format: :long
    end

    # Returns the current Time instance as a localized long string
    #   without year.
    #
    # @return [String] the localized Time string representation
    def l18n_long_date_no_year_string
      I18n.l self, format: :long_no_year
    end

    # Format the current Time as a HTML `time` tag showing a long date.
    #
    # @return [String] the HTML `time` tag
    def l18n_long_date_html
      "<time datetime=\"#{xmlschema}\">#{l18n_long_date_string}</time>"
    end
  end
end
