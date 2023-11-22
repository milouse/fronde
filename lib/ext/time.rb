# frozen_string_literal: true

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

    def no_time=(value)
      @no_time = value
    end

    # Returns the current Time instance as a localized long string.
    #
    # @param with_year [Boolean] wether or not the string must contain the
    #   year
    # @return [String] the localized Time string representation
    def l18n_long_date_string(with_year: true)
      locale = R18n.get.locale
      long_fmt = R18n.t.fronde.index.full_date_format(
        date: locale.format_date_full(self, year: with_year)
      )
      unless @no_time
        long_fmt = R18n.t.fronde.index.full_date_with_time_format(
          date: long_fmt, time: locale.time_format.delete('_').strip
        )
      end
      locale.strftime(self, long_fmt)
    end

    # Format the current Time as a HTML `time` tag showing a long date.
    #
    # @return [String] the HTML `time` tag
    def l18n_long_date_html
      "<time datetime=\"#{xmlschema}\">#{l18n_long_date_string}</time>"
    end
  end
end
