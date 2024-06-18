# frozen_string_literal: true

require_relative 'r18n'
using R18nPatch

using TimePatch

# A time emulator to keep the fact that the time information was
# missing, even if it behaves as a Time object for the given date.
class TimeNoTime < Time
  # Returns the current Time instance as a localized long string.
  #
  # @param with_year [Boolean] wether or not the string must contain the
  #   year
  # @return [String] the localized Time string representation
  def l18n_long_date_string(with_year: true)
    R18n.t.long_date_string(self, with_year: with_year, with_time: false)
  end

  def self.parse_no_time(string)
    strptime("#{string} 00:00:00", '%Y-%m-%d %H:%M:%S')
  end
end
