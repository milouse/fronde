# frozen_string_literal: true

using TimePatch

# A time emulator to keep the fact that the time information was
# missing, even if it behaves as a Time object for the given date.
class TimeNoTime < Time
  # Returns the current Time instance as a localized long string
  #   without time.
  #
  # @return [String] the localized Time string representation
  def l18n_long_date_string
    I18n.l self, format: :long_no_time
  end

  # Returns the current Time instance as a localized long string
  #   without time nor year.
  #
  # @return [String] the localized Time string representation
  def l18n_long_date_no_year_string
    I18n.l self, format: :long_no_time_no_year
  end

  def self.parse_no_time(string)
    strptime("#{string} 00:00:00", '%Y-%m-%d %H:%M:%S')
  end
end
