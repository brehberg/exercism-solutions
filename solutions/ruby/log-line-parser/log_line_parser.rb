class LogLineParser
  def initialize(line)
    @line = line
  end

  # Get message from a log line
  def message
    @line.gsub(/\[\w+\]:/m, '').strip
  end

  # Get log level from a log line
  def log_level
    @line.gsub(/\[(\w+)\]:.*/m, '\1').downcase
  end

  # Reformat a log line
  def reformat
    "#{message} (#{log_level})"
  end
end
