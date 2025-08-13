class PasswordLock
  @password : (Int32 | String | Float64 | Nil)

  # Set a password
  def initialize(password)
    @password = password
  end

  # Encrypt the password
  def encrypt
    @password = do_encrypt(@password)
  end

  # Check if a password is correct
  def unlock?(testpass) : String?
    "Unlocked" if @password == do_encrypt(testpass)
  end

  private def do_encrypt(password)
    case password
    when Int32
      (password.as(Int32) / 2).round
    when String
      password.as(String).reverse
    when Float64
      password.as(Float64).* 4.0
    end
  end
end
