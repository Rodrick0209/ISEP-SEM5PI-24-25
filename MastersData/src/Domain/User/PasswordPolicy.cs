using System.Linq;

namespace DDDSample1.Domain.User
{
    public class PasswordPolicy
    {
        public static bool IsSatisfiedBy(string password)
        {
            if (password.Length < 10) // At least 10 characters long
            {
                return false;
            }
            if (!password.Any(char.IsUpper)) // At least one capital letter
            {
                return false;
            }

            if (!password.Any(char.IsDigit)) // At least one digit
            {
                return false;
            }

            return password.Any(ch => !char.IsLetterOrDigit(ch)); // At least one special character
        }
    }
}