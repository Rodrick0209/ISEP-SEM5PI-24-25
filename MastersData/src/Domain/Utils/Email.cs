using System;
using DDDSample1.Domain.Shared;
using System.Text.RegularExpressions;

namespace DDDSample1.Domain.Utils
{

    public class Email : IValueObject
    {

        public string email { get; private set; }

        public Email(string email)
        {
            if (email != null)
            {
                email = email.Trim();
                validateEmail(email);
            } else {
                throw new ArgumentNullException(nameof(email));
            }
            this.email = email;
        }

        private void validateEmail(string email)
        {
            //More validations can be added format,etc.....

            if (email.Length < 1)
            {
                throw new ArgumentNullException(nameof(email));
            }

        }
        
        public string getFirstPartOfEmail()
        {
            if (string.IsNullOrEmpty(this.email))
            {
                throw new ArgumentException("Email cannot be null or empty", nameof(this.email));
            }

            var atIndex = this.email.IndexOf('@');
            if (atIndex == -1)
            {
                throw new ArgumentException("Invalid email format", nameof(this.email));
            }

            return email.Substring(0, atIndex);
        }
    }
}