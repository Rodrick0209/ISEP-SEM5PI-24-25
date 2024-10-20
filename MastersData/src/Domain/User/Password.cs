using System;
using DDDSample1.Domain.Shared;
using System.Text.RegularExpressions;

namespace DDDSample1.Domain.User
{

    public class Password : IValueObject
    {
        public string password { get; set; }         
        public Password(string password)
        {
            validatesPassword(password);
            this.password = password;
        }

        private void validatesPassword(string password)
        {
            if (password.Length < 10)
            {
                throw new BusinessRuleValidationException("Password must be at least 10 characters long");
            }
            if (!Regex.IsMatch(password, @"[0-9]"))
            {
                throw new BusinessRuleValidationException("Password must contain at least one digit");
            }
            if (!Regex.IsMatch(password, @"[A-Z]"))
            {
                throw new BusinessRuleValidationException("Password must contain at least one capital letter");
            }
            if (!Regex.IsMatch(password, @"[!@#$%^&*()_+}{:|>?<]"))
            {
                throw new BusinessRuleValidationException("Password must contain at least one special character");
            }
        }
    }

}