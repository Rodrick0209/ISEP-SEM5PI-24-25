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
            if (string.IsNullOrWhiteSpace(password))
            {
                throw new BusinessRuleValidationException("Password can't be null or empty");
            }
            this.password = password;
        }
    }

}