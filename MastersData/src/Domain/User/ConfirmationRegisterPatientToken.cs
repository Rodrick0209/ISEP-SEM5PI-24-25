using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.User
{
    public class ConfirmationRegisterPatientToken : IValueObject
    {
        public string Token { get; private set; }

        public ConfirmationRegisterPatientToken(string token)
        {
            if (string.IsNullOrEmpty(token)) throw new ArgumentException("Token cannot be null or empty.");
            this.Token = token;
        }
    }
}