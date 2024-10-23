using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.User
{
    public class ConfirmationDeletePatientToken : IValueObject
    {
        public string Token { get; private set; }

        public ConfirmationDeletePatientToken(string token)
        {
            if (string.IsNullOrWhiteSpace(token))
            {
                throw new ArgumentNullException("Token can't be null or empty.");
            }
            this.Token = token;
        }
    }
}