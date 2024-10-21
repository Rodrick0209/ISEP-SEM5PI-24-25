using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.User
{
    public class ConfirmationEditPatientToken : IValueObject
    {
        public string Token { get; private set; }

        public ConfirmationEditPatientToken(string token)
        {
            if (string.IsNullOrWhiteSpace(token))
            {
                throw new ArgumentNullException("Token can't be null or empty.");
            }
            this.Token = token;
        }
    }
}