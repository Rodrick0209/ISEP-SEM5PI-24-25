using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.User
{
    public class ConfirmationEditPatientTokenExpiration : IValueObject
    {
        public DateTime ExpirationDate { get; private set; }

        public ConfirmationEditPatientTokenExpiration(DateTime expirationDate)
        {
            if(expirationDate < DateTime.Now) throw new ArgumentException("Expiration date cannot be in the past.");
            this.ExpirationDate = expirationDate;
        }
    }
}